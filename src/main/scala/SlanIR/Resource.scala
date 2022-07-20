/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.CreateLogger
import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, IncorrectParameter, IncorrectSlanSpecStructure, MissingDefinition}
import HelperUtils.ExtentionMethods.*
import SlanIR.{EntityId, EntityOrError}
import Translator.SlanAbstractions.{ResourceReference, SlanConstructs, StorageTypeReference, YamlPrimitiveTypes}
import Translator.SlanConstruct.SlanError
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.kernel.Eq
import cats.kernel.Eq.catsKernelEqForOption
import cats.syntax.*
import cats.syntax.eq.catsSyntaxEq
import org.slf4j.Logger

import scala.collection.mutable.Map

trait Resource extends SlanEntity

case class ResourceRecord(id: EntityId, storage: StorageTypeReference, compositesOrValues: SlanEntityValidated[Option[List[Resource] | List[StoredValue]]]) extends SlanEntity(id), Resource

case class ProducerConsumer(id: EntityId, storage: Option[ResourceStorage], compositesOrValues: Option[List[Resource] | List[StoredValue]]) extends SlanEntity(id), Resource

case class Generator(id: EntityId, pdf: String) extends SlanEntity(id), Resource

case class StoredValue(content: (YamlPrimitiveTypes, YamlPrimitiveTypes) | YamlPrimitiveTypes)

object Resource:
  private val bookkeeper = new EntityBookkeeper[Resource]
  def apply(id: EntityId): Option[Resource] = bookkeeper.get(id)

  /*
  * Resources can be defined under the section Resources or as Fields in a message. In both
  * cases they are wrapped either in the case classes Resources or Fields at the top level. However, for each
  * resource attributes can be defined as either values or nested resources, not both at the same time.
  * Therefore, if the check of the parameter translated reveals that it contains List(Resources(_var_))
  * then the content of _var_ is of the type List[Translator.SlanConstruct.Resource] that contains globally
  * defined resources under Agents.Resources. Otherwise, it is expected that the list of SlanConstruct is the list
  * of message fields or resource attributes, i.e., a composite resource contains nested resources. Global resources
  * are entered into the bookkeeper table whereas local resources are stored in the corresponding SLAN IR data type.
  * */
  def apply(translated: SlanConstructs): SlanEntityValidated[Int] =
    checkForResourcesCaseClass(translated).andThen(resources => SlanResources2IR(resources)).andThen(rr => constructResources(rr))

  private def checkForResourcesCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val filteredResources = translated.filter(entity => entity.isInstanceOf[Translator.SlanConstruct.Resources])
    Validated.condNel(filteredResources.containsHeadOnly, filteredResources, IncorrectSlanSpecStructure("global entry Resources"))

  private def constructResources(resources: Option[List[ResourceRecord] | List[StoredValue]]): SlanEntityValidated[Int] =
    resources match
      case Some(v) if v.isInstanceOf[List[_]] => resourcesRecordProcessor(v)
      case Some(e) => IncorrectParameter(e.toString).invalidNel
      case None => IncorrectParameter("no resources specified").invalidNel

  private def resourcesRecordProcessor(rrlst: List[Resource] | List[StoredValue]): SlanEntityValidated[Int] =
    if rrlst.count(elem => elem.isInstanceOf[ResourceRecord]) =!= rrlst.length then
      IncorrectParameter("only top level resources must be specified, not their values").invalidNel
    else
      rrlst.foreach {
        rr =>
          val rrObj = rr.asInstanceOf[ResourceRecord]
          resourceRecordProcessor(rrObj.id, rrObj.storage, rrObj.compositesOrValues).andThen(r => insertIntoGlobalResourceTable(r))
      }
      Validated.Valid(bookkeeper.size)

  private def resourceRecordProcessor(id: EntityId, storageOrPdf: Option[String], compositesOrValues: SlanEntityValidated[Option[List[Resource] | List[StoredValue]]]): SlanEntityValidated[Resource] =
    compositesOrValues match
      case Invalid(e) => e.invalid
      case Valid(cov) =>
        storageOrPdf match
          case None => ProducerConsumer(id, None, cov).valid
          case Some(sp) =>
            ResourceStorage(sp) match
                 case ResourceStorage.UNRECOGNIZED =>
                    if PDFs.PdfStreamGenerator.listOfSupportedDistributions.contains(sp.toUpperCase) then
                      if cov.isEmpty then
                        Generator(id, sp.toUpperCase).valid
                      else
                        IncorrectParameter(s"PDF generator $storageOrPdf cannot be combined with stored values or other resources").invalidNel
                    else
                      IncorrectParameter(s"$storageOrPdf in resource definition").invalidNel
                 case rs => ProducerConsumer(id, Option(rs), cov).valid

  private def insertIntoGlobalResourceTable(rs: Resource) = bookkeeper.set(rs.name.trim, rs).valid