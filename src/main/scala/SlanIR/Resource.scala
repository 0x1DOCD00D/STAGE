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
import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, IncorrectParameter, MissingDefinition}
import SlanIR.{EntityId, EntityOrError}
import Translator.SlanAbstractions.{SlanConstructs, YamlPrimitiveTypes}
import Translator.SlanConstruct.SlanError
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.slf4j.Logger
import cats.implicits.*
import cats.kernel.Eq
import cats.kernel.Eq.catsKernelEqForOption
import cats.syntax.*
import cats.syntax.eq.catsSyntaxEq

import scala.collection.mutable.Map

trait Resource extends SlanEntity

case class ProducerConsumer(id: EntityId, storage: Option[ResourceStorage], compositesOrValues: Option[List[Resource] | List[StoredValue]]) extends SlanEntity(id), Resource

case class Generator(id: EntityId, pdf: String) extends SlanEntity(id), Resource

case class StoredValue(content: (YamlPrimitiveTypes, YamlPrimitiveTypes) | YamlPrimitiveTypes)

object Resource:
  private val bookkeeper = new EntityBookkeeper[Resource]
  def apply(id: EntityId): Option[Resource] = bookkeeper.get(id)

  /*
  * Resources can be defined under the section Resources or as fields in a message. In both
  * cases they are wrapped in the case class Resources at the top level. However, for each
  * resource attributes can be defined as either values or nested resources, not both at the same time.
  * Therefore, if the check of the parameter translated reveals that it contains List(Resources(_var_))
  * then the content of _var_ is of the type List[Translator.SlanConstruct.Resource]. Otherwise, it is expected
  * that the list of SlanConstruct is the list of resource attributes, i.e.,
  * */
  def apply(translated: SlanConstructs) =
    checkForMessagesCaseClass(translated) match
      case Valid(msgs) => println(SlanMessages2IR(msgs))
      case _ => println("error")
    ()

  private def checkForMessagesCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    Validated.condNel(translated.exists(_.isInstanceOf[Translator.SlanConstruct.Messages]), translated.filter(entity => entity.isInstanceOf[Translator.SlanConstruct.Messages]), MissingDefinition("case class Messages"))

  def apply(id: EntityId, storageOrPdf: Option[String], compositesOrValues: SlanEntityValidated[Option[List[Resource] | List[StoredValue]]]): SlanEntityValidated[Boolean] =
    compositesOrValues match
      case Invalid(e) => e.invalid
      case Valid(cov) =>
        storageOrPdf match
          case None => bookkeeper.set(id, ProducerConsumer(id, None, cov))
                        true.valid
          case Some(sp) =>
            ResourceStorage(sp) match
                 case ResourceStorage.UNRECOGNIZED =>
                    if PDFs.PdfStreamGenerator.listOfSupportedDistributions.contains(sp.toUpperCase) then
                      if cov.isEmpty then
                        bookkeeper.set(id, Generator(id, sp.toUpperCase))
                        true.valid
                      else
                        IncorrectParameter(s"PDF generator $storageOrPdf cannot be combined with stored values or other resources").invalidNel
                    else
                      IncorrectParameter(s"$storageOrPdf in resource definition").invalidNel
                 case rs => bookkeeper.set(id, ProducerConsumer(id, Option(rs), cov))
                            true.valid
