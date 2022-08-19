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
import HelperUtils.ErrorWarningMessages.*
import HelperUtils.ExtentionMethods.*
import SlanIR.{EntityId, EntityOrError}
import Translator.SlanAbstractions.{ResourceReference, SlanConstructs, StorageTypeReference, YamlPrimitiveTypes}
import Translator.SlanConstruct
import Translator.SlanConstruct.*
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}
import cats.implicits.*
import cats.instances.option.*
import cats.kernel.Eq
import cats.kernel.Eq.catsKernelEqForOption
import cats.syntax.*
import cats.syntax.eq.catsSyntaxEq
import org.slf4j.Logger

trait ResourceIR extends SlanEntity

case class BadResource(id: EntityId, reason: SlanEntityValidated[Option[List[ResourceIR] | List[StoredValue] | List[PdfParameters]]]) extends SlanEntity(id), ResourceIR

case class ResourceRecord(id: EntityId, storage: StorageTypeReference, compositesOrValues: SlanEntityValidated[Option[List[ResourceIR] | List[StoredValue] | List[PdfParameters]]]) extends SlanEntity(id), ResourceIR

case class BasicProducerConsumer(id: EntityId, storage: Int, initValues: ResourceValues) extends SlanEntity(id), ResourceIR

case class ProducerConsumerComposite(id: EntityId, storage: Int, composites: List[SlanEntityValidated[ResourceIR]]) extends SlanEntity(id), ResourceIR

case class Generator(id: EntityId, pdf: PdfName, pdfParms: PdfParameters) extends SlanEntity(id), ResourceIR

case class StoredValue(content: (YamlPrimitiveTypes, YamlPrimitiveTypes) | YamlPrimitiveTypes)

case class PdfParameters(seed: Option[YamlPrimitiveTypes], parameters: Option[List[StoredValue]], fromTo: Option[List[StoredValue]])

object ResourceIR:
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
  def apply(translated: SlanConstructs): SlanEntityValidated[Map[EntityId, ResourceIR]] =
    checkForResourcesCaseClass(translated)
      .andThen(resources => checkForListOfResources(resources))
      .andThen(resources => checkResourceTagStructure(resources))
      .andThen(resources => checkDuplicateResources(resources))
      .andThen(resources => SlanResources2IR(resources)).andThen(rr => constructResources(rr))

  private def checkForResourcesCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val filteredResources = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Resources])
    Validated.condNel(filteredResources.containsHeadOnly, filteredResources.head.asInstanceOf[Translator.SlanConstruct.Resources].lstOfResources, IncorrectSlanSpecStructure("global entry Resources"))

  private def checkForListOfResources(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val onlyResources: SlanConstructs = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Resource])
    Validated.condNel(translated.length === onlyResources.length, translated, IncorrectParameter(s" other data structures than Resource are present"))

  private def checkResourceTagStructure(resources: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val allResources = resources.asInstanceOf[List[Translator.SlanConstruct.Resource]]
    val rtags: List[ResourceTag] = allResources.flatMap(r => r.id match {
      case null => IncorrectParameter(s"null tag for ${r.toString} is not permitted")
                   None
      case tag: SlanConstruct.ResourceTag => Option(tag)
      case _ => IncorrectParameter(s"tag for ${r.toString}")
                None
    })
    Validated.condNel(rtags.length === allResources.length, resources, IncorrectParameter(s"some resources do not contain tag identification"))

  private def checkDuplicateResources(resources: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val allResources = resources.asInstanceOf[List[Translator.SlanConstruct.Resource]]
    val ids: CollectionOfEntities = allResources.map(r => r.id.asInstanceOf[ResourceTag].id)
    Validated.condNel(ids.distinct.length === ids.length, resources,
      DuplicateDefinition(s"resources ${
        ids.groupBy(identity).collect { case (elem, y: List[_]) => if y.length > 1 then elem.some else None }.flatten.mkString(", ")
      }"))

  private def constructResources(resources: Option[List[ResourceRecord] | List[StoredValue] | List[PdfParameters]]): SlanEntityValidated[Map[EntityId, ResourceIR]] =
    resources match
      case Some(rrlst) if rrlst.isInstanceOf[List[_]] =>
        if rrlst.count(e => e.isInstanceOf[ResourceRecord]) =!= rrlst.length then
          IncorrectParameter("only top level resources must be specified, not their values").invalidNel
        else
          rrlst.asInstanceOf[List[ResourceRecord]].foldLeft(Map[EntityId, ResourceIR]()){
            (map, entry) =>
              map ++ identificationOfEntity(entry)
          }.valid

      case Some(e) => IncorrectParameter(e.toString).invalidNel
      case None => IncorrectParameter("resources are not specified").invalidNel

  private def identificationOfEntity(resourceEntity: ResourceRecord): Map[EntityId, ResourceIR] = resourceEntity.compositesOrValues match
      case Invalid(e) => Map(resourceEntity.id -> BadResource(resourceEntity.id, resourceEntity.compositesOrValues))
      case Valid(cov) => ResourceStorage(resourceEntity.storage) match
        case ResourceStorage.SLANDISTRIBUTION =>
          cov match
            case None => Map(resourceEntity.id -> BadResource(resourceEntity.id, IncorrectParameter(s"PDF generator ${resourceEntity.storage} does not contains parameters").invalidNel))
            case Some(lst) if lst.isInstanceOf[List[_]] =>
              val pdfParms = lst.asInstanceOf[List[PdfParameters]]
              if pdfParms.containsHeadOnly then
                Map(resourceEntity.id-> Generator(resourceEntity.id, resourceEntity.storage.get, pdfParms.head))
              else Map(resourceEntity.id -> BadResource(resourceEntity.id, IncorrectParameter(s"PDF generator ${resourceEntity.storage} has incorrect parameters: ${pdfParms.mkString("; ")}").invalidNel))
            case spec => Map(resourceEntity.id -> BadResource(resourceEntity.id, IncorrectParameter(s"PDF generator ${resourceEntity.storage} contains incorrect spec: ${spec.toString}").invalidNel))

        case ResourceStorage.UNRECOGNIZED => Map(resourceEntity.id -> BadResource(resourceEntity.id, IncorrectParameter(s"${resourceEntity.storage} in resource ${resourceEntity.id}").invalidNel))

        case rs => cov match
          case None => Map(resourceEntity.id -> BasicProducerConsumer(resourceEntity.id, rs.id, List()))
          case Some(lst) if lst.isInstanceOf[List[_]] =>
            if lst.count(elem=>elem.isInstanceOf[StoredValue]) === lst.length then
              Map(resourceEntity.id -> BasicProducerConsumer(resourceEntity.id, rs.id, processContainerOfValues(lst.asInstanceOf[List[StoredValue]])))
            else if lst.count(elem=>elem.isInstanceOf[ResourceRecord]) === lst.length then Map(resourceEntity.id -> ProducerConsumerComposite(resourceEntity.id, rs.id, processCompositeNestedResources(lst.asInstanceOf[List[ResourceRecord]])))
            else Map(resourceEntity.id -> BadResource(resourceEntity.id, IncorrectSlanSpecStructure(lst.toString).invalidNel))
          case unknown => Map(resourceEntity.id -> BadResource(resourceEntity.id, IncorrectSlanSpecStructure(unknown.toString).invalidNel))

  private def processCompositeNestedResources(lst: List[ResourceRecord] | List[StoredValue]): List[SlanEntityValidated[ResourceIR]] =
    require(lst.count(elem=>elem.isInstanceOf[ResourceRecord]) === lst.length)
    lst.foldLeft(List[SlanEntityValidated[ResourceIR]]()) {
      (acc, elem) =>
        elem match
          case ResourceRecord(id, store, covValidated) => covValidated match
            case Invalid(e) => e.invalid :: acc
            case Valid(v) =>
              v match
                case None => acc
                case Some(rr) =>
                  if rr.count(elem=>elem.isInstanceOf[ResourceRecord]) === rr.length then
                    processCompositeNestedResources(rr.asInstanceOf[List[ResourceRecord]]) ::: acc
                  else if rr.count(elem=>elem.isInstanceOf[StoredValue]) === rr.length then
                    BasicProducerConsumer(id, ResourceStorage(store).id, processContainerOfValues(rr.asInstanceOf[List[StoredValue]])).validNel :: acc
                  else
                    IncorrectSlanSpecStructure(lst.toString).invalidNel :: acc
          case _ => IncorrectSlanSpecStructure(elem.toString).invalidNel :: acc
    }
  private def processContainerOfValues(lst: List[StoredValue]): ResourceValues =
    require(lst.count(elem=>elem.isInstanceOf[StoredValue]) === lst.length)
    lst.foldLeft(List[ResourceValueType]())((acc, elem) => elem.content :: acc)