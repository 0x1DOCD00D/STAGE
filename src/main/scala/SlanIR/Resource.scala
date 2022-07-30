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
import Translator.SlanConstruct.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.instances.option.*
import cats.kernel.Eq
import cats.kernel.Eq.catsKernelEqForOption
import cats.syntax.*
import cats.syntax.eq.catsSyntaxEq
import org.slf4j.Logger

trait Resource extends SlanEntity

case class ResourceRecord(id: EntityId, storage: StorageTypeReference, compositesOrValues: SlanEntityValidated[Option[List[Resource] | List[StoredValue] | List[PdfParameters]]]) extends SlanEntity(id), Resource

case class BasicProducerConsumer(id: EntityId, storage: Option[ResourceStorage], initValues: ResourceValues) extends SlanEntity(id), Resource

case class ProducerConsumerComposite(id: EntityId, storage: Option[ResourceStorage], composites: List[SlanEntityValidated[Resource]]) extends SlanEntity(id), Resource

case class Generator(id: EntityId, pdf: PdfName, pdfParms: PdfParameters) extends SlanEntity(id), Resource

case class StoredValue(content: (YamlPrimitiveTypes, YamlPrimitiveTypes) | YamlPrimitiveTypes)

case class PdfParameters(seed: Option[YamlPrimitiveTypes], parameters: Option[List[StoredValue]], fromTo: Option[List[StoredValue]])

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
    Validated.condNel(filteredResources.containsHeadOnly, filteredResources.head.asInstanceOf[Translator.SlanConstruct.Resources].lstOfResources, IncorrectSlanSpecStructure("global entry Resources"))

  private def constructResources(resources: Option[List[ResourceRecord] | List[StoredValue] | List[PdfParameters]]): SlanEntityValidated[Int] =
    resources match
      case Some(v) if v.isInstanceOf[List[_]] => resourcesRecordProcessor(v)
      case Some(e) => IncorrectParameter(e.toString).invalidNel
      case None => IncorrectParameter("no resources specified").invalidNel

  private def resourcesRecordProcessor(rrlst: List[Resource] | List[StoredValue] | List[PdfParameters]): SlanEntityValidated[Int] =
    if rrlst.count(elem => elem.isInstanceOf[ResourceRecord]) =!= rrlst.length then
      IncorrectParameter("only top level resources must be specified, not their values").invalidNel
    else
      rrlst.foreach {
        rr =>
          val rrObj = rr.asInstanceOf[ResourceRecord]
          rrObj.compositesOrValues match
            case Invalid(e) => e.invalid
            case Valid(cov) =>  identificationOfEntity(rrObj.id, rrObj.storage, cov).andThen(r => insertIntoGlobalResourceTable(r))
      }
      Validated.Valid(bookkeeper.size)

  private def identificationOfEntity(id: EntityId, storeId: Option[String], cov: Option[List[Resource] | List[StoredValue] | List[PdfParameters]]): SlanEntityValidated[Resource] = ResourceStorage(storeId) match
    case ResourceStorage.SLANDISTRIBUTION =>
      cov match
        case None => IncorrectParameter(s"PDF generator $storeId does not contains parameters").invalidNel
        case Some(lst) if lst.isInstanceOf[List[_]] =>
          val pdfParms = lst.asInstanceOf[List[PdfParameters]]
          if pdfParms.containsHeadOnly then
            Generator(id, storeId.get, pdfParms.head).valid
          else IncorrectParameter(s"PDF generator $storeId has incorrect parameters: ${pdfParms.mkString("; ")}").invalidNel
        case spec => IncorrectParameter(s"PDF generator $storeId contains incorrect spec: ${spec.toString}").invalidNel

    case ResourceStorage.UNRECOGNIZED => IncorrectParameter(s"$storeId in resource definition").invalidNel

    case rs => cov match
      case None => BasicProducerConsumer(id, Option(rs), List()).validNel
      case Some(lst) if lst.isInstanceOf[List[_]] =>
        if lst.count(elem=>elem.isInstanceOf[StoredValue]) === lst.length then
          BasicProducerConsumer(id, Option(rs), processContainerOfValues(lst.asInstanceOf[List[StoredValue]])).validNel
        else if lst.count(elem=>elem.isInstanceOf[ResourceRecord]) === lst.length then ProducerConsumerComposite(id, Option(rs), processCompositeNestedResources(lst.asInstanceOf[List[ResourceRecord]])).validNel
        else IncorrectSlanSpecStructure(lst.toString).invalidNel
      case unknown => IncorrectSlanSpecStructure(unknown.toString).invalidNel

  private def processCompositeNestedResources(lst: List[ResourceRecord] | List[StoredValue]): List[SlanEntityValidated[Resource]] =
    require(lst.count(elem=>elem.isInstanceOf[ResourceRecord]) === lst.length)
    lst.foldLeft(List[SlanEntityValidated[Resource]]()) {
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
                    BasicProducerConsumer(id, Option(ResourceStorage(store)), processContainerOfValues(rr.asInstanceOf[List[StoredValue]])).validNel :: acc
                  else
                    IncorrectSlanSpecStructure(lst.toString).invalidNel :: acc
          case spec => IncorrectSlanSpecStructure(spec.toString).invalidNel :: acc
    }
  private def processContainerOfValues(lst: List[StoredValue]): ResourceValues =
    require(lst.count(elem=>elem.isInstanceOf[StoredValue]) === lst.length)
    lst.foldLeft(List[ResourceValueType]())((acc, elem) => elem.content :: acc)

  private def insertIntoGlobalResourceTable(rs: Resource) = bookkeeper.set(rs.name.trim, rs).validNel

  @main def runResources(): Unit =
    import Translator.SlanConstruct.{Agents, Resource, ResourceTag, Resources, SlanError, SlanValue}
    val resExample = List(Agents(List(Resources(List(
      Translator.SlanConstruct.Resource(ResourceTag("SomeUniformGenerator",Some("UniformRealDistribution")),
      List(ResourcePDFParameters(List(SlanValue(1), SlanValue("totalMessages"))),
        ResourcePDFConstraintsAndSeed(List(PdfSeed(200), SlanKeyValue(">",0.1), SlanKeyValue("<",0.8))))),
      Translator.SlanConstruct.Resource(ResourceTag("OtherUniformGenerator",Some("UniformRealDistribution")),
        List(ResourcePDFParameters(List(SlanValue(1))))),
      Translator.SlanConstruct.Resource(ResourceTag("autoInitializedPrimitiveListResource",Some("list")),
        List(SlanValue("aUniformGeneratorReference"))),
      Translator.SlanConstruct.Resource(ResourceTag("compositeResource",None),
        List(Translator.SlanConstruct.Resource(ResourceTag("someBasicResource1V",Some("list")),
          List(SlanValue(100), SlanValue(1000))),
          Translator.SlanConstruct.Resource(ResourceTag("valueHolder4compositeResource",None),List(SlanValue(1)))))
    )))))

    resExample.headOption match
      case None => ()
      case Some(agents) if agents.isInstanceOf[Agents] =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        println(SlanIR.Resource(slanconstructs))
        println(bookkeeper.toString)
      case _ => ()
