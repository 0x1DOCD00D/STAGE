/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{IncorrectParameter, WrongCardinality}
import HelperUtils.ExtentionMethods.*
import Translator.SlanAbstractions.SlanConstructs
import Translator.SlanConstruct.SlanError
import cats.data.Validated
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}

/*
* This function is called to process a list of resources as SlanConstructs from multiple locations.
* The primary location is the list of global resources under Agents/Resources key path. All top level
* resources under this path should be added to the resources table in the object Resource and each top-level
* resource is uniquely identified by its name. Resources, however, can also be defined as fields in messages,
* local resources in agents and as the constituents of the other composite resources, which are not added
* to the global resource tables and their references are local. Therefore, top level resources are processed
* in the object Resource whereas local resources are processed by this function.
* */
object SlanResources2IR extends (SlanConstructs => SlanEntityValidated[Option[List[ResourceRecord] | List[StoredValue]]]):
  override def apply(resourceOrValsSpec: SlanConstructs): SlanEntityValidated[Option[List[ResourceRecord] | List[StoredValue]]] =
    if incorrectResourceValueCollection(resourceOrValsSpec) || notAllResourceAttributesAreValues(resourceOrValsSpec) then
      IncorrectParameter("attributes should contain either nested resources or values only").invalidNel
    else
      processResources(resourceOrValsSpec)

  private def incorrectResourceValueCollection(resourceSpec: SlanConstructs): Boolean =
    resourceSpec.exists(_.isInstanceOf[Translator.SlanConstruct.Resource]) &&
    resourceSpec.count(_.isInstanceOf[Translator.SlanConstruct.Resource]) =!= resourceSpec.length

  private def notAllResourceAttributesAreValues(resourceSpec: SlanConstructs): Boolean =
    val allValues = resourceSpec.count(_.isInstanceOf[Translator.SlanConstruct.SlanValue]) +
      resourceSpec.count(_.isInstanceOf[Translator.SlanConstruct.SlanKeyValue]) +
      resourceSpec.count(_.isInstanceOf[Translator.SlanConstruct.SlanKeyNoValue])
    if allValues > 0 && allValues =!= resourceSpec.length then true
    else false


  private def processResources(resourcesOrValues: SlanConstructs): SlanEntityValidated[Option[List[ResourceRecord] | List[StoredValue]]] =
    if resourcesOrValues.count(_.isInstanceOf[Translator.SlanConstruct.Resource]) === resourcesOrValues.length then
      obtainResources(resourcesOrValues.asInstanceOf[List[Translator.SlanConstruct.Resource]]).validNel
    else
      obtainResourceValues(resourcesOrValues).validNel


  private def constructValue(resValue: Translator.SlanConstruct.SlanValue): StoredValue =
    StoredValue(resValue.value)

  private def constructValue(resValue: Translator.SlanConstruct.SlanKeyValue): StoredValue =
    StoredValue((resValue.key, resValue.value))

  private def constructValue(resValue: Translator.SlanConstruct.SlanKeyNoValue): StoredValue =
    StoredValue((resValue.key, PDFs.PdfStreamGenerator.probabilityRange(1)))

  private def constructResource(resource: Translator.SlanConstruct.Resource): ResourceRecord =
    val tag = resource.id.asInstanceOf[Translator.SlanConstruct.ResourceTag]
    ResourceRecord(tag.id, tag.storageType, SlanResources2IR(resource.attributes))

  private def obtainResources(collOfResources: List[Translator.SlanConstruct.Resource]): Option[List[ResourceRecord]] =
    collOfResources match
      case resource :: theRestOfColl => Option(constructResource(resource) :: obtainResources(theRestOfColl).getOrElse(List()))
      case List() => None

  private def obtainResourceValues(attributes: SlanConstructs): Option[List[StoredValue]] =
    attributes match
      case hd :: tl if hd.isInstanceOf[Translator.SlanConstruct.SlanValue] => Option(constructValue(hd.asInstanceOf[Translator.SlanConstruct.SlanValue]) :: obtainResourceValues(tl).getOrElse(Nil))
      case hd :: tl if hd.isInstanceOf[Translator.SlanConstruct.SlanKeyValue] => Option(constructValue(hd.asInstanceOf[Translator.SlanConstruct.SlanKeyValue]) :: obtainResourceValues(tl).getOrElse(Nil))
      case hd :: tl if hd.isInstanceOf[Translator.SlanConstruct.SlanKeyNoValue] => Option(constructValue(hd.asInstanceOf[Translator.SlanConstruct.SlanKeyNoValue]) :: obtainResourceValues(tl).getOrElse(Nil))
      case _ => None
