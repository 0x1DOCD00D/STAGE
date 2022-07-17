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

object SlanResources2IR extends (SlanConstructs => SlanEntityValidated[Option[List[Resource] | List[StoredValue]]]):
  override def apply(resourceOrValsSpec: SlanConstructs): SlanEntityValidated[Option[List[Resource] | List[StoredValue]]] =
    if incorrectResourceValueCollection(resourceOrValsSpec) || notAllResourceAttributesAreValues(resourceOrValsSpec) then
      IncorrectParameter("attributes should contain either nested resources or values only").invalidNel
    else
      processResources(resourceOrValsSpec)

  private def incorrectResourceValueCollection(resourceSpec: SlanConstructs): Boolean =
    resourceSpec.exists(_.isInstanceOf[Translator.SlanConstruct.Resource]) &&
    resourceSpec.filter(_.isInstanceOf[Translator.SlanConstruct.Resource]).length =!= resourceSpec.length

  private def notAllResourceAttributesAreValues(resourceSpec: SlanConstructs): Boolean =
    resourceSpec.filter(_.isInstanceOf[Translator.SlanConstruct.SlanValue]).length +
      resourceSpec.filter(_.isInstanceOf[Translator.SlanConstruct.SlanKeyValue]).length +
      resourceSpec.filter(_.isInstanceOf[Translator.SlanConstruct.SlanKeyNoValue]).length
      =!= resourceSpec.length


  private def processResources(resourcesOrValues: SlanConstructs): SlanEntityValidated[Option[List[Resource] | List[StoredValue]]] =
    if resourcesOrValues.filter(_.isInstanceOf[Translator.SlanConstruct.Resource]).length === resourcesOrValues.length then
      obtainResources(resourcesOrValues.asInstanceOf[List[Translator.SlanConstruct.Resource]]).validNel
    else
      obtainResourceValues(resourcesOrValues).validNel


  private def constructValue(resValue: Translator.SlanConstruct.SlanValue): StoredValue =
    StoredValue(resValue.value)

  private def constructValue(resValue: Translator.SlanConstruct.SlanKeyValue): StoredValue =
    StoredValue((resValue.key, resValue.value))

  private def constructValue(resValue: Translator.SlanConstruct.SlanKeyNoValue): StoredValue =
    StoredValue((resValue.key, PDFs.PdfStreamGenerator.probabilityRange(1)))

  private def constructResource(resource: Translator.SlanConstruct.Resource): Resource =
    val tag = resource.id.asInstanceOf[Translator.SlanConstruct.ResourceTag]
    val entityId = tag.id
    val storage = tag.storageType
    val attrs = SlanResources2IR(resource.attributes)
    ???
//    Resource(tag.id, tag.storageType, attrs)

  private def obtainResources(collOfResources: List[Translator.SlanConstruct.Resource]): Option[List[Resource]] =
    collOfResources match
      case resource :: theRestOfColl => Option(constructResource(resource) :: obtainResources(theRestOfColl).getOrElse(Nil))
      case Nil => None

  private def obtainResourceValues(attributes: SlanConstructs): Option[List[StoredValue]] =
    attributes match
      case hd :: tl if hd.isInstanceOf[Translator.SlanConstruct.SlanValue] => Option(constructValue(hd.asInstanceOf[Translator.SlanConstruct.SlanValue]) :: obtainResourceValues(tl).getOrElse(Nil))
      case hd :: tl if hd.isInstanceOf[Translator.SlanConstruct.SlanKeyValue] => Option(constructValue(hd.asInstanceOf[Translator.SlanConstruct.SlanKeyValue]) :: obtainResourceValues(tl).getOrElse(Nil))
      case hd :: tl if hd.isInstanceOf[Translator.SlanConstruct.SlanKeyNoValue] => Option(constructValue(hd.asInstanceOf[Translator.SlanConstruct.SlanKeyNoValue]) :: obtainResourceValues(tl).getOrElse(Nil))
      case _ => None
