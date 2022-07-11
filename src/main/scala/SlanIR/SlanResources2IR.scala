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

object SlanResources2IR extends (SlanConstructs => SlanEntityValidated[ResourceTupleCollection]):
  override def apply(resourceSpec: SlanConstructs): SlanEntityValidated[ResourceTupleCollection] =
    if resourceSpec.filter(_.isInstanceOf[Translator.SlanConstruct.Resource]).length == resourceSpec.length then
      processResources(resourceSpec.asInstanceOf[List[Translator.SlanConstruct.Resource]])
    else if resourceSpec.filter(_.isInstanceOf[Translator.SlanConstruct.SlanValue]).length == resourceSpec.length then
      processResourceValues(resourceSpec.asInstanceOf[List[Translator.SlanConstruct.SlanValue]])
    else
        IncorrectParameter("mixed resource attributes combine values and included resources").invalidNel
      /*
        private def checkMessagesEntry(slanMsgs: SlanConstructs): SlanEntityValidated[Translator.SlanConstruct.Messages] =
          val theResourceStruct = slanMsgs.filter(_.isInstanceOf[Translator.SlanConstruct.Resource])
          Validated.condNel(theResourceStruct.containsHeadOnly, theResourceStruct.head.asInstanceOf[Translator.SlanConstruct.Messages], WrongCardinality(s"there should be only one Messages structure, not ${theResourceStruct.length}"))
      */
  private def processResources(resources: List[Translator.SlanConstruct.Resource]): SlanEntityValidated[ResourceTupleCollection] = ???

  private def processResourceValues(values: List[Translator.SlanConstruct.SlanValue]): SlanEntityValidated[ResourceTupleCollection] = ???

  private def constructResource(resource: Translator.SlanConstruct.Resource): Resource =
    val tag = resource.id.asInstanceOf[Translator.SlanConstruct.ResourceTag]
    val entityId = tag.id
    val storage = tag.storageType
    null

