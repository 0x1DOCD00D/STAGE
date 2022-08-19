/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, IncorrectParameter, MissingDefinition, WrongCardinality}
import HelperUtils.ExtentionMethods.*
import Translator.SlanAbstractions.{MessageReference, SlanConstructs}
import Translator.SlanConstruct.{MessageDeclaration, Resources, SlanError}
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}

object SlanMessages2IR extends (List[Translator.SlanConstruct.Message] => SlanEntityValidated[Map[EntityId, MessageIR]]):

  override def apply(slanMsgs: List[Translator.SlanConstruct.Message]): SlanEntityValidated[Map[EntityId, MessageIR]] =
    slanMsgs.foldLeft(Map[MessageReference, MessageIR]()){
      (map, elem) =>
        val msgDecl: MessageDeclaration = elem.id.asInstanceOf[List[MessageDeclaration]].head
        map + (msgDecl.id -> MessageIR(msgDecl.id, msgDecl.parent, ResourceIR(elem.fields)))
    }.valid
