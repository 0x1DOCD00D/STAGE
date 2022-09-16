/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.*
import HelperUtils.ExtentionMethods.containsHeadOnly
import SlanIR.{EntityId, EntityOrError, SlanEntity}
import Translator.SlanAbstractions.{MessageReference, SlanConstructs}
import Translator.SlanConstruct
import Translator.SlanConstruct.{Message, MessageDeclaration, Messages, SlanError}
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.*
import cats.instances.*
import cats.syntax.*
import cats.syntax.validated.*
import cats.{Eq, Semigroup}

case class MessageIR(id: MessageReference, parent: Option[MessageReference | MessageIR] | SlanError, fields: SlanEntityValidated[Map[MessageReference, ResourceIR]]) extends SlanEntity(Option(id)):
  def withParent(newParent: Option[MessageIR] | SlanError): MessageIR = this.copy(parent = newParent )

object MessageIR extends UniversalChecks[Message, Messages]:
  private def checkForListOfMessages(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val onlyMessages: SlanConstructs = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Message])
    Validated.condNel(translated.length === onlyMessages.length, translated, IncorrectParameter(s" other data structures than Message are present under Messages"))

  private def checkMessageStructure(msgs: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val allMsgs = msgs.asInstanceOf[List[Translator.SlanConstruct.Message]]
    val mdecl: List[MessageDeclaration] = allMsgs.flatMap(m => {
      if m.id.containsHeadOnly then
        m.id.head match {
          case declaration: SlanConstruct.MessageDeclaration => List(declaration)
          case _ => None
        }
      else None
    })
    Validated.condNel(mdecl.length === allMsgs.length, allMsgs, IncorrectParameter(s"some messages do not contain tag identification"))

  private def checkOrphanedParents(msgs: SlanConstructs): SlanEntityValidated[SlanConstructs] =
      val allMsgs = msgs.asInstanceOf[List[Translator.SlanConstruct.Message]]
      val mIds: List[MessageReference] = allMsgs.map(_.id.head.asInstanceOf[MessageDeclaration].id)

      val parents: List[MessageReference] = allMsgs.flatMap(m=>m.id.head.asInstanceOf[MessageDeclaration].parent)
      val orphanedParents: List[MessageReference] = parents.filterNot(parent => mIds.contains(parent))
      Validated.condNel(orphanedParents.isEmpty, msgs, MissingDefinition(s"orphaned parents: [${orphanedParents.mkString(", ")}]"))

  def apply(translated: SlanConstructs): SlanEntityValidated[Map[EntityId, MessageIR]] =
    checkForEncasingClass(translated, (scLst:SlanConstructs)=>{scLst.filter(_.isInstanceOf[Messages]).asInstanceOf[List[Messages]]}, (m: Messages)=> m.messages.asInstanceOf[List[Message]] )
      .andThen(msgs => checkForListOfEntities(msgs, (scLst:SlanConstructs)=>{scLst.filter(_.isInstanceOf[Message]).asInstanceOf[List[Message]]}))
      .andThen(msgs => checkMessageStructure(msgs))
      .andThen(msgs => checkDuplicateNames(msgs, (m: Message) => {m.id.head.asInstanceOf[MessageDeclaration].id}))
      .andThen(msgs => checkOrphanedParents(msgs))
      .andThen(msgs => SlanMessages2IR(msgs.asInstanceOf[List[Translator.SlanConstruct.Message]]))
