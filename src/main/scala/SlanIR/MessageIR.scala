/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
import Translator.SlanConstruct.{MessageDeclaration, SlanError}
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.*
import cats.instances.*
import cats.syntax.*
import cats.syntax.validated.*
import cats.{Eq, Semigroup}

case class MessageIR(id: MessageReference, parent: Option[MessageReference | MessageIR] | SlanError, fields: SlanEntityValidated[Map[MessageReference, ResourceIR]]) extends SlanEntity(id):
  def withParent(parent: Option[MessageIR] | SlanError): MessageIR = MessageIR(id, parent, fields)

object MessageIR:
  private def checkForMessagesCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val filteredMessages = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Messages])
    Validated.condNel(filteredMessages.containsHeadOnly, filteredMessages.head.asInstanceOf[Translator.SlanConstruct.Messages].messages, IncorrectSlanSpecStructure("global entry Messages is missing"))

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

  private def checkDuplicateMessages(msgs: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val allMsgs = msgs.asInstanceOf[List[Translator.SlanConstruct.Message]]
    val mIds: List[MessageReference] = allMsgs.map(_.id.head.asInstanceOf[MessageDeclaration].id)
    Validated.condNel(mIds.distinct.length === mIds.length, msgs,
      DuplicateDefinition(s"messages ${
        mIds.groupBy(identity).collect { case (elem, y: List[_]) => if y.length > 1 then elem.some else None }.flatten.mkString(", ")
      }"))

  private def checkOrphanedParents(msgs: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val allMsgs = msgs.asInstanceOf[List[Translator.SlanConstruct.Message]]
    val mIds: List[MessageReference] = allMsgs.map(_.id.head.asInstanceOf[MessageDeclaration].id)

    val parents: List[MessageReference] = allMsgs.flatMap(m=>m.id.head.asInstanceOf[MessageDeclaration].parent)
    val orphanedParents: List[MessageReference] = parents.filterNot(parent => mIds.contains(parent))
    Validated.condNel(orphanedParents.isEmpty, msgs, MissingDefinition(s"orphaned parents: [${orphanedParents.mkString(", ")}]"))

  def apply(translated: SlanConstructs): SlanEntityValidated[Map[EntityId, MessageIR]] =
    checkForMessagesCaseClass(translated)
      .andThen(msgs => checkForListOfMessages(msgs))
      .andThen(msgs => checkMessageStructure(msgs))
      .andThen(msgs => checkDuplicateMessages(msgs))
      .andThen(msgs => checkOrphanedParents(msgs))
      .andThen(msgs => SlanMessages2IR(msgs.asInstanceOf[List[Translator.SlanConstruct.Message]]))
