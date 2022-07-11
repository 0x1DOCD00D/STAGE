/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, MissingDefinition}
import SlanIR.{EntityId, EntityOrError, SlanEntity}
import Translator.SlanAbstractions.SlanConstructs
import Translator.SlanConstruct
import Translator.SlanConstruct.SlanError
import cats.{Eq, Semigroup}
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.*
import cats.instances.*
import cats.syntax.*
import cats.syntax.validated.*

case class Message(id: EntityId, parent: Option[Message], fields: Option[List[Resource]]) extends SlanEntity(id):
  def withParent(parent: Option[Message]): Message = Message(id, parent, fields)

object Message:
  private val bookkeeper = new EntityBookkeeper[Message]

  private def checkDuplicateMessages(ids: CollectionOfEntities): SlanEntityValidated[CollectionOfEntities] =
    Validated.condNel(ids.distinct.length === ids.length, ids,
      DuplicateDefinition(s"messages ${
        ids.groupBy(identity).collect { case (elem, y: List[_]) => if y.length > 1 then elem.some else None }.flatten.mkString(", ")
      }"))

  private def checkOrphanedParents(messages: MessageTripleCollection): SlanEntityValidated[CollectionOfEntities] =
    val allEntityIds = messages.map(triple => triple(0))
    val orphanedParents = messages.map(triple => triple(1)).flatten.filterNot(parent => allEntityIds.contains(parent))
    Validated.condNel(orphanedParents.isEmpty, allEntityIds, MissingDefinition(s"orphaned parents: [${orphanedParents.mkString(", ")}]"))

  private def constructMessages(messages: MessageTripleCollection): SlanEntityValidated[Int] =
    messages.foreach {
      triple => bookkeeper.set(triple(0), Message(triple(0), None, triple(2)))
    }

    messages.foreach {
      triple =>
        val parent = triple(1) match
          case Some(id) => bookkeeper.get(id)
          case None => None
        bookkeeper.get(triple(0)) match
          case Some(msg) => msg.withParent(parent)
          case None => Validated.Invalid(MissingDefinition(s"message ${triple(0)}"))
    }
    Validated.Valid(bookkeeper.size)

  def apply(id: EntityId): Option[Message] = bookkeeper.get(id)

  def apply(translated: SlanConstructs) =
    checkForMessagesCaseClass(translated) match
      case Valid(msgs) => println(SlanMessages2IR(msgs))
      case _ => println("error")
    ()

  private def checkForMessagesCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    Validated.condNel(translated.exists(_.isInstanceOf[Translator.SlanConstruct.Messages]), translated.filter(entity => entity.isInstanceOf[Translator.SlanConstruct.Messages]), MissingDefinition("case class Messages"))

  /*
  * This is the main factory for processing the SLAN input for Messages as a list of triples
  * of the entity id, the parent and the list of fields defined as resource because they store data.
  * Two main checks are performed: checking if a message is not a duplicate and checking to see
  * if the parent id references a valid message. More advanced checks of cyclicity and recursive
  * field definitions will be performed once the SLAN program network is constructed.
  * */
  private def build(messages: MessageTripleCollection): SlanEntityValidated[Int] =
    //  first we check if the parent ids are present among message ids, then we check to see if there are no duplicates among message ids
    checkOrphanedParents(messages).andThen(ids =>checkDuplicateMessages(ids)).andThen(_ => constructMessages(messages))

  @main def runMsgs =
    import Translator.SlanConstruct.{MessageDeclaration, Resource, ResourceTag, SlanKeyValue, SlanValue}
    val msgs = List(Translator.SlanConstruct.Messages(List(
      Translator.SlanConstruct.Message(List(MessageDeclaration("Message Name",None)),
        List(Resource(ResourceTag("Recursive Field",None),List(Resource(ResourceTag("Message Name",None),List(SlanValue(3))))),
          Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value",None),List(SlanValue(100))),
          Resource(ResourceTag("Another Recursive Field",None),
            List(Resource(ResourceTag("Message Name",None),List(SlanValue(10))))),
          Resource(ResourceTag("Field Name",None),List(SlanValue("generatorUniformPdf"))))),
      Translator.SlanConstruct.Message(List(MessageDeclaration("Message Name",None)),List(Resource(ResourceTag("Field Name",None),
        List(Resource(ResourceTag("Uniform",None),List(SlanValue(1), SlanValue(200))))),
        Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),List(SlanValue(1), SlanValue(10), SlanValue(100))),
        Resource(ResourceTag("Some Fixed Value",None),List(SlanValue(100))))),
      Translator.SlanConstruct.Message(List(MessageDeclaration("Derived Message",Some("Message Name"))),
        List(Resource(ResourceTag("FieldX",None),
          List(Resource(ResourceTag("Discrete",None),List(SlanKeyValue(1,0.3), SlanKeyValue(2,0.5), SlanKeyValue(3,0.6)))))))
    )
    )
    )

    Message(msgs)