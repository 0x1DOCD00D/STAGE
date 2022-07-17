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
import Translator.SlanAbstractions.SlanConstructs
import Translator.SlanConstruct.{Resources, SlanError}
import cats.data.Validated
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}

object SlanMessages2IR extends (SlanConstructs => SlanEntityValidated[MessageTripleCollection]):
  override def apply(slanMsgs: SlanConstructs): SlanEntityValidated[MessageTripleCollection] =
    checkMessagesEntry(slanMsgs).andThen(msgStruct =>msgProcessor(msgStruct))

  private def checkMessagesEntry(slanMsgs: SlanConstructs): SlanEntityValidated[Translator.SlanConstruct.Messages] =
    val theMessagesStruct = slanMsgs.filter(_.isInstanceOf[Translator.SlanConstruct.Messages])
    Validated.condNel(theMessagesStruct.containsHeadOnly, theMessagesStruct.head.asInstanceOf[Translator.SlanConstruct.Messages], WrongCardinality(s"there should be only one Messages structure, not ${theMessagesStruct.length}"))

  private def msgProcessor(ms: Translator.SlanConstruct.Messages):SlanEntityValidated[MessageTripleCollection] =
    val lstOfValidations = ms.messages.foldLeft(List[SlanEntityValidated[MessageTriple]]()) {
      (theResult, msg) => transformMessage(msg.asInstanceOf[Translator.SlanConstruct.Message]) :: theResult
    }

    val invalids = lstOfValidations.filterNot(_.isValid)
    if invalids.isEmpty then
      lstOfValidations.filter(_.isValid).foldLeft(List[MessageTriple]()) {
        (acc, elem) =>
          elem.toEither.toOption.get :: acc
      }.validNel
    else
      invalids.foldLeft(Translator.SlanConstruct.SlanError().invalidNel) {
        (acc, elem) =>
          acc.combine(elem.asInstanceOf)
      }

  private def transformMessage(msg: Translator.SlanConstruct.Message): SlanEntityValidated[MessageTriple] =
    if msg.id.containsHeadOnly && msg.id.head.isInstanceOf[Translator.SlanConstruct.MessageDeclaration] then
      val msgDeclaration: Translator.SlanConstruct.MessageDeclaration = msg.id.head.asInstanceOf[Translator.SlanConstruct.MessageDeclaration]
      val fields = msg.fields.asInstanceOf[List[Translator.SlanConstruct.Resources]]
      if !fields.containsHeadOnly then
        DuplicateDefinition(s"fields for message ID [${msg.id}] has multiple entries").invalidNel
      else
        SlanResources2IR(fields.head.asInstanceOf) match
          case Valid(rv) => (msgDeclaration.id, msgDeclaration.parent, None).validNel
          case Invalid(err) => err.invalid
    else
      IncorrectParameter(s"message ID [${msg.id}]").invalidNel

  @main def runsm2ir =
    import Translator.SlanConstruct.{MessageDeclaration, Resource, ResourceTag, SlanKeyValue, SlanValue}
    val msgs = List(Translator.SlanConstruct.Messages(List(
      Translator.SlanConstruct.Message(List(MessageDeclaration("Message X",None)),
        List(Resources(List(Resource(ResourceTag("Recursive Field",None),List(Resource(ResourceTag("Message Name",None),List(SlanValue(3))))),
          Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value",None),List(SlanValue(100))),
          Resource(ResourceTag("Another Recursive Field",None),
            List(Resource(ResourceTag("Message Name",None),List(SlanValue(10))))),
          Resource(ResourceTag("Field Name",None),List(SlanValue("generatorUniformPdf"))))))),
      Translator.SlanConstruct.Message(List(MessageDeclaration("Message Y",None)),List(Resources(List(Resource(ResourceTag("Field Name",None),
        List(Resource(ResourceTag("Uniform",None),List(SlanValue(1), SlanValue(200))))),
        Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),List(SlanValue(1), SlanValue(10), SlanValue(100))),
        Resource(ResourceTag("Some Fixed Value",None),List(SlanValue(100))))))),
      Translator.SlanConstruct.Message(List(MessageDeclaration("Derived Message",Some("Message X"))),
        List(Resources(List(Resource(ResourceTag("FieldX",None),
          List(Resource(ResourceTag("Discrete",None),List(SlanKeyValue(1,0.3), SlanKeyValue(2,0.5), SlanKeyValue(3,0.6)))))))))
    )
    )
    )
    val res = SlanMessages2IR(msgs)
    println(res)
    res
