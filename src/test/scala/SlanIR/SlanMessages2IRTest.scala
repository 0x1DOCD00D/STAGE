/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.SlanProcessingFailure
import Translator.SlanAbstractions.{BehaviorReference, SlanConstructs, StateReference, YamlTypes}
import Translator.SlanConstruct.*
import Translator.{SlanTranslator, SlantParser}
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.List
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class SlanMessages2IRTest extends AnyFlatSpec with Matchers:
  behavior of "the messages translator to the message IR representation"

  it should "translate a message with several fields" in {
    val oneMessage = List(Messages(List(
      Message(List(MessageDeclaration("Message Name", None)),
        List(Resources(List(Resource(ResourceTag("Recursive Field", None), List(Resource(ResourceTag("Message Name", None), List(SlanValue(3))))),
          Resource(ResourceTag("someBasicResourceListOfValues", Some("queue")), List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value", None), List(SlanValue(100))),
          Resource(ResourceTag("Another Recursive Field", None),
            List(Resource(ResourceTag("Message Name", None), List(SlanValue(10))))),
          Resource(ResourceTag("Field Name", None), List(SlanValue("generatorUniformPdf"))))))))))
    SlanIR.MessageIR(oneMessage) shouldBe Valid(Map(
      "Message Name" -> MessageIR("Message Name",None,Valid(Map(
        "Recursive Field" -> ProducerConsumerComposite("Recursive Field",0,List(Valid(
          BasicProducerConsumer("Message Name",0,List(3))))),
        "someBasicResourceListOfValues" -> BasicProducerConsumer("someBasicResourceListOfValues",1,List(100, 10, 1)),
        "Some Fixed Value" -> BasicProducerConsumer("Some Fixed Value",0,List(100)),
        "Another Recursive Field" -> ProducerConsumerComposite("Another Recursive Field",0,List(Valid(
          BasicProducerConsumer("Message Name",0,List(10))))),
        "Field Name" -> BasicProducerConsumer("Field Name",0,List("generatorUniformPdf")))))))
  }

  it should "translate two messages where one is derived from the other" in {
    val twoMsgs = List(Messages(List(
      Message(List(MessageDeclaration("Message Name", None)), List(Resources(
        List(Resource(ResourceTag("Field Name", Some("list")), List(SlanValue(1), SlanValue(200))),
          Resource(ResourceTag("someBasicResourceListOfValues", Some("queue")), List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value", None), List(SlanValue(100))))))),
      Message(List(MessageDeclaration("Derived Message", Some("Message Name"))),
        List(Resources(List(Resource(ResourceTag("FieldX", None),
          List(Resource(ResourceTag("Discrete", None), List(SlanKeyValue(1, 0.3), SlanKeyValue(2, 0.5), SlanKeyValue(3, 0.6)))))))))
    )))
    SlanIR.MessageIR(twoMsgs) shouldBe Valid(Map(
      "Message Name" -> MessageIR("Message Name",None,Valid(Map(
        "Field Name" -> BasicProducerConsumer("Field Name",3,List(200, 1)),
        "someBasicResourceListOfValues" -> BasicProducerConsumer("someBasicResourceListOfValues",1,List(100, 10, 1)),
        "Some Fixed Value" -> BasicProducerConsumer("Some Fixed Value",0,List(100))))),
      "Derived Message" -> MessageIR("Derived Message",Some(
        MessageIR("Message Name",None,Valid(Map(
          "Field Name" -> BasicProducerConsumer("Field Name",3,List(200, 1)),
          "someBasicResourceListOfValues" -> BasicProducerConsumer("someBasicResourceListOfValues",1,List(100, 10, 1)),
          "Some Fixed Value" -> BasicProducerConsumer("Some Fixed Value",0,List(100)))))),Valid(Map(
        "FieldX" -> ProducerConsumerComposite("FieldX",0,List(Valid(
          BasicProducerConsumer("Discrete",0,List((3,0.6), (2,0.5), (1,0.3)))))))))))
  }

  it should "translate a message and produce an error because of the orphan" in {
    val errorOrphan = List(Messages(List(
      Message(List(MessageDeclaration("Message Name", Some("x"))), List(Resources(
        List(Resource(ResourceTag("Field Name", Some("list")), List(SlanValue(1), SlanValue(200))),
          Resource(ResourceTag("someBasicResourceListOfValues", Some("queue")), List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value", None), List(SlanValue(100))))))),
      Message(List(MessageDeclaration("Derived Message", Some("Message Name"))),
        List(Resources(List(Resource(ResourceTag("FieldX", None),
          List(Resource(ResourceTag("Discrete", None), List(SlanKeyValue(1, 0.3), SlanKeyValue(2, 0.5), SlanKeyValue(3, 0.6)))))))))
    )))
    SlanIR.MessageIR(errorOrphan) shouldBe Invalid(NonEmptyList(SlanError("Definition orphaned parents: [x] is not specified"), List()))
  }
