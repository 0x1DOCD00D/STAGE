/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstructs, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

import scala.collection.mutable

class MessageResponseBehaviorProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) if v(1) == null => convertJ2S(v(0)) match {
      //msgName: {blah blah behavior actions}
      case msgId: String => Eval.now(List(MessageResponseBehavior(List(SlanValue(msgId)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      //null defines a periodic behavior that is invoked at specified time intervals
      case None => Eval.now(List(MessageResponseBehavior(List(), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      //? [msg1, msg2, ..., msgN]: {blah blah behavior actions}
      case msgIdList: Map[_, _] =>
        val lst = msgIdList.toList
        val forcedConversion = (lst.head, lst.tail.toMap)
        Eval.now(List(MessageResponseBehavior((new CompositeMessageKeyProcessor).commandProcessor(convertJ2S(forcedConversion._1)).value, (new BehaviorActionsProcessor).commandProcessor(convertJ2S(forcedConversion._2)).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }
    case v: (_, _) if v(1) != null => convertJ2S(v(0)) match {
      //msgName: {blah blah behavior actions}
      case msgId: String => Eval.now(List(MessageResponseBehavior(List(SlanValue(msgId)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case None => Eval.now(List(MessageResponseBehavior(List(), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      //? [msg1, msg2, ..., msgN]: {blah blah behavior actions}
      case msgIdList: (_,_) => Eval.now(List(MessageResponseBehavior((new CompositeMessageKeyProcessor).commandProcessor(convertJ2S(v(0))).value, (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case msgIdList: List[_] => Eval.now(List(MessageResponseBehavior((new CompositeMessageKeyProcessor).commandProcessor(convertJ2S(v(0))).value, (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
}