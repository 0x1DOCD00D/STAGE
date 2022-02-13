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
import Translator.SlanAbstractions.YamlTypes
import Translator.SlanConstruct.*
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

import scala.collection.mutable

class MessageResponseBehaviorProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) if v._2 == null => convertJ2S(v._1) match {
      //msgName: {blah blah behavior actions}
      case msgId: String => List(MessageResponseBehavior(List(SlanValue(msgId)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      //null defines a periodic behavior that is invoked at specified time intervals
      case None => List(MessageResponseBehavior(List(), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      //? [msg1, msg2, ..., msgN]: {blah blah behavior actions}
      case msgIdList: Map[_, _] =>
        val lst = msgIdList.toList
        val forcedConversion = (lst.head, lst.tail.toMap)
        List(MessageResponseBehavior((new CompositeMessageKeyProcessor).commandProcessor(convertJ2S(forcedConversion._1)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(forcedConversion._2))))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }
    case v: (_, _) if v._2 != null => convertJ2S(v._1) match {
      //msgName: {blah blah behavior actions}
      case msgId: String => List(MessageResponseBehavior(List(SlanValue(msgId)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case None => List(MessageResponseBehavior(List(), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      //? [msg1, msg2, ..., msgN]: {blah blah behavior actions}
      case msgIdList: (_,_) => List(MessageResponseBehavior((new CompositeMessageKeyProcessor).commandProcessor(convertJ2S(v._1)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case msgIdList: List[_] => List(MessageResponseBehavior((new CompositeMessageKeyProcessor).commandProcessor(convertJ2S(v._1)), (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}