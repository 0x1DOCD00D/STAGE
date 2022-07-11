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
import Translator.SlanKeywords.Eventual
import Translator.SlantParser.convertJ2S
import cats.Eval

class BehaviorMessageKeyProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case cv: String => Eval.now(List(SlanValue(cv.trim)))
    case v: (_, _) if v(1) != null => convertJ2S(v(0)) match {
      case compositeMessageKey: List[_] =>
        val msgIds = for {
          msgId <- compositeMessageKey
        } yield SlanValue(convertJ2S(msgId).toString)
        Eval.now(List(MessageResponseBehavior(msgIds, (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }
    case v: (_, _) if v(1) == null => convertJ2S(v(0)) match {
      case compositeKey: List[_] =>
        val msgIds = for {
          msgId <- compositeKey
        } yield SlanValue(convertJ2S(msgId).toString)
        Eval.now(List(MessageResponseBehavior(msgIds, (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }
    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
}
