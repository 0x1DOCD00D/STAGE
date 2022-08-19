/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{YamlKeyIsMissing, YamlKeyIsNotString, YamlUnexpectedTypeFound}
import Translator.SlanAbstractions.{SlanConstructs, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlantParser.convertJ2S
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

class MessagesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case cv: String => Eval.now(List(Message(List(MessageDeclaration(cv.trim, None)), List(Resources((new MessageFieldsProcessor).commandProcessor(convertJ2S(v(1))).value)))))
      case kv: Map[_, _] => Eval.now(List(Message((new MessageInheritanceKeyProcessor).commandProcessor(convertJ2S(kv)).value, List(Resources((new MessageFieldsProcessor).commandProcessor(convertJ2S(v(1))).value)))))
      case None => Eval.now(List(YamlKeyIsMissing(convertJ2S(v(1).toString).toString)))
      case unknown => Eval.now(List(YamlUnexpectedTypeFound(unknown.getClass().toString + ": " + unknown.toString)))
    }

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
}