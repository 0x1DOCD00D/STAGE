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
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class MessagesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case cv: String => List(Message(List(MessageDeclaration(cv, None)), (new MessageFieldsProcessor).commandProcessor(convertJ2S(v._2))))
      case kv: Map[_, _] => List(Message((new MessageInheritanceKeyProcessor).commandProcessor(convertJ2S(kv)), (new MessageFieldsProcessor).commandProcessor(convertJ2S(v._2))))
      case None => throw new Exception(YamlKeyIsMissing(convertJ2S(v._2).toString))
      case unknown => throw new Exception(YamlUnexpectedTypeFound(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}