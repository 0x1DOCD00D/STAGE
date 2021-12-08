/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.{Agents, Messages, Models}
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

object SlanTranslator extends GenericProcessor :
  def apply(yamlObj: Any): List[SlanConstruct] = commandProcessor(convertJ2S(yamlObj))

  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case cv: String if cv.toLowerCase === Agents.toLowerCase => (new AgentsProcessor).commandProcessor(convertJ2S(v._2))
      case cv: String if cv.toLowerCase === Messages.toLowerCase => (new MessagesProcessor).commandProcessor(convertJ2S(v._2))
      case cv: String if cv.toLowerCase === Models.toLowerCase => (new ModelsProcessor).commandProcessor(convertJ2S(v._2))
      case unknown: String => new UnknownEntryProcessor(unknown, Some(unknown.getClass().toString)).constructSlanRecord
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
