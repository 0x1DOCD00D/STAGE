/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{YamlKeyIsMissing, YamlKeyIsNotString}
import Translator.SlanAbstractions.YamlTypes
import Translator.SlanConstruct.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class ChannelsProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case cv: String => List(Channel(cv, (new MessageResponseBehaviorProcessor).commandProcessor(convertJ2S(v._2))))
      case None => throw new Exception(YamlKeyIsMissing(convertJ2S(v._2).toString))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }
    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}