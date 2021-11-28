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
import Translator.SlantParser.convertJ2S
import cats.Eq
import cats.implicits.*

class IfThenElseBehaviorProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case messageID: String => null //List(MessageResponseBehavior(List(messageID), (new BehaviorsProcessor).commandProcessor(convertJ2S(v._2))))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case v => convertJ2S(v) match {
      case cv: String => List(SlanValue(cv))
      case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
    }
  }

}
