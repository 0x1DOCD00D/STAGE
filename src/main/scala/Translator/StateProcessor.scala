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
import Translator.SlanAbstractions.{YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.InitState
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class StateProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    //there are two cases to process: a key/value pair where the key designates the behavior reference
    //and the value is the state reference to switch to or null; or simply a string value that designates
    //the behavior reference in case when the state for this behavior is terminal.
    case v: (_, _) => convertJ2S(v(0)) match {
      case behaviorRef: String => convertJ2S(v(1)) match {
        case switchTo: String => List(StateBehavior(Some(behaviorRef),Some(switchTo)))
        case None => List(StateBehavior(Some(behaviorRef),None))
        case unknown => List(StateProbBehavior(Some(behaviorRef),(new StateProbSwitchProcessor).commandProcessor(unknown)))
      }
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }
    case behaviorRef: String => List(StateBehavior(Some(behaviorRef), None))

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }

}
