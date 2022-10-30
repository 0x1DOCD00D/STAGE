/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{SlanUnexpectedTypeFound, YamlKeyIsNotString}
import Translator.SlanAbstractions.{SlanConstructs, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

class StateProbSwitchProcessor extends GenericProcessor:
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case stateId: String => Eval.now(List(StateProbabilitySwitch(Some(stateId.trim), SlanValue(1.0))))
    case v: (_, _) => (convertJ2S(v(0)), convertJ2S(v(1))) match {
/*
      case (ck: (_, _), cv: YamlPrimitiveTypes) => convertJ2S(ck(0)) match {
        case unknown =>Eval.now(List(SlanValue(ck.toString)))
      }
*/

      case (stateId: String, valIfAny: YamlPrimitiveTypes) => Eval.now(List(StateProbabilitySwitch(Some(stateId.trim), SlanValue(valIfAny))))
      case (None, valIfAny: YamlPrimitiveTypes) => Eval.now(List(StateProbabilitySwitch(None, SlanValue(valIfAny))))
      case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass.toString)).constructSlanRecord)
    }
    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }