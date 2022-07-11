/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{YamlKeyIsNotString, YamlUnexpectedTypeFound}
import Translator.SlanAbstractions.{SlanConstructs, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.{Eq, Eval}

class RelOpProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.trim === LessThen => Eval.now(List(ROPLess((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim === LessEqual => Eval.now(List(ROPLessEqual((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim === GreaterEqual => Eval.now(List(ROPGreaterEqual((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim === GreaterThen => Eval.now(List(ROPGreater((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim === EqualTo => Eval.now(List(ROPEqual((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }

    case entry: YamlPrimitiveTypes => Eval.now(List(SlanValue(entry)))

    case unknown => Eval.now(List(YamlUnexpectedTypeFound(unknown.getClass().toString + ": " + unknown.toString)))
  }
