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
import Translator.SlanAbstractions.{SlanConstructs, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.{Eq, Eval}

class BooleanOpsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.trim.toUpperCase === NOT.toUpperCase => Eval.now(List(Not((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === AND.toUpperCase => Eval.now(List(And((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === OR.toUpperCase => Eval.now(List(Or((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === XOR.toUpperCase => Eval.now(List(Xor((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))

      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }

    case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
  }
