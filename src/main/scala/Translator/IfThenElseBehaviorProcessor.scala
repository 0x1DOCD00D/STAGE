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
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.{Eq, Eval}

class IfThenElseBehaviorProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.trim.toUpperCase === THEN.toUpperCase => Eval.now(List(Then((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === ELSE.toUpperCase => Eval.now(List(Else((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === AND.toUpperCase || entry.trim.toUpperCase === OR.toUpperCase || entry.trim.toUpperCase === NOT.toUpperCase ||
                            entry.trim.toUpperCase === LessThen.toUpperCase || entry.trim.toUpperCase === LessEqual.toUpperCase || entry.trim.toUpperCase === GreaterThen.toUpperCase ||
                            entry.trim.toUpperCase === GreaterEqual.toUpperCase || entry.trim.toUpperCase === EqualTo.toUpperCase => (new RelOpProcessor).commandProcessor(convertJ2S(v))
      case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
    }

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
