/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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

class BehaviorActionsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if isBooleanOp(entry) => (new BooleanOpsProcessor).commandProcessor(convertJ2S(v))
      case entry: String if isRelationalOp(entry) => (new RelOpProcessor).commandProcessor(convertJ2S(v))
      case entry: String if entry.toUpperCase === IF.toUpperCase => Eval.now(List(IfThenElse((new IfThenElseProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.toUpperCase.startsWith(FnPrefix.toUpperCase) => (new FunctionProcessor).commandProcessor(convertJ2S(v))
      case period: Map[_, _] if period.size == 1 => //only one entry is allowed for intervalTime: howManytimes
        (convertJ2S(period.toList.head._1), convertJ2S(period.toList.head._2)) match {
        case (timeInterval, iterations): (YamlPrimitiveTypes, YamlPrimitiveTypes) => Eval.now(List(PeriodParameters(SlanValue(timeInterval), SlanValue(iterations))) ::: (new BehaviorActionsProcessor).commandProcessor(convertJ2S(v(1))).value)
        case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass.toString)).constructSlanRecord)
      }
      case wrongEntry: String if wrongEntry.toUpperCase === THEN.toUpperCase || wrongEntry.toUpperCase === ELSE.toUpperCase || wrongEntry.toUpperCase === ELSEIF.toUpperCase =>
        Eval.now(List(SlanUnexpectedTypeFound(wrongEntry.getClass.toString + ": " + wrongEntry)))

      //some custom external function
      //TODO: implement an external function processor
      case entry: String => (new FunctionProcessor).commandProcessor(convertJ2S(v))

      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }

    case entry: YamlPrimitiveTypes => Eval.now(List(SlanValue(entry)))

    case None => Eval.now(List())

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }

  def isBooleanOp(yamlOp: String): Boolean = yamlOp match {
    case entry: String if entry.toUpperCase === NOT.toUpperCase || entry.toUpperCase === AND.toUpperCase ||
      entry.toUpperCase === OR.toUpperCase || entry.toUpperCase === XOR.toUpperCase => true
    case _ => false
  }

  def isRelationalOp(yamlOp: String): Boolean = yamlOp match {
    case entry: String if entry === LessThen || entry === LessEqual ||
      entry === GreaterThen || entry === GreaterEqual ||
      entry === EqualTo => true
    case _ => false
  }
