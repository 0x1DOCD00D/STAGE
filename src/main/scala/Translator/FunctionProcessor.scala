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
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

class FunctionProcessor extends GenericProcessor:
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.trim.toUpperCase === Fn_Update.toUpperCase => Eval.now(List(FnUpdate((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Remove.toUpperCase => Eval.now(List(FnRemove((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Create.toUpperCase => Eval.now(List(FnCreate((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Destroy.toUpperCase => Eval.now(List(FnDestroy((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Send.toUpperCase => Eval.now(List(FnSend((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_ForEach.toUpperCase => Eval.now(List(FnForEach((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Select.toUpperCase => Eval.now(List(FnSelect((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Add.toUpperCase => Eval.now(List(FnAdd((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Inc.toUpperCase => Eval.now(List(FnInc((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Dec.toUpperCase => Eval.now(List(FnDec((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Substract.toUpperCase => Eval.now(List(FnSubstract((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Multiply.toUpperCase => Eval.now(List(FnMultiply((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Divide.toUpperCase => Eval.now(List(FnDivide((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Join.toUpperCase => Eval.now(List(FnJoin((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Leave.toUpperCase => Eval.now(List(FnLeave((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: YamlPrimitiveTypes => (new ReferenceProcessor).commandProcessor(convertJ2S(v))

      case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
    }

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
