/*
 * Copyright (c) 2021-2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
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
  /*
  * val Fn_Update: String = FnPrefix + "update"
  val Fn_Inc: String = FnPrefix + "inc"
  val Fn_Dec: String = FnPrefix + "dec"
  val Fn_Add: String = FnPrefix + "add"
  val Fn_Sub: String = FnPrefix + "sub"
  val Fn_Mult: String = FnPrefix + "mult"
  val Fn_Div: String = FnPrefix + "div"
  val Fn_Exp: String = FnPrefix + "exp"
  val Fn_Log: String = FnPrefix + "log"
  val Fn_Mod: String = FnPrefix + "mod"
  val Fn_Max: String = FnPrefix + "max"
  val Fn_Min: String = FnPrefix + "min"
  val Fn_Abs: String = FnPrefix + "abs"
  val Fn_Sqrt: String = FnPrefix + "sqrt"

  val Fn_Create: String = FnPrefix + "create"
  val Fn_Destroy: String = FnPrefix + "destroy"
  val Fn_Send: String = FnPrefix + "send"
  val Fn_ForEach: String = FnPrefix + "foreach"
  val Fn_Invoke: String = FnPrefix + "invoke"
  val Fn_Terminate: String = FnPrefix + "terminate"
  val Fn_Join: String = FnPrefix + "join"
  val Fn_Leave: String = FnPrefix + "leave"
  * */
  
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.trim.toUpperCase === Fn_Update.toUpperCase => Eval.now(List(FnUpdate((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Inc.toUpperCase => Eval.now(List(FnInc((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Dec.toUpperCase => Eval.now(List(FnDec((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Add.toUpperCase => Eval.now(List(FnAdd((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Sub.toUpperCase => Eval.now(List(FnSub((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Mult.toUpperCase => Eval.now(List(FnMult((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Div.toUpperCase => Eval.now(List(FnDiv((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Exp.toUpperCase => Eval.now(List(FnExp((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Mod.toUpperCase => Eval.now(List(FnMod((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Max.toUpperCase => Eval.now(List(FnMax((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Min.toUpperCase => Eval.now(List(FnMin((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Abs.toUpperCase => Eval.now(List(FnAbs((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Sqrt.toUpperCase => Eval.now(List(FnSqrt((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Destroy.toUpperCase => Eval.now(List(FnRemove((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Create.toUpperCase => Eval.now(List(FnCreate((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Send.toUpperCase => Eval.now(List(FnSend((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_ForEach.toUpperCase => Eval.now(List(FnForEach((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Select.toUpperCase => Eval.now(List(FnSelect((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Invoke.toUpperCase => Eval.now(List(FnInvoke((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Terminate.toUpperCase => Eval.now(List(FnTerminate((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Join.toUpperCase => Eval.now(List(FnJoin((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.trim.toUpperCase === Fn_Leave.toUpperCase => Eval.now(List(FnLeave((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: YamlPrimitiveTypes => (new ReferenceProcessor).commandProcessor(convertJ2S(v))
      case None => convertJ2S(v(1)) match {
        case entry: YamlPrimitiveTypes => Eval.now(List(GlobalReference((new ReferenceProcessor).commandProcessor(convertJ2S(v(1))).value)))
        case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
      }

      case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
    }

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
