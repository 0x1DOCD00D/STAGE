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
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class FunctionProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.toUpperCase === Fn_Update.toUpperCase => List(FnUpdate((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Remove.toUpperCase => List(FnRemove((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Create.toUpperCase => List(FnCreate((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Destroy.toUpperCase => List(FnDestroy((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Send.toUpperCase => List(FnSend((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_ForEach.toUpperCase => List(FnForEach((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Select.toUpperCase => List(FnSelect((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Add.toUpperCase => List(FnAdd((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Inc.toUpperCase => List(FnInc((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Dec.toUpperCase => List(FnDec((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Substract.toUpperCase => List(FnSubstract((new FunctionContentProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Multiply.toUpperCase => List(FnMultiply((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Divide.toUpperCase => List(FnDivide((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Join.toUpperCase => List(FnJoin((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: String if entry.toUpperCase === Fn_Leave.toUpperCase => List(FnLeave((new FnMultiplyProcessor).commandProcessor(convertJ2S(v(1)))))
      case entry: YamlPrimitiveTypes => (new ReferenceProcessor).commandProcessor(convertJ2S(v))

      case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
