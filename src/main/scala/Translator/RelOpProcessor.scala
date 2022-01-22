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
import Translator.SlanAbstractions.{SlanConstruct, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.Eq
import cats.implicits.*

class RelOpProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case entry: String if entry === LessThen => List(ROPLess((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry === LessEqual => List(ROPLessEqual((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry === GreaterEqual => List(ROPGreaterEqual((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry === GreaterThen => List(ROPGreater((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry === EqualTo
      => List(ROPEqual((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case entry: YamlPrimitiveTypes => List(SlanValue(entry))

    case unknown => throw new Exception(YamlUnexpectedTypeFound(unknown.getClass().toString + ": " + unknown.toString))
  }
