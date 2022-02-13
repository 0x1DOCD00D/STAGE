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
import cats.Eq
import cats.implicits.*

class BooleanOpsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case entry: String if entry.toUpperCase === NOT.toUpperCase => List(Not((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === AND.toUpperCase => List(And((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === OR.toUpperCase => List(Or((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))
      case entry: String if entry.toUpperCase === XOR.toUpperCase => List(Xor((new BehaviorActionsProcessor).commandProcessor(convertJ2S(v._2))))

      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
  }
