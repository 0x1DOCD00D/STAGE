/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.SlanAbstractions.YamlTypes
import Translator.SlanConstruct.SlanValue
import Translator.SlantParser.convertJ2S
import cats.Eval

object ComplexKeyLookAhead:
  def lookAhead4ComplexKey(yamlObj: YamlTypes): Boolean = (new CompositeKeyLookaheadProcessor).commandProcessor(convertJ2S(yamlObj)).value match
    case List(SlanValue(true)) => true
    case List(SlanValue(false)) => false
    case _ => false
/*
    val res = yamlObj match {
      case v: (_, _) => Eval.now((new ComplexKeyParamsLookaheadProcessor).commandProcessor(convertJ2S(v(0))).value)
      case None => Eval.now(List(SlanValue(false)))
      case unknown => Eval.now(List(SlanValue(false)))
    }
    res.value match
      case List(SlanValue(true)) => true
      case List(SlanValue(false)) => false
      case _ => false
*/
