/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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

class RelationalOpsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.toUpperCase === NOT.toUpperCase => Eval.now(List(Not((new BooleanOpsProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case entry: String if entry.toUpperCase === AND.toUpperCase || entry.toUpperCase === OR.toUpperCase || entry.toUpperCase === LessThen.toUpperCase ||
        entry.toUpperCase === LessEqual.toUpperCase || entry.toUpperCase === GreaterThen.toUpperCase ||
        entry.toUpperCase === GreaterEqual.toUpperCase || entry.toUpperCase === EqualTo.toUpperCase => (new RelOpProcessor).commandProcessor(convertJ2S(v(1)))
      case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
    }

    case entry: YamlPrimitiveTypes => Eval.now(List(SlanValue(entry)))

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
