/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
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
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

import scala.collection.mutable

class CompositeKeyLookaheadProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => Eval.now((new ComplexKeyParamsLookaheadProcessor).commandProcessor(convertJ2S(v(0))).value)
    case None => Eval.now(List(SlanValue(false)))

    case unknown => Eval.now(List(SlanValue(true)))
  }
}