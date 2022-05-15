/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.SlanAbstractions.{SlanConstructs, YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlantParser.convertJ2S
import cats.Eval

class MessageFieldsProcessor extends GenericProcessor:
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: List[_] => Eval.now(v.map(aV => SlanValue(convertJ2S(aV).toString)))
    case v: (_, _) => (new ResourcesProcessor).commandProcessor(convertJ2S(v))
    case simpleValue: YamlPrimitiveTypes => Eval.now(List(SlanValue(simpleValue)))
    case None => Eval.now(List())
    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
