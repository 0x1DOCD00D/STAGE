/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
import Translator.SlantParser.convertJ2S
import cats.Eval

class ResourcePDFSeedConstraintsProcessor extends GenericProcessor:
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => (convertJ2S(v(0)), convertJ2S(v(1))) match {
      case (seed:YamlPrimitiveTypes, None) => Eval.now(List(PdfSeed(seed)))
      case (seed:YamlPrimitiveTypes, value:YamlPrimitiveTypes) => Eval.now(List(PdfSeed(seed)) ::: List(SlanValue(value)))
      case (seed:YamlPrimitiveTypes, constraints:Map[_,_]) => Eval.now(List(PdfSeed(seed)) ::: (new ResourcePDFConstraintProcessor).commandProcessor(convertJ2S(constraints)).value)
      case (seed:YamlPrimitiveTypes, constraints:List[_]) => Eval.now(List(PdfSeed(seed)) ::: (new ResourcePDFConstraintProcessor).commandProcessor(convertJ2S(constraints)).value)
      case (None, constraints:Map[_,_]) => Eval.now((new ResourcePDFConstraintProcessor).commandProcessor(convertJ2S(constraints)).value)
      case (None, constraints:List[_]) => Eval.now((new ResourcePDFConstraintProcessor).commandProcessor(convertJ2S(constraints)).value)
      case _ => Eval.now(List())
    }
    case simpleValue: YamlPrimitiveTypes => Eval.now(List(SlanValue(simpleValue)))
    case None => Eval.now(List())
    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
