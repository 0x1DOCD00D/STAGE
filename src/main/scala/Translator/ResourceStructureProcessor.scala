/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{SlanUnexpectedTypeFound, YamlKeyIsNotString}
import Translator.SlanAbstractions.{SlanConstructs, YamlPrimitiveTypes, YamlPrimitiveTypesNoString, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlantParser.convertJ2S
import cats.Eval

class ResourceStructureProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: List[_] => Eval.now(v.map(aV => SlanValue(convertJ2S(aV).toString)))
    case v: (_, _) => (convertJ2S(v(0)), convertJ2S(v(1))) match {
      case (key:YamlPrimitiveTypesNoString, value:YamlPrimitiveTypes) => Eval.now(List(SlanKeyValue(key, value)))
      case (key:String, value:YamlPrimitiveTypes) => Eval.now(List(Resource(
        ResourceTag(key.trim, None), List(SlanValue(value))
      )))
      case (key:YamlPrimitiveTypesNoString, None) =>Eval.now(List(SlanKeyNoValue(key)))
      case (key:String, None) =>Eval.now(List(Resource(
        ResourceTag(key.trim, None), List()
      )))
      case (key: List[_], value:Map[_,_]) => Eval.now(List(ResourcePDFParameters(key.map(aV => SlanValue(convertJ2S(aV).toString))))
      :::  List(ResourcePDFConstraintsAndSeed((new ResourcePDFSeedConstraintsProcessor).commandProcessor(convertJ2S(value)).value)))
      case (key: List[_], None) => Eval.now(List(ResourcePDFParameters(key.map(aV => SlanValue(convertJ2S(aV).toString))))
        :::  List())
      case _ => (new ResourcesProcessor).commandProcessor(convertJ2S(v))
    }
    case simpleValue: YamlPrimitiveTypes => Eval.now(List(SlanValue(simpleValue)))
    case None => Eval.now(List())
    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
