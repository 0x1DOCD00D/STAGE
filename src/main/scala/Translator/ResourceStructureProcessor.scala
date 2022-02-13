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
import Translator.SlanAbstractions.{YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlantParser.convertJ2S

class ResourceStructureProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: List[_] => v.map(aV => SlanValue(convertJ2S(aV).toString))

    case v: (_, _) => (convertJ2S(v(0)), convertJ2S(v(1))) match {
      case (key:YamlPrimitiveTypes, value:YamlPrimitiveTypes) => List(SlanKeyValue(key, value))
      case (key: List[_], value:Map[_,_]) => List(ResourcePDFParameters(key.map(aV => SlanValue(convertJ2S(aV).toString))))
      :::  List(ResourcePDFConstraintsAndSeed((new ResourcePDFSeedConstraintsProcessor).commandProcessor(convertJ2S(value))))
      case _ => (new ResourcesProcessor).commandProcessor(convertJ2S(v))
    }
    case simpleValue: YamlPrimitiveTypes => List(SlanValue(simpleValue))
    case None => List()
    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
