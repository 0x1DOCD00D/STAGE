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
import Translator.SlanAbstractions.{YamlPrimitiveTypes, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class StateProbSwitchProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case stateId: String => List(StateProbabilitySwitch(Some(stateId), SlanValue(1.0)))
    case v: (_, _) => (convertJ2S(v(0)), convertJ2S(v(1))) match {
      case (stateId: String, valIfAny: YamlPrimitiveTypes) => List(StateProbabilitySwitch(Some(stateId), SlanValue(valIfAny)))
      case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass.toString)).constructSlanRecord
    }
    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}