/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Analyzer

import Analyzer.SlanAbstractions.{Key2Yaml2Construct, SlanConstruct, SlanProcessorSwitch, YamlTypes}
import Analyzer.SlanCommonUtilities.{containerContentProcessor, isContainerizedContent, unknownProcessor}
import Analyzer.SlanKeywords.Agents
import Analyzer.SlantParser.convertJ2S
import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString

trait GenericSwitchProcessor:
  private[Analyzer] val slanStructure: SlanProcessorSwitch

  private[Analyzer] val switchProcessor: Key2Yaml2Construct = keyword =>
    keyword match {
      case Some(key) => slanStructure.getOrElse(key.toLowerCase, unknownProcessor)(keyword)
      case None => unknownProcessor(None)
    }

  def apply(topObj: YamlTypes): List[SlanConstruct] = containerContentProcessor(constructProcessor)(topObj)

  private[Analyzer] def constructProcessor: Key2Yaml2Construct = key => yamlObject =>
    logger.info(s"Top processor: ${convertJ2S(yamlObject)}")
    if isContainerizedContent(yamlObject) then SlanTranslator(yamlObject)
    else yamlObject match {
      case v: (_, _) => convertJ2S(v._1) match {
        case cv: String => switchProcessor(Some(cv))(convertJ2S(v._2))
        case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
      }

      case unknown => containerContentProcessor(constructProcessor)(convertJ2S(yamlObject))
    }