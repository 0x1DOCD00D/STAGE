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
import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import SlanKeywords.*
import SlantParser.convertJ2S

object AgentsProcessors extends GenericSwitchProcessor :
  private[Analyzer] override val slanStructure: SlanProcessorSwitch = Map(Groups.toLowerCase -> groupsProcessor)

  override def apply(topObj: YamlTypes): List[SlanConstruct] = containerContentProcessor(agentsProcessor)(topObj)

  private[Analyzer] def agentsProcessor: Key2Yaml2Construct = key => yamlObject =>
    logger.info(s"Agents processor: ${convertJ2S(yamlObject)}")
    if isContainerizedContent(yamlObject) then SlanTranslator(yamlObject)
    else yamlObject match {
      case v: (_, _) => convertJ2S(v._1) match {
        case cv: String => switchProcessor(Some(cv))(convertJ2S(v._2))
        case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
      }

      case unknown => containerContentProcessor(constructProcessor)(convertJ2S(yamlObject))
    }

  private[Analyzer] def groupsProcessor: Key2Yaml2Construct = key => yamlObject =>
    logger.info(s"Groups processor: ${convertJ2S(yamlObject)}")
    if isContainerizedContent(yamlObject) then SlanTranslator(yamlObject)
    else yamlObject match {
      case v: (_, _) => convertJ2S(v._1) match {
        case cv: String => switchProcessor(Some(cv))(convertJ2S(v._2))
        case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
      }

      case unknown => containerContentProcessor(constructProcessor)(convertJ2S(yamlObject))
    }