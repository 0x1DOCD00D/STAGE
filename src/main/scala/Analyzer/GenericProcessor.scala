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
import Analyzer.SlanKeywords.Agents
import Analyzer.SlantParser.convertJ2S
import HelperUtils.ErrorWarningMessages.{LogGenericMessage, YamlKeyIsNotString}

abstract class GenericProcessor:
  final def commandProcessor(yamlObj: YamlTypes): List[SlanConstruct] =
    LogGenericMessage(getClass, s"commandProcessor: ${convertJ2S(yamlObj)}")
    if isContainerizedContent(yamlObj) then containerContentProcessor(yamlObj)
    else yamlContentProcessor(yamlObj)

  protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] =
    yamlObj match {
      case v: (_, _) => convertJ2S(v._1) match {
        case cv: String => (new UnknownEntryProcessor(convertJ2S(v._2), Some(cv))).constructSlanRecord
        case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
      }

      case unknown => (new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString))).constructSlanRecord
    }

  protected final def isContainerizedContent(yamlObj: Any): Boolean = convertJ2S(yamlObj) match {
    case v@(List | Map) => true
    case v: Iterable[_] =>
      val c = 2
      true
    case unknown => false
  }


  protected final def containerContentProcessor(yamlObj: YamlTypes, key: Option[String] = None): List[SlanConstruct] =
    yamlObj match {
      case v: List[_] =>
        val res = v.foldLeft(List[SlanConstruct]())((a, e) => a ::: commandProcessor(convertJ2S(e)))
        logger.info(s"List result: ${res.toString()}")
        res

      case v: Map[_, _] =>
        val res = v.foldLeft(List[SlanConstruct]())((a, e) => a ::: commandProcessor(convertJ2S(e)))
        logger.info(s"Map result: ${res.toString()}")
        res

      case unknown => (new UnknownEntryProcessor(unknown, key)).constructSlanRecord
    }