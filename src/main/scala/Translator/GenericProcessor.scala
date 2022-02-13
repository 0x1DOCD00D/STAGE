/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.{LogGenericMessage, YamlKeyIsNotString}
import Translator.SlanAbstractions.YamlTypes
import Translator.SlanConstruct.*
import Translator.SlanKeywords.Agents
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

/*
* The main idea of this generic processing logic is to extract values wrapped into containers
  as key/value pairs or simply as values while discarding the wrapping containers in the process.
  It may lead to certain problem. Consider the following yaml definition:
  methodName:
          - [ p1name: parm1, p2name: parm2 ]
          - parm1
          - null
  The key methodName will have four instead of three values, since the list value is a list that
  will be discarded and key/value pairs will be added to the complex object defined by the key methodName.
  To avoid side-effects of this auto-flattening behavior please use the following construct.
  - methodName: [ p1name: parm1, p2name: parm2 ]
  - methodName:  parm1
  - methodName:
  - otherMethodName: [ parm1, parm2, parm3 ]
* */

abstract class GenericProcessor:
  final def commandProcessor(yamlObj: YamlTypes): List[SlanConstruct] =
    LogGenericMessage(getClass, s"commandProcessor: ${convertJ2S(yamlObj)}")
    if isContainerizedContent(yamlObj) then containerContentProcessor(yamlObj)
    else yamlContentProcessor(yamlObj)

  protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] =
    yamlObj match {
      case v: (_, _) => convertJ2S(v._1) match {
        case cv: String => new UnknownEntryProcessor(convertJ2S(v._2), Some(cv)).constructSlanRecord
        case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
      }

      case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
    }

  protected final def isContainerizedContent(yamlObj: Any): Boolean = convertJ2S(yamlObj) match {
    //    case v@(List | Map) => true
    case v: Iterable[_] => true
    case _ => false
  }

  protected final def containerContentProcessor(yamlObj: YamlTypes, key: Option[String] = None): List[SlanConstruct] = convertJ2S(yamlObj) match {
    case v: List[_] => v.foldLeft(List[SlanConstruct]())((a, e) => a ::: commandProcessor(convertJ2S(e)))
    case v: Map[_, _] => v.foldLeft(List[SlanConstruct]())((a, e) => a ::: commandProcessor(convertJ2S(e)))
    case unknown => new UnknownEntryProcessor(unknown, key).constructSlanRecord
  }

  protected[Translator] final def lookAhead(key: String, yamlObj: YamlTypes): Boolean =
    if isContainerizedContent(yamlObj) then convertJ2S(yamlObj) match {
      case v: List[_] => v.foldLeft(false)((result, e) => result | lookAhead(key, convertJ2S(e)))
      case v: Map[_, _] => v.foldLeft(false)((result, e) => result | lookAhead(key, convertJ2S(e)))
      case unknown => false
    }
    else yamlObj match {
      case v: (_, _) => convertJ2S(v._1) match {
        case cv: String if cv.toLowerCase === key.toLowerCase => true
        case unknown => false
      }
      case unknown => false
    }
