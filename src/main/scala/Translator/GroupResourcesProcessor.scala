/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.YamlKeyIsNotString
import Translator.SlanAbstractions.YamlTypes
import Translator.SlanConstruct.*
import Translator.SlanKeywords.Eventual
import Translator.SlantParser.convertJ2S

class GroupResourcesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v._1) match {
      case cv: String => List(ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup(Eventual, cv)), SlanValue(convertJ2S(v._2).asInstanceOf)))
      case compositeKey: (Map[_, _] | List[_]) => List(ResourceReferenceInGroup(new GroupResourceConsistencyModelKeyProcessor().commandProcessor(compositeKey), SlanValue(convertJ2S(v._2).asInstanceOf)))
      case compositeKey: (_, _) => List(
        ResourceReferenceInGroup(new GroupResourceConsistencyModelKeyProcessor().commandProcessor(compositeKey),
          new GroupResourceReplicationCoeffProcessor().commandProcessor(convertJ2S(v._2)).asInstanceOf
        )
      )
      case unknown => (new GroupResourcesProcessor).commandProcessor(convertJ2S(v._1)).asInstanceOf
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}