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
import Translator.SlanAbstractions.{SlanConstructs, YamlTypes}
import Translator.SlanConstruct.*
import Translator.SlanKeywords.Eventual
import Translator.SlantParser.convertJ2S
import cats.Eval

class GroupResourcesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case cv: String => Eval.now(List(ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup(Eventual, cv)), List(SlanValue(convertJ2S(v(1)).asInstanceOf)))))
      case compositeKey: (Map[_, _] | List[_]) => Eval.now(List(ResourceReferenceInGroup(new GroupResourceConsistencyModelKeyProcessor().commandProcessor(compositeKey).value, List(SlanValue(convertJ2S(v(1)).asInstanceOf)))))
      case compositeKey: (_, _) => Eval.now(List(
        ResourceReferenceInGroup(new GroupResourceConsistencyModelKeyProcessor().commandProcessor(compositeKey).value,
          new GroupResourceReplicationCoeffProcessor().commandProcessor(convertJ2S(v(1))).value
        )
      ))
      case unknown => (new GroupResourcesProcessor).commandProcessor(convertJ2S(v(0)))
    }

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
}