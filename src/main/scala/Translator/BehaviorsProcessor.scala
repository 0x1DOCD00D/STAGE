/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
import Translator.SlanKeywords.PERIODICBEHAVIORDESIGNATOR
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class BehaviorsProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case cv: String if cv.startsWith(PERIODICBEHAVIORDESIGNATOR) => List(PeriodicBehavior(cv, (new PeriodicBehaviorProcessor).commandProcessor(convertJ2S(v(1)))))
      case cv: String => List(Behavior(cv, (new MessageResponseBehaviorProcessor).commandProcessor(convertJ2S(v(1))).asInstanceOf))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }

    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}