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
import Translator.SlanAbstractions.{SlanConstruct, YamlTypes}
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.implicits.*
import cats.kernel.Eq

class StatesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): List[SlanConstruct] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      //each agent can be assigned local resources that are not generators
      case resources: String if resources.toLowerCase === Resources.toLowerCase => List(LocalResources((new AgentLocalResourcesProcessor).commandProcessor(convertJ2S(v(1)))))
      case stateRef: String => List(State(Some(stateRef), (new StateProcessor).commandProcessor(convertJ2S(v(1)))))
      case None => List(State(None, (new StateProcessor).commandProcessor(convertJ2S(v(1)))))
      case unknown => throw new Exception(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString))
    }
    case None => List(State(None, List()))
    //there is the single anonymous state whose behavior is defined by the behavior reference
    case behaviorRef: String => List(State(None, List(StateBehavior(Some(behaviorRef), None))))
    case unknown => new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord
  }
}
