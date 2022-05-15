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
import Translator.SlanKeywords.*
import Translator.SlantParser.convertJ2S
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

class StatesResourcesProcessor extends GenericProcessor {
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      //each agent can be assigned local resources that are not generators
      case resources: String if resources.toLowerCase === Resources.toLowerCase => Eval.now(List(LocalResources((new AgentLocalResourcesProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case stateRef: String => Eval.now(List(State(Some(stateRef), (new StateProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case None => Eval.now(List(State(None, (new StateProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }
    case None => Eval.now(List(State(None, List())))
    //there is the single anonymous state whose behavior is defined by the behavior reference
    case behaviorRef: String => Eval.now(List(State(None, List(StateBehavior(Some(behaviorRef), None)))))
    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
}
