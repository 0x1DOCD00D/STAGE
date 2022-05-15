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


class AgentsProcessor extends GenericProcessor :
  override protected def yamlContentProcessor(yamlObj: YamlTypes): Eval[SlanConstructs] = yamlObj match {
    case v: (_, _) => convertJ2S(v(0)) match {
      case entry: String if entry.toUpperCase === Groups.toUpperCase => (new GroupsProcessor).commandProcessor(convertJ2S(v(1)))
      case entry: String if entry.toUpperCase === Behaviors.toUpperCase => (new BehaviorsProcessor).commandProcessor(convertJ2S(v(1)))
      case entry: String if entry.toUpperCase === Channels.toUpperCase => (new ChannelsProcessor).commandProcessor(convertJ2S(v(1)))
      case entry: String if entry.toUpperCase === Resources.toUpperCase => (new ResourcesProcessor).commandProcessor(convertJ2S(v(1)))
      //the ambiguity comes from distinguishing key: value pairs as designating states or messages under the entry agent
      //if an agent contains the keyword Behavior under its name then it means that there is the single state for this agent
      //otherwise it is a sequence of states each defining its own behavior
      case cv: String => Eval.now(List(Agent(cv, (new StatesResourcesProcessor).commandProcessor(convertJ2S(v(1))).value)))
      case unknown => Eval.now(List(YamlKeyIsNotString(unknown.getClass().toString + ": " + unknown.toString)))
    }

    case unknown => Eval.now(new UnknownEntryProcessor(unknown.toString, Some(unknown.getClass().toString)).constructSlanRecord)
  }
