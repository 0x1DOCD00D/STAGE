/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License. 
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, IncorrectParameter, IncorrectSlanSpecStructure, MissingDefinition}
import Translator.SlanAbstractions.*
import Translator.SlanConstruct
import Translator.SlanConstruct.{Agent, Agents, MessageDeclaration}
import cats.data.Validated
import cats.implicits.catsSyntaxEq

case class AgentIR(id: AgentReference, localResources: SlanEntityValidated[Map[ResourceReference, ResourceIR] | List[ResourceReference]], states: SlanEntityValidated[StateMachine]) extends SlanEntity(Option(id))

object AgentIR extends UniversalChecks[Agent, Agents]:
  private def checkForAgentsEntryContent(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val onlyAgents: SlanConstructs = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Agent])
    val otherEntries: SlanConstructs = translated.filter(e=>e.isInstanceOf[Translator.SlanConstruct.Channels] || e.isInstanceOf[Translator.SlanConstruct.Groups] || e.isInstanceOf[Translator.SlanConstruct.Behaviors])
    Validated.condNel(onlyAgents.length + otherEntries.length === translated.length, onlyAgents, IncorrectSlanSpecStructure("unknown entities are defined under Agents"))

  private def checkAgentNames(agents: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val agentNames = agents.asInstanceOf[List[Translator.SlanConstruct.Agent]].filterNot(_.id.isBlank)
    Validated.condNel(agentNames.length === agents.length, agents, IncorrectParameter("some agent names are empty"))

  def apply(translated: SlanConstructs): SlanEntityValidated[Map[AgentReference, AgentIR]] =
    checkForEncasingClass(translated, (scLst:SlanConstructs)=>{scLst.filter(_.isInstanceOf[Agents]).asInstanceOf[List[Agents]]}, (as: Agents)=> as.agents.asInstanceOf[List[Agent]] )
      .andThen(agents => checkForListOfEntities(agents, (scLst:SlanConstructs)=>{scLst.filter(_.isInstanceOf[Agent]).asInstanceOf[List[Agent]]}))
      .andThen(agents => checkForAgentsEntryContent(agents))
      .andThen(agents => checkAgentNames(agents))
      .andThen(agents => checkDuplicateNames(agents, (a: Agent) => {a.id}))
      .andThen(agents => SlanAgents2IR(agents.asInstanceOf[List[Translator.SlanConstruct.Agent]]))
