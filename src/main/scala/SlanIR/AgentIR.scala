/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
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
import Translator.SlanConstruct.MessageDeclaration
import cats.data.Validated
import cats.implicits.catsSyntaxEq

case class AgentIR(id: AgentReference, localResources: SlanEntityValidated[Map[ResourceReference, ResourceIR] | List[ResourceReference]], states: SlanEntityValidated[StateMachine]) extends SlanEntity(Option(id))

object AgentIR:
  private def checkForAgentsCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    import HelperUtils.ExtentionMethods.containsHeadOnly
    val filteredAgentsEntry = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Agents])
    Validated.condNel(filteredAgentsEntry.containsHeadOnly, filteredAgentsEntry.head.asInstanceOf[Translator.SlanConstruct.Agents].agents, IncorrectSlanSpecStructure("global entry Agents is missing"))

  private def checkForListOfAgents(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val onlyAgents: SlanConstructs = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Agent])
    Validated.condNel(onlyAgents.nonEmpty, onlyAgents, IncorrectParameter("no agents are defined"))

  private def checkForAgentsEntryContent(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val onlyAgents: SlanConstructs = translated.filter(_.isInstanceOf[Translator.SlanConstruct.Agent])
    val otherEntries: SlanConstructs = translated.filter(e=>e.isInstanceOf[Translator.SlanConstruct.Channels] || e.isInstanceOf[Translator.SlanConstruct.Groups] || e.isInstanceOf[Translator.SlanConstruct.Behaviors])
    Validated.condNel(onlyAgents.length + otherEntries.length === translated.length, onlyAgents, IncorrectSlanSpecStructure("unknown entities are defined under Agents"))

  private def checkAgentNames(agents: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val agentNames = agents.asInstanceOf[List[Translator.SlanConstruct.Agent]].filterNot(_.id.isBlank)
    Validated.condNel(agentNames.length === agents.length, agents, IncorrectParameter("some agent names are empty"))
  private def checkDuplicateAgents(agents: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val agentNames = agents.asInstanceOf[List[Translator.SlanConstruct.Agent]].map(_.id)
    Validated.condNel(agentNames.distinct.length === agentNames.length, agents,
      DuplicateDefinition(s"agents ${
        agentNames.groupBy(identity).collect { case (elem, y: List[_]) => if y.length > 1 then Some(elem) else None }.flatten.mkString(", ")
      }"))

  def apply(translated: SlanConstructs): SlanEntityValidated[Map[AgentReference, AgentIR]] =
    checkForAgentsCaseClass(translated)
      .andThen(agents => checkForListOfAgents(agents))
      .andThen(agents => checkForAgentsEntryContent(agents))
      .andThen(agents => checkAgentNames(agents))
      .andThen(agents => checkDuplicateAgents(agents))
      .andThen(agents => SlanAgents2IR(agents.asInstanceOf[List[Translator.SlanConstruct.Agent]]))
