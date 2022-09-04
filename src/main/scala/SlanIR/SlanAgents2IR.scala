/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.*
import HelperUtils.ExtentionMethods.*
import Translator.SlanAbstractions.{AgentReference, MessageReference, ResourceReference, SlanConstructs}
import Translator.SlanConstruct.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}

object SlanAgents2IR extends (List[Translator.SlanConstruct.Agent] => SlanEntityValidated[Map[AgentReference, AgentIR]]):
  override def apply(slanAgents: List[Translator.SlanConstruct.Agent]): SlanEntityValidated[Map[AgentReference, AgentIR]] =
    slanAgents.foldLeft(Map[AgentReference, AgentIR]()){
      (map, agent) =>
          map + (agent.id -> AgentIR(agent.id, getResourceReferences(agent), StateMachine(agent.statesAndResources.filter(_.isInstanceOf[Translator.SlanConstruct.State]))))
    }.validNel

  private def processResourceReferences(resourceRefEntry: List[Resources]): SlanEntityValidated[Map[EntityId, ResourceIR]] =
  if resourceRefEntry.containsHeadOnly then
    val resourceRefs = resourceRefEntry.head.lstOfResources
    if resourceRefs.count(_.isInstanceOf[SlanValue]) =!= resourceRefs.length then IncorrectSlanSpecStructure("resource references' structure").invalidNel
    else Map().valid
  else Map().valid

  private def getResourceReferences(agent: Agent): SlanEntityValidated[List[ResourceReference]] =
    val resourcesEntry = agent.statesAndResources.filter(_.isInstanceOf[Resources])
    if resourcesEntry.containsHeadOnly then
      val resourceNames: List[SlanValue] = resourcesEntry.head.asInstanceOf[Resources].lstOfResources.filter(_.isInstanceOf[SlanValue]).asInstanceOf[List[SlanValue]]
      val otherEntries: List[_] = resourcesEntry.head.asInstanceOf[Resources].lstOfResources.filterNot(_.isInstanceOf[SlanValue])
      if otherEntries.nonEmpty then IncorrectSlanSpecStructure(s"the following entries are specified in resource refs: ${otherEntries.toString}").invalidNel
      else resourceNames.flatMap(re => re.value match
        case name: String => List(name)
        case _ => None
      ).valid
    else
      List().valid


