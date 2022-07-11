/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, EmptyWarning, MissingDefinition}
import SlanIR.Behavior.bookkeeper
import SlanIR.EntityId
import cats.Eq
import cats.syntax.eq.*

import scala.collection.mutable.{Map, Set}

/*
* The state is specified by its name unique within a given agent and it is defined by some behavior(s) and
* the transition function that specifies to what other state to switch after the execution of the behavior
* or to stay in the same state if no destination state is specified. A state can be associated with multiple
* behaviors and with many destination states. Each behavior can be triggered in response to specific messages
* and whichever behavior is triggered first the agent will be transitioned to a new state specified for this
* triggered behavior. Each destination state, on the other hand is specified with some likelihood
* as a Double value or as a reference to a resource Generator that produces these likelihood values.
* If no behavior (null) is specified it means that received messages are logged and discarded.
* */
case class State(id: Option[EntityId], behavior: Option[List[Behavior]], switchTo: Option[List[(State, Option[PdfGenerator])]]) extends SlanEntity(id.getOrElse(StateMachine.INITSTATE))

case class StateMachine(agent: EntityId, initState: State, states: Option[List[State]]):
  require(!agent.isEmpty)

object StateMachine:
  val INITSTATE: EntityId = ""
  private val bookkeeper: Map[AgentId, EntityBookkeeper[State]] = Map()
/*
  def apply(agentId: EntityId, statesInfo: List[(Option[EntityId], Option[List[Behavior]], Option[List[(State, Option[PdfGenerator])]])]): Option[StateMachine] =
    if verifyStates(agentId, statesInfo) then
*/


  /*
  * The internal method verifyStates is invoked recursively to check the number of the init states,
  * the duplicate state declarations and whether the declarations of states exist that are specified
  * in the transition destination for each state.
  * */
/*
  private def verifyStates(agentId: EntityId, statesInfo: List[(Option[EntityId], Option[EntityId], Option[List[EntityId]])]): Boolean =
    val filteredOutInitStates:List[(Option[EntityId], Option[EntityId], Option[List[EntityId]])] = statesInfo.filter(stateElem => stateElem(0) =!= None)

    val checkInitStates = if statesInfo.filter(stateElem => stateElem(0) === None).length > 1 then
      DuplicateDefinition(s"multiple init states in agent $agentId")
      false
    else true


    val checkDeclaredStates = filteredOutInitStates.foldLeft(true) {
      (acc, stateElem) =>
        stateElem(0) match
          case Some(stateId) =>
            if bookkeeper.contains(agentId) then
              if bookkeeper(agentId).contains(stateId) then
                DuplicateDefinition(s"state $stateId in agent $agentId")
                acc & false
              else
                bookkeeper(agentId) += null -> null//stateId -> State(Some(stateId), )
            else
              bookkeeper(agentId -> Set(stateId))
          case None => InternalError(s"[agent $agentId] states contain an init state that was filtered out")
    }

    val checkTransitionedStates = stateIds.foldLeft(true) {
      (acc, stateElem) =>
        if bookkeeper.contains(agentId) then
          if !bookkeeper(agentId).contains(stateElem(2)) then
            MissingDefinition(s"transition to state $stateId in agent $agentId")
            acc & false
        else
          MissingDefinition(s"agent $agentId")
    }

    agentId
*/
