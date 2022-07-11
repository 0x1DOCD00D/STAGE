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
import SlanIR.{EntityId, SlanEntity}

import scala.collection.mutable.SortedSet

/*
* An agent is defined not only by its unique name but also by its resources, behaviors and the state machine.
* An agent is constructed in stages - first, its object is created with resources if any and then
* a state machine is added to the object with state transitions and the corresponding behaviors.
* */
case class Agent(id: EntityId, stateMachine: Option[StateMachine], resources: List[Resource]) extends SlanEntity(id):
  def withStateMachine(stateMachine4Agent: Option[StateMachine]): Agent =
    Agent(id, stateMachine4Agent, resources)

object Agent:
  private val bookkeeper = new EntityBookkeeper[Agent]

  def apply(id: EntityId): Option[Agent] = bookkeeper.get(id)

  def apply(id: EntityId, stateMachine: Option[StateMachine]): EntityOrError[Agent] =
    bookkeeper.get(id) match
      case Some(agent) => agent.withStateMachine(stateMachine)
      case None => MissingDefinition(s"agent $id")

  def apply(id: EntityId, resources: List[Resource]): EntityOrError[Agent] =
    if bookkeeper.contains(id) then
      DuplicateDefinition(s"agent $id")
    else
      bookkeeper.set(id, Agent(id, None, resources))
