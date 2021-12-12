/*
 * Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.SlanAbstractions.*

/*
* An agent is a basic computing unit in Stage. An agent is defined by its name and its behavior. Its specification defines its lifecycle,
* i.e., how it is instantiated and destroyed, what its behavior is and what groups it belongs to if any. Each agent can be in one or more states
* where a separate behavior is associated with each state. An agent can generate periodic messages according to some PDF, it can receive
* messages from the attached channels and to respond to them by invoking the behavior that is associated with the state in which the agent is in.
* Hence, the structure of the agent specification includes the agent's name, the section that describes the periodic message generation and the state
* section that consists of state entries where each entry is designated by its name and its behavior in response to receiving messages of certain types.
*
*
* */
case class Agent(id: AgentReference, states: SlanConstructs) extends SlanConstruct

case class State(id: StateReference, behavior: SlanConstructs) extends SlanConstruct

case class StateBehavior(behavior: BehaviorReference, switchTo: StateReference) extends SlanConstruct

case class SlanValue(value: YamlPrimitiveTypes) extends SlanConstruct

case class Pdf(id: PdfReference, actions: SlanConstructs) extends SlanConstruct

case class Behavior(id: BehaviorReference, actions: SlanConstructs) extends SlanConstruct

case class MessageResponseBehavior(messageIds: SlanConstructs, actions: SlanConstructs) extends SlanConstruct

case class IfThenElse(body: SlanConstructs) extends SlanConstruct

case class Then(thenActions: SlanConstructs) extends SlanConstruct

case class Else(elseActions: SlanConstructs) extends SlanConstruct

case class And(relops: SlanConstructs) extends SlanConstruct

case class Or(relops: SlanConstructs) extends SlanConstruct

case class Not(relops: SlanConstructs) extends SlanConstruct

case class ROPEqual(operands: SlanConstructs) extends SlanConstruct

case class ROPLessEqual(operands: SlanConstructs) extends SlanConstruct

case class ROPGreaterEqual(operands: SlanConstructs) extends SlanConstruct

case class ROPLess(operands: SlanConstructs) extends SlanConstruct

case class ROPGreater(operands: SlanConstructs) extends SlanConstruct

case class FnUpdate(operands: SlanConstructs) extends SlanConstruct

case class FnRemove(operands: SlanConstructs) extends SlanConstruct

case class FnCreate(operands: SlanConstructs) extends SlanConstruct

case class FnDestroy(operands: SlanConstructs) extends SlanConstruct

case class FnSend(operands: SlanConstructs) extends SlanConstruct

case class FnForEach(operands: SlanConstructs) extends SlanConstruct

case class FnAdd(operands: SlanConstructs) extends SlanConstruct

case class FnInc(operands: SlanConstructs) extends SlanConstruct

case class FnDec(operands: SlanConstructs) extends SlanConstruct

case class FnSubstract(operands: SlanConstructs) extends SlanConstruct

case class FnMultiply(operands: SlanConstructs) extends SlanConstruct

case class FnDivide(operands: SlanConstructs) extends SlanConstruct


case class PeriodicBehavior(elements: SlanConstructs) extends SlanConstruct

case class Messages2Send(messages: SlanValues) extends SlanConstruct

case class MixFrequence(elements: SlanConstructs) extends SlanConstruct


case class GoTo(state: StateReference) extends SlanConstruct

case class Group(id: GroupReference, members: SlanConstructs) extends SlanConstruct

case class GroupAgent(id: AgentReference, cardinality: SlanConstructs) extends SlanConstruct

case class ResourceConsistencyModelInGroup(cmr: ConsistencyModelReference, id: ResourceReference) extends SlanConstruct

case class ResourceReferenceInGroup(resource: SlanConstructs, replicationCoeff: SlanValue) extends SlanConstruct

case class Channel(id: ChannelReference, behaviors: SlanConstructs) extends SlanConstruct

case class Resource(id: SlanConstruct, attributes: SlanConstructs) extends SlanConstruct

case class ResourceTag(id: ResourceReference, storageType: StorageTypeReference) extends SlanConstruct

case class ResourceAttribute(id: SlanConstruct, value: SlanValues) extends SlanConstruct

case class Message(id: MessageReference, fields: SlanConstructs) extends SlanConstruct

case class Model(id: ModelReference, elements: SlanConstructs) extends SlanConstruct

case class AgentPopulation(agent: AgentReference, instances: SlanConstructs) extends SlanConstruct

case class ModelGraph(id: ModelGraphReference, vEv: SlanConstructs) extends SlanConstruct

case class Agent2AgentViaChannel(agent: AgentReference, channel2Agent: SlanConstructs) extends SlanConstruct

case class Channel2Agent(channel: ChannelReference, agent: AgentReference) extends SlanConstruct

case class ModelDeployment(key: String, elements: SlanConstructs) extends SlanConstruct

case class UnknownConstruct(key: String, typeOfConstruct: String, obj: String) extends SlanConstruct
