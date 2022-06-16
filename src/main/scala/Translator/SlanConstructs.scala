/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.SlanAbstractions.*
import Translator.SlanConstruct.{GroupDesignators, SlanValue}

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

enum SlanConstruct:
  case Agent(id: AgentReference, states: SlanConstructs)
  
  case State(id: StateReference, behavior: SlanConstructs)

  case StateBehavior(behavior: BehaviorReference, switchTo: StateReference)

  case StateProbBehavior(behavior: BehaviorReference, switchTo: SlanConstructs)

  case StateProbabilitySwitch(stateId: StateReference, probSource: SlanValue)

  case SlanError(errorMessage: String)

  case IncorrectYamlType(typeName: String)

  case SlanValue(value: YamlPrimitiveTypes)
  
  case SlanNoValue
  
  case SlanKeyValue(key: YamlPrimitiveTypes, value: YamlPrimitiveTypes)

  case SlanKeyNoValue(key: YamlPrimitiveTypes)

  case Pdf(id: PdfReference, actions: SlanConstructs)
  
  case Behavior(id: BehaviorMandatoryReference, actions: SlanConstructs)
  
  case PeriodicBehavior(id: BehaviorMandatoryReference, actions: SlanConstructs)
  
  case MessageResponseBehavior(messageIds: SlanConstructs, actions: SlanConstructs)
  
  case CorrelationToken(ID: SlanValue)
  
  case IfThenElse(body: SlanConstructs)
  
  case Then(thenActions: SlanConstructs)
  
  case Else(elseActions: SlanConstructs)
  
  case ElseIf(elseIfActions: SlanConstructs)
  
  case And(relops: SlanConstructs)
  
  case Or(relops: SlanConstructs)
  
  case Xor(relops: SlanConstructs)
  
  case Not(relops: SlanConstructs)
  
  case ROPEqual(operands: SlanConstructs)
  
  case ROPLessEqual(operands: SlanConstructs)
  
  case ROPGreaterEqual(operands: SlanConstructs)
  
  case ROPLess(operands: SlanConstructs)
  
  case ROPGreater(operands: SlanConstructs)
  
  case FnUpdate(operands: SlanConstructs)
  
  case FnRemove(operands: SlanConstructs)
  
  case FnCreate(operands: SlanConstructs)
  
  case FnDestroy(operands: SlanConstructs)
  
  case FnSend(operands: SlanConstructs)
  
  case FnSelect(operands: SlanConstructs)
  
  case FnForEach(operands: SlanConstructs)
  
  case FnAdd(operands: SlanConstructs)
  
  case FnInc(operands: SlanConstructs)
  
  case FnDec(operands: SlanConstructs)
  
  case FnSubstract(operands: SlanConstructs)
  
  case FnMultiply(operands: SlanConstructs)
  
  case FnDivide(operands: SlanConstructs)
  
  case FnJoin(operands: SlanConstructs)
  
  case FnLeave(operands: SlanConstructs)
  
  case Reference(key: Option[SlanConstruct], value: Option[SlanConstructs])
  
  case PeriodParameters(timeInterval: SlanValue, iterations: SlanValue)
  
  case Messages2Send(messages: SlanValues)
  
  case MixFrequence(elements: SlanConstructs)
  
  case GoTo(state: StateReference)
  
  case LocalResources(localResourceList: SlanConstructs)
  
  case Group(id: SlanConstructs, members: SlanConstructs)

  case GroupDesignators(id: GroupReference, behaviorRef: BehaviorReference)

  case GroupAgent(id: AgentReference, cardinality: SlanConstructs)

  case ResourceConsistencyModelInGroup(cmr: ConsistencyModelReference, id: ResourceReference)
  
  case ResourceReferenceInGroup(resource: SlanConstructs, replicationCoeff: SlanConstructs)
  
  case Channel(id: ChannelReference, behaviors: SlanConstructs)
  
  case Resource(id: SlanConstruct, attributes: SlanConstructs)
  
  case ResourceTag(id: ResourceReference, storageType: StorageTypeReference)
  
  case ResourcePeriodicGenerator(resourceTypes: SlanConstructs)
  
  case ResourceProbability(id: ResourceReference, probability: SlanValue)
  
  case ResourcePDFParameters(params: SlanConstructs)
  
  case ResourcePDFConstraintsAndSeed(constraints: SlanConstructs)
  
  case PdfSeed(seed: YamlPrimitiveTypes)
  
  case ResourceAttribute(id: SlanConstruct, value: SlanValues)
  
  case PeriodicBehaviorFiringDuration(timeInterval: SlanValue, howManyTimes2Fire: Option[SlanValue])
  
  case Message(id: SlanConstructs, fields: SlanConstructs)
  
  case MessageDeclaration(id: MessageReference, parent: Option[MessageReference])
  
  case Model(id: ModelReference, elements: SlanConstructs)
  
  case AgentPopulation(agent: AgentReference, instances: SlanConstructs)
  
  case ModelGraph(id: ModelGraphReference, vEv: SlanConstructs)
  
  case Agent2AgentViaChannel(agent: AgentReference, channel2Agent: SlanConstructs)
  
  case Channel2Agent(channel: ChannelReference, agent: AgentReference)
  
  case ModelDeployment(key: String, elements: SlanConstructs)

  case ComputingNodes(nodes: SlanConstructs)

  case AkkaConfigurationParameters(akka: SlanConstructs)

  case ResourceConstructors(ingesters: SlanConstructs)

  case TableLoader(resourceName: ResourceReference, table: SlanConstructs)

  case ResourceCsvTable(table: SlanConstructs)

  case ResourceDatabaseTable(table: SlanConstructs)

  case UnknownConstruct(key: String, typeOfConstruct: String, obj: String)

/*
* Describes an action of filtering out values based on some relational operator
*     MixFrequence: {
                  Rate: Normal, #create three probabilities for three messages
                  Parameters: {
                    1: 0.6,
                    2: 0.2
                  },
                  Seed: 200,
                  Constraints: [">=": 0, "<": 1]
                }
* */

object ConstraintFilter:
  def apply(key: String, v: YamlPrimitiveTypes): ConstraintFilter = key match {
    case "<" => LESS(SlanValue(v))
    case "<=" => LESSEQUAL(SlanValue(v))
    case ">" => GREATER(SlanValue(v))
    case ">=" => GREATEREQUAL(SlanValue(v))
    case "=" => EQUAL(SlanValue(v))
    case "==" => EQUAL(SlanValue(v))
    case "===" => EQUAL(SlanValue(v))
    case _ => UNDEFINED(SlanValue(v))
  }

enum ConstraintFilter(val boundary: SlanValue):
  case LESS(v: SlanValue) extends ConstraintFilter(v)
  case LESSEQUAL(v: SlanValue) extends ConstraintFilter(v)
  case GREATER(v: SlanValue) extends ConstraintFilter(v)
  case GREATEREQUAL(v: SlanValue) extends ConstraintFilter(v)
  case EQUAL(v: SlanValue) extends ConstraintFilter(v)
  case UNDEFINED(v: SlanValue) extends ConstraintFilter(v)

