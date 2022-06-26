/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

object SlanKeywords:
  val AgentsSection = "agents"
  val GroupsSection = "groups"
  val BehaviorsSection = "behaviors"
  val ChannelsSection = "channels"
  val ResourcesSection = "resources"
  val MessagesSection = "messages"
  val ModelsSection = "models"
  val DeploymentSection = "deployment"

  val InitState = "init"
  val Behavior = "behavior"
  val MixFrequencies = "MixFrequencies"
  val Rate = "Rate"
  val ParameterPrefix = "parameter"
  val Seed = "seed"
  val Constraints = "constraints"
  val Correlation = "~>"
  val LessThen = "<"
  val LessEqual = "<="
  val GreaterThen = ">"
  val GreaterEqual = ">="
  val EqualTo = "=="
  val Limit = "limit"
  val Time = "time"
  val Minutes = "minutes"
  val Seconds = "seconds"
  val Hours = "hours"
  val Days = "days"
  val Milliseconds: String = "milli" + Seconds
  val AndThen = "andthen"
  val SelfRef = "self"
  val GoTo = "goto"
  val Population = "population"
  val Eventual = "eventual"

  val Nodes = "nodes"
  val AkkaConfiguration = "Akka Configuration"

  val ExternalService = "externalservice"
  val IF = "if"
  val THEN = "then"
  val ELSE = "else"
  val ELSEIF = "elseif"
  val AND = "and"
  val OR = "or"
  val XOR = "xor"
  val NOT = "not"
  val PARENT = "parent"
  val CHILDREN = "children"
  val SENDER = "sender"

  val FnPrefix = "fn_"
  //update the value of some resource
  //if a resource holds a single value, it is overwritten
  //if a resource is a collection then the value is added to the collection
  val Fn_Update: String = FnPrefix + "update"
  //removes a value or values from a collection
  //it is a crude operation that checks each value
  //in a collection and if it equal to the specified one
  //then this value is removed permanently
  val Fn_Remove: String = FnPrefix + "remove"
  //create a new agent
  val Fn_Create: String = FnPrefix + "create"
  //destroys an agent: an agent can kill itself or its children
  val Fn_Destroy: String = FnPrefix + "destroy"
  //puts a message in a channel
  val Fn_Send: String = FnPrefix + "send"
  //apply some behavior to each value in a collection
  val Fn_ForEach: String = FnPrefix + "foreach"
  val Fn_Select: String = FnPrefix + "select"
  //arithmetic operations
  val Fn_Add: String = FnPrefix + "add"
  val Fn_Inc: String = FnPrefix + "inc"
  val Fn_Dec: String = FnPrefix + "dec"
  val Fn_Substract: String = FnPrefix + "substract"
  val Fn_Multiply: String = FnPrefix + "multiply"
  val Fn_Divide: String = FnPrefix + "divide"
  val Fn_Join: String = FnPrefix + "join"
  val Fn_Leave: String = FnPrefix + "leave"

  val QUEUE = "queue"
  val MAP = "list"
  val STACK = "stack"
  val LIST = "list"
  val JAR = "jar"
  val REST = "rest"