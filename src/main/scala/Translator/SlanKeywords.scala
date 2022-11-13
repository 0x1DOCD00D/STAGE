/*
 * Copyright (c) 2021-2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
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
  /*
  * \begin{description}
     \item[Fn\_Update] takes two parameters: the reference to a resource and an expression.
      \item[Fn\_Inc] takes an expression and increments its value.
      \item[Fn\_Dec] takes an expression and decrements its value.
      \item[Fn\_Add] takes two expressions and computes the sum of their values.
      \item[Fn\_Sub] takes two expressions and computes the difference of their values.
      \item[Fn\_Mult] takes two expressions and computes the product of their values.
      \item[Fn\_Div] takes three expressions and computes the quotient of the values of the first two expressions. If the divisor is zero then the value of the third expression is used to return -- the third expression should be a number literal or a resource.
      \item[Fn\_Exp] takes two expressions and computes the value of the first expression power the value of the second expression.
      \item[Fn\_Log] takes three expressions and computes the log of the value of the second expression with the value of the first one as the base. The third expression should be a number literal or a resource returned if the value of the second expression is negative.
      \item[Fn\_Mod] takes two expressions and computes the modulo operation of the value of the first operand with the value of the second operand as its modulus.
      \item[Fn\_Max] takes two expressions and computes the maximum value of either of them.
      \item[Fn\_Min] takes two expressions and computes the minimum value of either of them.
      \item[Fn\_Abs] takes one expression and computes its absolute value.
      \item[Fn\_Sqrt] takes two expressions and either computes the square root of the first expression or returns the value of the second expression that should be a number literal or a resource.
  \end{description}

  The other class of functions deal with simulation entities like agents, groups and messages. Agents and messages can be created and destroyed programmatically, agents can join and leave groups and they can send messages from their behaviors.
  \begin{description}
     \item[Fn\_Create] takes one parameter that is the mapping between the reference to a message or an agent and its quantity, e.g, \textbf{Fn\_Create:} \texttt{[AgentName: 2]}. The function creates agents or messages and returns a reference to their collection.
     \item[Fn\_Destroy] destroy the current agent in which it is invoked and it returns void -- the destruction is the side-effect of the operation.
     \item[Fn\_Send] takes one parameter that is the reference to a message and puts this message in all channels attached to the agent from which behavior this function is called.
     \item[Fn\_ForEach] takes three parameters where the first one is the reference to a resource that holds a collection of values, e.g., a list, the second is the name of a local resource that will be assigned the value of each iterated upon element, and the third is an expression that is applied it to each element of the collection that is specified by the second parameter.
      \item[Fn\_Invoke] takes one parameter that is the reference to an external resource as a composite object that contains the reference to an invoked method and its parameter values.
      \item[Fn\_Terminate] terminates a behavior in which it is invoked.
  \end{description}
  * */
  val Fn_Update: String = FnPrefix + "update"
  val Fn_Inc: String = FnPrefix + "inc"
  val Fn_Dec: String = FnPrefix + "dec"
  val Fn_Add: String = FnPrefix + "add"
  val Fn_Sub: String = FnPrefix + "sub"
  val Fn_Mult: String = FnPrefix + "mult"
  val Fn_Div: String = FnPrefix + "div"
  val Fn_Exp: String = FnPrefix + "exp"
  val Fn_Log: String = FnPrefix + "log"
  val Fn_Mod: String = FnPrefix + "mod"
  val Fn_Max: String = FnPrefix + "max"
  val Fn_Min: String = FnPrefix + "min"
  val Fn_Abs: String = FnPrefix + "abs"
  val Fn_Sqrt: String = FnPrefix + "sqrt"

  val Fn_Create: String = FnPrefix + "create"
  val Fn_Destroy: String = FnPrefix + "destroy"
  val Fn_Send: String = FnPrefix + "send"
  val Fn_ForEach: String = FnPrefix + "foreach"
  val Fn_Select: String = FnPrefix + "select"
  val Fn_Invoke: String = FnPrefix + "invoke"
  val Fn_Terminate: String = FnPrefix + "terminate"
  val Fn_Join: String = FnPrefix + "join"
  val Fn_Leave: String = FnPrefix + "leave"

  val QUEUE = "queue"
  val MAP = "map"
  val STACK = "stack"
  val LIST = "list"
  val JAR = "jar"
  val REST = "rest"