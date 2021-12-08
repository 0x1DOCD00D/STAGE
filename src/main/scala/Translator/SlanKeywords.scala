/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *   
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *  
 */

package Translator

object SlanKeywords:
  val Agents = "agents"
  val Groups = "groups"
  val Behaviors = "behaviors"
  val Channels = "channels"
  val Resources = "resources"
  val Messages = "messages"
  val Models = "models"

  val InitState = "init"
  val Behavior = "behavior"
  val MixFrequencies = "MixFrequencies"
  val Rate = "Rate"
  val ParameterPrefix = "parameter"
  val Seed = "seed"
  val Constraints = "constraints"
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

  val ExternalService = "externalservice"
  val IF = "if"
  val THEN = "then"
  val ELSE = "else"
  val AND = "and"
  val OR = "or"
  val NOT = "not"
  val FOREACH = "foreach"

  val FnPrefix = "fn_"
  val Fn_Update: String = FnPrefix + "update"
  val Fn_Create: String = FnPrefix + "create"
  val Fn_Destroy: String = FnPrefix + "destroy"
  val Fn_Send: String = FnPrefix + "send"
  val Fn_Store: String = FnPrefix + "store"
  val Fn_Retrieve: String = FnPrefix + "retrieve"
  val Fn_Add: String = FnPrefix + "add"
  val Fn_Inc: String = FnPrefix + "inc"
  val Fn_Dec: String = FnPrefix + "dec"
  val Fn_Substract: String = FnPrefix + "substract"
  val Fn_Multiply: String = FnPrefix + "multiply"
  val Fn_Divide: String = FnPrefix + "divide"
