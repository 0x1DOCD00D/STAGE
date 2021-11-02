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
  val Periodic = "periodic"
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
  val Milliseconds = "milli" + Seconds
  val AndThen = "andthen"
  val SelfRef = "self"
  val GoTo = "goto"
  val Population = "population"
  val Fn_Create = "fn_create"

  val FnPrefix = "fn_"
  val Fn_Destroy = "destroy"
  val Fn_Send = "send"
  val Fn_Store = "store"
  val Fn_Retrieve = "retrive"
  val Fn_Add = "add"
  val Fn_Inc = "inc"
  val Fn_Dec = "dec"
  val Fn_Substract = "substract"
  val Fn_Multiply = "multiply"
  val Fn_Divide = "divide"
  val Fn_Substring = "substring"
  val Fn_Iterate = "iterate"
