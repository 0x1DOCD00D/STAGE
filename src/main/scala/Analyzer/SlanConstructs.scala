/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Analyzer

import SlanAbstractions.SlanConstruct

case class Agent(name: String, structure: List[SlanConstruct]) extends SlanConstruct

case class Message(name: String) extends SlanConstruct

case class Model(name: String) extends SlanConstruct

case class UnknownConstruct(key: Option[String], typeOfConstruct: String, obj: String) extends SlanConstruct


