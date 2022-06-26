/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import SlanIR.{EntityId, SlanEntity}

import scala.collection.mutable.SortedSet

case class Agent(val id: EntityId, val resources: List[Resource]) extends SlanEntity(id)
//  private val stateTable: Map[String, String] = Map()

object Agent:
  private val AllAgents: SortedSet[EntityId] = SortedSet()

  def apply(id: EntityId, resources: List[Resource]): Option[Agent] =
    if AllAgents.contains(id) then None else
      AllAgents += id
      Some(new Agent(id, resources))