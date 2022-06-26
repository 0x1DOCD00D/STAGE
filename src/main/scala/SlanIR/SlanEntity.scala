/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.DuplicateDefinition
import SlanIR.EntityId

import scala.collection.mutable.Map
import scala.reflect.{ClassTag, classTag}

trait SlanEntity(val name: EntityId)

class EntityBookkeeper[T <: SlanEntity : ClassTag]:
  private val EntityTable: Map[EntityId, T] = Map()
  
  def contains(id: EntityId): Boolean = EntityTable.contains(id)
  def get(id: EntityId): Option[T] = EntityTable.get(id)
  def set(id: EntityId, obj: T): EntityOrError[T] =
    if EntityTable.contains(id) then
      DuplicateDefinition(s"[${classTag[T].runtimeClass.getName}] $id")
    else
      EntityTable += id -> obj
      obj

