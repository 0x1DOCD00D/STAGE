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

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.reflect.{ClassTag, classTag}

trait SlanEntity(val name: Option[EntityId])

class EntityBookkeeper[T <: SlanEntity : ClassTag]:
  private val EntityTable: mutable.Map[EntityId, T] = mutable.Map()

  override def toString: _root_.java.lang.String = EntityTable.toList.mkString("[", "; ", "]")
  def size: Int = EntityTable.size
  def contains(id: EntityId): Boolean = EntityTable.contains(id)
  def get(id: EntityId): Option[T] = EntityTable.get(id)
  def set(id: EntityId, obj: T): Option[T] =
    if id.isBlank then None
    else if EntityTable.contains(id) then
      EntityTable(id) = obj
//      DuplicateDefinition(s"[${classTag[T].runtimeClass.getName}] $id")
    else
      EntityTable += id -> obj
    get(id)

  def clear: Int =
    val sz = EntityTable.size
    EntityTable.clear()
    sz
