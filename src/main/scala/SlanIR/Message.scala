/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, MissingDefinition}
import SlanIR.{EntityId, EntityOrError, SlanEntity}
import Translator.SlanConstruct.SlanError

case class Message(id: EntityId, parent: Option[Message], fields: List[Resource]) extends SlanEntity(id)

object Message:
  private val bookkeeper = new EntityBookkeeper[Message]
  def apply(id: EntityId): Option[Message] = bookkeeper.get(id)

  def apply(id: EntityId, parentId: Option[EntityId], fields: List[Resource]): EntityOrError[Message] =
    if bookkeeper.contains(id) then
      DuplicateDefinition(s"message $id")
    else
      parentId match
        case None => bookkeeper.set(id, new Message(id, None, fields))
        case Some(parent) =>
          if bookkeeper.contains(parent) then
            bookkeeper.set(id, Message(id, bookkeeper.get(parent), fields))
          else
            MissingDefinition(s"parent message $parent")