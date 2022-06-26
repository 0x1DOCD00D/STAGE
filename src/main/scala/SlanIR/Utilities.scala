/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import scala.reflect.{ClassTag, classTag}

/*
  println(lookup[Givens.ChainGivens.type])
  println(lookup[XXX.type])
  println(lookup[XXX.type].getName.contains(XXX.getClass.getName))
  println(XXX.getClass.getName)
*/

def lookup[T <: SlanEntity : ClassTag](id: EntityId): Option[T] =
  classTag[T].runtimeClass.getName match
    case entity if entity.contains(Message.getClass.getName) => Message(id).asInstanceOf
    case entity if entity.contains(ProducerConsumer.getClass.getName) => Resource(id).asInstanceOf
    case entity if entity.contains(Generator.getClass.getName) => Resource(id).asInstanceOf
    case _ => None