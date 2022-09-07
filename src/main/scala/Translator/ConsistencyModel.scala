/*
 * Copyright (c) 2021-2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.ConsistencyModel.CACHE
import cats.Eq

import scala.util.{Failure, Success, Try}

enum ConsistencyModel:
  case STRICT, SEQUENTIAL, PROCESSOR, PRAM, FIFO, CACHE, SLOW, RELEASE, DELTA, EVENTUAL

object ConsistencyModel:
  def apply(name: String): Option[ConsistencyModel] = Try(ConsistencyModel.valueOf(name.toUpperCase)) match
    case Success(v) => Some(v)
    case Failure(e) => None