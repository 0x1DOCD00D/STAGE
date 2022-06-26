/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import SlanIR.ResourceStorage.{SLANQUEUE, SLANSTACK}
import Translator.SlanKeywords.*
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

enum ResourceStorage:
  case SLANQUEUE, SLANSTACK, SLANLIST, SLANMAP, SLANJAR, SLANREST, UNRECOGNIZED

object ResourceStorage:  
  def apply(storeKw:String): ResourceStorage =
    storeKw.toUpperCase match
      case s if s === QUEUE.toUpperCase => SLANQUEUE
      case s if s === STACK.toUpperCase => SLANSTACK
      case s if s === LIST.toUpperCase => SLANLIST
      case s if s === MAP.toUpperCase => SLANMAP
      case s if s === JAR.toUpperCase => SLANJAR
      case s if s === REST.toUpperCase => SLANREST
      case _ => UNRECOGNIZED


