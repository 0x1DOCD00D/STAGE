/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.UnrecoverableError
import HelperUtils.ExtentionMethods.existsOne
import SlanIR.ResourceStorage.{SLANQUEUE, SLANSTACK}
import Translator.SlanKeywords.*
import cats.Eval
import cats.implicits.*
import cats.kernel.Eq

enum ResourceStorage(val id: Int):
  case SLANQUEUE extends ResourceStorage(1)
  case SLANSTACK extends ResourceStorage(2)
  case SLANLIST extends ResourceStorage(3)
  case SLANMAP extends ResourceStorage(4)
  case SLANJAR extends ResourceStorage(5)
  case SLANREST extends ResourceStorage(6)
  case SLANDISTRIBUTION extends ResourceStorage(7)
  case SLANNOTHING extends ResourceStorage(0)
  case UNRECOGNIZED extends ResourceStorage(-1)


object ResourceStorage:  
  def apply(storeKw:Option[String]): ResourceStorage =
    storeKw match
      case None => SLANNOTHING
      case Some(sp) =>
        sp.toUpperCase match
          case s if s === QUEUE.toUpperCase => SLANQUEUE
          case s if s === STACK.toUpperCase => SLANSTACK
          case s if s === LIST.toUpperCase => SLANLIST
          case s if s === MAP.toUpperCase => SLANMAP
          case s if s === JAR.toUpperCase => SLANJAR
          case s if s === REST.toUpperCase => SLANREST
          case _ => if PDFs.PdfStreamGenerator.listOfSupportedDistributions.count(dist => dist === sp.toUpperCase).existsOne then SLANDISTRIBUTION else UNRECOGNIZED
  def toResourceStorage(id: Int): ResourceStorage =
    if id === -1 then UNRECOGNIZED
    else if id === 0 then SLANNOTHING
    else if id === 1 then SLANQUEUE
    else if id === 2 then SLANSTACK
    else if id === 3 then SLANLIST
    else if id === 4 then SLANMAP
    else if id === 5 then SLANJAR
    else if id === 6 then SLANREST
    else if id === 7 then SLANDISTRIBUTION
    else throw new IndexOutOfBoundsException(UnrecoverableError("ResourceStorage id", id))