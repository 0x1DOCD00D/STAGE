/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.*
import HelperUtils.ExtentionMethods.containsHeadOnly
import Translator.SlanAbstractions.SlanConstructs
import Translator.SlanConstruct
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.*
import cats.instances.*
import cats.syntax.*
import cats.syntax.validated.*

import scala.reflect.{ClassTag, classTag}

trait UniversalChecks[T <: SlanConstruct : ClassTag, W <: SlanConstruct : ClassTag]:
  def checkDuplicateNames(input: SlanConstructs, getIds: T => String): SlanEntityValidated[SlanConstructs] =
    val allEntries = input.asInstanceOf[List[T]]
    val mIds: List[String] = allEntries.map(elem => getIds(elem))
    Validated.condNel(mIds.distinct.length === mIds.length, input,
      DuplicateDefinition(s"${classTag[T].runtimeClass.getName}: ${
        mIds.groupBy(identity).collect { case (elem, y: List[_]) => if y.length > 1 then elem.some else None }.flatten.mkString(", ")
      }"))

  def checkForEncasingClass(translated: SlanConstructs, getEncasingEntry: SlanConstructs => List[W], getContent: W => List[T]): SlanEntityValidated[SlanConstructs] =
    val filtered: List[W] = getEncasingEntry(translated)
    Validated.condNel(filtered.containsHeadOnly, getContent(filtered.head), IncorrectSlanSpecStructure(s"global entry ${classTag[T].runtimeClass.getName}"))

  def checkForListOfEntities(translated: SlanConstructs, getEntities: SlanConstructs => List[T]): SlanEntityValidated[SlanConstructs] =
    Validated.condNel(translated.length === getEntities(translated).length, translated, IncorrectParameter(s" other data structures than ${classTag[T].runtimeClass.getName} are present"))



