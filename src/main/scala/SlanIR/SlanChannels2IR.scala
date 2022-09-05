/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.*
import HelperUtils.ExtentionMethods.*
import Translator.SlanAbstractions.{ChannelReference, MessageReference, SlanConstructs}
import Translator.SlanConstruct.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}

object SlanChannels2IR extends (List[Translator.SlanConstruct.Channel] => SlanEntityValidated[Map[EntityId, ChannelIR]]):

  override def apply(slanChnz: List[Channel]): SlanEntityValidated[Map[EntityId, ChannelIR]] = slanChnz.foldLeft(Map[ChannelReference, ChannelIR]()){
      (map, elem) =>
        val allBehs: List[String] = elem.behaviors.foldLeft(List[String]()){
          (acc, bref) => bref.asInstanceOf[SlanValue].value match
            case v: String => v :: acc
            case somethingElse => somethingElse.toString :: acc
        }
        map + (elem.id -> ChannelIR(elem.id, allBehs))
    }.validNel
