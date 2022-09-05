/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License. 
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, IncorrectParameter, IncorrectSlanSpecStructure, SeriousInternalError}
import HelperUtils.ExtentionMethods.containsHeadOnly
import Translator.SlanAbstractions.{ChannelReference, SlanConstructs}
import Translator.SlanConstruct.{Agents, Channel, Channels, SlanValue}
import cats.data.Validated
import cats.syntax.all.{catsSyntaxEq, catsSyntaxValidatedId}
case class ChannelIR(id: ChannelReference, behaviors: List[BehaviorIR] | List[EntityId]) extends SlanEntity(Option(id)):
  def withBehavior(newBehavior: List[BehaviorIR]): ChannelIR = this.copy(behaviors = newBehavior )


object ChannelIR:
  def apply(translated: SlanConstructs): SlanEntityValidated[Map[ChannelReference, ChannelIR]] =
    checkForChannelsCaseClass(translated)
      .andThen(c => checkForListOfChannels(c))
      .andThen(c => checkChannelStructure(c))
      .andThen(c => checkDuplicateChannels(c))
      .andThen(c => SlanChannels2IR(c.asInstanceOf[List[Channel]]))


  private def checkForChannelsCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val filteredAgents = translated.filter(_.isInstanceOf[Agents])
    if !filteredAgents.containsHeadOnly then IncorrectSlanSpecStructure("Agents is not located").invalidNel
    else
      val filteredChannels = filteredAgents.head.asInstanceOf[Agents].agents.filter(_.isInstanceOf[Channels])
      Validated.condNel(filteredChannels.containsHeadOnly, filteredChannels.head.asInstanceOf[Channels].content, IncorrectSlanSpecStructure("global entry Channels is missing"))

  private def checkForListOfChannels(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val onlyChannels: SlanConstructs = translated.filter(_.isInstanceOf[Channel])
    Validated.condNel(translated.length === onlyChannels.length, translated, IncorrectParameter(s"other data structures than Channel are present under Channels"))

  private def checkChannelStructure(channelz: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val allChannels = channelz.asInstanceOf[List[Channel]]
    val svLst: List[String] = allChannels.flatMap(c =>
      val cb = c.behaviors.filterNot(_.isInstanceOf[SlanValue])
      if cb.nonEmpty then List(s"${c.id}: [${cb.mkString(", ")}]") else None )
    Validated.condNel(svLst.isEmpty, channelz, IncorrectParameter(svLst.mkString(ErrorMsgSeparator)))

  private def checkDuplicateChannels(channelz: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    import cats.syntax.all.catsSyntaxOptionId
    val cIds: List[ChannelReference] = channelz.asInstanceOf[List[Channel]].map(_.id)
    Validated.condNel(cIds.distinct.length === cIds.length, channelz,
    DuplicateDefinition(s"messages ${
      cIds.groupBy(identity).collect { case (elem, y: List[_]) => if y.length > 1 then elem.some else None }.flatten.mkString(", ")
    }"))