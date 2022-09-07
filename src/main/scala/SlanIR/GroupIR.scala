/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  
 *  See the License for the specific language governing permissions and limitations under the License. 
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.{DuplicateDefinition, IncorrectParameter, IncorrectSlanSpecStructure, SeriousInternalError}
import HelperUtils.ExtentionMethods.containsHeadOnly
import Translator.SlanAbstractions.*
import Translator.SlanConstruct.*
import cats.data.Validated
import cats.syntax.all.{catsSyntaxEq, catsSyntaxValidatedId}

case class GroupIR(id: GroupReference,
                   behavior: Option[BehaviorIR | EntityId],
                   groupResources: SlanEntityValidated[Map[ResourceReference, (Translator.ConsistencyModel, Cardinality)]],
                   agents: SlanEntityValidated[List[(AgentReference, Cardinality)]]) extends SlanEntity(Option(id)):
  def withBehavior(newBehavior: BehaviorIR): GroupIR = this.copy(behavior = Option(newBehavior))

object GroupIR:
  def apply(translated: SlanConstructs): SlanEntityValidated[Map[GroupReference, GroupIR]] =
    checkForGroupsCaseClass(translated)
      .andThen(g => checkForListOfGroups(g))
      .andThen(g => checkGroupStructure(g))
      .andThen(g => checkDuplicateGroups(g))
      .andThen(g => SlanGroups2IR(g.asInstanceOf[List[Group]]))


  private def checkForGroupsCaseClass(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val filteredAgents = translated.filter(_.isInstanceOf[Agents])
    if !filteredAgents.containsHeadOnly then IncorrectSlanSpecStructure("Agents is not located").invalidNel
    else
      val filteredGroups = filteredAgents.head.asInstanceOf[Agents].agents.filter(_.isInstanceOf[Groups])
      Validated.condNel(filteredGroups.containsHeadOnly, filteredGroups.head.asInstanceOf[Groups].content, IncorrectSlanSpecStructure("global entry Groups is missing"))

  private def checkForListOfGroups(translated: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val onlyChannels: SlanConstructs = translated.filter(_.isInstanceOf[Group])
    Validated.condNel(translated.length === onlyChannels.length, translated, IncorrectParameter(s"other data structures than Group are present under Groups"))

  private def checkGroupStructure(groupz: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val allGroups = groupz.asInstanceOf[List[Group]]
    val gdecl: List[GroupDesignators] = allGroups.flatMap(g => {
      if g.id.containsHeadOnly then
        g.id.head match {
          case declaration: GroupDesignators => List(declaration)
          case _ => None
        }
      else None
    })
    Validated.condNel(gdecl.length === allGroups.length, allGroups, IncorrectParameter(s"some groups do not contain identifications"))

  private def checkDuplicateGroups(groupz: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    import cats.syntax.all.catsSyntaxOptionId
    val gIds: List[GroupReference] = groupz.asInstanceOf[List[Group]].map(_.id.head.asInstanceOf[GroupDesignators].id)
    Validated.condNel(gIds.distinct.length === gIds.length, groupz,
      DuplicateDefinition(s"groups ${
        gIds.groupBy(identity).collect { case (elem, y: List[_]) => if y.length > 1 then elem.some else None }.flatten.mkString(", ")
      }"))