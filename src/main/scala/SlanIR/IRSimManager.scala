/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import Translator.SlanAbstractions.*
import cats.data.Validated.{Invalid, Valid}

import scala.reflect.{ClassTag, classTag}

case class IRSimManager(simId: EntityId,
                        agentsTbl: Option[SlanEntityValidated[Map[AgentReference, AgentIR]]] = None,
                        groupsTbl: Option[SlanEntityValidated[Map[GroupReference, GroupIR]]] = None,
                        channelTbl: Option[SlanEntityValidated[Map[ChannelReference, ChannelIR]]] = None,
                        behaviorTbl: Option[SlanEntityValidated[Map[BehaviorMandatoryReference, BehaviorIR]]] = None,
                        messageTbl: Option[SlanEntityValidated[Map[MessageReference, MessageIR]]] = None,
                        resourceTbl: Option[SlanEntityValidated[Map[ResourceReference, ResourceIR]]] = None,
                        modelTbl: Option[SlanEntityValidated[Map[ModelReference, ModelIR]]] = None,
                    ) extends SlanEntity(Option(simId)):
  require(!simId.isBlank)

  def withAgents(agents: SlanEntityValidated[Map[AgentReference, AgentIR]]): Option[IRSimManager] = IRSimManager(this.copy(agentsTbl = Option(agents)))
  def withGroups(groups: SlanEntityValidated[Map[GroupReference, GroupIR]]): Option[IRSimManager] = IRSimManager(this.copy(groupsTbl = Option(groups)))
  def withChannels(channels: SlanEntityValidated[Map[ChannelReference, ChannelIR]]): Option[IRSimManager] = IRSimManager(this.copy(channelTbl = Option(channels)))
  def withBehaviors(behaviors: SlanEntityValidated[Map[BehaviorMandatoryReference, BehaviorIR]]): Option[IRSimManager] = IRSimManager(this.copy(behaviorTbl = Option(behaviors)))
  def withMessages(messages: SlanEntityValidated[Map[MessageReference, MessageIR]]): Option[IRSimManager] = IRSimManager(this.copy(messageTbl = Option(messages)))
  def withResources(resources: SlanEntityValidated[Map[ResourceReference, ResourceIR]]): IRSimManager = this.copy(resourceTbl=Option(resources))
  def withModels(models: SlanEntityValidated[Map[ModelReference, ModelIR]]): IRSimManager = this.copy(modelTbl=Option(models))

  private def findReference[T <: SlanEntity](refId: EntityId, tbl: Option[SlanEntityValidated[Map[EntityId, T]]]): Option[T] =
    tbl match
      case None => None
      case Some(t) => t match
        case Invalid(e) => None
        case Valid(a) => a.get(refId)


object IRSimManager:
  private val bookkeeper: EntityBookkeeper[IRSimManager] = new EntityBookkeeper[IRSimManager]

  def apply(irm: IRSimManager): Option[IRSimManager] =
    bookkeeper.set(irm.simId, irm)

  def apply(simId:EntityId, slanExpressions: SlanConstructs): Option[IRSimManager] =
    if simId.isBlank then None
    else if bookkeeper.contains(simId.trim()) then bookkeeper.get(simId.trim())
    else bookkeeper.set(simId, IRSimManager(simId,
      Some(AgentIR(slanExpressions)),
      Some(GroupIR(slanExpressions)),
      Some(ChannelIR(slanExpressions)),
      Some(BehaviorIR(slanExpressions)),
      Some(MessageIR(slanExpressions)),
      Some(ResourceIR(slanExpressions)),
      Some(ModelIR(slanExpressions))
    )
    )

  def findReference[T <: SlanEntity : ClassTag](simId:EntityId, refId: EntityId): Option[SlanEntity] =
    import cats.implicits.catsSyntaxEq
    val objectTypeSpec = classTag[T].runtimeClass.getName + "$"
    bookkeeper.get(simId) match
    case None => None
      case Some(eIR) =>
        if objectTypeSpec === AgentIR.getClass.getName then
          eIR.findReference(refId, eIR.agentsTbl.asInstanceOf)
        else if objectTypeSpec === GroupIR.getClass.getName then
          eIR.findReference(refId, eIR.groupsTbl.asInstanceOf)
        else if objectTypeSpec === ChannelIR.getClass.getName then
          eIR.findReference(refId, eIR.channelTbl.asInstanceOf)
        else if objectTypeSpec === BehaviorIR.getClass.getName then
          eIR.findReference(refId, eIR.behaviorTbl.asInstanceOf)
        else if objectTypeSpec === MessageIR.getClass.getName then
          eIR.findReference(refId, eIR.messageTbl.asInstanceOf)
        else if objectTypeSpec === ResourceIR.getClass.getName then
          eIR.findReference(refId, eIR.resourceTbl.asInstanceOf)
        else if objectTypeSpec === ModelIR.getClass.getName then
          eIR.findReference(refId, eIR.modelTbl.asInstanceOf)
        else None

  @main def runn(): Option[SlanEntity] =
    import SlanIR.*
    IRSimManager.findReference[ResourceIR]("s","d")