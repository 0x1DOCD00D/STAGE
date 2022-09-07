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
import HelperUtils.ExtentionMethods.*
import Translator.ConsistencyModel
import Translator.SlanAbstractions.*
import Translator.SlanConstruct.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}

object SlanGroups2IR extends (List[Translator.SlanConstruct.Group] => SlanEntityValidated[Map[GroupReference, GroupIR]]):
  override def apply(slanGs: List[Translator.SlanConstruct.Group]): SlanEntityValidated[Map[GroupReference, GroupIR]] =
    slanGs.foldLeft(Map[GroupReference, GroupIR]()){
      (map, elem) =>
        val gDecl: GroupDesignators = elem.id.asInstanceOf[List[GroupDesignators]].head
        val agents: List[GroupAgent] = elem.members.filter(_.isInstanceOf[GroupAgent]).asInstanceOf[List[GroupAgent]]
        val resourceRefs: List[ResourceReferenceInGroup] = elem.members.filter(_.isInstanceOf[ResourceReferenceInGroup]).asInstanceOf
        map + (gDecl.id -> GroupIR(gDecl.id, gDecl.behaviorRef, processResources(resourceRefs), processAgents(agents)))
    }.validNel

  private def check4ErrorsInResources(rLst: List[ResourceReferenceInGroup]): Option[SlanError] =
    val allErrors: List[String] = rLst.foldLeft(List[String]()) {
      (errors, elem) =>
        if elem.replicationCoeff.containsHeadOnly then
          if !elem.replicationCoeff.head.isInstanceOf[SlanValue] then
            s"replication coeff: ${elem.replicationCoeff.mkString(",")}" :: errors
          else
            elem.replicationCoeff.head.asInstanceOf[SlanValue].value match
              case _: Int => errors
              case _: String => errors
              case selse => s"replication coeff value: $selse" :: errors
        else
          s"multiple replication coeff entries: ${elem.replicationCoeff.mkString(",")}" :: errors
        if !elem.resource.containsHeadOnly then
          s"multiple resource entries: ${elem.resource.mkString(",")}" :: errors
        else if !elem.resource.head.isInstanceOf[ResourceConsistencyModelInGroup] then
          "no entry for ResourceConsistencyModelInGroup" :: errors
        else
          val resData: ResourceConsistencyModelInGroup = elem.resource.head.asInstanceOf[ResourceConsistencyModelInGroup]
          if ConsistencyModel(resData.cmr).isEmpty then
            s"wrong consistency model: ${resData.cmr}" :: errors
          else errors
    }
    if allErrors.isEmpty then None else Some(IncorrectSlanSpecStructure(allErrors.mkString(ErrorMsgSeparator)))

  private def processResources(rLst: List[ResourceReferenceInGroup]): SlanEntityValidated[Map[ResourceReference, (Translator.ConsistencyModel, Cardinality)]]=
    import cats.syntax.eq.*
    import cats.syntax.option.*
//    ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("Causal", "hdd")), List(SlanValue(2))),
    val errors = check4ErrorsInResources(rLst)
    if errors.nonEmpty then errors.get.invalidNel
    else
      rLst.foldLeft(Map[ResourceReference, (Translator.ConsistencyModel, Cardinality)]()){
        (map, elem) =>
          val rdata: ResourceConsistencyModelInGroup = elem.resource.head.asInstanceOf[ResourceConsistencyModelInGroup]
          val resourceReference: ResourceReference = rdata.id
          val cardinality: Cardinality = elem.replicationCoeff.head.asInstanceOf[SlanValue].value match
            case v: String => v
            case v: Int => v
            case err => throw new Exception(s"unrecoverable error with replication corff $err") //this line should not be executed
          val cm:Translator.ConsistencyModel = ConsistencyModel(rdata.cmr) match
            case Some(m) => m
            case None => throw new Exception(s"unrecoverable error with consistency ${rdata.cmr}") //this line should not be executed
          map + (resourceReference -> (cm, cardinality))
      }.validNel

  private def check4ErrorsInGroupAgents(aLst: List[GroupAgent]): Option[SlanError] =
    val allErrors: List[String] = aLst.foldLeft(List[String]()) {
      (errors, elem) =>
        if elem.cardinality.containsHeadOnly then
          if !elem.cardinality.head.isInstanceOf[SlanValue] then
            s"agent cardinalit: ${elem.cardinality.mkString(",")}" :: errors
          else
            elem.cardinality.head.asInstanceOf[SlanValue].value match
              case _: Int => errors
              case _: String => errors
              case selse => s"agent cardinality value: $selse" :: errors
        else
          s"multiple agent cardinality entries: ${elem.cardinality.mkString(",")}" :: errors
    }
    if allErrors.isEmpty then None else Some(IncorrectSlanSpecStructure(allErrors.mkString(ErrorMsgSeparator)))

  private def processAgents(aLst: List[GroupAgent]): SlanEntityValidated[List[(AgentReference, Cardinality)]] =
//    GroupAgent("agentname1", List(SlanValue("randomGenerator4Agent1")))
    val errors = check4ErrorsInGroupAgents(aLst)
    if errors.nonEmpty then errors.get.invalidNel
    else
      aLst.foldLeft(List[(AgentReference, Cardinality)]()) {
        (lst, elem) =>
          val cardinality: Cardinality = elem.cardinality.head.asInstanceOf[SlanValue].value match
            case v: String => v
            case v: Int => v
            case err => throw new Exception(s"unrecoverable error - agent's cardinality $err") //this line should not be executed
          (elem.id, cardinality) :: lst
      }.validNel
