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
import HelperUtils.ExtentionMethods.containsHeadOnly
import SlanIR.{EntityId, EntityOrError, SlanEntity}
import Translator.SlanAbstractions.*
import Translator.SlanConstruct
import Translator.SlanConstruct.*
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.*
import cats.instances.*
import cats.syntax.*
import cats.syntax.all.catsSyntaxValidatedId
import cats.syntax.validated.*
import cats.{Eq, Semigroup}
import com.google.common.collect.{HashBasedTable, Table}
case class ModelIR(id: ModelReference,
                   agents: SlanEntityValidated[List[(AgentReference, Cardinality)]] | SlanEntityValidated[List[(AgentIR, Cardinality)]],
                   nodes: List[String],
                   config: List[String],
                   resourceConstructors: SlanEntityValidated[Map[ResourceReference,List[YamlPrimitiveTypes]]] | SlanEntityValidated[Map[ResourceIR,List[YamlPrimitiveTypes]]],
                   graph: SlanEntityValidated[Map[ModelReference,Table[AgentReference, AgentReference, List[ChannelReference]]]] | SlanEntityValidated[Map[ModelReference,Table[AgentIR, AgentIR, List[ChannelIR]]]]
                  ) extends SlanEntity(Option(id)):
  def withAgents(refedAgents: SlanEntityValidated[List[(AgentIR, Cardinality)]]): ModelIR = this.copy(agents = refedAgents)
  def withResources(refedResources: SlanEntityValidated[Map[ResourceIR,List[YamlPrimitiveTypes]]]): ModelIR = this.copy(resourceConstructors = refedResources)
  def withModelGraph(refedGraph: SlanEntityValidated[Map[ModelReference,Table[AgentIR, AgentIR, List[ChannelIR]]]]): ModelIR = this.copy(graph = refedGraph)

object ModelIR extends UniversalChecks[Model, Models]:
  private def checkModelStructure(mdlz: SlanConstructs): SlanEntityValidated[SlanConstructs] =
    val errors:List[String] = mdlz.asInstanceOf[List[Translator.SlanConstruct.Model]].foldLeft(List[String]()) {
      (acc, model) =>
        val agents: List[AgentPopulation] = model.elements.filter(_.isInstanceOf[AgentPopulation]).asInstanceOf[List[AgentPopulation]]
        val rcs: List[ResourceConstructors] = model.elements.filter(_.isInstanceOf[ResourceConstructors]).asInstanceOf[List[ResourceConstructors]]
        val mg: List[ModelGraph] = model.elements.filter(_.isInstanceOf[ModelGraph]).asInstanceOf[List[ModelGraph]]
        checkAgents(agents) ::: acc
        checkResourceConstructors(rcs) ::: acc
        checkModelGraph(mg) ::: acc
    }
    Validated.condNel(errors.isEmpty, mdlz, IncorrectParameter(s"${errors.mkString(ErrorMsgSeparator)}"))

  private def checkAgents(agents: List[AgentPopulation]): List[String] =
    agents.foldLeft(List[String]()){
      (errors, agent) =>
        val slanValueCount = agent.instances.count(_.isInstanceOf[SlanValue])
        if slanValueCount =!= 1 || slanValueCount  =!= agent.instances.length then
          "only one SlanValue entries are allowed in agent population" :: errors
        else agent.instances.filter(_.isInstanceOf[SlanValue]).head.asInstanceOf[SlanValue].value match
          case v: Int => errors
          case v: String => errors
          case selse => s"agent population value is incorrect: $selse" :: errors
    }

  private def checkResourceConstructors(rc: List[ResourceConstructors]): List[String] =
    val err = if !rc.containsHeadOnly then List(s"only one ResourceConstructors entry is allowed in ${rc.mkString(",")}") else List()
    err ::: rc.foldLeft(List[String]()) {
      (errors, r) =>
        if r.ingesters.count(e=>e.isInstanceOf[JsonLoader] || e.isInstanceOf[TableLoader]) =!= r.ingesters.length then
          "only TableLoader and JsonLoader are allowed in resource constructors" :: errors
        else errors
    }

  private def checkModelGraph(mg: List[ModelGraph]): List[String] =
    mg.foldLeft(List[String]()) {
      (errors, g) =>
        if g.vEv.count(_.isInstanceOf[Agent2AgentViaChannel]) =!= g.vEv.length then
          "only Agent2AgentViaChannel entries are allowed in model graph" :: errors
        else
          g.vEv.asInstanceOf[List[Agent2AgentViaChannel]].foldLeft(List[String]()){
            (errAcc, a2a) =>
              if a2a.channel2Agent.count(_.isInstanceOf[Channel2Agent]) =!= a2a.channel2Agent.length then
                s"only channel2Agent entries are allowed in ${a2a.channel2Agent.mkString(", ")}" :: errAcc
              else errAcc
          } ::: errors
    }

  def apply(translated: SlanConstructs): SlanEntityValidated[Map[ModelReference, ModelIR]] =
      checkForEncasingClass(translated, (scLst: SlanConstructs) => {
        scLst.filter(_.isInstanceOf[Models]).asInstanceOf[List[Models]]
      }, (ms: Models) => ms.models.asInstanceOf[List[Model]])
      .andThen(mdlz => checkForListOfEntities(mdlz, (scLst: SlanConstructs) => {
        scLst.filter(_.isInstanceOf[Model]).asInstanceOf[List[Model]]
      }))
      .andThen(mdlz => checkDuplicateNames(mdlz, (m: Model) => m.id ))
      .andThen(m => checkModelStructure(m))
      .andThen(m => SlanModels2IR(m.asInstanceOf[List[Translator.SlanConstruct.Model]]))