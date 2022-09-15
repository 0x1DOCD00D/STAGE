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
import Translator.SlanAbstractions.*
import Translator.SlanConstruct
import Translator.SlanConstruct.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import cats.syntax.*
import cats.{Eq, Semigroup}
import com.google.common.collect.{HashBasedTable, Table}

object SlanModels2IR extends (List[Translator.SlanConstruct.Model] => SlanEntityValidated[Map[ModelReference, ModelIR]]):

  override def apply(slanMdlz: List[Translator.SlanConstruct.Model]): SlanEntityValidated[Map[ModelReference, ModelIR]] =
    slanMdlz.foldLeft(Map[ModelReference, ModelIR]()){
      (map, mdl) =>
        val agents: List[AgentPopulation] = mdl.elements.filter(_.isInstanceOf[AgentPopulation]).asInstanceOf[List[AgentPopulation]]
        val nodes: List[ComputingNodes] = mdl.elements.filter(_.isInstanceOf[ComputingNodes]).asInstanceOf[List[ComputingNodes]]
        val config: List[AkkaConfigurationParameters] = mdl.elements.filter(_.isInstanceOf[AkkaConfigurationParameters]).asInstanceOf[List[AkkaConfigurationParameters]]
        val rcs: List[ResourceConstructors] = mdl.elements.filter(_.isInstanceOf[ResourceConstructors]).asInstanceOf[List[ResourceConstructors]]
        val mgs: List[ModelGraph] = mdl.elements.filter(_.isInstanceOf[ModelGraph]).asInstanceOf[List[ModelGraph]]
        map + (mdl.id -> ModelIR(mdl.id, constructAgents(agents), constructNodes(nodes), constructConfigs(config), constructResourceLoaders(rcs), constructTheGraph(mgs) ))
    }.validNel

  private def constructAgents(agents: List[AgentPopulation]): SlanEntityValidated[List[(AgentReference, Cardinality)]] =
    agents.foldLeft(List[(AgentReference, Cardinality)]()) {
      (lst, agent) =>
        agent.instances.filter(_.isInstanceOf[SlanValue]).head.asInstanceOf[SlanValue].value match
          case v: Int => (agent.agent, v) :: lst
          case v: String => (agent.agent, v) :: lst
          case selse => lst
    }.validNel

  private def constructNodes(nodes: List[ComputingNodes]): List[String] =
    nodes.foldLeft(List[String]()){
    (lst, node) => lst ::: node.nodes.foldLeft(List[String]()){
      (all, entry) =>
        entry match {
          case entryValue: SlanConstruct.SlanValue => entryValue.value match
            case v: String => v :: all
            case selse => all
          case _ => all
        }
    }
  }

  private def constructConfigs(config: List[AkkaConfigurationParameters]): List[String] = config.foldLeft(List[String]()){
    (allConf, parm) =>
        allConf ::: parm.akka.foldLeft(List[String]()) {
          (all, entry) =>
            entry match {
              case evalue: SlanConstruct.SlanValue => evalue.value match
                case v: String => v :: all
                case _ => all
              case _ => all
            }
        }
  }

  private def constructResourceLoaders(rcs: List[ResourceConstructors]): SlanEntityValidated[Map[ResourceReference,List[YamlPrimitiveTypes]]] =
    if rcs.containsHeadOnly then
      rcs.head.ingesters.foldLeft(Map[ResourceReference,List[YamlPrimitiveTypes]]()){
        (map, rc) =>
          rc match
            case  r: JsonLoader => map +(r.resourceName -> r.json.filter(_.isInstanceOf[SlanValue]).map(_.asInstanceOf[SlanValue].value))
            case r: TableLoader => map +(r.resourceName -> (
              if r.table.count(_.isInstanceOf[ResourceCsvTable]) === 1 then r.table.head.asInstanceOf[ResourceCsvTable].table.filter(_.isInstanceOf[SlanValue]).asInstanceOf[List[SlanValue]].map(_.value)
              else if r.table.count(_.isInstanceOf[ResourceDatabaseTable]) === 1 then r.table.head.asInstanceOf[ResourceDatabaseTable].table.filter(_.isInstanceOf[SlanValue]).asInstanceOf[List[SlanValue]].map(_.value)
              else List()))
            case _ => map
      }.validNel
    else IncorrectSlanSpecStructure(s"multiple ResourceConstructors present").invalidNel
    
  private def constructTheGraph(rcs: List[ModelGraph]): SlanEntityValidated[Map[ModelReference,Table[AgentReference, AgentReference, List[ChannelReference]]]] = {
//    Table<R,C,V>
    def extractTable(vEv: SlanConstructs): Table[AgentReference, AgentReference, List[ChannelReference]] = {
      val table: Table[AgentReference, AgentReference, List[ChannelReference]] = HashBasedTable.create[AgentReference, AgentReference, List[ChannelReference]]()
      vEv.filter(_.isInstanceOf[Agent2AgentViaChannel]).asInstanceOf[List[Agent2AgentViaChannel]].foreach(
        entry => {
          entry.channel2Agent.filter(_.isInstanceOf[Channel2Agent]).asInstanceOf[List[Channel2Agent]].foreach(
            c2a => table.put(entry.agent, c2a.agent,
              c2a.channel :: (if table.contains(entry.agent, c2a.agent) then table.get(entry.agent, c2a.agent) else List())
                )
            )
        })
      table
    }

    rcs.foldLeft(Map[ModelReference,Table[AgentReference, AgentReference, List[ChannelReference]]]()) {
      (map, mg) =>
        map + (mg.id -> extractTable(mg.vEv) )
    }.validNel
  }

