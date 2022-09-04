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
import SlanIR.EntityId
import Translator.SlanAbstractions.*
import Translator.SlanConstruct
import Translator.SlanConstruct.{SlanError, StateBehavior, StateProbBehavior, StateProbabilitySwitch}
import cats.Eq
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}
import cats.implicits.*
import cats.instances.option.*
import cats.kernel.Eq
import cats.kernel.Eq.catsKernelEqForOption
import cats.syntax.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.syntax.validated.catsSyntaxValidatedId

import scala.collection.immutable.{Map, Set}

/*
* The state is specified by its name unique within a given agent and it is defined by some behavior(s) and
* the transition function that specifies to what other state to switch after the execution of the behavior
* or to stay in the same state if no destination state is specified. A state can be associated with multiple
* behaviors and with many destination states. Each behavior can be triggered in response to specific messages
* and whichever behavior is triggered first the agent will be transitioned to a new state specified for this
* triggered behavior. Each destination state, on the other hand is specified with some likelihood
* as a Double value or as a reference to a resource Generator that produces these likelihood values.
* If no behavior (null) is specified it means that received messages are logged and discarded.
* */

case class StateIR(id: StateReference, behavior: SlanEntityValidated[List[(BehaviorReference, List[(StateReference, Option[PdfGenerator])])]]) extends SlanEntity(id)
case class StateMachine(initState: StateIR, transition: Map[StateIR, List[(StateIR, Option[PdfGenerator])]])

object StateMachine:
  def apply(allStates: SlanConstructs):SlanEntityValidated[StateMachine] =
    import HelperUtils.ExtentionMethods.containsHeadOnly
    val states = allStates.filter(_.isInstanceOf[SlanConstruct.State])
    val otherEntries: List[_] = allStates.filterNot(_.isInstanceOf[SlanConstruct.State])
    if otherEntries.nonEmpty then IncorrectSlanSpecStructure(s"the following entries are specified in resource refs: ${otherEntries.mkString(", ")}").invalidNel
    else
      val agentStates: List[SlanConstruct.State] = states.asInstanceOf[List[SlanConstruct.State]]
      val lstOfIRStates: List[StateIR] = agentStates.foldLeft(List[StateIR]()) {
        (acc, state) => StateIR(state.id, processStateBehavior(state.behavior) ) :: acc
      }
      val orphanedStates = check4OrphanedStates(lstOfIRStates)
      if orphanedStates.nonEmpty then IncorrectSlanSpecStructure(s"${orphanedStates.mkString(", ")} are referenced orphaned states").invalidNel
      val initStates: List[StateIR] = lstOfIRStates.filterNot(_.id === None)
      if initStates.containsHeadOnly then
        StateMachine(initStates.head, createStateTransitionMap(lstOfIRStates)).valid
      else
        IncorrectSlanSpecStructure(s"${initStates.length} initial states are specified, exactly one init state is required").invalidNel
  private def processStateBehavior(behavior: SlanConstructs): SlanEntityValidated[List[(BehaviorReference, List[(StateReference, Option[PdfGenerator])])]] =
    val behs = behavior.filter(b => b.isInstanceOf[StateBehavior] || b.isInstanceOf[StateProbBehavior])
    if behs.length =!= behavior.length then
      val nonbehs = behavior.filterNot(b => b.isInstanceOf[StateBehavior] || b.isInstanceOf[StateProbBehavior])
      IncorrectSlanSpecStructure(s"the state contains non-behavior related data: ${nonbehs.mkString(", ")}").invalidNel
    val psb = errorsProbStateBehaviors(behavior.filter(_.isInstanceOf[StateProbBehavior]).asInstanceOf[List[StateProbBehavior]])
    if psb.nonEmpty then
      val allErrors = psb.foldLeft(List[String]())((acc, error) => error.errorMessage :: acc)
      IncorrectSlanSpecStructure(s"${allErrors.mkString(ErrorMsgSeparator)}").invalidNel
    else behs.foldLeft(List[(BehaviorReference, List[(StateReference, Option[PdfGenerator])])]()) {
          (acc, b) =>
            b match
              case sb if b.isInstanceOf[StateBehavior] =>
                sb.asInstanceOf[StateBehavior].switchTo match {
                  case None => (sb.asInstanceOf[StateBehavior].behavior, List((None, None))) :: acc
                  case Some(s) => (sb.asInstanceOf[StateBehavior].behavior, List((Some(s), None))) :: acc
                }
              case spb if b.isInstanceOf[StateProbBehavior] =>
                (spb.asInstanceOf[StateProbBehavior].behavior, spb.asInstanceOf[StateProbBehavior].switchTo.foldLeft(List[(StateReference, Option[PdfGenerator])]()) {
                  (accProb, elem) =>
                    elem.asInstanceOf[StateProbabilitySwitch].probSource.value match
                      case v: String => (elem.asInstanceOf[StateProbabilitySwitch].stateId, Some(elem.asInstanceOf[StateProbabilitySwitch].probSource.value.asInstanceOf[EntityId])) :: accProb
                      case v: Double => (elem.asInstanceOf[StateProbabilitySwitch].stateId, Some(elem.asInstanceOf[StateProbabilitySwitch].probSource.value.asInstanceOf[Likelihood])) :: accProb
                      case _ => accProb
                }
                ) :: acc
              case ie =>
                SeriousInternalError(s"entry $ie should not occur in the state behaviors at this point")
                acc
    }.validNel

  private def errorsProbStateBehaviors(psb: List[StateProbBehavior]): List[SlanError] =
    psb.foldLeft(List[SlanError]()){
      (acc, b) =>
        val swState = b.switchTo.filter(_.isInstanceOf[StateProbabilitySwitch])
        val errorState = b.switchTo.filterNot(_.isInstanceOf[StateProbabilitySwitch])
        if swState.length =!= b.switchTo.length then
          IncorrectSlanSpecStructure(s"Incorrect constructs are used in place of nondeterministic state transitions: ${errorState.mkString(", ")}") :: acc
        else
          swState.asInstanceOf[List[StateProbabilitySwitch]].foldLeft(List[SlanError]()){
            (_, ps) => ps.probSource.value match
              case v:String => List()
              case v:Double => List()
              case baad => IncorrectParameter(s"$baad should be a string or a double") :: acc
          }
          acc
    }

  private def createStateTransitionMap(states: List[StateIR]): Map[StateIR, List[(StateIR, Option[PdfGenerator])]] =
    states.foldLeft(Map[StateIR, List[(StateIR, Option[PdfGenerator])]]()) {
      (map, state) =>
        locateStateRef(states, state.id) match
          case None => map
          case Some(sir) => sir.behavior match
            case Invalid(e) => map + (sir -> List())
            case Valid(sirBeh) => map + (sir -> sirBeh.asInstanceOf[List[(BehaviorReference, List[(StateReference, PdfGenerator)])]].flatMap(_._2).foldLeft(List[(StateIR, Option[PdfGenerator])]()){
              (lstStates, stuple) => locateStateRef(states, stuple._1) match
                case None => lstStates
                case Some(s) => (s, Option(stuple._2)) :: lstStates
            })
    }

  private def check4OrphanedStates(states: List[StateIR]): Set[EntityId] =
    val allStateIdList: Set[EntityId] = states.flatMap(_.id).toSet[EntityId]
    states.foldLeft(List[EntityId]()){
      (refStateLst, state) => state.behavior match
        case Invalid(e) => refStateLst
        case Valid(v) => //v.asInstanceOf[List[(BehaviorReference, List[(StateReference, PdfGenerator)])]].map(_._2).flatten.map(_._1).flatten.asInstanceOf.toList ::: refStateLst
          val r1 = v.asInstanceOf[List[(BehaviorReference, List[(StateReference, PdfGenerator)])]]
          val r2 = r1.flatMap(_._2)
          val r3 = r2.flatMap(_._1)
          val r4 = r3 ::: refStateLst
          r4
      }.toSet.diff(allStateIdList)
  private def locateStateRef(states: List[StateIR], state: StateReference): Option[StateIR] =
    state match
      case None => None
      case Some(_) => states.find(sir =>sir.id === state)