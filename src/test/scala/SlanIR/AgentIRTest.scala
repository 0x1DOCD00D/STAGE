/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package SlanIR

import HelperUtils.ErrorWarningMessages.SlanProcessingFailure
import Translator.SlanAbstractions.{BehaviorReference, SlanConstructs, StateReference, YamlTypes}
import Translator.SlanConstruct.*
import Translator.{SlanConstruct, SlanTranslator, SlantParser}
import cats.data.Validated.Valid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class AgentIRTest extends AnyFlatSpec with Matchers:
  behavior of "the agent IR translation manager for SLAN constructs obtained from the original SLAN specification"

  it should "translate a single agent entry" in {
    val expected = List(Translator.SlanConstruct.Agents(List(
      Translator.SlanConstruct.Agent("Agent Name X", List(Translator.SlanConstruct.Resources(List(Translator.SlanConstruct.SlanValue("resource1"), Translator.SlanConstruct.SlanValue("resource2"))),
        Translator.SlanConstruct.State(None, List(Translator.SlanConstruct.StateBehavior(Some("GenerateMessages X, W, and U"), None),
          Translator.SlanConstruct.StateBehavior(Some("GenerateMessages P or Q"), Some("State X")))),
        Translator.SlanConstruct.State(Some("State X"), List(Translator.SlanConstruct.StateBehavior(Some("Spawn Agent Y"), None)))))
    )))
    AgentIR(expected) shouldBe Valid(Map("Agent Name X" -> AgentIR("Agent Name X",Valid(List("resource1", "resource2")),
      Valid(StateMachine(StateIR(Some("State X"),Valid(List((Some("Spawn Agent Y"),List((None,None)))))),
        Map(StateIR(Some("State X"),Valid(List((Some("Spawn Agent Y"),List((None,None)))))) -> List()))))))
  }
