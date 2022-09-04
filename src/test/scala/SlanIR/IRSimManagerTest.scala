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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class IRSimManagerTest extends AnyFlatSpec with Matchers:
  behavior of "the simulation translation manager for SLAN constructs obtained from the original SLAN specification"

  val agentsAndResources: List[SlanConstruct] = List(Agents(List(
    Translator.SlanConstruct.Agent("Agent X", List(Translator.SlanConstruct.Resources(List(Translator.SlanConstruct.SlanValue("resource1"), Translator.SlanConstruct.SlanValue("resource2"))),
      Translator.SlanConstruct.State(None, List(Translator.SlanConstruct.StateBehavior(Some("GenerateMessages X, W, and U"), None),
        Translator.SlanConstruct.StateBehavior(Some("GenerateMessages P or Q"), Some("State X")))),
      Translator.SlanConstruct.State(Some("State X"), List(Translator.SlanConstruct.StateBehavior(Some("Spawn Agent Y"), None))))),
    Agent("Agent Y",
      List(State(None, List(StateProbBehavior(Some("Some default behavior and then"),
        List(StateProbabilitySwitch(Some("State A"), SlanValue(0.1)), StateProbabilitySwitch(Some("State B"), SlanValue(0.6)), StateProbabilitySwitch(Some("State X"), SlanValue("somePdfGenerator")))))),
        State(Some("State A"), List(StateProbBehavior(Some("stateAbehavior"),
          List(StateProbabilitySwitch(Some("State A"), SlanValue(0.01)), StateProbabilitySwitch(Some("State B"), SlanValue(0.3)), StateProbabilitySwitch(Some("State X"), SlanValue("somePdfGenerator")))))),
        State(Some("State B"), List(StateBehavior(Some("Respond to messages A and Y"), Some("State X")))),
        State(Some("State X"), List(StateBehavior(Some("Spawn Agent Y"), None))))),
      Resources(List(
    Translator.SlanConstruct.Resource(ResourceTag("SomeUniformGenerator", Some("UniformRealDistribution")),
      List(ResourcePDFParameters(List(SlanValue(1), SlanValue("totalMessages"))),
        ResourcePDFConstraintsAndSeed(List(PdfSeed(200), SlanKeyValue(">", 0.1), SlanKeyValue("<", 0.8))))),
    Translator.SlanConstruct.Resource(ResourceTag("OtherUniformGenerator", Some("UniformRealDistribution")),
      List(ResourcePDFParameters(List(SlanValue(1))))),
    Translator.SlanConstruct.Resource(ResourceTag("autoInitializedPrimitiveListResource", Some("list")),
      List(SlanValue("aUniformGeneratorReference"))),
    Translator.SlanConstruct.Resource(ResourceTag("SomeValuesGenerator", Some("EnumIntDistribution")),
      List(ResourcePDFParameters(List(SlanKeyValue(1, 0.2), SlanKeyValue(2, "someGeneratedProbabilityValue"), SlanKeyValue(3, 0.01))),
        ResourcePDFConstraintsAndSeed(List(PdfSeed("seedRandom"))))),
    Translator.SlanConstruct.Resource(ResourceTag("compositeResource", None),
      List(Translator.SlanConstruct.Resource(ResourceTag("someBasicResource1V", Some("list")),
        List(SlanValue(100), SlanValue(1000))),
        Translator.SlanConstruct.Resource(ResourceTag("valueHolder4compositeResource", None), List(SlanValue(1)))))
  )))))

  it should "translate an empty message to an empty list" in {
    1 shouldBe 1
  }