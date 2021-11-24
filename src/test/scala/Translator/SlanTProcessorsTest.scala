/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Translator

import Translator.SlanAbstractions.{BehaviorReference, SlanConstruct, StateReference}
import Translator.{SlanTranslator, SlantParser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scalaz.Lens

import scala.util.{Failure, Success, Try}


class SlanTProcessorsTest extends AnyFlatSpec with Matchers :

  behavior of "the Slan traslator for SLAN Yaml specifications"

  val agentsFull_Flow = "Agents_Full_v1.yaml"
  val agentsFull_Block = "Agents_Full_v1.yaml"
  val agentsGroups1 = "Agents_Groups_v1.yaml"
  val basicYamlTemplate_v1 = "Template_v1.yaml"
  val basicYamlTemplate_v2 = "Template_v2.yaml"
  val stringScalarValue = "just one string value"
  val intScalarValue = 1234567
  val floatScalarValue = 123450.6789
  val boolScalarValue = false

  it should "translate an agents spec" in {
    val path1 = getClass.getClassLoader.getResource(agentsFull_Flow).getPath
    val path2 = getClass.getClassLoader.getResource(agentsFull_Block).getPath
    val res1 = SlanTranslator(SlantParser.convertJ2S(SlantParser(path1).yamlModel))
    val res2 = SlanTranslator(SlantParser.convertJ2S(SlantParser(path2).yamlModel))
    val expected1 = Agent("Agent Name X",
      List(
        State(Some("Init"), List(StateBehavior(Some("GenerateMessages X, W, and U"), Some("State A")))),
        State(Some("State A"), List(StateBehavior(Some("stateAbehavior"), Some("State B")))),
        State(Some("State B"), List(StateBehavior(Some("Respond to messages A and Y"), None))),
        State(Some("State X"), List(StateBehavior(Some("Last Behavior"), None)))))
    val expected2 = Agent("Agent Name Y", List(State(None, List(StateBehavior(Some("behaviorWithOneState"), None)))))
    val agent2States: Lens[Agent, List[SlanConstruct]] = Lens.lensu[Agent, List[SlanConstruct]]((a, sv) => a.copy(states = sv), _.states)
    val state2Behavior: Lens[State, List[SlanConstruct]] = Lens.lensu[State, List[SlanConstruct]]((s, bv) => s.copy(behavior = bv), _.behavior)
    val behavior2Ref: Lens[StateBehavior, BehaviorReference] = Lens.lensu[StateBehavior, BehaviorReference]((b, br) => b.copy(behavior = br), _.behavior)
    val as = agent2States.get(expected2)
    val sb = state2Behavior.get(as.head.asInstanceOf[State])
    val br = behavior2Ref.get(sb.head.asInstanceOf[StateBehavior])
    br shouldBe Some("behaviorWithOneState")
    res1.head shouldBe expected1
    res1.tail.head shouldBe expected2
    res2.head shouldBe expected1
    res2.tail.head shouldBe expected2
  }

  it should "translate a group spec" in {
    val path = getClass.getClassLoader.getResource(agentsGroups1).getPath
    val res = SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel))


    val cv = ConsistencyModel.values.toList
    println(cv)
    val b = Try(ConsistencyModel.valueOf("SEQUENTIAL1".toUpperCase)) match {
      case Success(res) => res
      case Failure(e) => e.getMessage
    }
    println(b)
  }
