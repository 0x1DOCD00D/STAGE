/*
 * Copyright (c) 2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
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
import Translator.{ConsistencyModel, SlanConstruct, SlanTranslator, SlantParser}
import cats.data.Validated.Valid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class GroupIRTest extends AnyFlatSpec with Matchers:
  behavior of "the group IR translation manager for SLAN constructs obtained from the original SLAN specification"

  it should "translate a single group entry" in {
    val expected = List(Agents(List(
      Groups(List(
        Group(List(GroupDesignators("Group Name", Some("behaviorName"))),
          List(
            GroupAgent("agentname1", List(SlanValue("randomGenerator4Agent1"))),
            GroupAgent("agentname2", List(SlanValue(100))),
            GroupAgent("bubbaSubgroupName", List(SlanValue(1))),
            ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("sequential", "hdd")), List(SlanValue(2))),
            ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("fifo", "vNic")), List(SlanValue(3))),
            ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("Eventual", "varX")), List(SlanValue("randomGenerator1")))
          ))
      )
      )
    )))
    GroupIR(expected) shouldBe Valid(Map("Group Name" ->
      GroupIR("Group Name",Some("behaviorName"),
        Valid(Map("hdd" -> (ConsistencyModel.SEQUENTIAL,2), "vNic" -> (ConsistencyModel.FIFO,3), "varX" -> (ConsistencyModel.EVENTUAL,"randomGenerator1"))),
        Valid(List(("bubbaSubgroupName",1), ("agentname2",100), ("agentname1","randomGenerator4Agent1"))))))


  }

