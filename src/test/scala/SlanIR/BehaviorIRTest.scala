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
import Translator.{SlanConstruct, SlanTranslator, SlantParser}
import cats.data.Validated.Valid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class BehaviorIRTest extends AnyFlatSpec with Matchers:
  behavior of "the behavior IR translation manager for SLAN constructs obtained from the original SLAN specification"

  it should "translate a simple behavior" in {
    val expected = List(Models(List(
      Model("Model Name", List(
        AgentPopulation("Professor", List(SlanValue("DrMark"))),
        AgentPopulation("Student", List(SlanValue(1000))),
        ModelGraph("Graph Name X", List(
          Agent2AgentViaChannel("Professor", List(Channel2Agent("Teaches", "Student"), Channel2Agent("Mentors", "Professor"))))),
        ComputingNodes(List()),
        AkkaConfigurationParameters(List()),
        ResourceConstructors(List(
          JsonLoader("resourceJsonData", List(SlanValue(raw"/path/to/gradebook.json"), SlanValue(false))),
          TableLoader("resourceCsvTableName", List(ResourceCsvTable(List(SlanValue(raw"/path/to/gradebook.csv"), SlanValue(false))))),
          TableLoader("resourceCsvOtherTableName", List(ResourceCsvTable(List(SlanValue(raw"/path/to/gradebook.csv"), SlanValue(false))))),
          TableLoader("resourceDbTableName", List(ResourceDatabaseTable(List(SlanValue("User Id=scott;password=tiger;data source=oracle: defined"), SlanValue("tableName"))))))))))))
    ModelIR(expected).toString shouldBe "Valid(Map(Model Name -> ModelIR(Model Name,Valid(List((Student,1000), (Professor,DrMark))),List(),List(),Valid(Map(resourceJsonData -> List(/path/to/gradebook.json, false), resourceCsvTableName -> List(/path/to/gradebook.csv, false), resourceCsvOtherTableName -> List(/path/to/gradebook.csv, false), resourceDbTableName -> List(User Id=scott;password=tiger;data source=oracle: defined, tableName))),Valid(Map(Graph Name X -> {Professor={Student=List(Teaches), Professor=List(Mentors)}})))))"
  }
