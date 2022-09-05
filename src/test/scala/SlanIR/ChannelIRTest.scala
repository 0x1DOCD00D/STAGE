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

class ChannelIRTest extends AnyFlatSpec with Matchers:
  behavior of "the channel IR translation manager for SLAN constructs obtained from the original SLAN specification"

  it should "translate a single channel entry" in {
    val expected = List(Agents(List(Channels(List(
      Channel("Sensors2CloudChannel",
        List(
          SlanValue("behaviorAttached2Channel"), SlanValue("otherBehaviorAttached2Channel"), SlanValue("moreBehaviors"), SlanValue("someWierdBehavior"))))))))
    ChannelIR(expected) shouldBe Valid(Map("Sensors2CloudChannel" -> ChannelIR("Sensors2CloudChannel",
      List("someWierdBehavior", "moreBehaviors", "otherBehaviorAttached2Channel", "behaviorAttached2Channel"))))
  }
