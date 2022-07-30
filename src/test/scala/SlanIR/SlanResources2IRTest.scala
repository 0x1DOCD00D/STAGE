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
import Translator.{SlanTranslator, SlantParser}
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class SlanResources2IRTest extends AnyFlatSpec with Matchers:
  behavior of "the case class resources translator to the resources IR representation"

  it should "translate an example set of resources to its IR representation" in {
    import Translator.SlanConstruct.{Agents, Resource, ResourceTag, Resources, SlanError, SlanValue}
    val resExample = List(Agents(List(Resources(List(
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

    val res = resExample.headOption match
      case None => Invalid(0)
      case Some(agents) if agents.isInstanceOf[Agents] =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        SlanIR.Resource(slanconstructs)
      case _ => Invalid(1)
    res shouldBe Valid(10)
  }
