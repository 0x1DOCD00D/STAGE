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
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class SlanResources2IRTest extends AnyFlatSpec with Matchers:
  behavior of "the resources translator to the resources IR representation"
  import Translator.SlanConstruct.{Agents, Resource, ResourceTag, Resources, SlanError, SlanValue}

  val resourcesEntry: List[SlanConstruct] = List(Agents(List(Resources(List(
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

  val missingResourcesEntry: List[SlanConstruct] = List(Agents(List(
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
  )))

  it should "translate a list of resources to their IR representations" in {
    val res = resourcesEntry.headOption match
      case None => Invalid(0)
      case Some(agents) if agents.isInstanceOf[Agents] =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        SlanIR.ResourceIR(slanconstructs)
      case _ => Invalid(1)
    res shouldBe Valid(Map(
      "compositeResource" ->
        ProducerConsumerComposite("compositeResource",0,List(
          Valid(BasicProducerConsumer("valueHolder4compositeResource",0,List(1))),
          Valid(BasicProducerConsumer("someBasicResource1V",3,List(1000, 100))))),
      "OtherUniformGenerator" -> Generator("OtherUniformGenerator","UniformRealDistribution",
        PdfParameters(None,Some(List(StoredValue(1))),None)),
      "autoInitializedPrimitiveListResource" -> BasicProducerConsumer("autoInitializedPrimitiveListResource",3,List("aUniformGeneratorReference")),
      "SomeValuesGenerator" -> Generator("SomeValuesGenerator","EnumIntDistribution",
        PdfParameters(Some("seedRandom"),Some(
          List(StoredValue((1,0.2)),
            StoredValue((2,"someGeneratedProbabilityValue")),
            StoredValue((3,0.01)))),None)),
      "SomeUniformGenerator" -> Generator("SomeUniformGenerator","UniformRealDistribution",
        PdfParameters(Some(200),Some(List(StoredValue(1), StoredValue("totalMessages"))),
          Some(List(StoredValue((">",0.1)), StoredValue(("<",0.8))))))
    ))
  }

  it should "return an error of the missing entry Resources" in {
    val res = missingResourcesEntry.headOption match
      case None => Invalid(0)
      case Some(agents) =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        SlanIR.ResourceIR(slanconstructs) match
          case Invalid(err) => err.head
          case _ => Invalid(2)
    res shouldBe SlanError("Incorrect spec structure with global entry Resources")
  }

  it should "construct a resource table with one bad composite resource where a value is specified with contained resources" in {
    val errorMixedResourcesValuesEntry = List(Agents(List(Resources(
      List(
      Translator.SlanConstruct.Resource(ResourceTag("SomeValuesGenerator1", Some("EnumIntDistribution")),
        List(ResourcePDFParameters(List(SlanKeyValue(1, 0.2), SlanKeyValue(2, "someGeneratedProbabilityValue"), SlanKeyValue(3, 0.01))),
          ResourcePDFConstraintsAndSeed(List(PdfSeed("seedRandom"))))),
      Translator.SlanConstruct.Resource(ResourceTag("compositeResource1", None),
        List(Translator.SlanConstruct.Resource(ResourceTag("someBasicResource1V", Some("list")),
          List(SlanValue(100), SlanValue(1000))),
          Translator.SlanConstruct.Resource(ResourceTag("valueHolder4compositeResource", None), List(SlanValue(1))),
          //mixing a value in the composite resource container
          SlanValue("aUniformGeneratorReference")
        ))
    )
    ))))
    val res = errorMixedResourcesValuesEntry.headOption match
      case None => Invalid(0)
      case Some(agents) =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        SlanIR.ResourceIR(slanconstructs) match
          case Invalid(err) => Map()
          case Valid(x) => x
    res shouldBe Map(
      "SomeValuesGenerator1" -> Generator("SomeValuesGenerator1","EnumIntDistribution",
        PdfParameters(Some("seedRandom"),Some(List(StoredValue((1,0.2)), StoredValue((2,"someGeneratedProbabilityValue")), StoredValue((3,0.01)))),None)),
      "compositeResource1" -> BadResource("compositeResource1",Invalid(NonEmptyList(
        SlanError("Incorrect parameter is given: attributes should contain either nested resources or values only"), List()))))
  }

  it should "return an error for specifying null as a resource tag" in {
    val errorMixedResourcesValuesEntry = List(Agents(List(Resources(
      List(
        Translator.SlanConstruct.Resource(null,
          List(ResourcePDFParameters(List(SlanKeyValue(1, 0.2), SlanKeyValue(2, "someGeneratedProbabilityValue"), SlanKeyValue(3, 0.01))),
            ResourcePDFConstraintsAndSeed(List(PdfSeed("seedRandom"))))),
        Translator.SlanConstruct.Resource(ResourceTag("compositeResource1", None),
          List(Translator.SlanConstruct.Resource(ResourceTag("someBasicResource1V", Some("list")),
            List(SlanValue(100), SlanValue(1000))),
            Translator.SlanConstruct.Resource(ResourceTag("valueHolder4compositeResource", None), List(SlanValue(1)))
          ))
      )
    ))))
    val res = errorMixedResourcesValuesEntry.headOption match
      case None => Invalid(0)
      case Some(agents) =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        SlanIR.ResourceIR(slanconstructs) match
          case Invalid(err) => err.head
          case _ => Invalid(0)
    res shouldBe SlanError("Incorrect parameter is given: some resources do not contain tag identification")
  }


  it should "return an error for mixing resources with values at the top level" in {
    val errorMixedResourcesValuesEntry = List(Agents(List(Resources(
      List(
        Translator.SlanConstruct.Resource(null,
          List(ResourcePDFParameters(List(SlanKeyValue(1, 0.2), SlanKeyValue(2, "someGeneratedProbabilityValue"), SlanKeyValue(3, 0.01))),
            ResourcePDFConstraintsAndSeed(List(PdfSeed("seedRandom"))))),
        SlanValue("aUniformGeneratorReference"),
        Translator.SlanConstruct.Resource(ResourceTag("compositeResource1", None),
          List(Translator.SlanConstruct.Resource(ResourceTag("someBasicResource1V", Some("list")),
            List(SlanValue(100), SlanValue(1000))),
            Translator.SlanConstruct.Resource(ResourceTag("valueHolder4compositeResource", None), List(SlanValue(1))),
            SlanValue("aUniformGeneratorReference")
          ))
      )
    ))))
    val res = errorMixedResourcesValuesEntry.headOption match
      case None => Invalid(0)
      case Some(agents) =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        SlanIR.ResourceIR(slanconstructs) match
          case Invalid(err) => err.head
          case _ => Invalid(0)
    res shouldBe SlanError("Incorrect parameter is given:  other data structures than Resource are present")
  }

  it should "return an error when duplicate resource names are given at the top level" in {
    val errorMixedResourcesValuesEntry = List(Agents(List(Resources(
      List(
        Translator.SlanConstruct.Resource(ResourceTag("compositeResource1", Some("EnumIntDistribution")),
          List(ResourcePDFParameters(List(SlanKeyValue(1, 0.2), SlanKeyValue(2, "someGeneratedProbabilityValue"), SlanKeyValue(3, 0.01))),
            ResourcePDFConstraintsAndSeed(List(PdfSeed("seedRandom"))))),
        Translator.SlanConstruct.Resource(ResourceTag("compositeResource1", None),
          List(Translator.SlanConstruct.Resource(ResourceTag("someBasicResource1V", Some("list")),
            List(Translator.SlanConstruct.Resource(ResourceTag("valueHolder4compositeResource", None), List(SlanValue(1))))
          ))
      )
    )))))
    val res = errorMixedResourcesValuesEntry.headOption match
      case None => Invalid(0)
      case Some(agents) =>
        val slanconstructs = agents.asInstanceOf[Agents].agents
        SlanIR.ResourceIR(slanconstructs) match
          case Invalid(err) => err.head
          case _ => Invalid(0)
    res shouldBe SlanError("Definition resources compositeResource1 is already specified")
  }
