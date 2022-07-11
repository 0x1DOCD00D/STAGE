/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import HelperUtils.ErrorWarningMessages.SlanProcessingFailure
import Translator.SlanAbstractions.{BehaviorReference, SlanConstructs, StateReference, YamlTypes}
import Translator.SlanConstruct.*
import Translator.{SlanTranslator, SlantParser}
import cats.Eval
import cats.effect.IO
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import scalaz.Lens

import scala.Double.NaN
import scala.io.Source
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.util.{Failure, Success, Try}

class SlanTProcessorsTest extends AsyncFreeSpec with AsyncIOSpec with Matchers:
  val fullSimulation = "SlanFeatureTesting/Full_Simulation_v1.yaml"
  val agentsFull_Block = "SlanFeatureTesting/Agents_Full_v1.yaml"
  val agentsFull_Flow = "SlanFeatureTesting/Agents_Full_v2.yaml"
  val agentsFull_manyBehaviorsInState = "SlanFeatureTesting/Agents_Full_v3.yaml"
  val agentsFull_localResources_Flow = "SlanFeatureTesting/Agents_Full_v4.yaml"
  val agentsFull_probStates = "SlanFeatureTesting/Agents_Full_v5.yaml"
  val agentsFull_smartphone = "SlanFeatureTesting/Agents_Full_v6.yaml"
  val agentsGroups1 = "SlanFeatureTesting/Agents_Groups_v1.yaml"
  val behaviorMessages_flow = "SlanFeatureTesting/Behavior_Messages_KeyFlow.yaml"
  val behaviorMessages_list = "SlanFeatureTesting/Behavior_Messages_KeyList.yaml"
  val behaviorIfThenElse_1 = "SlanFeatureTesting/Behavior_IfThenElse.yaml"
  val behaviorOneMessage = "SlanFeatureTesting/Behavior_One_Message.yaml"
  val behaviorOneMessageIFTHEN = "SlanFeatureTesting/Behavior_One_Message_IFTHEN.yaml"
  val behaviorNullMessage = "SlanFeatureTesting/Behavior_Default_Null.yaml"
  val behaviorMultipleMessages = "SlanFeatureTesting/Behavior_Multiple_Messages.yaml"
  val behaviorPeriodic = "SlanFeatureTesting/Behavior_Period.yaml"
  val behaviorPeriodic_null_duration = "SlanFeatureTesting/Behavior_Period_null_duration.yaml"
  val behaviorPeriodic_null_timeInterval = "SlanFeatureTesting/Behavior_Period_null_timeInterval.yaml"
  val behaviorPeriodic_null_allParams = "SlanFeatureTesting/Behavior_Period_null_allParams.yaml"
  val model_v1 = "SlanFeatureTesting/Model_v1.yaml"
  val model_v2 = "SlanFeatureTesting/Model_v2.yaml"
  val basicYamlTemplate_v1 = "SlanFeatureTesting/Template_v1.yaml"
  val basicYamlTemplate_v2 = "SlanFeatureTesting/Template_v2.yaml"
  val channelYaml_v1 = "SlanFeatureTesting/Channels_v1.yaml"
  val channelYaml_v2 = "SlanFeatureTesting/Channels_v2.yaml"
  val messageYaml = "SlanFeatureTesting/Messages.yaml"
  val messageDerivedYaml = "SlanFeatureTesting/Message_Derived.yaml"
  val resource_generator = "SlanFeatureTesting/ResourceGenerators.yaml"
  val resource_generator_v1 = "SlanFeatureTesting/ResourceGenerators_v1.yaml"
  val resource_generator_v2 = "SlanFeatureTesting/ResourceGenerators_v2.yaml"
  val resource_generator_v3 = "SlanFeatureTesting/ResourceGenerators_v3.yaml"
  val resources_v0 = "SlanFeatureTesting/Resources_v0.yaml"
  val resources_v1 = "SlanFeatureTesting/Resources_v1.yaml"
  val resources_v2 = "SlanFeatureTesting/Resources_v2.yaml"
  val resources_v3 = "SlanFeatureTesting/Resources_v3.yaml"
  val resources_v4 = "SlanFeatureTesting/Resources_v4.yaml"
  val resources_v4_1 = "SlanFeatureTesting/Resources_v4_1.yaml"
  val resources_v4_2 = "SlanFeatureTesting/Resources_v4_2.yaml"
  val resources_v5 = "SlanFeatureTesting/Resources_v5.yaml"
  val resources_v6 = "SlanFeatureTesting/Resources_v6.yaml"
  val resources_v7 = "SlanFeatureTesting/Resources_v7.yaml"
  val resources_v8 = "SlanFeatureTesting/Resources_v8.yaml"
  val resources_v9 = "SlanFeatureTesting/Resources_v9.yaml"
  val resources_v10 = "SlanFeatureTesting/Resources_v10.yaml"
  val resources_v11 = "SlanFeatureTesting/Resources_v11.yaml"
  val resources_v12 = "SlanFeatureTesting/Resources_v12.yaml"

  val socialMediaCompanySimulation = "Simulations/SocialMediaCompany.yaml"
  val primitiveSimulation = "Simulations/PrimitiveMessageExchange.yaml"

  val stringScalarValue = "just one string value"
  val intScalarValue = 1234567
  val floatScalarValue = 123450.6789
  val boolScalarValue = false

  private def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val ioResult = for {
      fib <- io.start
      result <- fib.join
    } yield result

    ioResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Computation canceled."))
    }
  }

  private def translateSlanProgram(path: String): IO[SlanConstructs] =
    val parsedProgram = processResultsFromFiber(SlantParser(path))
    val yml = for {
      parseResult <- parsedProgram
      ymlOut = parseResult match
        case Right(SlanYamlHandle(ymlref)) => ymlref
        case Left(_) | Right(_) => IO.raiseError(new RuntimeException("Parsing failed"))
    } yield ymlOut
    for {
      yaml <- yml
      slan <- processResultsFromFiber(SlanTranslator(yaml))
    } yield slan

  "the Slan traslator for SLAN Yaml specifications" - {
    val expected = Agents(List(Agent("Agent Name X",
      List(
        State(Some("Init"), List(StateBehavior(Some("GenerateMessages X, W, and U"), Some("State A")))),
        State(Some("State A"), List(StateBehavior(Some("stateAbehavior"), Some("State B")))),
        State(Some("State B"), List(StateBehavior(Some("Respond to messages A and Y"), None))),
        State(Some("State X"), List(StateBehavior(Some("Last Behavior"), None))))),
        Agent("Agent Name Y",List(State(None,List(StateBehavior(Some("behaviorWithOneState"),None)))))))

    "translate an agents spec with full flow structure" in {
      val path = getClass.getClassLoader.getResource(agentsFull_Flow).getPath
      translateSlanProgram(path).asserting(_.head shouldBe expected)
    }

    "translate an agents spec with a block structure" in {
      val path = getClass.getClassLoader.getResource(agentsFull_Block).getPath
      translateSlanProgram(path).asserting(_.head shouldBe expected)
    }

    "translate agents specs with lenses" in {
      val agent2States: Lens[Agent, List[SlanConstruct]] = Lens.lensu[Agent, List[SlanConstruct]]((a, sv) => a.copy(states = sv), _.states)
      val state2Behavior: Lens[State, List[SlanConstruct]] = Lens.lensu[State, List[SlanConstruct]]((s, bv) => s.copy(behavior = bv), _.behavior)
      val behavior2Ref: Lens[StateBehavior, BehaviorReference] = Lens.lensu[StateBehavior, BehaviorReference]((b, br) => b.copy(behavior = br), _.behavior)
      val as = agent2States.get(Agent("Agent Name Y", List(State(None, List(StateBehavior(Some("behaviorWithOneState"), None))))))
      val sb = state2Behavior.get(as.head.asInstanceOf[State])
      val br = behavior2Ref.get(sb.head.asInstanceOf[StateBehavior])
      br shouldBe Some("behaviorWithOneState")
    }

    "translate a behavior spec with multiple behaviors for a single state" in {
      val expected = List(Agents(List(
        Agent("Agent Name X",List(
          State(None,List(StateBehavior(Some("GenerateMessages X, W, and U"),Some("State A")),
            StateBehavior(Some("GenerateMessages P or Q"),Some("State X")))),
          State(Some("State A"),List(StateBehavior(Some("stateAbehavior"),Some("State B")))),
          State(Some("State B"),List(StateBehavior(Some("Respond to messages A and Y"),None))),
          State(Some("State X"),List(StateBehavior(Some("Spawn Agent Y"),None)))))
      )))
      val path = getClass.getClassLoader.getResource(agentsFull_manyBehaviorsInState).getPath
      translateSlanProgram(path).asserting(_ shouldBe expected)
    }

    "translate a behavior spec with local resources in an agent" in {
      val expected = List(Agents(List(
        Agent("Agent Name X",List(LocalResources(List(SlanValue("resource1"), SlanValue("resource2"))),
          State(None,List(StateBehavior(Some("GenerateMessages X, W, and U"),None),
            StateBehavior(Some("GenerateMessages P or Q"),Some("State X")))),
          State(Some("State X"),List(StateBehavior(Some("Spawn Agent Y"),None)))))
      )))
      val path = getClass.getClassLoader.getResource(agentsFull_localResources_Flow).getPath
      translateSlanProgram(path).asserting(_ shouldBe expected)
    }

    "translate a behavior spec with probabilistic state switches in an agent" in {
      val expected = List(Agents(List(
        Agent("Agent Name X",
          List(State(None,List(StateProbBehavior(Some("Some default behavior and then"),
            List(StateProbabilitySwitch(Some("State A"),SlanValue(0.1)), StateProbabilitySwitch(Some("State B"),SlanValue(0.6)), StateProbabilitySwitch(Some("State X"),SlanValue("somePdfGenerator")))))),
            State(Some("State A"),List(StateProbBehavior(Some("stateAbehavior"),
              List(StateProbabilitySwitch(Some("State A"),SlanValue(0.01)), StateProbabilitySwitch(Some("State B"),SlanValue(0.3)), StateProbabilitySwitch(Some("State X"),SlanValue("somePdfGenerator")))))),
            State(Some("State B"),List(StateBehavior(Some("Respond to messages A and Y"),Some("State X")))),
            State(Some("State X"),List(StateBehavior(Some("Spawn Agent Y"),None)))))
      )))
      val path = getClass.getClassLoader.getResource(agentsFull_probStates).getPath
      translateSlanProgram(path).asserting(_ shouldBe expected)
    }

    "translate a behavior spec with probabilistic state switches for a smartphone agent" in {
      val expected = List(Agents(List(
        Agent("Smartphone",List(
          State(None,List(
            StateProbBehavior(None,List(
              StateProbabilitySwitch(Some("InstallApp"),SlanValue(0.9)),
              StateProbabilitySwitch(Some("DisableApp"),SlanValue(0.08)),
              StateProbabilitySwitch(Some("Leave"),SlanValue(0.02)))))),
          State(Some("InstallApp"),List(
            StateProbBehavior(Some("InstallApp"),List(
              StateProbabilitySwitch(Some("UseApp"),SlanValue(0.9)),
              StateProbabilitySwitch(Some("DisableApp"),SlanValue(0.08)),
              StateProbabilitySwitch(Some("Leave"),SlanValue(0.02)))))),
          State(Some("Leave"),List(
            StateProbBehavior(Some("GoOfflineForSomeTime"),List(
              StateProbabilitySwitch(Some("Leave"),SlanValue(0.7)),
              StateProbabilitySwitch(None,SlanValue(0.2)),
              StateProbabilitySwitch(Some("OffPermanently"),SlanValue(0.1)))))),
          State(Some("DisableApp"),List(
            StateProbBehavior(Some("DeactivateSomeApp"),List(
              StateProbabilitySwitch(Some("UseApp"),SlanValue(0.9)),
              StateProbabilitySwitch(Some("DisableApp"),SlanValue(0.08)),
              StateProbabilitySwitch(Some("Leave"),SlanValue(0.02)))))),
          State(Some("OffPermanently"),List()),
          State(Some("UseApp"),List(
            StateProbBehavior(Some("UseSomeApp"),List(
              StateProbabilitySwitch(Some("UseApp"),SlanValue(0.7)),
              StateProbabilitySwitch(Some("InstallApp"),SlanValue(0.2)),
              StateProbabilitySwitch(Some("DisableApp"),SlanValue(0.08)),
              StateProbabilitySwitch(Some("Leave"),SlanValue(0.02))))))
        )
        )
      )))
      val path = getClass.getClassLoader.getResource(agentsFull_smartphone).getPath
      translateSlanProgram(path).asserting(_ shouldBe expected)
    }

    "translate a group spec" in {
      val expected = List(Agents(List(
        Groups(List(
        Group(List(GroupDesignators("Group Name",Some("behaviorName"))),
          List(
              GroupAgent("agentname1",List(SlanValue("randomGenerator4Agent1"))),
              GroupAgent("agentname2",List(SlanValue(100))),
              GroupAgent("bubbaSubgroupName",List(SlanValue(1))),
              ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("Causal","hdd")),List(SlanValue(2))),
              ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("eventual","vNic")),List(SlanValue(3))),
              ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("Eventual","varX")),List(SlanValue("randomGenerator1")))
            ))
          )
        )
      )))

      val path = getClass.getClassLoader.getResource(agentsGroups1).getPath
      translateSlanProgram(path).asserting(_ shouldBe expected)
    }
  }

  "translate a behavior spec with multiple messages as the key flow sequence" in {
    val expected = List(Agents(List(Behaviors(List(
      Behavior("Behavior 4 Messages 1",
      List(MessageResponseBehavior(List(SlanValue("MessageX"), SlanValue("MessageY"), SlanValue("MessageZ")),
        List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))))))
    val path = getClass.getClassLoader.getResource(behaviorMessages_flow).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a behavior spec with multiple messages as the key list" in {
    val expected = List(Agents(List(Behaviors(List(
      Behavior("Behavior 4 Messages 2",
      List(MessageResponseBehavior(List(SlanValue("MessageXX"), SlanValue("MessageYY"), SlanValue("MessageZZ")),
        List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))))))
    val path = getClass.getClassLoader.getResource(behaviorMessages_list).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a behavior spec with one message response" in {
    val expected = List(Agents(List(Behaviors(List(
      Behavior("Behavior 4 Messages 3",
                        List(MessageResponseBehavior(List(SlanValue("SomeMessage")),
                        List(FnUpdate(List(SlanValue("resourceName2Update"),
                          FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))))))
    val path = getClass.getClassLoader.getResource(behaviorOneMessage).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a default behavior spec for null message" in {
    val expected = List(Agents(List(Behaviors(List(
      Behavior("Default Behavior 4 All Messages",
      List(MessageResponseBehavior(List(),
        List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))))))
    val path = getClass.getClassLoader.getResource(behaviorNullMessage).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate an if-then-else behavior" in {
    val expected = List(Agents(List(Behaviors(List(
      Behavior("BehaviorIfThenElse",
      List(MessageResponseBehavior(List(SlanValue("MessageX"), SlanValue("MessageY"), SlanValue("MessageZ")),
        List(IfThenElse(List(
          And(List(ROPLessEqual(List(SlanValue("someResource"), SlanValue(3.1415926))),
            Not(List(
              Or(List(ROPEqual(List(SlanValue("someResource"), SlanValue("someOtherResource"))),
                ROPEqual(List(Not(List(SlanValue("someBooleanResource"))), SlanValue(false))))))))),
          Then(List(
            FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))),
            FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(Reference(Some(SlanValue("MessageY")),Some(List(Reference(Some(SlanValue("field")),None)))), SlanValue("generatorRefId")))))
          )))))))))))))
    val path = getClass.getClassLoader.getResource(behaviorIfThenElse_1).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a simple resource spec with a fixed value" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("autoInitializedPrimitiveResource",None),
        List(SlanValue(10))))))))
    val path = getClass.getClassLoader.getResource(resources_v0).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a simple resource spec with a randomly generated value" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("autoInitializedPrimitiveResource",None),
        List(
          Resource(ResourceTag("Uniform",None),
            List(SlanValue(0), SlanValue(1))))))))))
    val path = getClass.getClassLoader.getResource(resources_v1).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec with a composite key and a randomly generated value" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("autoInitializedPrimitiveListResource",Some("list")),
        List(SlanValue("aUniformGeneratorReference")))
      )))))
    val path = getClass.getClassLoader.getResource(resources_v2).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec with a composite key and a list of fixed values" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),
        List(SlanValue(1), SlanValue(10), SlanValue(100)))
      )))))
    val path = getClass.getClassLoader.getResource(resources_v3).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec with a composite key whose attributes include a composite and a simple resources" in {
    val expected = List(Agents(List(Resources(List(Resource(ResourceTag("compositeResource",None),
      List(Resource(ResourceTag("someBasicResource1V",Some("list")),
        List(SlanValue(100), SlanValue(1000))),
        Resource(ResourceTag("valueHolder4compositeResource",None),List(SlanValue(1))))))))))
    val path = getClass.getClassLoader.getResource(resources_v4).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec with a composite resource map whose key is some id and the value is a composite list" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("compositeResourceMap",Some("map")),
        List(Resource(ResourceTag("instanceID",None),
          List(Resource(ResourceTag("someBasicResource1V",Some("list")),List())))))
    )))))
    val path = getClass.getClassLoader.getResource(resources_v4_1).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec with a composite resource map whose key is some id and the value is a composite object" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("compositeResourceMap",Some("map")),
        List(Resource(ResourceTag("instanceID",None),
          List(Resource(ResourceTag("CPU",None),List(SlanValue(100))),
            Resource(ResourceTag("RAM",None),List(SlanValue(0))),
            Resource(ResourceTag("NetworkBandwidth",None),List(SlanValue(10000))))))))))))
    val path = getClass.getClassLoader.getResource(resources_v4_2).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a complex resource spec for generating random distributions" in {
    val expected = List(Agents(List(Resources(List(
        Resource(ResourceTag("SomeUniformGenerator",Some("Uniform")),
          List(ResourcePDFParameters(List(SlanValue(1), SlanValue("totalMessages"))),
            ResourcePDFConstraintsAndSeed(List(PdfSeed(200), SlanKeyValue(">",0.1), SlanKeyValue("<",0.8)))))
    )))))
    val path = getClass.getClassLoader.getResource(resource_generator).getPath
    translateSlanProgram(path).asserting(_.toString() shouldBe expected.toString())
  }

  "translate a complex resource spec for generating a unique random integer starting with one" in {
    val expected = List(Agents(List(Resources(List(Resource(ResourceTag("SomeUniformGenerator",Some("Uniform")),List(ResourcePDFParameters(List(SlanValue(1), SlanValue("None"))))))))))
    val path = getClass.getClassLoader.getResource(resource_generator_v1).getPath
    translateSlanProgram(path).asserting(_.toString() shouldBe expected.toString())
  }

  "translate a complex resource spec for generating a unique random integer without any constraints" in {
    val expected = List(Agents(List(Resources(List(Resource(ResourceTag("SomeUniformGenerator",Some("Uniform")),List()))))))
    val path = getClass.getClassLoader.getResource(resource_generator_v2).getPath
    translateSlanProgram(path).asserting(_.toString() shouldBe expected.toString())
  }

  "translate a complex resource spec for generating a unique random integer with a seed only" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("SomeUniformGenerator",Some("Uniform")),
        List(Resource(SlanNoValue,List(SlanKeyNoValue(200)))
    )))))))
    val path = getClass.getClassLoader.getResource(resource_generator_v3).getPath
    translateSlanProgram(path).asserting(_.toString() shouldBe expected.toString())
  }

  "translate a complex resource spec that describes a table populated with random data" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("dim3",None),
        List(Resource(ResourceTag("column1",None),
          List(Resource(ResourceTag("Enum",None),
            List(Resource(ResourceTag("Item1",None),List(SlanValue("someGenerator"))),
              Resource(ResourceTag("Item2",None),List(SlanValue(0.2))),
              Resource(ResourceTag("Item3",None),List(SlanValue("someOtherGenerator"))))))),
          Resource(ResourceTag("column2",None),List(SlanValue("someGenerator"))),
          Resource(ResourceTag("column3",None),List(SlanValue("someBehavior"))))),
      Resource(ResourceTag("dim4",None),List(Resource(ResourceTag("dim3",Some("list")),
        List(Resource(ResourceTag("column4",None),List(SlanValue("generator200_500"))),
          Resource(ResourceTag("column5",None),List(SlanValue("generator1_5"))))),
        Resource(ResourceTag("someOtherAttribute",None),List()))),
      Resource(ResourceTag("dim5",None),
        List(Resource(ResourceTag("dim4",Some("list")),
          List(Resource(ResourceTag("column6",None),List(SlanValue("generator0_1"))))))))))))

    val path = getClass.getClassLoader.getResource(resources_v5).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec with a list of the instances of a composite resources that contains a queue initialized with some values" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("HDD",Some("list")),List(Resource(ResourceTag("Size",None),List(SlanValue("pdfgenerator"))),
        Resource(ResourceTag("Utilization",None),
          List(Resource(ResourceTag(">=",None),List(SlanValue(0))),
          Resource(ResourceTag("<",None),List(SlanValue("Size"))))),
        Resource(ResourceTag("ItemCount",None),List(SlanValue(0),
          Resource(ResourceTag(">=",None),List(SlanValue(0))))),
        Resource(ResourceTag("DataStore",Some("queue")),List(SlanValue(100), SlanValue(1000), SlanValue(100000))))))))))
    val path = getClass.getClassLoader.getResource(resources_v6).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec that is a direct representation of a table with columns" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("someTableResource",None),
        List(Resource(ResourceTag("columns",Some("list")),
          List(Resource(ResourceTag("column3",None),List()),
            Resource(ResourceTag("column1",None),List()),
            Resource(ResourceTag("column6",None),List()),
            Resource(ResourceTag("column2",None),List()),
            Resource(ResourceTag("column5",None),List()),
            Resource(ResourceTag("column4",None),List())
          )
        )
        )))))))
    val path = getClass.getClassLoader.getResource(resources_v7).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec for designating an external service: RESTful or JAR" in {
    val expected = List(Agents(List(Resources(List(
      Resource(ResourceTag("UniqueServiceId",Some("jar")),
        List(Resource(ResourceTag("http://url/to/Jar/name.jar",None),
          List(Resource(ResourceTag("methodName",None),List(SlanValue("parm1"), SlanValue("parm2"))),
            Resource(ResourceTag("methodName",None),List(SlanValue("parm1"))),
            Resource(ResourceTag("methodName",None),List()),
            Resource(ResourceTag("otherMethodName",None),List(SlanValue("parm1"), SlanValue("parm2"), SlanValue("parm3"))))))))))))
    val path = getClass.getClassLoader.getResource(resources_v8).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a channel spec with multiple behaviors for different messages" in {
    val expected = List(Agents(List(Channels(List(
      Channel("Sensors2CloudChannel",
      List(
        SlanValue("behaviorAttached2Channel"), SlanValue("otherBehaviorAttached2Channel"), SlanValue("moreBehaviors"), SlanValue("someWierdBehavior"))))))))
    val path = getClass.getClassLoader.getResource(channelYaml_v1).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a message spec with multiple fields" in {
    val expected = List(Messages(List(
      Message(List(MessageDeclaration("Message Name",None)),
        List(Resources(List(Resource(ResourceTag("Recursive Field",None),List(Resource(ResourceTag("Message Name",None),List(SlanValue(3))))),
          Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value",None),List(SlanValue(100))),
          Resource(ResourceTag("Another Recursive Field",None),
            List(Resource(ResourceTag("Message Name",None),List(SlanValue(10))))),
          Resource(ResourceTag("Field Name",None),List(SlanValue("generatorUniformPdf"))))))))))
    val path = getClass.getClassLoader.getResource(messageYaml).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a derived message spec" in {
    val expected = List(Messages(List(
      Message(List(MessageDeclaration("Message Name",None)), List(Resources(
        List(Resource(ResourceTag("Field Name",None),
        List(Resource(ResourceTag("Uniform",None),List(SlanValue(1), SlanValue(200))))),
        Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),List(SlanValue(1), SlanValue(10), SlanValue(100))),
        Resource(ResourceTag("Some Fixed Value",None),List(SlanValue(100))))))),
      Message(List(MessageDeclaration("Derived Message",Some("Message Name"))),
        List(Resources(List(Resource(ResourceTag("FieldX",None),
          List(Resource(ResourceTag("Discrete",None),List(SlanKeyValue(1,0.3), SlanKeyValue(2,0.5), SlanKeyValue(3,0.6)))))))))
    )))
    val path = getClass.getClassLoader.getResource(messageDerivedYaml).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a spec for a periodic behavior" in {
    val expected = List(Agents(List(Behaviors(
      List(
      PeriodicBehavior("...Some Periodic Behavior",
        List(PeriodicBehaviorFiringDuration(SlanValue(10),Some(SlanValue("howManyTimes"))),
          FnSend(List(SlanValue("generatorResourceOfMessages"))),
          FnCreate(List(SlanValue("generatorResourceOfAgents")))))
    )))))
    val path = getClass.getClassLoader.getResource(behaviorPeriodic).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a spec for a periodic behavior without params" in {
    val expected = List(Agents(List(Behaviors(List(
      PeriodicBehavior("...Some Periodic Behavior",
        List(PeriodicBehaviorFiringDuration(SlanValue(0),None),
          FnSend(List(SlanValue("generatorResourceOfMessages"))),
          FnCreate(List(SlanValue("generatorResourceOfAgents")))))
    )))))
    val path = getClass.getClassLoader.getResource(behaviorPeriodic_null_allParams).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a spec for a periodic behavior without a time interval" in {
    val expected = List(Agents(List(Behaviors(List(
      PeriodicBehavior("...Some Periodic Behavior",
        List(PeriodicBehaviorFiringDuration(SlanValue(0),Some(SlanValue("howManyTimes"))),
          FnSend(List(SlanValue("generatorResourceOfMessages"))),
          FnCreate(List(SlanValue("generatorResourceOfAgents")))))
    )))))
    val path = getClass.getClassLoader.getResource(behaviorPeriodic_null_timeInterval).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a spec for a behavior with multiple messages" in {
    val expected = List(Agents(List(Behaviors(
      List(
      Behavior("Behavior 4 Multiple Messages",
        List(MessageResponseBehavior(List(SlanValue("MessageX"), SlanValue("MessageY"), SlanValue("MessageZ")),
          List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))),
          MessageResponseBehavior(List(SlanValue("MessageW")),
            List(FnUpdate(List(SlanValue("resourceName2Update"), Reference(Some(SlanValue("MessageW")),Some(List(Reference(Some(SlanValue("fieldW")),None))))
            )))),
          MessageResponseBehavior(List(SlanValue("MessageC"), SlanValue("MessageB")),
            List(FnUpdate(List(SlanValue("resourceName2Update"),
              Reference(Some(SlanValue("MessageA")),Some(List(Reference(Some(SlanValue("fieldA")),None)))))))
          )
        )
      )
    )))))

    val path = getClass.getClassLoader.getResource(behaviorMultipleMessages).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }


  "translate a spec for a periodic behavior without duration" in {
    val expected = List(Agents(List(Behaviors(
      List(
      PeriodicBehavior("...Some Periodic Behavior",
        List(PeriodicBehaviorFiringDuration(SlanValue(10),None),
          FnSend(List(SlanValue("generatorResourceOfMessages"))),
          FnCreate(List(SlanValue("generatorResourceOfAgents")))))
    )))))
    val path = getClass.getClassLoader.getResource(behaviorPeriodic_null_duration).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec for a map of some agent to its coordinate resource" in {
    val expected = List(Agents(List(Resources(
      List(
        Resource(ResourceTag("Coordinates",None),
          List(Resource(ResourceTag("x",None),List()),
            Resource(ResourceTag("y",None),List()))),
        Resource(ResourceTag("mapOfAgentCoordinates",Some("map")),
          List(Resource(ResourceTag("Pedestrian",None),List(SlanValue("Coordinates"))))))))))
    val path = getClass.getClassLoader.getResource(resources_v9).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec for a generator of messages or agents with given probabilities" in {
    val expected = List(Agents(List(
      Resources(List(
      Resource(ResourceTag("generatorOfMessagesXYZ",None),
        List(ResourcePeriodicGenerator(List(ResourceProbability("MessageX",SlanValue(true)),
          ResourceProbability("MessageY",SlanValue(0.6)),
          ResourceProbability("MessageZ",SlanValue("somePdfGenerator")), SlanValue("pdfgenerator")))))
    )))))
    val path = getClass.getClassLoader.getResource(resources_v11).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a resource spec for a map that constains an empty list as a value" in {
    val expected = List(Agents(List(
      Resources(List(
      Resource(ResourceTag("DopplegangerGAPs",Some("map")),
        List(Resource(ResourceTag("targetGapID",None),
          List(Resource(ResourceTag("phishingGaps",Some("list")),List())))))
    )))))
    val path = getClass.getClassLoader.getResource(resources_v12).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a model for pedestrians, vehicles and buildings" in {
    val expected = List(Models(List(
      Model("Model Name",List(
        AgentPopulation("Pedestrian",List(SlanValue("quantity 1"))),
        AgentPopulation("Vehicle",List(SlanValue(1000))),
        AgentPopulation("Building",List(Pdf("Normal",List(SlanValue(500), SlanValue(10))))),
        ModelGraph("Graph Name X",List(
          Agent2AgentViaChannel("Pedestrian",List(Channel2Agent("TalksT0","Pedestrian"), Channel2Agent("Enters","Building"))),
          Agent2AgentViaChannel("Vehicle",List(Channel2Agent("Yields","Pedestrian"), Channel2Agent("ParksBy","Building"))),
          Agent2AgentViaChannel("Building",List(Channel2Agent("ConnectedByUndergroundPassagewayTo","Building"), Channel2Agent("Alerts","Pedestrian"))))),
        ComputingNodes(List(SlanValue("cancun"), SlanValue("austin"))), AkkaConfigurationParameters(List()))))))
    val path = getClass.getClassLoader.getResource(model_v1).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a model where resources are loaded with data from external tables" in {
    val expected = List(Models(List(
      Model("Model Name",List(
        AgentPopulation("Professor",List(SlanValue("DrMark"))),
        AgentPopulation("Student",List(SlanValue(1000))),
        ModelGraph("Graph Name X",List(
          Agent2AgentViaChannel("Professor",List(Channel2Agent("Teaches","Student"))))),
        ComputingNodes(List()),
        AkkaConfigurationParameters(List()),
        ResourceConstructors(List(
          TableLoader("resourceCsvTableName",List(ResourceCsvTable(List(SlanValue(raw"/path/to/gradebook.csv"), SlanValue(false))))),
          TableLoader("resourceCsvOtherTableName",List(ResourceCsvTable(List(SlanValue(raw"/path/to/gradebook.csv"), SlanValue(false))))),
          TableLoader("resourceDbTableName",List(ResourceDatabaseTable(List(SlanValue("User Id=scott;password=tiger;data source=oracle: defined"), SlanValue("tableName"))))))))))))
    val path = getClass.getClassLoader.getResource(model_v2).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a full semantically meaningless simulation" in {
    val expected = List(Agents(
      List(Resources(
        List(Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),List(SlanValue(1), SlanValue(10), SlanValue(100))))
      ),
        Agent("Agent Name X",List(State(None,List(StateBehavior(Some("GenerateMessages X, W, and U"),Some("State A")))),
          State(Some("State A"),List(StateBehavior(Some("stateAbehavior"),None))))),
        Groups(List(Group(List(GroupDesignators("Group Name",Some("behavior"))),List(GroupAgent("Agent Name X",List()),
          ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("Causal","someBasicResourceListOfValues")),List(SlanValue(2))))))),
        Behaviors(List(Behavior("Behavior 4 Messages 1",List(MessageResponseBehavior(List(SlanValue("MessageX"), SlanValue("MessageY"), SlanValue("MessageZ")),
          List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))),
        Agent("Agent Name Y",List(State(None,List()))), Channels(List(Channel("TalksT0",List(SlanValue("behaviorAttached2Channel"), SlanValue("moreBehaviors"))))))),
      Messages(List(Message(List(MessageDeclaration("Message Name",None)), List(Resources(
        List(Resource(ResourceTag("Some Fixed Value",None),List(SlanValue(100))))))))),
      Models(List(Model("Model Name",List(AgentPopulation("Agent Name X",List(SlanValue("quantity 1"))), AgentPopulation("Agent Name Y",List()),
        ModelGraph("Graph Name X",List(Agent2AgentViaChannel("Agent Name X",List(Channel2Agent("TalksT0","Agent Name Y"))))))))))

    val path = getClass.getClassLoader.getResource(fullSimulation).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a behavior with a graph resource and a single message to respond" in {
    val expected = List(
      Agents(List(Behaviors(List(
      Behavior("HandleFriendRequests",List(MessageResponseBehavior(List(SlanValue("BeMyFriend")),
        List(IfThenElse(List(ROPEqual(List(FnSend(List(SlanValue("generatorOfFriendAcceptances"), CorrelationToken(SlanValue("friendRequestCorrID")))), SlanValue("AcceptedFriend"))),
          Then(List(FnUpdate(List(Reference(Some(SlanValue("TheFriendsGraph")),
            Some(List(Reference(Some(SlanValue("AddEdge")),Some(List(Reference(Some(SlanValue("this")),Some(List(Reference(Some(SlanValue("sender")),
              Some(List(Reference(Some(SlanValue(true)),None)))))))))))))))))))))))
    )))))
    val path = getClass.getClassLoader.getResource(behaviorOneMessageIFTHEN).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }


  "translate a simulation of the social media company" in {
    val expected = List(Agents(List(Resources(List(
    /*
        generatorOfInsultResponseMessages:
          ? [Opinion: 0.2, Fact: 0.1, Insult: 0.7]
          :
            departureGenerator

    */

      Resource(ResourceTag("generatorOfInsultResponseMessages",None),
        List(ResourcePeriodicGenerator(List(ResourceProbability("Opinion",SlanValue(0.2)),
          ResourceProbability("Fact",SlanValue(0.1)),
          ResourceProbability("Insult",SlanValue(0.7)), SlanValue("departureGenerator"))))),

      /*
            generatorOfMessages:
            ? [Opinion: 0.6, Fact: 0.3, Insult: 0.2]
            : arrivalGenerator
      */
      Resource(ResourceTag("generatorOfMessages",None),
        List(ResourcePeriodicGenerator(List(ResourceProbability("Opinion",SlanValue(0.6)),
          ResourceProbability("Fact",SlanValue(0.3)), ResourceProbability("Insult",SlanValue(0.2)), SlanValue("arrivalGenerator"))))),

      /*
          acceptRejectGenerator:
            Uniform: [0, 1]
      * */
      Resource(ResourceTag("acceptRejectGenerator",None),
        List(Resource(ResourceTag("Uniform",None),List(SlanValue(0), SlanValue(1))))),
      /*
        ? graph: TheFriendsGraph
        : null
      * */
      Resource(ResourceTag("TheFriendsGraph",Some("graph")),List()),
      /*
        generatorOfFriendAcceptances:
        ? [AcceptedFriend: 0.6, RejectedFriend: 0.4]
        : 1
      * */
      Resource(ResourceTag("generatorOfFriendAcceptances",None),
        List(ResourcePeriodicGenerator(
          List(ResourceProbability("AcceptedFriend",SlanValue(0.6)),
            ResourceProbability("RejectedFriend",SlanValue(0.4)), SlanValue(1))))),

      /*
        departureGenerator:
          Uniform: [1, 3]
      */
      Resource(ResourceTag("departureGenerator",None),List(
        Resource(ResourceTag("Uniform",None),List(SlanValue(1), SlanValue(3))))),

      /*
        generatorOfAgents:
          Person: arrivalGenerator
      * */
      Resource(ResourceTag("generatorOfAgents",None),List(Resource(ResourceTag("Person",None),List(SlanValue("arrivalGenerator"))))),

      /*
        generatorOfEncouragementResponseMessages:
        ? [Opinion: 0.5, Fact: 0.4, Insult: 0.7]
        :
          arrivalGenerator
      * */
      Resource(ResourceTag("generatorOfEncouragementResponseMessages",None),
        List(ResourcePeriodicGenerator(List(
          ResourceProbability("Opinion",SlanValue(0.5)), ResourceProbability("Fact",SlanValue(0.4)),
          ResourceProbability("Insult",SlanValue(0.7)), SlanValue("arrivalGenerator"))))),

      /*
        generatorOfFriends:
          BeMyFriend: arrivalGenerator
      * */
      Resource(ResourceTag("generatorOfFriends",None),List(Resource(ResourceTag("BeMyFriend",None),List(SlanValue("arrivalGenerator"))))),

      /*
      arrivalGenerator:
        Poisson: [3, 14]
      * */
      Resource(ResourceTag("arrivalGenerator",None),
        List(Resource(ResourceTag("Poisson",None),List(SlanValue(3), SlanValue(14))))))),

      /*
        TheCompany:
          null:
            ...NewPersonsJoinSocialMediaPlatform:
            ...PersonLeavesSocialMediaPlatform:
      * */
      Agent("TheCompany",List(State(None,
        List(StateBehavior(Some("...NewPersonsJoinSocialMediaPlatform"),None),
          StateBehavior(Some("...PersonLeavesSocialMediaPlatform"),None))))),

      /*
          Respond2FactsAndOpinions:
            Insult:
              - Fn_Send: [generatorOfEncouragementResponseMessages, "~>": messageResponseCorrID]
              - Fn_Send: [generatorOfInsultResponseMessages]
      */
      Behaviors(List(Behavior("Respond2FactsAndOpinions",
        List(MessageResponseBehavior(List(SlanValue("Insult")),
          List(FnSend(List(SlanValue("generatorOfEncouragementResponseMessages"), CorrelationToken(SlanValue("messageResponseCorrID")))),
            FnSend(List(SlanValue("generatorOfInsultResponseMessages"))))))),

      /*
          HandleInsults:
            Insult:
              - Fn_Send: [generatorOfInsultResponseMessages, "~>": messageResponseCorrID]
              - Fn_Send: [generatorOfInsultResponseMessages]
              - Fn_Update: [goodWill, Fn_Substract: [goodWill, 0.01]]
      * */
      Behavior("HandleInsults",
        List(MessageResponseBehavior(List(SlanValue("Insult")),
          List(FnSend(List(SlanValue("generatorOfInsultResponseMessages"), CorrelationToken(SlanValue("messageResponseCorrID")))),
            FnSend(List(SlanValue("generatorOfInsultResponseMessages"))),
            FnUpdate(List(SlanValue("goodWill"), FnSubstract(List(SlanValue("goodWill"), SlanValue(0.01))))))))),

      /*
          ...PersonLeavesSocialMediaPlatform:
            ? 1000: null
            :
              IF:
                ">":
                  - acceptRejectGenerator
                  - 0.9
                THEN:
                  Fn_ForEach:
                    Fn_Select: # this function provides some randomly selected nodes
                      TheFriendsGraph: departureGenerator
                      # for each of the selected nodes perform some operation/function
                    Fn_Send: [this, Unfriend]
                    Fn_Remove: [this, TheFriendsGraph]
                    Fn_Destroy: this
      * */
      PeriodicBehavior("...PersonLeavesSocialMediaPlatform",
        List(PeriodicBehaviorFiringDuration(SlanValue(1000),None),
          IfThenElse(
            List(ROPGreater(
              List(SlanValue("acceptRejectGenerator"), SlanValue(0.9))),
              Then(
                List(FnForEach(List(FnSelect(List(Reference(Some(SlanValue("TheFriendsGraph")), Some(List(Reference(Some(SlanValue("departureGenerator")),None)))))),
                  FnSend(List(SlanValue("this"), SlanValue("Unfriend"))),
                  FnRemove(List(SlanValue("this"), SlanValue("TheFriendsGraph"))),
                  FnDestroy(List(SlanValue("this"))))))))))),

      /*
          HandleFriendRequests: #used
            BeMyFriend:
              IF:
                "==":
                  - Fn_Send: [generatorOfFriendAcceptances, "~>": friendRequestCorrID]
                  - AcceptedFriend
                THEN:
                  Fn_Update:
                    TheFriendsGraph: [this, sender, true]
      * */
      Behavior("HandleFriendRequests",List(MessageResponseBehavior(List(SlanValue("BeMyFriend")),
        List(IfThenElse(
          List(ROPEqual(
            List(FnSend(List(SlanValue("generatorOfFriendAcceptances"), CorrelationToken(SlanValue("friendRequestCorrID")))), SlanValue("AcceptedFriend"))),
            Then(List(FnUpdate(
              List(Reference(Some(SlanValue("TheFriendsGraph")),Some(List(Reference(Some(SlanValue("this")),None), Reference(Some(SlanValue("sender")),None), Reference(Some(SlanValue(true)),None)))))))))))))),

      /*
      Transmissions: # send message to some friend
        null: # for all messages
          Fn_Send:
            Fn_Select:
              TheFriendsGraph:
                Sender:
                  RelatedTo:
              departureGenerator:
      */
      Behavior("Transmissions",List(MessageResponseBehavior(List(),
        List(FnSend(
          List(FnSelect(List(Reference(Some(SlanValue("TheFriendsGraph")),
            Some(List(Reference(Some(SlanValue("Sender")),
              Some(List(Reference(Some(SlanValue("RelatedTo")),Some(List(Reference(None,None)))))))))),
            Reference(Some(SlanValue("departureGenerator")),Some(List(Reference(None,None)))))))))))),

      /*
      ...Post2Friends:
        ? 100: null
        :
          IF:
            ">":
              - TheFriendsGraph: EdgeCount
              - 0
            THEN:
              Fn_Send: [generatorOfMessages, "~>": friendRequestCorrID]
      */
      PeriodicBehavior("...Post2Friends",List(PeriodicBehaviorFiringDuration(SlanValue(100),None),
        IfThenElse(
          List(ROPGreater(
            List(Reference(Some(SlanValue("TheFriendsGraph")),Some(List(Reference(Some(SlanValue("EdgeCount")),None)))), SlanValue(0))),
            Then(List(FnSend(List(SlanValue("generatorOfMessages"), CorrelationToken(SlanValue("friendRequestCorrID")))))))))),

      /*
        HandleSupport:
          StrongSupport:
            - Fn_Send: [generatorOfEncouragementResponseMessages, "~>": messageResponseCorrID]
            - Fn_Send: [generatorOfEncouragementResponseMessages]
            - Fn_Update: [goodWill, Fn_Add: [goodWill, 0.005]]
      */
      Behavior("HandleSupport",
        List(MessageResponseBehavior(List(SlanValue("StrongSupport")),
          List(FnSend(List(SlanValue("generatorOfEncouragementResponseMessages"), CorrelationToken(SlanValue("messageResponseCorrID")))),
            FnSend(List(SlanValue("generatorOfEncouragementResponseMessages"))),
            FnUpdate(List(SlanValue("goodWill"),
              FnAdd(List(SlanValue("goodWill"), SlanValue(0.005))))))))),


      /*
          HandleFriendRequestResponses:
            ? [AcceptedFriend, RejectedFriend]:
              IF:
                - AcceptedFriend
                - THEN:
                    Fn_Update:
                      TheFriendsGraph: [this, sender, true]
      */
      Behavior("HandleFriendRequestResponses",
        List(MessageResponseBehavior(List(SlanValue("AcceptedFriend"), SlanValue("RejectedFriend")),
          List(IfThenElse(List(SlanValue("AcceptedFriend"),
            Then(
              List(FnUpdate(
                List(Reference(Some(SlanValue("TheFriendsGraph")),
                  Some(List(Reference(Some(SlanValue("this")),None), Reference(Some(SlanValue("sender")),None), Reference(Some(SlanValue(true)),None)))))))))))))),

      /*
          ...NewPersonsJoinSocialMediaPlatform:
        ? 1000: null
        :
        Fn_Add:
          Fn_Create: generatorOfAgents
          TheFriendsGraph:
      * */
      PeriodicBehavior("...NewPersonsJoinSocialMediaPlatform",
        List(PeriodicBehaviorFiringDuration(SlanValue(1000),None),
          FnAdd(
            List(FnCreate(List(SlanValue("generatorOfAgents"))),
              Reference(Some(SlanValue("TheFriendsGraph")),Some(List(Reference(None,None)))))))))),

      /*
        Person:
          null:
            ...Post2Friends: respond2Messages

          # if no behavior is provided in a state to handle some messages then these messages are discarded
          respond2Messages:
            HandleFriendRequests:
            HandleInsults: offended
            HandleSupport: happy
            Respond2FactsAndOpinions:

          offended:
            HandleFriendRequests: respond2Messages
            HandleSupport: respond2Messages

          happy:
            HandleFriendRequests:
            HandleInsults: respond2Messages
            Respond2FactsAndOpinions: respond2Messages

          Resources:
            goodWill: 100
      */
      Agent("Person",
        List(State(None,List(StateBehavior(Some("...Post2Friends"),Some("respond2Messages")))),
          LocalResources(List(SlanKeyValue("goodWill",100))),
          State(Some("offended"),List(StateBehavior(Some("HandleFriendRequests"),Some("respond2Messages")),
            StateBehavior(Some("HandleSupport"),Some("respond2Messages")))),
          State(Some("happy"),List(StateBehavior(Some("HandleFriendRequests"),None),
            StateBehavior(Some("HandleInsults"),Some("respond2Messages")),
            StateBehavior(Some("Respond2FactsAndOpinions"),Some("respond2Messages")))),
          State(Some("respond2Messages"),List(StateBehavior(Some("HandleFriendRequests"),None),
            StateBehavior(Some("HandleInsults"),Some("offended")),
            StateBehavior(Some("HandleSupport"),Some("happy")),
            StateBehavior(Some("Respond2FactsAndOpinions"),None))))),

      /*
        Channels:
          TalksT0:
            null: Transmissions
      */
      Channels(List(Channel("TalksT0",List(SlanValue("Transmissions"))))))),

      /*
        Messages:
          BeMyFriend:
          Unfriend:
          AcceptedFriend:
          RejectedFriend:
          Opinion:
          Fact:
          Insult:
          StrongSupport:
      */
      Messages(List(
      Message(List(MessageDeclaration("Opinion",None)),List(Resources(List()))),
      Message(List(MessageDeclaration("StrongSupport",None)),List(Resources(List()))),
      Message(List(MessageDeclaration("Unfriend",None)),List(Resources(List()))),
      Message(List(MessageDeclaration("AcceptedFriend",None)),List(Resources(List()))),
      Message(List(MessageDeclaration("RejectedFriend",None)),List(Resources(List()))),
      Message(List(MessageDeclaration("Insult",None)),List(Resources(List()))),
      Message(List(MessageDeclaration("BeMyFriend",None)),List(Resources(List()))),
      Message(List(MessageDeclaration("Fact",None)),List(Resources(List()))))),
      /*
        Models:
          SocialMediaSimulation:
            Agents:
              TheCompany: 1

            SimGraph:
              Person:
                TalksT0: Person

            Deployment:
              Nodes: # if no names or ip addresses provided then switch to autodiscovery
                - cancun
                - austin
              Akka Configuration: # this section will contain akka configuration data provided in app.conf files.
      */
      Models(List(
      Model("SocialMediaSimulation",
        List(AgentPopulation("TheCompany",List(SlanValue(1))),
          ModelGraph("SimGraph",List(Agent2AgentViaChannel("Person",List(Channel2Agent("TalksT0","Person"))))),
          ComputingNodes(List(SlanValue("cancun"), SlanValue("austin"))), AkkaConfigurationParameters(List()))
      ))))
    val path = getClass.getClassLoader.getResource(socialMediaCompanySimulation).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }

  "translate a primitive simulation for two agents exchanging messages" in {
    val expected = List(Agents(List(
    /*
      Person:
        null:
          ...SendMessage: respond2Messages

        respond2Messages:
          Respond2Message:
    * */
    Agent("Person",
    List(State(None,List(StateBehavior(Some("...SendMessage"),Some("respond2Messages")))),
    State(Some("respond2Messages"),List(StateBehavior(Some("Respond2Message"),None))))),

    /*
        Respond2Message: #used
          MyOpinionAboutYou:
            IF:
              ">=":
                - messageCounter
                - maxMessagesThreshold
              THEN:
                Fn_Send: IdoNotCareResponse
                Fn_Destroy: this
              ELSE:
                Fn_Send: IdoNotCareResponse
    * */
      Behaviors(List(Behavior("Respond2Message",
    List(MessageResponseBehavior(List(SlanValue("MyOpinionAboutYou")),
    List(IfThenElse(
      List(ROPGreaterEqual(
      List(SlanValue("messageCounter"), SlanValue("maxMessagesThreshold"))),
      Then(List(FnSend(List(SlanValue("IdoNotCareResponse"))),
      FnDestroy(List(SlanValue("this"))))),
      Else(List(FnSend(List(SlanValue("IdoNotCareResponse"))))))))))),
    /*
      Transmissions:
        IdoNotCareResponse:
          Fn_Inc: responseCounter
    * */
    Behavior("Transmissions",
    List(MessageResponseBehavior(List(SlanValue("IdoNotCareResponse")),
    List(FnInc(List(SlanValue("responseCounter"))))))),

    /*
        ...SendMessage:
          ? 1000: null
          :
            Fn_Send: MyOpinionAboutYou
            Fn_Inc: messageCounter
    * */
    PeriodicBehavior("...SendMessage",
      List(PeriodicBehaviorFiringDuration(SlanValue(1000),None),
        FnSend(List(SlanValue("MyOpinionAboutYou"))), FnInc(List(SlanValue("messageCounter"))))))),

    /*
      Channels:
        TalksT0:
          null: Transmissions
    * */
    Channels(List(Channel("TalksT0",List(SlanValue("Transmissions"))))),
    /*
      Resources:
        messageCounter: 0
        responseCounter: 0
        maxMessagesThreshold: 10
    * */
    Resources(List(Resource(ResourceTag("messageCounter",None),List(SlanValue(0))),
    Resource(ResourceTag("responseCounter",None),List(SlanValue(0))),
    Resource(ResourceTag("maxMessagesThreshold",None),List(SlanValue(10))))),
    )),

    /*
    Messages:
      MyOpinionAboutYou:
      IdoNotCareResponse:
    * */
    Messages(List(
    Message(List(MessageDeclaration("MyOpinionAboutYou",None)),List(Resources(List()))),
    Message(List(MessageDeclaration("IdoNotCareResponse",None)),List(Resources(List()))))),

      /*
        Models:
          SocialMediaSimulation:
            Agents:
              Person: 2

            SimGraph:
              Person:
                TalksT0: Person

            Deployment:
              Nodes: # if no names or ip addresses provided then switch to autodiscovery
                - cancun
                - austin
              Akka Configuration: # this section will contain akka configuration data provided in app.conf files.
      * */
      Models(List(
      Model("SocialMediaSimulation",List(AgentPopulation("Person",List(SlanValue(2))),
        ModelGraph("SimGraph",List(Agent2AgentViaChannel("Person",List(Channel2Agent("TalksT0","Person"))))),
        ComputingNodes(List(SlanValue("cancun"), SlanValue("austin"))), AkkaConfigurationParameters(List())))
    )))
    val path = getClass.getClassLoader.getResource(primitiveSimulation).getPath
    translateSlanProgram(path).asserting(_ shouldBe expected)
  }
