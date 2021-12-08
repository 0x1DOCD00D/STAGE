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
  val behaviorMessages_flow = "Behavior_Messages_KeyFlow.yaml"
  val behaviorMessages_list = "Behavior_Messages_KeyList.yaml"
  val behaviorIfThenElse_1 = "Behavior_IfThenElse.yaml"
  val behaviorOneMessage = "Behavior_One_Message.yaml"
  val behaviorNullMessage = "Behavior_Default_Null.yaml"
  val behaviorPeriodic = "Behavior_Period.yaml"
  val basicYamlTemplate_v1 = "Template_v1.yaml"
  val basicYamlTemplate_v2 = "Template_v2.yaml"
  val channelYaml = "Channels.yaml"
  val messageYaml = "Messages.yaml"
  val resources_v0 = "Resources_v0.yaml"
  val resources_v1 = "Resources_v1.yaml"
  val resources_v2 = "Resources_v2.yaml"
  val resources_v3 = "Resources_v3.yaml"
  val resources_v4 = "Resources_v4.yaml"
  val resources_v5 = "Resources_v5.yaml"
  val resources_v6 = "Resources_v6.yaml"
  val resources_v7 = "Resources_v7.yaml"
  val resources_v8 = "Resources_v8.yaml"
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
    val expected = List(Group("Group Name", List(
      GroupAgent("agentname1", List(SlanValue("randomGenerator4Agent1"))),
      GroupAgent("agentname2", List(SlanValue(100))),
      GroupAgent("bubba", List()),
      ResourceReferenceInGroup(List(
        ResourceConsistencyModelInGroup("Causal", "hdd")), SlanValue(2)),
      ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("eventual", "vNic")), SlanValue(3)),
      ResourceReferenceInGroup(List(ResourceConsistencyModelInGroup("Eventual", "varX")), SlanValue("randomGenerator1"))))
    )
    val path = getClass.getClassLoader.getResource(agentsGroups1).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a behavior spec with multiple messages as the key flow sequence" in {
    val expected = List(Behavior(Some("Behavior 4 Messages 1"),
      List(MessageResponseBehavior(List(SlanValue("MessageX"), SlanValue("MessageY"), SlanValue("MessageZ")),
        List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))
    val path = getClass.getClassLoader.getResource(behaviorMessages_flow).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a behavior spec with multiple messages as the key list" in {
    val expected = List(Behavior(Some("Behavior 4 Messages 2"),
      List(MessageResponseBehavior(List(SlanValue("MessageXX"), SlanValue("MessageYY"), SlanValue("MessageZZ")),
        List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))
    val path = getClass.getClassLoader.getResource(behaviorMessages_list).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a behavior spec with one message response" in {
    val expected = List(Behavior(Some("Behavior 4 Messages 3"),
                        List(MessageResponseBehavior(List(SlanValue("SomeMessage")),
                        List(FnUpdate(List(SlanValue("resourceName2Update"),
                          FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))
    val path = getClass.getClassLoader.getResource(behaviorOneMessage).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a default behavior spec for null message" in {
    val expected = List(Behavior(Some("Default Behavior 4 All Messages"),
      List(MessageResponseBehavior(List(),
        List(FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))
    val path = getClass.getClassLoader.getResource(behaviorNullMessage).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate an if-then-else behavior" in {
    val expected = List(Behavior(Some("BehaviorIfThenElse"),
      List(MessageResponseBehavior(List(SlanValue("MessageX"), SlanValue("MessageY"), SlanValue("MessageZ")),
        List(IfThenElse(List(
          And(List(ROPLessEqual(List(SlanValue("someResource"), SlanValue(3.1415926))),
            Not(List(
              Or(List(ROPEqual(List(SlanValue("someResource"), SlanValue("someOtherResource"))),
                ROPEqual(List(Not(List(SlanValue("someBooleanResource"))), SlanValue(false))))))))),
          Then(List(
            FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))),
            FnUpdate(List(SlanValue("resourceName2Update"), FnMultiply(List(SlanValue(3.141), SlanValue("generatorRefId"))))))))))))))
    val path = getClass.getClassLoader.getResource(behaviorIfThenElse_1).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a simple resource spec with a fixed value" in {
    val expected = List(
      Resource(ResourceTag("autoInitializedPrimitiveResource",None),
        List(SlanValue(10))))
    val path = getClass.getClassLoader.getResource(resources_v0).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a simple resource spec with a randomly generated value" in {
    val expected = List(Resource(ResourceTag("autoInitializedPrimitiveResource",None),
        List(
          Resource(ResourceTag("Uniform",None),
            List(SlanValue(0), SlanValue(1))))))
    val path = getClass.getClassLoader.getResource(resources_v1).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a resource spec with a composite key and a randomly generated value" in {
    val expected = List(
      Resource(ResourceTag("autoInitializedPrimitiveListResource",Some("list")),
        List(
          Resource(ResourceTag("Uniform",None),List(SlanValue(0), SlanValue(1))))))
    val path = getClass.getClassLoader.getResource(resources_v2).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a resource spec with a composite key and a list of fixed values" in {
    val expected = List(
      Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),
        List(SlanValue(1), SlanValue(10), SlanValue(100)))
      )
    val path = getClass.getClassLoader.getResource(resources_v3).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }


  it should "translate a resource spec for a composite resource with a list of resources and a single simple resource" in {
    val expected = List(
      Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),
        List(SlanValue(1), SlanValue(10), SlanValue(100)))
    )
    val path = getClass.getClassLoader.getResource(resources_v3).getPath
    val res = SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) //shouldBe expected
    println(res)
  }

  it should "translate a resource spec with a composite key whose attributes include a composite and a simple resources" in {
    val expected = List(
      Resource(ResourceTag("compositeResource",None),
        List(
          Resource(ResourceTag("someBasicResource1V",Some("list")),List(SlanValue(100), SlanValue(1000))),
          Resource(ResourceTag("valueHolder4compositeResource",None),List(SlanValue(1)))))
    )
    val path = getClass.getClassLoader.getResource(resources_v4).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a complex resource spec that describes a table populated with random data" in {
    val expected = List(
      Resource(ResourceTag("dim3",None),
        List(Resource(ResourceTag("column1",None),
          List(Resource(ResourceTag("Enum",None),
            List(Resource(ResourceTag("Item1",None),
              List(Resource(ResourceTag("Uniform",None),
                List(SlanValue(0.2), SlanValue(0.8))))),
              Resource(ResourceTag("Item2",None),List(SlanValue(0.2))),
              Resource(ResourceTag("Item3",None),
                List(Resource(ResourceTag("Uniform",None),
                  List(SlanValue(0.1), SlanValue(0.6))))))))),
          Resource(ResourceTag("column2",None),
            List(Resource(ResourceTag("Uniform",None),
              List(SlanValue(0), SlanValue(1))))),
          Resource(ResourceTag("column3",None),
            List(Resource(ResourceTag("Fn_Multiply",None),
              List(Resource(ResourceTag("Uniform",None),
                List(SlanValue(0), SlanValue(1))), SlanValue("column2"))))))),
      Resource(ResourceTag("dim4",None),
        List(Resource(ResourceTag("dim3",Some("list")),
          List(Resource(ResourceTag("Uniform",None),
            List(SlanValue(100), SlanValue(1000))),
            Resource(ResourceTag("column4",None),
              List(Resource(ResourceTag("Uniform",None),
                List(SlanValue(200), SlanValue(500))))),
            Resource(ResourceTag("column5",None),
              List(Resource(ResourceTag("Uniform",None),
                List(SlanValue(1), SlanValue(5))))))),
          Resource(ResourceTag("someOtherAttribute",None),List()))),
      Resource(ResourceTag("dim5",None),
        List(Resource(ResourceTag("dim4",Some("list")),List()),
          Resource(ResourceTag("column6",None),
            List(Resource(ResourceTag("Uniform",None),
              List(SlanValue(200), SlanValue(500)))))))
    )
    val path = getClass.getClassLoader.getResource(resources_v5).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a resource spec with a list of the instances of a composite resources that contains a queue initialized with some values" in {
    val expected = List(
      Resource(ResourceTag("HDD",Some("list")),
        List(Resource(ResourceTag("Size",None),
          List(Resource(ResourceTag("Normal",None),List(SlanValue(3000), SlanValue(1000))),
            Resource(ResourceTag(">=",None),List(SlanValue(0))),
            Resource(ResourceTag("<",None),List(SlanValue(2000))))),
          Resource(ResourceTag("Utilization",None),List(Resource(ResourceTag(">=",None),List(SlanValue(0))),
            Resource(ResourceTag("<",None),List(SlanValue("Size"))))),
          Resource(ResourceTag("ItemCount",None),List(SlanValue(0),
            Resource(ResourceTag(">=",None),List(SlanValue(0))))),
          Resource(ResourceTag("DataStore",Some("queue")),
            List(SlanValue(100), SlanValue(1000), SlanValue(100000)))))
    )
    val path = getClass.getClassLoader.getResource(resources_v6).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a resource spec that is a direct representation of a table with columns" in {
    val expected = List(
      Resource(ResourceTag("someTableResource",None),
        List(Resource(ResourceTag("columns",Some("list")),
          List(Resource(ResourceTag("column3",None),List()),
            Resource(ResourceTag("column1",None),List()),
            Resource(ResourceTag("column6",None),List()),
            Resource(ResourceTag("column2",None),List()),
            Resource(ResourceTag("column5",None),List()),
            Resource(ResourceTag("column4",None),List())))))
    )
    val path = getClass.getClassLoader.getResource(resources_v7).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a resource spec for designating an external service: RESTful or JAR" in {
    val expected = List(
      Resource(ResourceTag("UniqueServiceId",None),
        List(Resource(ResourceTag("protocol",None),
          List(SlanValue("Jar"))),
          Resource(ResourceTag("http://url/to/Jar/name.jar",None),
            List(Resource(ResourceTag("methodName",None),
              List(Resource(ResourceTag("p1name",None),
                List(SlanValue("parm1"))),
                Resource(ResourceTag("p2name",None),List(SlanValue("parm2"))))),
              Resource(ResourceTag("otherMethodName",None),
                List(SlanValue("parm1"), SlanValue("parm2"), SlanValue("parm3")))))))
    )
    val path = getClass.getClassLoader.getResource(resources_v8).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a channel spec with multiple behaviors for different messages" in {
    val expected = List(
      Channel("Sensors2CloudChannel",
        List(MessageResponseBehavior(
          List(MessageResponseBehavior(List(SlanValue("MessageX"), SlanValue("MessageY")),
            List(SlanValue("behaviorAttached2Channel")))),List()),
          MessageResponseBehavior(List(
            MessageResponseBehavior(List(SlanValue("MessageZ")),
              List(SlanValue("otherBehaviorAttached2Channel")))),List()),
          MessageResponseBehavior(List(SlanValue("MessageXX"), SlanValue("MessageYY")),
            List(SlanValue("moreBehaviors"))),
          MessageResponseBehavior(List(SlanValue("MessageW")),List(SlanValue("someWierdBehavior")))))
    )
    val path = getClass.getClassLoader.getResource(channelYaml).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a message spec with multiple fields" in {
    val expected = List(
      Message("Message Name",
        List(Resource(ResourceTag("Recursive Field",None),
          List(Resource(ResourceTag("Message Name",None),List(SlanValue(3))))),
          Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),
            List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value",None),
            List(SlanValue(100))),
          Resource(ResourceTag("Another Recursive Field",None),
            List(Resource(ResourceTag("Message Name",None),
              List(Resource(ResourceTag("Uniform",None),
                List(SlanValue(0), SlanValue(2))))))),
          Resource(ResourceTag("Field Name",None),
            List(Resource(ResourceTag("Uniform",None),
              List(SlanValue(1), SlanValue(200)))))))
    )
    val path = getClass.getClassLoader.getResource(messageYaml).getPath
    SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel)) shouldBe expected
  }

  it should "translate a spec for a periodic behavior" in {
    val expected = List(
      Message("Message Name",
        List(Resource(ResourceTag("Recursive Field",None),
          List(Resource(ResourceTag("Message Name",None),List(SlanValue(3))))),
          Resource(ResourceTag("someBasicResourceListOfValues",Some("queue")),
            List(SlanValue(1), SlanValue(10), SlanValue(100))),
          Resource(ResourceTag("Some Fixed Value",None),
            List(SlanValue(100))),
          Resource(ResourceTag("Another Recursive Field",None),
            List(Resource(ResourceTag("Message Name",None),
              List(Resource(ResourceTag("Uniform",None),
                List(SlanValue(0), SlanValue(2))))))),
          Resource(ResourceTag("Field Name",None),
            List(Resource(ResourceTag("Uniform",None),
              List(SlanValue(2019-11-01), SlanValue(2021-12-31)))))))
    )
    val path = getClass.getClassLoader.getResource(messageYaml).getPath
    val res = SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel))// shouldBe expected
    println(res)
  }
