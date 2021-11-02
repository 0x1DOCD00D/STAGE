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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}

class SlantParserTest extends AnyFlatSpec with Matchers {
  behavior of "the Slant parser for single value scalars"
  val basicYamlTemplate = "Template_v1.yaml"
  val illFormedYaml = "IllFormed.yaml"
  val nonexistentYamlFile = "Hades.yaml"
  val singleScalarFloatingPointValueFile = "OneScalarFloatingPointValue.yaml"
  val singleScalarIntValueFile = "OneScalarIntValue.yaml"
  val singleScalarStringValueFile = "OneScalarStringValue.yaml"
  val singleScalarBooleanValueFile = "OneScalarBooleanValue.yaml"
  val singleScalarNullValueFile = "OneScalarNULLValue.yaml"
  val blockSequenceFile = "BlockSequenceSimple.yaml"
  val stringScalarValue = "just one string value"
  val intScalarValue = 1234567
  val floatScalarValue = 123450.6789
  val boolScalarValue = false

  the[IllegalArgumentException] thrownBy SlantParser(nonexistentYamlFile) should have message s"Error occured when loading input Yaml script $nonexistentYamlFile: $nonexistentYamlFile (No such file or directory)"

  it should "load up and extract the content of the single null entry in yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarNullValueFile).getPath
    val res = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: Option[_] => None
      case _ => Some(path)
    }

    res shouldBe None
  }


  it should "load up and extract the content of the single scalar string value incorrectly treated as boolean" in {
    val path = getClass.getClassLoader.getResource(singleScalarStringValueFile).getPath
    val res = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: Boolean => v
      case _ => None
    }
    res shouldBe None
  }

  it should "load up and extract the content of the single scalar boolean value yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarBooleanValueFile).getPath
    val res = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: Boolean => v
      case _ => !boolScalarValue
    }
    res shouldBe boolScalarValue
  }

  it should "load up and extract the content of the single scalar string value yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarStringValueFile).getPath
    val res = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: String => v
      case _ => null
    }
    res shouldBe stringScalarValue
  }

  it should "load up and extract the content of the single scalar floating point value yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarFloatingPointValueFile).getPath
    val res = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: Double => v
      case _ => -floatScalarValue
    }
    res shouldBe floatScalarValue
  }

  it should "load up and extract the content of the single scalar int value yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarIntValueFile).getPath
    val res = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: Int => v
      case _ => -intScalarValue
    }
    res shouldBe intScalarValue
  }


  it should "throw an exception for ill-formed yaml" in {
    val path = getClass.getClassLoader.getResource(illFormedYaml).getPath
    the[java.lang.IllegalArgumentException] thrownBy SlantParser(path) should have message
      """Error occured when parsing input Yaml script: mapping values are not allowed here
        | in 'string', line 2, column 4:
        |    key: scalar2
        |       ^
        |""".stripMargin
  }

  behavior of "the Slant parser for lists of values"
  val seqOfScalarsFile = "SeqOfScalars.yaml"
  val seqFlowOfScalarsFile = "SeqFlowScalars.yaml"
  val seqOfSeqOfScalarsFile = "SeqOfSeqOfScalars.yaml"

  it should "load up and extract the content of the sequence of scalars from a yaml file" in {
    val path = getClass.getClassLoader.getResource(seqOfScalarsFile).getPath
    val result = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: List[_] => v
      case _ => null
    }
    result.asInstanceOf[List[_]] shouldBe List(stringScalarValue, intScalarValue, floatScalarValue, boolScalarValue, null)
  }

  it should "load up and extract the content of the flow sequence of scalars from a yaml file" in {
    val path = getClass.getClassLoader.getResource(seqFlowOfScalarsFile).getPath
    val result = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: List[_] => v
      case _ => null
    }
    result.asInstanceOf[List[_]] shouldBe List(stringScalarValue, intScalarValue, floatScalarValue, boolScalarValue, null)
  }

  it should "load up and extract the content of the sequence of flow sequences of scalars from a yaml file" in {
    val path = getClass.getClassLoader.getResource(seqOfSeqOfScalarsFile).getPath
    val result = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: List[_] => v
      case _ => List()
    }
    result should not be List()
    val convResult = result.asInstanceOf[List[_]].toList
    convResult.length shouldBe 3
    convResult.head.asInstanceOf[java.util.ArrayList[_]].asScala.toList shouldBe List(stringScalarValue, intScalarValue, floatScalarValue, boolScalarValue, null)
  }

  behavior of "the Slant parser for complex yaml scripts"
  val simpleMapFile = "OneSimpleMap.yaml"
  val keyName = "key"
  val keyValue = "value"
  val threeSimpleMapsFile = "ThreeSimpleMaps.yaml"

  it should "load up and extract the content of one simple map from a yaml file" in {
    val path = getClass.getClassLoader.getResource(simpleMapFile).getPath
    val result = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: Map[_, _] => v
      case _ => Map()
    }
    result should not be null
    val convResult = result.asInstanceOf[Map[String, String]].toMap
    convResult.get(keyName) shouldBe Option(keyValue)
  }

  it should "load up and extract the content of three simple maps from a yaml file" in {
    val path = getClass.getClassLoader.getResource(threeSimpleMapsFile).getPath
    val result = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: Map[_, _] => v
      case _ => Map()
    }
    result should not be null
    val convResult = result.asInstanceOf[Map[String, String]].toMap
    (1 to 3).foreach(iter => {
      convResult.get(keyName + iter.toString) shouldBe Option(keyValue + iter.toString)
    })
  }


  it should "load up and extract the content of the yaml script from a script file" in {
    val path = getClass.getClassLoader.getResource(basicYamlTemplate).getPath
    //    SlantParser(path).visit
  }

  it should "load up and extract the content of block sequences with dashes from a script file" in {
    val path = getClass.getClassLoader.getResource(blockSequenceFile).getPath
    val result = SlantParser.convertJ2S(SlantParser(path).yamlModel) match {
      case v: List[_] => v
      case _ => List()
    }
    result.asInstanceOf[List[_]].toList.length shouldBe 4
  }
}



