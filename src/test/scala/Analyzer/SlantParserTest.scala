/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Analyzer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class SlantParserTest extends AnyFlatSpec with Matchers {
  behavior of "the Slant parser"
  val basicYamlTemplate = "Template.yaml"
  val illFormedYaml = "IllFormed.yaml"
  val nonexistentYamlFile = "Hades.yaml"
  val singleScalarFloatingPointValueFile = "OneScalarFloatingPointValue.yaml"
  val singleScalarIntValueFile = "OneScalarIntValue.yaml"
  val singleScalarStringValueFile = "OneScalarStringValue.yaml"
  val stringScalarValue = "just one string value"
  val intScalarValue = 1234567
  val floatScalarValue = 123450.6789

  the[IllegalArgumentException] thrownBy SlantParser(nonexistentYamlFile) should have message s"Error occured when loading input Yaml script $nonexistentYamlFile: $nonexistentYamlFile (No such file or directory)"

  it should "load up and extract the content of the single scalar string value yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarStringValueFile).getPath
    SlantParser(path) match {
      case v: String => v
      case _ => null
    }
    shouldBe stringScalarValue
  }

  it should "load up and extract the content of the single scalar floating point value yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarFloatingPointValueFile).getPath
    SlantParser(path) match {
      case v: Double => v
      case _ => null
    }
    shouldBe floatScalarValue
  }

  it should "load up and extract the content of the single scalar int value yaml" in {
    val path = getClass.getClassLoader.getResource(singleScalarIntValueFile).getPath
    SlantParser(path) match {
      case v: Int => v
      case _ => null
    }
    shouldBe intScalarValue
  }

  it should "load up and extract the content of the yaml script from a script file" in {
    val path = getClass.getClassLoader.getResource(basicYamlTemplate).getPath
    val parser = SlantParser(path)
    val k = 0
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

}

