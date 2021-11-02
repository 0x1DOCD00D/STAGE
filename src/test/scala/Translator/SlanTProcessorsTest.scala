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

import Translator.{SlanTranslator, SlantParser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}

class SlanTProcessorsTest extends AnyFlatSpec with Matchers :

  behavior of "the Slan traslator for SLAN Yaml specifications"

  val basicYamlTemplate_v1 = "Template_v1.yaml"
  val basicYamlTemplate_v2 = "Template_v2.yaml"
  val stringScalarValue = "just one string value"
  val intScalarValue = 1234567
  val floatScalarValue = 123450.6789
  val boolScalarValue = false
  /*

    it should "load up and translate a basic SLAN template as a list of maps" in {
      val path = getClass.getClassLoader.getResource(basicYamlTemplate_v2).getPath
      val res = SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel))
      logger.info(res.toString())
    }
  */

  it should "load up and translate a basic SLAN template as a map of maps" in {
    val path = getClass.getClassLoader.getResource(basicYamlTemplate_v1).getPath
    val res = SlanTranslator(SlantParser.convertJ2S(SlantParser(path).yamlModel))
    logger.info(res.toString())
  }

