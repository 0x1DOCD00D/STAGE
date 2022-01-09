/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.SlanKeywords.{Agents, Behavior, Models}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenericProcessorTest extends AnyFlatSpec with Matchers {

  behavior of "GenericProcessor"
  val basicYamlTemplate = "SlanFeatureTesting/Template_v1.yaml"

  it should "load up and find the key Agents" in {
    val path = getClass.getClassLoader.getResource(basicYamlTemplate).getPath
    val ymlModel = SlantParser(path).yamlModel
    new GenericProcessor {}.lookAhead(Agents, SlantParser.convertJ2S(ymlModel)) shouldBe true
  }

  it should "load up and fail to find the key Models" in {
    val path = getClass.getClassLoader.getResource(basicYamlTemplate).getPath
    val ymlModel = SlantParser(path).yamlModel
    new GenericProcessor {}.lookAhead(Models, SlantParser.convertJ2S(ymlModel)) shouldBe false
  }

  it should "load up and fail to find the key Behavior" in {
    val path = getClass.getClassLoader.getResource(basicYamlTemplate).getPath
    val ymlModel = SlantParser(path).yamlModel
    new GenericProcessor {}.lookAhead(Behavior, SlantParser.convertJ2S(ymlModel)) shouldBe false
  }
}
