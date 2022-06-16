/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package Translator

import Translator.SlanAbstractions.YamlTypes
import Translator.SlanKeywords.{AgentsSection, Behavior, ModelsSection}
import akka.actor.Status.Success
import cats.effect.*
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class GenericProcessorTest extends AsyncFreeSpec with AsyncIOSpec with Matchers {
  val basicYamlTemplate = "SlanFeatureTesting/Template_v1.yaml"
  val path: String = getClass.getClassLoader.getResource(basicYamlTemplate).getPath
  def parseObtainYamlRef(entry: String): IO[Boolean] = {
    for {
      ymlModelFib <- SlantParser(path).start
      ymlModel <- ymlModelFib.join
      ymlIo = ymlModel match
        case Succeeded(result) => for {
          fa <- result
          outyml = fa match
            case Right(SlanYamlHandle(ymlref)) => ymlref
            case err => IO.raiseError( new RuntimeException(s"Incorrect value computed: ${err.toString}"))
        } yield outyml
        case Errored(e) => IO.raiseError(e)
        case _ => IO.raiseError(new RuntimeException("Fiber computation failure."))
      yml <- ymlIo
      reseval = new GenericProcessor {}.lookAhead(entry, SlantParser.convertJ2S(yml))
    } yield reseval
  }

  "GenericProcessor" - {
    "load up and find the key Agents" in {
      parseObtainYamlRef(AgentsSection).asserting(_ shouldBe true)
    }

    "load up and fail to find the key Models" in {
      parseObtainYamlRef(ModelsSection).asserting(_ shouldBe false)
    }

    "load up and fail to find the key Behavior" in {
      parseObtainYamlRef(Behavior).asserting(_ shouldBe false)
    }
  }
}
