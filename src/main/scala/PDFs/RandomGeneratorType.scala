/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package PDFs

import HelperUtils.ErrorWarningMessages.LogGenericMessage
import HelperUtils.Parameters.config
import org.apache.commons.math3.random.*

import scala.util.{Failure, Success, Try}


trait RandomGeneratorType:
  private val defaultSeed: Long = System.currentTimeMillis()

  def createRandomGenerator(seed: Option[Long]): RandomGenerator =
    val rgISAACRandom = "ISAACRandom".toUpperCase
    val rgJDKRandomGenerator = "JDKRandomGenerator".toUpperCase
    val rgMersenneTwister = "MersenneTwister".toUpperCase
    val rgWell512a = "Well512a".toUpperCase
    val rgWell1024a = "Well1024a".toUpperCase
    val rgWell19937a = "Well19937a".toUpperCase
    val rgWell19937c = "Well19937c".toUpperCase
    val rgWell44497a = "Well44497a".toUpperCase
    val rgWell44497b = "Well44497b".toUpperCase

    val randomGenerator: RandomGenerator = Try(config.getString("Stage.Random.generator")) match
      case Success(rgName) => rgName.toUpperCase match {
        case `rgISAACRandom` => new ISAACRandom
        case `rgJDKRandomGenerator` => new JDKRandomGenerator
        case `rgMersenneTwister` => new MersenneTwister
        case `rgWell512a` => new Well512a
        case `rgWell1024a` => new Well1024a
        case `rgWell19937a` => new Well19937a
        case `rgWell19937c` => new Well19937c
        case `rgWell44497a` => new Well44497a
        case `rgWell44497b` => new Well44497b
      }
      case Failure(exception) => new Well1024a

    randomGenerator.setSeed {
      if seed.exists(v => v > 0) then seed.getOrElse(System.currentTimeMillis()) else Try(config.getLong("Stage.Random.seed")) match {
        case Success(value) => value
        case Failure(exception) =>
          LogGenericMessage(getClass, s"Using default seed $defaultSeed because it is missing in the configuration file.")
          defaultSeed
      }
    }
    randomGenerator

  end createRandomGenerator
