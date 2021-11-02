/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package HelperUtils

import HelperUtils.ConfigReference.*
import org.apache.commons.math3.random.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class ConfigTestSuite extends AnyFlatSpec with Matchers {
  behavior of "configuration parameters module"
  val config = ConfigReference("Stage") match {
    case Some(value) => value
    case None => throw new RuntimeException("Cannot obtain a reference to the config data.")
  }

  it should "obtain the values of parameters for distributions" in {
    val mapDistParams = Map(
      "BetaDistribution" -> 2,
      "BinomialDistribution" -> 2,
      "CauchyDistribution" -> 2,
      "ChiSquaredDistribution" -> 1,
      "ExponentialDistribution" -> 1,
      "FDistribution" -> 2,
      "GammaDistribution" -> 2,
      "GeometricDistribution" -> 1,
      "GumbelDistribution" -> 2,
      "HypergeometricDistribution" -> 3,
      "LaplaceDistribution" -> 2,
      "LevyDistribution" -> 2,
      "LogNormalDistribution" -> 2,
      "LogisticDistribution" -> 2,
      "NakagamiDistribution" -> 2,
      "NormalDistribution" -> 2,
      "ParetoDistribution" -> 2,
      "PascalDistribution" -> 2,
      "PoissonDistribution" -> 1,
      "TDistribution" -> 1,
      "TriangularDistribution" -> 3,
      "UniformRealDistribution" -> 2,
      "UniformIntegerDistribution" -> 2,
      "WeibullDistribution" -> 2,
      "ZipfDistribution" -> 2
    )
    mapDistParams.map((k, v) => config.getInt(s"Stage.Distributions.${k.toLowerCase}") shouldBe v)
  }

  it should "obtain the same randomly generated values from two random generators with the same seed" in {
    val rgISAACRandom = "ISAACRandom".toUpperCase
    val rgJDKRandomGenerator = "JDKRandomGenerator".toUpperCase
    val rgMersenneTwister = "MersenneTwister".toUpperCase
    val rgWell512a = "Well512a".toUpperCase
    val rgWell1024a = "Well1024a".toUpperCase
    val rgWell19937a = "Well19937a".toUpperCase
    val rgWell19937c = "Well19937c".toUpperCase
    val rgWell44497a = "Well44497a".toUpperCase
    val rgWell44497b = "Well44497b".toUpperCase

    val defaultSeed = System.currentTimeMillis()

    def getRandomGenerator = Try(config.getString("Stage.Random.generator")) match {
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
        case _ => throw new Exception("incorrect name of the random generator")
      }
      case Failure(exception) => new Well1024a
    }

    val randomGenerator = getRandomGenerator
    val randomGenerator1 = getRandomGenerator

    randomGenerator.setSeed(Try(config.getLong("Stage.Random.seed")) match {
      case Success(value) => value
      case Failure(exception) => defaultSeed
    })
    randomGenerator1.setSeed(Try(config.getLong("Stage.Random.seed")) match {
      case Success(value) => value
      case Failure(exception) => defaultSeed
    })

    randomGenerator.nextInt() shouldBe randomGenerator1.nextInt()
  }
}
