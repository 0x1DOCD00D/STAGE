/*
 * Copyright (c) 2021-2022. Mark Grechanik and Grand Models, Inc, formerly Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

package PDFs

import HelperUtils.ConfigReference
import com.typesafe.config.Config
import org.apache.commons.math3.distribution.{BetaDistribution, NormalDistribution, PoissonDistribution, UniformRealDistribution}
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.hashing.MurmurHash3

class PdfStreamGeneratorTest extends AnyFlatSpec with Matchers {
  behavior of "random number generator"
  val config: Config = ConfigReference("Stage.Distributions") match {
    case Some(value) => value
    case None => throw new RuntimeException("Cannot obtain a reference to the config data for PDFs.")
  }

  it should "create hash codes from arrays of doubles" in {
    MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)) shouldBe MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187))
    MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)) should not be MurmurHash3.orderedHash(Array(1.12, 2.871, 52, 187))
  }

  /*
  * The Beta distribution is the conjugate prior for the Bernoulli, binomial, negative binomial and geometric distributions
  * (seems like those are the distributions that involve success & failure) in Bayesian inference. Computing a posterior using
  * a conjugate prior is very convenient, because you can avoid expensive numerical computation involved in Bayesian Inference.
  * Beta distribution can be understood as representing a distribution of probabilities, that is, it represents all the possible
  * values of a probability when we don't know what that probability is.

  * The beta distribution is a family of continuous probability distributions defined on the interval [0, 1] parameterized by
  * two positive shape parameters, denoted by α and β, that appear as exponents of the random variable and control the shape
  * of the distribution. The generalization to multiple variables is called a Dirichlet distribution.
  * We can use it to model the probabilities: the Click-Through Rate of your advertisement, the conversion rate of customers
  * actually purchasing on your website, how likely readers will clap for your blog, the 5-year survival chance for women with breast cancer.
  * Interpretation of α, β: α-1 as the number of successes and β-1 as the number of failures, just like n & n-x terms in binomial.
  * We can choose the α and β parameters this way: the probability of success is very high, let’s say 90%, set 90 for α and 10 for β.
  * Otherwise, 90 for β and 10 for α. As α becomes larger (more successful events), the bulk of the probability distribution will shift
  * towards the right, whereas an increase in β moves the distribution towards the left (more failures). Also, the distribution
  * will narrow if both α and β increase, for we are more certain.
  * https://towardsdatascience.com/beta-distribution-intuition-examples-and-derivation-cf00f4db57af
  * */
  the[IllegalArgumentException] thrownBy PdfStreamGenerator("BETADistribution", true, 90d, 10d, 1000d) should have message "Wrong number of arguments for distribution BETADistribution"

  the[IllegalArgumentException] thrownBy PdfStreamGenerator("BetaDistribution", true, 90d) should have message "Wrong number of arguments for distribution BetaDistribution"

  it should "create a Beta distribution, cache it and then make sure that it returns the cached stream" in {
    //90% success and 10% failure
    val betaDist1 = PdfStreamGenerator("betadistribution", true, 90d, 10d)
    val betaDist2 = PdfStreamGenerator("BetaDistribution", true, 90d, 10d)
    betaDist1.take(100) shouldBe betaDist2.take(100)
  }

  it should "create two different Beta distributions" in {
    //90% success and 10% failure
    val betaDist1 = PdfStreamGenerator("BetaDistribution", false, 90d, 10d)
    val betaDist2 = PdfStreamGenerator("BetaDistribution", false, 90d, 10d)
    betaDist1.take(100) shouldBe betaDist2.take(100)
  }

  it should "create a Beta distribution and drop its first value" in {
    //90% success and 10% failure
    val betaDist = PdfStreamGenerator("BetaDistribution", true, 90d, 10d)
    val betaDist1 = PdfStreamGenerator("BetaDistribution", true, 90d, 10d)
    betaDist.drop(1).head should not be betaDist1.head
    betaDist.drop(1).head shouldBe betaDist1(1)
  }

  it should "create a Binomial, distribution and drop its first value" in {
    val binomDist = PdfStreamGenerator("BinomialDistribution", false, 1000, 0.07)
    val binomDist1 = PdfStreamGenerator("BinomialDistribution", false, 1000, 0.07)
    binomDist.drop(1).head should not be binomDist1.head
    binomDist.drop(1).head shouldBe binomDist1(1)
  }

  it should "create two different Binomial distributions" in {
    val binomDist = PdfStreamGenerator("BinomialDistribution", false, Option(1L), Seq(1000d, 0.6d))
    val binomDist1 = PdfStreamGenerator("BinomialDistribution", false, Option(2L), Seq(1000d, 0.6d))
    binomDist.take(100) should not be binomDist1.take(100)
  }

  it should "create two identical Binomial distributions" in {
    val binomDist = PdfStreamGenerator("BinomialDistribution", false, Option(1L), Seq(1000d, 0.6d))
    val binomDist1 = PdfStreamGenerator("BinomialDistribution", false, Option(1L), Seq(1000d, 0.6d))
    binomDist.take(100) shouldBe binomDist1.take(100)
  }

  it should "create a chi-squared distribution and drop its first value" in {
    val chiDist = PdfStreamGenerator("ChiSquaredDistribution", false, 1)
    val chiDist1 = PdfStreamGenerator("ChiSquaredDistribution", false, 10)
    chiDist.drop(1).head should not be chiDist1.head
    chiDist1.drop(1).head shouldBe chiDist1(1)
  }


  it should "create a cauchy distribution and drop its first value" in {
    val cauchyDist = PdfStreamGenerator("cauchyDistribution", false, 20, 3)
    val cauchyDist1 = PdfStreamGenerator("cauchyDistribution", false, 20, 3)
    cauchyDist.drop(1).head should not be cauchyDist1.head
    cauchyDist.drop(1).head shouldBe cauchyDist1(1)
  }

  //The exponential distribution occurs naturally when describing the lengths of the inter-arrival times in a homogeneous Poisson process.
  it should "create an exponential distribution and drop its first value" in {
    val expDist1 = PdfStreamGenerator("exponentialdistributioN", false, 100)
    val expDist = PdfStreamGenerator("exponentialdistribution", false, 100)
    println(expDist.take(15).mkString(", "))
    expDist.drop(1).head should not be expDist1.head
    expDist.drop(1).head shouldBe expDist1(1)
  }

  it should "create an F distribution and drop its first value" in {
    val FDist = PdfStreamGenerator("fdistribution", false, 20, 3)
    val FDist1 = PdfStreamGenerator("fdistribution", false, 20, 3)
    FDist.drop(1).head should not be FDist1.head
    FDist.drop(1).head shouldBe FDist1(1)
  }

  it should "create a geometric distribution and drop its first value" in {
    val GDist = PdfStreamGenerator("geometricdistribution", false, 0.07)
    val GDist1 = PdfStreamGenerator("geometricdistribution", false, 0.07)
    GDist.drop(1).head should not be GDist1.head
    GDist.drop(1).head shouldBe GDist1(1)
  }


  it should "create a Gamma and F distributions that should be different" in {
    val Dist = PdfStreamGenerator("GAMMAdistribution", false, 8, 3)
    val Dist1 = PdfStreamGenerator("fdistribution", false, 20, 3)
    Dist.take(10).toList should not be Dist1.take(10).toList
  }

  //model the distribution of the maximum (or the minimum) of a number of samples of various distributions.
  it should "create Gamma and gumbel distributions that should be different" in {
    val Dist = PdfStreamGenerator("GAMMAdistribution", false, 8, 1)
    val Dist1 = PdfStreamGenerator("gumbeldistribution", false, 50, 10)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  //the hypergeometric distribution is a discrete probability distribution that describes the probability of k successes
  // (random draws for which the object drawn has a specified feature) in n draws, without replacement, from a finite
  // population of size N that contains exactly K objects with that feature, wherein each draw is either a success or a failure.
  // In contrast, the binomial distribution describes the probability of k successes in n draws with replacement.
  //populationSize: Int, numberOfSuccesses: Int, sampleSize: Int
  it should "create gumbel and hypergeometric distributions that should be different" in {
    val Dist = PdfStreamGenerator("hypergeometricdistribution", false, 100, 10, 80)
    val Dist1 = PdfStreamGenerator("gumbeldistribution", false, 50, 5)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  //The difference between two independent identically distributed exponential random variables is governed by a Laplace distribution, as is a Brownian motion evaluated at an exponentially distributed random time. I
  it should "create hypergeometric and laplace distributions that should be different" in {
    val Dist = PdfStreamGenerator("normaldistribution", false, 10, 3)
    val Dist1 = PdfStreamGenerator("laplacedistribution", false, 10, 3)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  //parameters are location and scale
  it should "create laplace and levy distributions that should be different" in {
    val Dist = PdfStreamGenerator("laplacedistribution", false, 10, 3)
    val Dist1 = PdfStreamGenerator("levydistribution", false, 10, 3)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create lognormal and normal distributions that should be different" in {
    val Dist = PdfStreamGenerator("normaldistribution", false, 2, 1)
    val Dist1 = PdfStreamGenerator("lognormaldistribution", false, 2, 1)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create normal and logistic distributions that should be different" in {
    val Dist = PdfStreamGenerator("logisticdistribution", false, 1, 1)
    val Dist1 = PdfStreamGenerator("normaldistribution", false, 1, 1)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create nakagami and normal distributions that should be different" in {
    val Dist = PdfStreamGenerator("nakagamidistribution", false, 10, 2)
    val Dist1 = PdfStreamGenerator("normaldistribution", false, 10, 2)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create normal and pareto distributions that should be different" in {
    val Dist = PdfStreamGenerator("paretodistribution", false, 10, 1)
    val Dist1 = PdfStreamGenerator("normaldistribution", false, 10, 1)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  //r > 0 — number of failures until the experiment is stopped (integer, but the definition can also be extended to reals)
  //p ∈ [0,1] — success probability in each experiment (real)
  it should "create hypergeometric and pascal distributions that should be different" in {
    val Dist = PdfStreamGenerator("pascaldistribution", false, 10, 0.9 )
    val Dist1 = PdfStreamGenerator("hypergeometricdistribution", false, 100, 10, 80)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  val wrongProbabilityValue = 1.2
  the[org.apache.commons.math3.exception.OutOfRangeException] thrownBy PdfStreamGenerator("pascaldistribution", false, 10, wrongProbabilityValue) should have message s"$wrongProbabilityValue out of [0, 1] range"

  //expresses the probability of a given number of events occurring in a fixed interval of time or space if these events occur with a known constant mean rate and independently of the time since the last event.
  it should "create normal and poisson distributions that should be different" in {
    val Dist = PdfStreamGenerator("poissondistribution", false, 5)
    val Dist1 = PdfStreamGenerator("normaldistribution", false, 5, 1)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  //it is more prone to producing values that fall far from its mean.
  it should "create normal and T distributions that should be different" in {
    val Dist = PdfStreamGenerator("tdistribution", false, 5)
    val Dist1 = PdfStreamGenerator("normaldistribution", false, 5, 1)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  the[org.apache.commons.math3.exception.NumberIsTooLargeException] thrownBy PdfStreamGenerator("triangulardistribution", false, -2, 10, 8) should have message "10 is larger than the maximum (8)"

  it should "create normal and triangular distributions that should be different" in {
    val Dist = PdfStreamGenerator("triangulardistribution", false, 1000, 2500, 3000)
    val Dist1 = PdfStreamGenerator("normaldistribution", false, 5, 1)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create uniform real and triangular distributions that should be different" in {
    val Dist = PdfStreamGenerator("triangulardistribution", false, -2, 8, 10)
    val Dist1 = PdfStreamGenerator("uniformrealdistribution", false, -2, 10)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create uniform real and integer distributions that should be different" in {
    val Dist = PdfStreamGenerator("uniformIntegerdistribution", false, -2, 10)
    val Dist1 = PdfStreamGenerator("uniformrealdistribution", false, -2, 10)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create normal and weibull distributions that should be different" in {
    val Dist = PdfStreamGenerator("weibulldistribution", false, 10, 20)
    val Dist1 = PdfStreamGenerator("normaldistribution", false, 5, 1)
/*
    import java.text.DecimalFormat
    val df = new DecimalFormat("0.00")
    println(Dist.take(15).map(df.format).mkString(", "))
    println(Dist1.take(15).map(df.format).mkString(", "))
*/
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  //number of elements and exponent
  it should "create zips and pareto distributions that should be different" in {
    val Dist = PdfStreamGenerator("paretodistribution", false, 1, 1)
    val Dist1 = PdfStreamGenerator("zipfdistribution", false, 1000, 1)
    Dist.take(10).toList.foldLeft(0.0)((acc, v) => acc + v) should not be Dist1.take(10).toList.foldLeft(0.0)((acc, v) => acc + v)
  }

  it should "create an enumerated distribution" in {
    val Dist = PdfStreamGenerator(false, None, Seq((1, 0.2), (10,0.5), (100,0.7),(1000, 0.9)))
    val res = Dist.take(20).toList
    res.count(elem => elem.toInt == 1) should be < res.count(elem => elem.toInt == 1000)
    res.count(elem => elem.toInt == 100) should be > res.count(elem => elem.toInt == 10)
  }

  it should "create a sequenced list of unique longs" in {
    val listOfLongs = PdfStreamGenerator(0)
    val res = listOfLongs.take(100).toList
    res.sum shouldEqual 4950
  }

}
