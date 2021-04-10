import PdfSampleGenerator.pdfs
import org.apache.commons.math4.linear.{MatrixUtils, RealMatrix}
import org.apache.commons.math4.random.{CorrelatedRandomVectorGenerator, GaussianRandomGenerator}
import org.apache.commons.rng.UniformRandomProvider
import org.apache.commons.rng.simple.RandomSource
import org.apache.commons.statistics.distribution._

import scala.collection.mutable.ListBuffer
import scala.util.hashing.MurmurHash3
/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *   
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *  
 */

import scala.util.{Failure, Success, Try}

object PdfSampleGenerator extends Enumeration {
  type PDF = Value
  val BetaDistribution,
  BinomialDistribution,
  CauchyDistribution,
  ChiSquaredDistribution,
  ConstantContinuousDistribution,
  ExponentialDistribution,
  FDistribution,
  GammaDistribution,
  GeometricDistribution,
  GumbelDistribution,
  HypergeometricDistribution,
  LaplaceDistribution,
  LevyDistribution,
  LogNormalDistribution,
  LogisticDistribution,
  NakagamiDistribution,
  NormalDistribution,
  ParetoDistribution,
  PascalDistribution,
  PoissonDistribution,
  TDistribution,
  TriangularDistribution,
  UniformContinuousDistribution,
  UniformDiscreteDistribution,
  WeibullDistribution,
  ZipfDistribution
  = Value
  //check if a given distribution object exists, then check to see if an object exists for specific parameters
  private[this] val pdfs: scala.collection.mutable.Map[PDF, scala.collection.mutable.Map[Int, Stream[Double]]] = scala.collection.mutable.Map()
  private val random = RandomSource.create(RandomSource.JDK)


  def apply(pdf: PDF, params: Array[Double]): Option[Stream[Double]] = {
    Try(retrievePdf(pdf, params)) match {
      case Success(streamValues) => {
        println(streamValues)
        Some(streamValues)
      }
      case Failure(ex) => None
    }
  }

  private def exists(pdf: PDF): Boolean = values.toList.contains(pdf)

  private def retrievePdf(pdf: PDF, params: Array[Double]): Stream[Double] = {
    val paramCode: Int = MurmurHash3.orderedHash(params)

    def wrongArguments(pdf: PDF, expected: Int) = if (params.length != expected) throw IllegalArgumentException(s"wrong arguments for distribution ${pdf.toString}")

    def createNewDistribution(pdf: PDF, params: Array[Double]): Either[ContinuousDistribution, DiscreteDistribution] = {
      pdf match {
        case BetaDistribution => wrongArguments(pdf, 2)
          Left(new BetaDistribution(params(0), params(1)))
        case BinomialDistribution => wrongArguments(pdf, 2)
          Right(new BinomialDistribution(params(0).toInt, params(1)))
        case CauchyDistribution => wrongArguments(pdf, 2)
          Left(new CauchyDistribution(params(0), params(1)))
        case ChiSquaredDistribution => wrongArguments(pdf, 1)
          Left(new ChiSquaredDistribution(params(0)))
        case ConstantContinuousDistribution => wrongArguments(pdf, 1)
          Left(new ConstantContinuousDistribution(params(0)))
        case ExponentialDistribution => wrongArguments(pdf, 1)
          Left(new ExponentialDistribution(params(0)))
        case FDistribution => wrongArguments(pdf, 2)
          Left(new FDistribution(params(0), params(1)))
        case GammaDistribution => wrongArguments(pdf, 2)
          Left(new GammaDistribution(params(0), params(1)))
        case GeometricDistribution => wrongArguments(pdf, 1)
          Right(new GeometricDistribution(params(0)))
        case GumbelDistribution => wrongArguments(pdf, 2)
          Left(new GumbelDistribution(params(0), params(1)))
        case HypergeometricDistribution => wrongArguments(pdf, 3)
          Right(new HypergeometricDistribution(params(0).asInstanceOf[Int], params(1).asInstanceOf[Int], params(2).asInstanceOf[Int]))
        case LaplaceDistribution => wrongArguments(pdf, 2)
          Left(new LaplaceDistribution(params(0), params(1)))
        case LevyDistribution => wrongArguments(pdf, 2)
          Left(new LevyDistribution(params(0), params(1)))
        case LogNormalDistribution => wrongArguments(pdf, 2)
          Left(new LogNormalDistribution(params(0), params(1)))
        case LogisticDistribution => wrongArguments(pdf, 2)
          Left(new LogisticDistribution(params(0), params(1)))
        case NakagamiDistribution => wrongArguments(pdf, 2)
          Left(new NakagamiDistribution(params(0), params(1)))
        case NormalDistribution => wrongArguments(pdf, 2)
          Left(new NormalDistribution(params(0), params(1)))
        case ParetoDistribution => wrongArguments(pdf, 2)
          Left(new ParetoDistribution(params(0), params(1)))
        case PascalDistribution => wrongArguments(pdf, 2)
          Right(new PascalDistribution(params(0).asInstanceOf[Int], params(1)))
        case PoissonDistribution => wrongArguments(pdf, 1)
          Right(new PoissonDistribution(params(0)))
        case TDistribution => wrongArguments(pdf, 1)
          Left(new TDistribution(params(0)))
        case TriangularDistribution => wrongArguments(pdf, 3)
          Left(new TriangularDistribution(params(0), params(1), params(2)))
        case UniformContinuousDistribution => wrongArguments(pdf, 2)
          Left(new UniformContinuousDistribution(params(0), params(1)))
        case UniformDiscreteDistribution => wrongArguments(pdf, 2)
          Right(new UniformDiscreteDistribution(params(0).asInstanceOf[Int], params(1).asInstanceOf[Int]))
        case WeibullDistribution => wrongArguments(pdf, 2)
          Left(new WeibullDistribution(params(0), params(1)))
        case ZipfDistribution => wrongArguments(pdf, 2)
          Right(new ZipfDistribution(params(0).asInstanceOf[Int], params(1)))
        case _ => throw IllegalArgumentException(s"Distribution ${pdf.toString} does not exist")
      }
    }

    if (!pdfs.contains(pdf)) {
      pdfs += (pdf -> scala.collection.mutable.Map(MurmurHash3.orderedHash(params) -> generateSamples(createNewDistribution(pdf, params))))
      println(s"created new dist ${pdf.toString}, map contains ${pdfs.toList.mkString}")
    } else if (!pdfs(pdf).contains(paramCode)) {
      pdfs(pdf) += (MurmurHash3.orderedHash(params) -> generateSamples(createNewDistribution(pdf, params)))
      println(s"found dist ${pdf.toString}, map contains ${pdfs.toList.mkString}")
    }
    pdfs(pdf)(paramCode)
  }

  private def generateSamples(dist: Either[ContinuousDistribution, DiscreteDistribution]): Stream[Double] = {
    if (dist.isLeft) {
      val sampler = dist.left.getOrElse(throw IllegalStateException("Something went terribly wrong")).createSampler(RandomSource.create(RandomSource.JDK))
      generateSample(sampler)
    } else {
      val sampler = dist.getOrElse(throw IllegalStateException("Something went terribly wrong")).createSampler(RandomSource.create(RandomSource.JDK))
      generateSample(sampler)
    }
  }

  private def generateSample(generator: ContinuousDistribution.Sampler): Stream[Double] = {
    generator.sample() #:: generateSample(generator)
  }

  private def generateSample(generator: DiscreteDistribution.Sampler): Stream[Double] = {
    generator.sample().asInstanceOf[Double] #:: generateSample(generator)
  }
}

object AcceptReject extends App {
  val arrivals = PdfSampleGenerator(PdfSampleGenerator.PoissonDistribution, Array(10d)) match {
    case Some(streamArrivals) => streamArrivals
    case None => Stream()
  }

  val arrivals3 = PdfSampleGenerator(PdfSampleGenerator.PoissonDistribution, Array(3d)) match {
    case Some(streamArrivals) => streamArrivals
    case None => Stream()
  }

  val arrivals10 = PdfSampleGenerator(PdfSampleGenerator.PoissonDistribution, Array(10d)) match {
    case Some(streamArrivals) => streamArrivals
    case None => Stream()
  }

  println(arrivals.take(20).toList)
  println(arrivals.drop(5).take(20).toList)
  println(arrivals10.take(20).toList)

  println(arrivals3.take(20).toList)

  import org.apache.commons.rng.UniformRandomProvider
  import org.apache.commons.rng.simple.RandomSource

  println(MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)))
  println(MurmurHash3.orderedHash(Array(1.12, 2.87, 52, 187)))
  println(MurmurHash3.orderedHash(Array(1.12, 2.871, 52, 187)))

  val random = RandomSource.create(RandomSource.JDK)
  //  println(PdfSampleGenerator.exists(PdfSampleGenerator.UniformDiscreteDistribution))
  val bd = new BetaDistribution(81d, 211d)
  val res = bd.density(0.27)
  val res1 = bd.createSampler(random)
  val s1 = res1.sample()
  val s2 = res1.sample()
  val s3 = res1.sample()
  val ud = new UniformContinuousDistribution(0, 1)
  val pud = ud.inverseCumulativeProbability(0.3)
  val nd = new NormalDistribution(100, 10)
  val rsampler = nd.createSampler(random)
  val ns1 = rsampler.sample()
  val ns2 = rsampler.sample()
  val ns3 = rsampler.sample()
  val pd = new PoissonDistribution(10)
  val pdsampler = pd.createSampler(random)
  val ps1 = pdsampler.sample()
  val ps2 = pdsampler.sample()
  val ps3 = pdsampler.sample()
  val ps4 = pdsampler.sample()
  val ps5 = pdsampler.sample()
  val ipd = pd.inverseCumulativeProbability(0.2);
  val value = nd.inverseCumulativeProbability(0.9)

  val c = 3 * 4 * 0.5
  val mean: Array[Double] = Array(1, 2)
  val cov: Array[Array[Double]] = Array(Array(9, c), Array(c, 16))
  val covariance: RealMatrix = MatrixUtils.createRealMatrix(cov)

  // Create (and possibly seed) a PRNG (could use any of the CM-provided generators).
  val seed = 17399225432L; // Fixed seed means same results every time 
  val rg = RandomSource.create(RandomSource.MT, seed);

  // Create a GaussianRandomGenerator using "rg" as its source of randomness.
  val rawGenerator = new GaussianRandomGenerator(rg);

  // Create a CorrelatedRandomVectorGenerator using "rawGenerator" for the components.
  val generator = new CorrelatedRandomVectorGenerator(mean, covariance, 1.0e-12 * covariance.getNorm(), rawGenerator);

  // Use the generator to generate correlated vectors.
  val randomVector = generator.nextVector();

}
