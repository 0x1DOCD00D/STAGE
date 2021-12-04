/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package PDFs

import HelperUtils.ErrorWarningMessages.LogGenericMessage
import HelperUtils.Parameters.config
import HelperUtils.{CreateLogger, ObtainConfigReference}
import Translator.SlantParser
import cats.implicits.*
import cats.kernel.Eq
import org.apache.commons.math3.distribution.*
import org.apache.commons.math3.linear.{MatrixUtils, RealMatrix}
import org.apache.commons.math3.random.*
import org.apache.commons.rng.UniformRandomProvider
import org.apache.commons.rng.simple.RandomSource
import org.slf4j.Logger

import scala.util.hashing.MurmurHash3
import scala.util.{Failure, Success, Try}

type DistributionType = AbstractIntegerDistribution | AbstractRealDistribution
given logger: Logger = CreateLogger(classOf[PdfStreamGenerator.type])

object PdfStreamGenerator extends RandomGeneratorType:
  val strEnumIntDistribution = "EnumIntDistribution"
  private val suffixDistribution = "istribution"
  private val strBetaDistribution = (BetaD.toString() + suffixDistribution).toUpperCase
  private val strBinomialDistribution = (BinomialD.toString() + suffixDistribution).toUpperCase
  private val strChiSquaredDistribution = (ChiSquaredD.toString() + suffixDistribution).toUpperCase
  private val strCauchyDistribution = (CauchyD.toString() + suffixDistribution).toUpperCase
  private val strExponentialDistribution = (ExponentialD.toString() + suffixDistribution).toUpperCase
  private val strFDistribution = (FD.toString() + suffixDistribution).toUpperCase
  private val strGammaDistribution = (GammaD.toString() + suffixDistribution).toUpperCase
  private val strGeometricDistribution = (GeometricD.toString() + suffixDistribution).toUpperCase
  private val strGumbelDistribution = (GumbelD.toString() + suffixDistribution).toUpperCase
  private val strHypergeometricDistribution = (HypergeometricD.toString() + suffixDistribution).toUpperCase
  private val strLaplaceDistribution = (LaplaceD.toString() + suffixDistribution).toUpperCase
  private val strLevyDistribution = (LevyD.toString() + suffixDistribution).toUpperCase
  private val strLogNormalDistribution = (LogNormalD.toString() + suffixDistribution).toUpperCase
  private val strLogisticDistribution = (LogisticD.toString() + suffixDistribution).toUpperCase
  private val strNakagamiDistribution = (NakagamiD.toString() + suffixDistribution).toUpperCase
  private val strNormalDistribution = (NormalD.toString() + suffixDistribution).toUpperCase
  private val strParetoDistribution = (ParetoD.toString() + suffixDistribution).toUpperCase
  private val strPascalDistribution = (PascalD.toString() + suffixDistribution).toUpperCase
  private val strPoissonDistribution = (PoissonD.toString() + suffixDistribution).toUpperCase
  private val strTDistribution = (TD.toString() + suffixDistribution).toUpperCase
  private val strTriangularDistribution = (TriangularD.toString() + suffixDistribution).toUpperCase
  private val strUniformRealDistribution = (UniformRealD.toString() + suffixDistribution).toUpperCase
  private val strUniformIntegerDistribution = (UniformIntegerD.toString() + suffixDistribution).toUpperCase
  private val strWeibullDistribution = (WeibullD.toString() + suffixDistribution).toUpperCase
  private val strZipfDistribution = (ZipfD.toString() + suffixDistribution).toUpperCase

  //check if a distribution object exists for specific parameters - a cache for pdfs
  private[this] val pdfs: scala.collection.mutable.Map[String, scala.collection.mutable.Map[Int, LazyList[Double]]] = scala.collection.mutable.Map()
  private val config = ObtainConfigReference("Stage.Distributions") match {
    case Some(value) => value
    case None => throw new RuntimeException("Cannot obtain a reference to the config data.")
  }
  val logger: Logger = CreateLogger(classOf[PdfStreamGenerator.type])

  /*
  * This is the main interface for creating and managing distributions.
  * */
  def apply(distributionName: String, useCache: Boolean, params: Double*): LazyList[Double] = apply(distributionName, useCache, None, params)

  def apply(useCache: Boolean, seed: Option[Long], params: Seq[(Int, Double)]): LazyList[Double] =
    if useCache then
      Try(pdfs(strEnumIntDistribution.toUpperCase)(MurmurHash3.orderedHash(params))) match {
        case Success(lzDataStream) => lzDataStream
        case Failure(_) => createDistDataStream(useCache, seed, params.toList)
      }
    else
      createDistDataStream(useCache, seed, params.toList)

  def apply(distributionName: String, useCache: Boolean, seed: Option[Long], params: Seq[Double]): LazyList[Double] =
    if params.length != config.getInt(s"Stage.Distributions.${distributionName.toLowerCase}") then
      throw IllegalArgumentException(s"Wrong number of arguments for distribution $distributionName")

    if useCache then
      Try(pdfs(distributionName.toUpperCase).get(MurmurHash3.orderedHash(params))) match {
        case Success(lzDataStream) => lzDataStream.get
        case Failure(_) => createDistDataStream(distributionName, useCache, seed, params.toArray)
      }
    else
      createDistDataStream(distributionName, useCache, seed, params.toArray)

  private def generateSamples(generator: DistributionType): LazyList[Double] =
    generator match {
      case v: AbstractIntegerDistribution => v.sample() #:: generateSamples(generator)
      case v: AbstractRealDistribution => v.sample() #:: generateSamples(generator)
    }
  end generateSamples

  private def createDistDataStream(add2Cache: Boolean, seed: Option[Long], params: List[(Int,Double)]): LazyList[Double] =
    val objectIds2Probs = params.unzip
    val rg = createRandomGenerator(seed)
    val distributionObject = new EnumeratedIntegerDistribution(rg, objectIds2Probs._1.toArray, objectIds2Probs._2.toArray)

    val dstream = generateSamples(distributionObject)
    if add2Cache then pdfs += (strEnumIntDistribution.toUpperCase() -> collection.mutable.Map(MurmurHash3.orderedHash(params) -> dstream))

    dstream
  end createDistDataStream


  private def createDistDataStream(distributionName: String, add2Cache: Boolean, seed: Option[Long], params: Array[Double]): LazyList[Double] =
    val distributionObject = distributionName.toUpperCase match {
      case `strBetaDistribution` => BetaD(params).create(seed)
      case `strBinomialDistribution` => BinomialD(params).create(seed)
      case `strChiSquaredDistribution` => ChiSquaredD(params).create(seed)
      case `strCauchyDistribution` => CauchyD(params).create(seed)
      case `strExponentialDistribution` => ExponentialD(params).create(seed)
      case `strFDistribution` => FD(params).create(seed)
      case `strGammaDistribution` => GammaD(params).create(seed)
      case `strGeometricDistribution` => GeometricD(params).create(seed)
      case `strGumbelDistribution` => GumbelD(params).create(seed)
      case `strHypergeometricDistribution` => HypergeometricD(params).create(seed)
      case `strLaplaceDistribution` => LaplaceD(params).create(seed)
      case `strLevyDistribution` => LevyD(params).create(seed)
      case `strLogNormalDistribution` => LogNormalD(params).create(seed)
      case `strLogisticDistribution` => LogisticD(params).create(seed)
      case `strNakagamiDistribution` => NakagamiD(params).create(seed)
      case `strNormalDistribution` => NormalD(params).create(seed)
      case `strParetoDistribution` => ParetoD(params).create(seed)
      case `strPascalDistribution` => PascalD(params).create(seed)
      case `strPoissonDistribution` => PoissonD(params).create(seed)
      case `strTDistribution` => TD(params).create(seed)
      case `strTriangularDistribution` => TriangularD(params).create(seed)
      case `strUniformRealDistribution` => UniformRealD(params).create(seed)
      case `strUniformIntegerDistribution` => UniformIntegerD(params).create(seed)
      case `strWeibullDistribution` => WeibullD(params).create(seed)
      case `strZipfDistribution` => ZipfD(params).create(seed)
      case _ => throw new IllegalArgumentException(s"Incorrect distribution is specified: $distributionName")
    }
    val dstream = generateSamples(distributionObject)
    if add2Cache then pdfs += (distributionName.toUpperCase() -> collection.mutable.Map(MurmurHash3.orderedHash(params) -> dstream))
    dstream
  end createDistDataStream

enum PdfStreamGenerator(params: Array[Double]) extends RandomGeneratorType:
  case BetaD(params: Array[Double]) extends PdfStreamGenerator(params)
  case BinomialD(params: Array[Double]) extends PdfStreamGenerator(params)
  case CauchyD(params: Array[Double]) extends PdfStreamGenerator(params)
  case ChiSquaredD(params: Array[Double]) extends PdfStreamGenerator(params)
  case ExponentialD(params: Array[Double]) extends PdfStreamGenerator(params)
  case FD(params: Array[Double]) extends PdfStreamGenerator(params)
  case GammaD(params: Array[Double]) extends PdfStreamGenerator(params)
  case GeometricD(params: Array[Double]) extends PdfStreamGenerator(params)
  case GumbelD(params: Array[Double]) extends PdfStreamGenerator(params)
  case HypergeometricD(params: Array[Double]) extends PdfStreamGenerator(params)
  case LaplaceD(params: Array[Double]) extends PdfStreamGenerator(params)
  case LevyD(params: Array[Double]) extends PdfStreamGenerator(params)
  case LogNormalD(params: Array[Double]) extends PdfStreamGenerator(params)
  case LogisticD(params: Array[Double]) extends PdfStreamGenerator(params)
  case NakagamiD(params: Array[Double]) extends PdfStreamGenerator(params)
  case NormalD(params: Array[Double]) extends PdfStreamGenerator(params)
  case ParetoD(params: Array[Double]) extends PdfStreamGenerator(params)
  case PascalD(params: Array[Double]) extends PdfStreamGenerator(params)
  case PoissonD(params: Array[Double]) extends PdfStreamGenerator(params)
  case TD(params: Array[Double]) extends PdfStreamGenerator(params)
  case TriangularD(params: Array[Double]) extends PdfStreamGenerator(params)
  case UniformRealD(params: Array[Double]) extends PdfStreamGenerator(params)
  case UniformIntegerD(params: Array[Double]) extends PdfStreamGenerator(params)
  case WeibullD(params: Array[Double]) extends PdfStreamGenerator(params)
  case ZipfD(params: Array[Double]) extends PdfStreamGenerator(params)

  def create(seed: Option[Long]): DistributionType =
    val rg = createRandomGenerator(seed)
    this match {
      case BetaD(params: Array[Double]) => new BetaDistribution(rg, params(0), params(1))
      case BinomialD(params: Array[Double]) => new BinomialDistribution(rg, params(0).toInt, params(1))
      case ChiSquaredD(params: Array[Double]) => new ChiSquaredDistribution(rg, params(0))
      case CauchyD(params: Array[Double]) => new CauchyDistribution(rg, params(0), params(1))
      case ExponentialD(params: Array[Double]) => new ExponentialDistribution(rg, params(0))
      case FD(params: Array[Double]) => new FDistribution(rg, params(0), params(1))
      case GammaD(params: Array[Double]) => new GammaDistribution(rg, params(0), params(1))
      case GeometricD(params: Array[Double]) => new GeometricDistribution(rg, params(0))
      case GumbelD(params: Array[Double]) => new GumbelDistribution(rg, params(0), params(1))
      case HypergeometricD(params: Array[Double]) => new HypergeometricDistribution(rg, params(0).toInt, params(1).toInt, params(2).toInt)
      case LaplaceD(params: Array[Double]) => new LaplaceDistribution(rg, params(0), params(1))
      case LevyD(params: Array[Double]) => new LevyDistribution(rg, params(0), params(1))
      case LogNormalD(params: Array[Double]) => new LogNormalDistribution(rg, params(0), params(1))
      case LogisticD(params: Array[Double]) => new LogisticDistribution(rg, params(0), params(1))
      case NakagamiD(params: Array[Double]) => new NakagamiDistribution(rg, params(0), params(1), NakagamiDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
      case NormalD(params: Array[Double]) => new NormalDistribution(rg, params(0), params(1))
      case ParetoD(params: Array[Double]) => new ParetoDistribution(rg, params(0), params(1))
      case PascalD(params: Array[Double]) => new PascalDistribution(rg, params(0).toInt, params(1))
      case PoissonD(params: Array[Double]) => new PoissonDistribution(rg, params(0), PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
      case TD(params: Array[Double]) => new TDistribution(rg, params(0))
      case TriangularD(params: Array[Double]) => new TriangularDistribution(rg, params(0), params(1), params(2))
      case UniformRealD(params: Array[Double]) => new UniformRealDistribution(rg, params(0), params(1))
      case UniformIntegerD(params: Array[Double]) => new UniformIntegerDistribution(rg, params(0).toInt, params(1).toInt)
      case WeibullD(params: Array[Double]) => new WeibullDistribution(rg, params(0), params(1))
      case ZipfD(params: Array[Double]) => new ZipfDistribution(rg, params(0).toInt, params(1))
    }
