import org.apache.commons.math4.random.{CorrelatedRandomVectorGenerator, GaussianRandomGenerator}
import org.apache.commons.rng.simple.RandomSource
import org.apache.commons.statistics.distribution.{NormalDistribution, UniformContinuousDistribution}
import org.objectweb.asm.{ClassReader, ClassWriter}
import scalaz.Scalaz.{ToOrderOps, doubleInstance}

/*
 *   Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under 
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 */

import com.github.nscala_time.time.Imports._
import org.scalactic._
import TripleEquals._
import scalaz._
import std.option._, std.list._
import syntax.bind._
import org.apache.commons.rng.UniformRandomProvider;
import monocle.syntax.all._

object Main {

  import org.apache.commons.math4.linear.MatrixUtils
  import org.apache.commons.math4.linear.RealMatrix

  case class User(name: String, address: Address)

  case class Address(streetNumber: Int, streetName: String)

  val user = User("Anna", Address(12, "high street"))

  //  user.focus(_.name).replace("Bob")
  //  // res: User = User("Bob", Address(12, "high street"))
  //
  //  user.focus(_.address.streetName).modify(_.toUpperCase)
  //  // res: User = User("Anna", Address(12, "HIGH STREET"))
  //
  //  user.focus(_.address.streetNumber).get
  // res: Int = 12

  val ud = new UniformContinuousDistribution(0, 1)
  val pud = ud.inverseCumulativeProbability(0.3)
  val nd = new NormalDistribution(100, 10)
  val value = nd.inverseCumulativeProbability(0.9)

  val ordering = 2.0 ?|? 3.0

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

  val res1 = Apply[Option].apply2(some(1), some(2))((a, b) => a + b)

  val res2 = Traverse[List].traverse(List(1, 2, 3))(i => some(i))

  val lst2 = List(true, false).ifM(List(0, 1), List(2, 3))


  println(Array(1, 2, 3) === Array(1, 2, 3))

  println(DateTime.now() + 2.months) // returns org.joda.time.DateTime = 2009-06-27T13:25:59.195-07:00

  DateTime.nextMonth < DateTime.now() + 2.months // returns Boolean = true

  DateTime.now() to DateTime.tomorrow // return org.joda.time.Interval = > 2009-04-27T13:47:14.840/2009-04-28T13:47:14.840

  (DateTime.now() to DateTime.nextSecond).millis // returns Long = 1000

  2.hours + 45.minutes + 10.seconds
  // returns com.github.nscala_time.time.DurationBuilder
  // (can be used as a Duration or as a Period)

  (2.hours + 45.minutes + 10.seconds).millis
  // returns Long = 9910000

  2.months + 3.days
  // returns Period
  DateTime.now() // returns org.joda.time.DateTime = 2009-04-27T13:25:42.659-07:00

  DateTime.now().hour(2).minute(45).second(10)

  val className = "java.lang.Integer"
  val cloneableInterface = "java/lang/Cloneable"
  var reader = new ClassReader(className)
  var writer = new ClassWriter(reader, 0)

  type ScopeTable =[T, S] =>> Map[T, S]

  case class FiveDimTable[A, B, C, D, E](val a: A, val b: B, val c: C, val d: D, val e: E)

  trait TableManipulator[T[_]] {
    def behavior[P](container: T[P]): T[P]
  }

  class SomeConcreteManipulator[A, B, C, D] extends TableManipulator[[E] =>> FiveDimTable[A, B, C, D, E]] {
    override def behavior[P](container: FiveDimTable[A, B, C, D, P]): FiveDimTable[A, B, C, D, P] = FiveDimTable(a = container.a, b = container.b, c = container.c, d = container.d, e = container.e)
  }

  val p = new SomeConcreteManipulator[Int, String, Float, Int => String]().behavior(FiveDimTable(1, "mark", 3.14, (x: Int) => x.toString, List()))
  println(p)

  lazy val unit = {
    val x = 3
    val y: 3 = 3

    def f(i: 3) = i

    f(y)
  }

  def f(i: => 3): Int = {
    println("in f")
    i
  }


  def main(args: Array[String]): Unit = {
    val tb: ScopeTable[String, Int] = Map("s" -> 2)
    println(f(3))
    println(msg)
  }

  def msg = "I was compiled by dotty :)"

}
