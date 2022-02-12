/*
 * Copyright (c) 2021-2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

import HelperUtils.*
import com.typesafe.scalalogging.Logger

//import org.slf4j.Logger

object Main:
//  val logger: Logger = CreateLogger(classOf[Main])
  /*
    val config = ObtainConfigReference("stage") match {
      case Some(value) => value
      case None => throw new RuntimeException("Cannot obtain a reference to the config data.")
    }
  */

  def processX(contextProcessor: Int => Int): Function1[Any, Int] =
    case v: String => contextProcessor(v.toInt)
    case v: Float => contextProcessor(v.toInt)
    case _ => 0

  def testMeth(s: String): String => Int => (Int => Int) => Int = s => i => (f: Int => Int) => f(s.toInt + i)


  type BasicType = Int
  enum Expression:
    case Var(s: String)
    case Val(v: BasicType)
    case Add(arg1: Expression, arg2: Expression)
    private var Env: Map[String, BasicType] = Map("x" -> 1, "y" -> 8)
    def eval: BasicType =
      this match {
        case Add(p1, p2) => p1.eval + p2.eval
        case Val(v) => v
        case Var(v) => Env(v)
      }

  //@main def runSimulation(): Unit =
  def main(args: Array[String]): Unit =
    val logger = Logger(Main.getClass.getName)
    logger.info("Constructing a cloud model...")
    logger.info("Finished cloud simulation...")

    import Expression.*
    val exp = Add(Add(Val(2), Var("y")), Add(Val(1),Val(9)))
    println(exp.eval)

    class ClassWithIntParam(cp: Int):
      infix def *(mp: Int): Int = mp * cp

    println(new ClassWithIntParam(3) * 5)
//println(processX(x=>x+1)(123.45f))
//    println(testMeth("10")())

class Main