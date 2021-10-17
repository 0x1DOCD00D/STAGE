/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

import HelperUtils.*

object Main:
  val logger = CreateLogger(classOf[Main])
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

  @main def runSimulation =
    logger.info("Constructing a cloud model...")
    logger.info("Finished cloud simulation...")
//println(processX(x=>x+1)(123.45f))
//    println(testMeth("10")())

class Main