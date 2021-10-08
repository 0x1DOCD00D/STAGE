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
  val config = ObtainConfigReference("stage") match {
    case Some(value) => value
    case None => throw new RuntimeException("Cannot obtain a reference to the config data.")
  }
  val co = config.getObject("stage.Distributions").entrySet().toArray
  println(co.toList)
  val k = 1

  @main def runSimulation =
    logger.info("Constructing a cloud model...")
    logger.info("Finished cloud simulation...")

class Main