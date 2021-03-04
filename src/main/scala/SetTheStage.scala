import akka.actor.typed.{ActorSystem, Behavior}
import com.typesafe.config.ConfigFactory

/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *   
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *  
 */

object SetTheStage {
  def main(args: Array[String]): Unit = {
    val conf = ConfigFactory.load()
    println("The answer is: " + conf.getString("simple-app.answer"))
    val theRootBehavior: Behavior[SystemMessage] = DefaultBehaviors()
    val stageSystem = ActorSystem(theRootBehavior, "TheStageForActors")
    stageSystem ! StartSimulation
    Thread.sleep(2000)
    stageSystem ! StartSimulation
    Thread.sleep(2000)
    stageSystem ! StartSimulation
    Thread.sleep(2000)
    stageSystem ! StopSimulation
    //    Thread.sleep(20000)
    println("All the world’s a stage")
  }
}