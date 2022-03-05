/*
 * Copyright (c) 2022. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 *  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *
 *  See the License for the specific language governing permissions and limitations under the License.
 */

import akka.actor.{ActorSystem, Props}

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object SpawnActor {
  val toolbox:  ToolBox[scala.reflect.runtime.universe.type] = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
  val code:String =
    """
      |import akka.actor._
      |class HelloActor extends Actor {
      |override def receive: Receive = {
      |case "changeX" => println("Received a message inside the actor...")
      |case None => println("None received!")
      |}
      |}
      |object HelloActor {
      |def props() : Props = Props(new HelloActor())
      | }
      |return HelloActor.props()
      |""".stripMargin

  def apply(): Unit = {
    val tree = toolbox.parse(code)
    val actorProps =  toolbox.compile(tree)().asInstanceOf[Props]
    val actorSystem = ActorSystem("system")
    (1 to 20000).foreach(_ =>{
      val helloActor = actorSystem.actorOf(actorProps)
      helloActor ! "changeX"
      Thread.sleep( 100)
    })
    actorSystem.terminate()
  }

}
