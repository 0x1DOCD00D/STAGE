import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.{Logging, LoggingAdapter}

import scala.reflect.runtime.currentMirror
/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

import scala.tools.reflect.ToolBox

object Main {
  var actor: ActorRef = _
  implicit val actorSystem = ActorSystem("STAGEAS")

  //we should generate this case class
  trait Msg

  var msg: Msg = _

  abstract class ActorNode extends Actor

  val code =
    """
      |    import Main._
      |    import akka.actor.{Actor, ActorRef, ActorSystem, Props}
      |    import akka.event.{Logging, LoggingAdapter}
      |    case class A(data:String) extends Msg
      |    case class B(data:String) extends Msg
      |    actor = actorSystem.actorOf(Props(new ActorNode() {
      |      val logger: LoggingAdapter = Logging(context.system, this)
      |
      |      override def receive: Receive = {
      |        case msg: A => logger.info(msg.data)
      |        case _ => logger.error("Oy Wey!!")
      |      }
      |    }), "1")
      |    msg = A("Howdy, Comrade!")
     """.stripMargin

  def main(args: Array[String]): Unit = {
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(code)
    val compiledCode = toolbox.compile(tree)

    def sendMessage: Any = compiledCode().asInstanceOf[Unit]

    sendMessage
    actor ! msg
    actorSystem.terminate()
  }
}
