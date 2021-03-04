import akka.actor.PoisonPill
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *   
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *  
 */
object DefaultBehaviors {
  def apply(): Behavior[SystemMessage] = {
    defaultBehavior(None)
  }

  def defaultBehavior(actor: Option[ActorRef[SystemMessage]]): Behavior[SystemMessage] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage {
        null
        /*
      case StartSimulation => actor match {
          case Some(ar) => context.log.info(s"Actor $ar already created")
          case None =>context.log.info("Starting the simulation and a new actor is spawned")
            actor = Some(context.spawn(defaultBehavior, "WhoKnows"))
        }
        Behavior.start(defaultBehavior,context)
      case StopSimulation => actor match {
        case Some(ar) => context.log.info(s"Stopping actor $ar")
          context.stop(ar)
          actor = None
        case None =>context.log.info("Terminated")
          context
      }
        Behavior.start(defaultBehavior,context)


      case _ => Behavior.start(defaultBehavior,context)
*/
      }
  }
}