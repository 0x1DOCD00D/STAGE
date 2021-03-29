
import org.junit.Test
import org.junit.Assert._
/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit

class Test1 extends ScalaTestWithActorTestKit {
  @Test def t1(): Unit = {
    val replyProbe = createTestProbe[SystemMessage]()
    val underTest = spawn(DefaultBehaviors())
    underTest ! StartSimulation
    replyProbe.expectMessage(StartSimulation)

    assertEquals("I was compiled by dotty :)", "I was compiled by dotty :)")
  }
}