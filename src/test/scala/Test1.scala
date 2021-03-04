
import org.junit.Test
import org.junit.Assert._
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit

class Test1 extends ScalaTestWithActorTestKit {
  @Test def t1(): Unit = {
    val replyProbe = createTestProbe[SystemMessage]()
    val underTest = spawn(DefaultBehaviors())
    underTest ! StartSimulation
    replyProbe.expectMessage(StartSimulation)

    assertEquals("I was compiled by dotty :)", Main.msg)
  }
}