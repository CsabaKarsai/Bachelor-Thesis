package BA

import _root_.io.gatling.core.Predef._
import akka.actor.ActorSystem
import com.chatwork.gatling.akka.Predef._
import com.typesafe.config.ConfigFactory
import io.gatling.core.action.Action
import io.gatling.core.action.builder.ActionBuilder
import io.gatling.core.session.Session
import io.gatling.core.structure.ScenarioContext

class MySimulation extends Simulation {

  val config = ConfigFactory.load()
  implicit val system = ActorSystem("MySimulation", config)

  // gatling-akka protocol configuration
  val akkaConfig = akkaActor.askTimeout(125)

  // recipient actorRef
  val actorUnderTest = system.actorOf(Supervisor.MySupervisor)

  // scenario definition
  val s = scenario("Request-Response")
    .exec {
    new ActionBuilder {
      override def build(ctx: ScenarioContext, next: Action): Action = {
        new Action {
          override def name: String = "test"

          override def execute(session: Session): Unit = {
            val a = akkaActor("Request").to(actorUnderTest) ? Request(session.userId) check expectMsg(Response(session.userId)).saveAs("Response")
            a.build(ctx, next) ! session
          }
        }
      }
    }
  }
  // inject configurations
  setUp(
    s.inject(
      nothingFor(10),
      rampUsersPerSec(0)to(2)during(10),
      constantUsersPerSec(2)during(10),
      rampUsersPerSec(2)to(0)during(10),
      nothingFor(10)
      //rampUsersPerSec(0)to(20)during(120)
    )
  ).protocols(akkaConfig).maxDuration(125)

}
