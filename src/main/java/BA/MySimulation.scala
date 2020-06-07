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
            val a = akkaActor("Request").to(actorUnderTest) ? Request(session.userId, "default") check expectMsg(Response(session.userId, "default")).saveAs("Response")
            a.build(ctx, next) ! session
          }
        }
      }
    }
  }
  val write = scenario("writeToFile")
      .exec{
        new ActionBuilder {
          override def build(ctx: ScenarioContext, next: Action): Action = {
            new Action {
              override def name: String = "test"

              override def execute(session: Session): Unit = {
                val a = akkaActor("writeRequest").to(actorUnderTest) ? writeToFileRequest check expectMsg(writeToFileResponse).saveAs("writeResponse")
                a.build(ctx, next) ! session
              }
            }
          }
        }
      }
  // inject configurations
  setUp(
    /*
    s.inject(
      nothingFor(10),
      rampUsersPerSec(0)to(4)during(10),
      constantUsersPerSec(4)during(10),
      rampUsersPerSec(4)to(0)during(10),
      nothingFor(10)
    )
    */
    s.inject(constantUsersPerSec(4)during(3)),
    write.inject(nothingFor(4),
      constantUsersPerSec(1)during(1)
    )
  ).protocols(akkaConfig).maxDuration(125)

}
