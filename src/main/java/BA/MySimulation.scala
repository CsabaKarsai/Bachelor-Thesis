package BA

import _root_.io.gatling.core.Predef._
import akka.actor.{ActorRef, ActorSystem}
import com.chatwork.gatling.akka.Predef._
import com.chatwork.gatling.akka.config.AkkaProtocol
import com.typesafe.config.{Config, ConfigFactory}
import io.gatling.core.action.Action
import io.gatling.core.action.builder.ActionBuilder
import io.gatling.core.session.Session
import io.gatling.core.structure.{ScenarioBuilder, ScenarioContext}

class MySimulation extends Simulation {

  val config: Config = ConfigFactory.load()
  implicit val system: ActorSystem = ActorSystem("MySimulation", config)

  // gatling-akka protocol configuration
  val akkaConfig: AkkaProtocol = akkaActor.askTimeout(60 * 46)

  // recipient actorRef
  val actorUnderTest: ActorRef = system.actorOf(Supervisor.MySupervisor)

  // scenario definition
  val default_Scenario: ScenarioBuilder = scenario("default_Request-default_Response")
    .exec {
      new ActionBuilder {
        override def build(ctx: ScenarioContext, next: Action): Action = {
          new Action {
            override def name: String = "test"

            override def execute(session: Session): Unit = {
              val a = akkaActor("default_Request").to(actorUnderTest) ? Request(session.userId, "default") check expectMsg(Response(session.userId, "default")).saveAs("default_Response")
              a.build(ctx, next) ! session
            }
          }
        }
      }
    }
  val SAI_Scenario: ScenarioBuilder = scenario("SAI_Request-SAI_Response")
    .exec {
      new ActionBuilder {
        override def build(ctx: ScenarioContext, next: Action): Action = {
          new Action {
            override def name: String = "test"

            override def execute(session: Session): Unit = {
              val a = akkaActor("SAI_Request").to(actorUnderTest) ? Request(session.userId, "SAI") check expectMsg(Response(session.userId, "SAI")).saveAs("SAI_Response")
              a.build(ctx, next) ! session
            }
          }
        }
      }
    }
  val UL_Scenario: ScenarioBuilder = scenario("UL_Request-UL_Response")
    .exec {
      new ActionBuilder {
        override def build(ctx: ScenarioContext, next: Action): Action = {
          new Action {
            override def name: String = "test"

            override def execute(session: Session): Unit = {
              val a = akkaActor("UL_Request").to(actorUnderTest) ? Request(session.userId, "UL") check expectMsg(Response(session.userId, "UL")).saveAs("UL_Response")
              a.build(ctx, next) ! session
            }
          }
        }
      }
    }
  val UL_GPRS_Scenario: ScenarioBuilder = scenario("UL_GPRS_Request-UL_GPRS_Response")
    .exec {
      new ActionBuilder {
        override def build(ctx: ScenarioContext, next: Action): Action = {
          new Action {
            override def name: String = "test"

            override def execute(session: Session): Unit = {
              val a = akkaActor("UL_GPRS_Request").to(actorUnderTest) ? Request(session.userId, "UL_GPRS") check expectMsg(Response(session.userId, "UL_GPRS")).saveAs("UL_GPRS_Response")
              a.build(ctx, next) ! session
            }
          }
        }
      }
    }
  val CL_Scenario: ScenarioBuilder = scenario("CL_Request-CL_Response")
    .exec {
      new ActionBuilder {
        override def build(ctx: ScenarioContext, next: Action): Action = {
          new Action {
            override def name: String = "test"

            override def execute(session: Session): Unit = {
              val a = akkaActor("CL_Request").to(actorUnderTest) ? Request(session.userId, "CL") check expectMsg(Response(session.userId, "CL")).saveAs("CL_Response")
              a.build(ctx, next) ! session
            }
          }
        }
      }
    }

  val write_Scenario: ScenarioBuilder = scenario("writeToFile")
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
    /*
    default_Scenario.inject(
      constantUsersPerSec(1)during 60 * 10 randomized
    ),
    */
    SAI_Scenario.inject(
      nothingFor(60 * 1),
      rampUsersPerSec(0)to(1)during(60 * 1),
      constantUsersPerSec(1)during 60 * 26 randomized,
      rampUsersPerSec(1)to(0)during(60 * 1),
      nothingFor(60 * 1)
    ),
    UL_Scenario.inject(
      nothingFor(60 * 1),
      rampUsersPerSec(0)to(3)during(60 * 1),
      constantUsersPerSec(3)during 60 * 26 randomized,
      rampUsersPerSec(3)to(0)during(60 * 1),
      nothingFor(60 * 1)
    ),
    UL_GPRS_Scenario.inject(
      nothingFor(60 * 1),
      rampUsersPerSec(0)to(4)during(60 * 1),
      constantUsersPerSec(4)during 60 * 26 randomized,
      rampUsersPerSec(4)to(0)during(60 * 1),
      nothingFor(60 * 1)
    ),
    CL_Scenario.inject(
      nothingFor(60 * 1),
      rampUsersPerSec(0)to(5)during(60 * 1),
      constantUsersPerSec(5)during 60 * 26 randomized,
      rampUsersPerSec(5)to(0)during(60 * 1),
      nothingFor(60 * 1)
    ),
    write_Scenario.inject(
      nothingFor(60 * 45),
      constantUsersPerSec(1)during 1
    )
  ).protocols(akkaConfig).maxDuration(60 * 46)

}
