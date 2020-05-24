package BA

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import akka.routing.RoundRobinPool
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Supervisor {

  def MySupervisor: Props = {
    Props(new mySupervisor)
  }

  class mySupervisor extends Actor {
    val w1: ActorRef =
      context.actorOf(RoundRobinPool(4).props(Props[Worker.Worker]), "w1")

    override def receive: Receive = {
      case Request(id) => {
        implicit val ec: ExecutionContext = context.dispatcher
        implicit val timeout = Timeout(30 seconds)
        (w1 ? Request(id)).pipeTo(sender())
      }
    }

  }


}
