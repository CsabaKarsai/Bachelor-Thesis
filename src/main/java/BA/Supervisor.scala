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
      context.actorOf(RoundRobinPool(4).props(Props(new Worker.Worker(Worker.workerID.getAndIncrement()))))

    override def receive: Receive = {
      case Request(id) => {
        implicit val ec: ExecutionContext = context.dispatcher
        implicit val timeout = Timeout(30 seconds)
        val timestamp = System.currentTimeMillis()
        (w1 ? SupervisorToWorker(id, timestamp)).pipeTo(sender())
      }
      case writeToFileRequest => {
        implicit val ec: ExecutionContext = context.dispatcher
        implicit val timeout = Timeout(30 seconds)
        for(i <- 0 to 3){
          (w1 ? writeToFileRequest).pipeTo(sender())
        }
      }
    }

  }


}
