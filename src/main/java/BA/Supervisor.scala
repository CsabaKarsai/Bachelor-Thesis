package BA

import java.io.{BufferedWriter, File, FileWriter}

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

    val file = new File("./results/Supervisor.txt")
    val fw = new FileWriter(file, true)
    val bw = new BufferedWriter(fw)
    var counter = 0
    var data = new Array[String](2)

    override def receive: Receive = {

      case Request(id) => {

        val messageArriveTime = System.currentTimeMillis()
        if (counter == (data.size)){
          for (i <- 0 to (counter - 1)){
            bw.write(data(i) + "\n")
          }
          counter = 0
        }
        implicit val ec: ExecutionContext = context.dispatcher
        implicit val timeout = Timeout(30 seconds)
        val timestamp = System.currentTimeMillis()
        (w1 ? SupervisorToWorker(id, timestamp)).pipeTo(sender())
        data(counter) = "Request id: " + id + " Bearbeitungszeit: " + (System.currentTimeMillis() - messageArriveTime)
        counter = counter + 1

      }
      case writeToFileRequest => {

        for (i <- 0 to (counter - 1)){
          bw.write(data(i) + "\n")
        }
        bw.close()
        counter = 0
        implicit val ec: ExecutionContext = context.dispatcher
        implicit val timeout = Timeout(30 seconds)
        for(i <- 0 to 3){
          (w1 ? writeToFileRequest).pipeTo(sender())
        }

      }

    }

  }


}
