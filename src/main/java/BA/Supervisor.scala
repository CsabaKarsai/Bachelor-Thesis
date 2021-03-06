package BA

import java.io.{BufferedWriter, File, FileWriter}

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import akka.routing.{RandomPool, RoundRobinPool}
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Supervisor {

  def MySupervisor: Props = {
    Props(new mySupervisor)
  }

  class mySupervisor extends Actor {

    val poolSize = 4
    val supervisorTimeout: Timeout = Timeout((60 * 18) seconds)
    val w1: ActorRef =
      context.actorOf(RoundRobinPool(poolSize).props(Props(new Worker.Worker(Worker.workerID.getAndIncrement()))))

    val file = new File("./results/Supervisor.txt")
    val fw = new FileWriter(file, true)
    val bw = new BufferedWriter(fw)
    var counter = 0
    var data = new Array[String](1000)

    //write header
    bw.write("id;messageArriveTime;processingTime;messagesProcessed;messageType" + "\n")

    //processedMessages
    var processedMessages = 1

    override def receive: Receive = {

      case Request(id, messageType) =>

        val messageArriveTime = System.nanoTime()

        if (counter == data.length){
          for (i <- 0 until counter){
            bw.write(data(i) + "\n")
          }
          counter = 0
        }
        implicit val ec: ExecutionContext = context.dispatcher
        implicit val timeout: Timeout = supervisorTimeout
        val supervisorSendTime = System.nanoTime()
        (w1 ? SupervisorToWorker(id, supervisorSendTime, messageType)).pipeTo(sender())
        data(counter) = "" + id +
          ";" + (messageArriveTime/1000) +
          ";" + ((System.nanoTime() - messageArriveTime) / 1000) +
          ";" + processedMessages +
          ";" + messageType
        counter = counter + 1
        processedMessages = processedMessages + 1

      case writeToFileRequest =>

        for (i <- 0 until counter){
          bw.write(data(i) + "\n")
        }
        bw.close()
        counter = 0
        implicit val ec: ExecutionContext = context.dispatcher
        implicit val timeout: Timeout = supervisorTimeout
        for(i <- 0 until poolSize){
          (w1 ? writeToFileRequest).pipeTo(sender())
        }

    }

  }


}
