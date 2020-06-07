package BA

import java.io.{BufferedWriter, File, FileWriter}

import akka.actor.{Actor, Props}

import scala.math._
import scala.util.Random

object Worker {

  def Worker: Props = {
    Props(new Worker)
  }

  class Worker extends Actor {

    val file = new File("./results/SupervisorToWorker.txt")
    val fw = new FileWriter(file, true)
    val bw = new BufferedWriter(fw)
    var counter = 0
    var timestampList = new Array[Long](1000)

    override def receive: Receive = {

      case SupervisorToWorker(id, timestamp) => {

        if (counter == (timestampList.size - 1)){
          for (i <- 0 to counter){
            bw.write("" + timestampList(i) + "\n")
          }
          bw.close()
          counter = 0
        }else{
          timestampList(counter) = System.currentTimeMillis() - timestamp
          counter = counter + 1
        }
        var s = System.currentTimeMillis()
        var break = 0
        while(break == 0){
          if(System.currentTimeMillis() - s > 1000){
            break = 1
          }
        }
        sender() ! Response(id)

      }
      case writeToFileRequest => {

        for (i <- 0 to (counter - 1)){
          bw.write("" + i + " " + timestampList(i) + "\n")
        }
        bw.close()
        counter = 0
        sender() ! writeToFileResponse

      }
    }

    def negExNumber(lambda : Double) : Double = {
      val random = new Random(System.currentTimeMillis())
      val randomNumber = random.nextDouble()
      log(1 - randomNumber)  / (-lambda)
    }
  }

}
