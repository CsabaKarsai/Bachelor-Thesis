package BA

import java.io.{BufferedWriter, File, FileWriter}

import akka.actor.{Actor, Props}

import scala.math._
import scala.util.Random

import java.util.concurrent.atomic.AtomicLong

object Worker {

  val workerID = new AtomicLong(1)

  //make new parameter for worker -> appen to file + parameter
  class Worker(workerID: Long) extends Actor {

    //set ID of this particular worker to the one given in constructor
    var myWorkerID = workerID
    println("myWorkerID: " + myWorkerID + " time: " + System.currentTimeMillis())

    //fields to make write on receive possible
    val file = new File("./results/Worker" + myWorkerID + ".txt")
    val fw = new FileWriter(file, true)
    val bw = new BufferedWriter(fw)
    var counter = 0
    var data = new Array[String](1000)

    override def receive: Receive = {

      case SupervisorToWorker(id, timestamp) => {

        val messageArriveTime = System.currentTimeMillis()
        if (counter == (data.size)){
          for (i <- 0 to (counter - 1)){
            bw.write(data(i) + "\n")
          }
          counter = 0
        }
        sender() ! Response(id)
        data(counter) = simulateWorkAndCalcLine(id, timestamp, messageArriveTime)
        counter = counter + 1

      }
      case writeToFileRequest => {

        for (i <- 0 to (counter - 1)){
          bw.write(data(i) + "\n")
        }
        bw.close()
        counter = 0
        sender() ! writeToFileResponse

      }
    }

    def simulateWorkAndCalcLine(id: Long, timestamp: Long, messageArriveTime: Long) : String = {
      var firstHalfToWrite = ("Request id: " + (id)
        + " Wartezeit vor Bearbeitung: " + (System.currentTimeMillis() - timestamp))
      //später durch negEx ersetzen
      workFor(1000)
      var secondHalfToWrite = " Bearbeitungszeit :" + (System.currentTimeMillis() - messageArriveTime)
      var stringToWrite = firstHalfToWrite + secondHalfToWrite
      stringToWrite
    }

    def workFor(timeInMillis: Long) : Any = {
      var timerStartTime = System.currentTimeMillis()
      var break = 0
      while(break == 0){
        if(System.currentTimeMillis() - timerStartTime > timeInMillis){
          break = 1
        }
      }
    }

    def getNegExNumber(lambda : Double) : Double = {
      val random = new Random(System.currentTimeMillis())
      val randomNumber = random.nextDouble()
      log(1 - randomNumber)  / (-lambda)
    }

  }

}
