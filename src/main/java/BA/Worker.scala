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

    //fields for SupervisorToWorker file
    val supervisorToWorkerfile = new File("./results/SupervisorToWorker.txt")
    val supervisorToWorkerFw = new FileWriter(supervisorToWorkerfile, true)
    val supervisorToWorkerBw = new BufferedWriter(supervisorToWorkerFw)
    var supervisorToWorkerCounter = 0
    var supervisorToWorkerTimestampList = new Array[String](1000)

    override def receive: Receive = {

      case SupervisorToWorker(id, timestamp) => {

        val messageArriveTime = System.currentTimeMillis()

        if (supervisorToWorkerCounter == (supervisorToWorkerTimestampList.size - 1)){
          //write array to file
          for (i <- 0 to supervisorToWorkerCounter){
            supervisorToWorkerBw.write(supervisorToWorkerTimestampList(i) + "\n")
          }
          supervisorToWorkerBw.close()
          //handle lost request
          supervisorToWorkerCounter = 0
          supervisorToWorkerTimestampList(supervisorToWorkerCounter) = simulateWorkAndCalcLine(id, timestamp, messageArriveTime)
          supervisorToWorkerCounter = supervisorToWorkerCounter + 1
          sender() ! Response(id)
        }
        else{
          supervisorToWorkerTimestampList(supervisorToWorkerCounter) = simulateWorkAndCalcLine(id, timestamp, messageArriveTime)
          supervisorToWorkerCounter = supervisorToWorkerCounter + 1
          sender() ! Response(id)
        }

      }
      case writeToFileRequest => {

        for (i <- 0 to (supervisorToWorkerCounter - 1)){
          supervisorToWorkerBw.write(supervisorToWorkerTimestampList(i) + "\n")
        }
        supervisorToWorkerBw.close()
        supervisorToWorkerCounter = 0
        sender() ! writeToFileResponse

      }
    }

    def getNegExNumber(lambda : Double) : Double = {
      val random = new Random(System.currentTimeMillis())
      val randomNumber = random.nextDouble()
      log(1 - randomNumber)  / (-lambda)
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
  }

}
