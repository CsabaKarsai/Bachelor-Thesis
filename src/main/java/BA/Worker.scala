package BA

import java.io.{BufferedWriter, File, FileWriter}

import akka.actor.Actor

import scala.math._
import scala.util.Random
import java.util.concurrent.atomic.AtomicLong

import breeze.stats.distributions.Gamma
import breeze.stats.distributions.Exponential

object Worker {

  val workerID = new AtomicLong(1)

  //make new parameter for worker -> appen to file + parameter
  class Worker(workerID: Long) extends Actor {

    //set ID of this particular worker to the one given in constructor
    var myWorkerID: Long = workerID
    //println("myWorkerID: " + myWorkerID + " time: " + System.currentTimeMillis())

    val file = new File("./results/Worker" + myWorkerID + ".txt")
    val fw = new FileWriter(file, true)
    val bw = new BufferedWriter(fw)
    var counter = 0
    var data = new Array[String](1000)

    //la
    var processedMessages = 1

    //write header
    bw.write("id;messageArriveTime;waitingTime;processingTime;processedMessages;SupervisorSendTime;messageType" + "\n")

    override def receive: Receive = {

      case SupervisorToWorker(id, supervisorSendTime, messageType) =>

        val messageArriveTime = System.nanoTime()
        if (counter == data.length){
          for (i <- 0 until counter){
            bw.write(data(i) + "\n")
          }
          counter = 0
        }

        if(messageType.equals("default")){
          data(counter) = simulateWorkAndCalcLine(id, supervisorSendTime, messageArriveTime, messageType, 1000 * 1000, processedMessages)
        }else if(messageType.equals("SAI")){
          data(counter) = simulateWorkAndCalcLine(id, supervisorSendTime, messageArriveTime, messageType, drawSampleFromExp(594.2857143), processedMessages)
        }else if(messageType.equals("UL")){
          data(counter) = simulateWorkAndCalcLine(id, supervisorSendTime, messageArriveTime, messageType, drawSampleFromExp(80), processedMessages)
        }else if(messageType.equals("UL_GPRS")){
          data(counter) = simulateWorkAndCalcLine(id, supervisorSendTime, messageArriveTime, messageType, drawSampleFromExp(28.571428), processedMessages)
        }else if(messageType.equals("CL")){
          data(counter) = simulateWorkAndCalcLine(id, supervisorSendTime, messageArriveTime, messageType, drawSampleFromExp(62.8571425), processedMessages)
        }

        processedMessages = processedMessages + 1
        sender() ! Response(id, messageType)
        counter = counter + 1

      case writeToFileRequest =>

        for (i <- 0 until counter){
          bw.write(data(i) + "\n")
        }
        bw.close()
        counter = 0
        sender() ! writeToFileResponse

    }

    def simulateWorkAndCalcLine(id: Long, supervisorSendTime: Long, messageArriveTime: Long, messageType: String, processingTime: Long, processedMessages: Long) : String = {
      val firstHalfToWrite = ("" + id
        + ";" + (messageArriveTime / 1000)
        + ";" + (System.nanoTime() - supervisorSendTime) / 1000)
      workFor(processingTime)
      val secondHalfToWrite = ";" + ((System.nanoTime() - messageArriveTime) / 1000) +
        ";" + processedMessages +
        ";" + (supervisorSendTime / 1000) +
        ";" + messageType
      val stringToWrite = firstHalfToWrite + secondHalfToWrite
      stringToWrite
    }

    def workFor(timeInMicros: Long) : Any = {
      val timerStartTime = System.nanoTime()
      var break = 0
      while(break == 0){
        if(System.nanoTime() - timerStartTime > (timeInMicros * 1000)){
          break = 1
        }
      }
    }

    def drawSampleFromExp(lambda : Double) : Long = {
      val exp = Exponential(lambda)
      (exp.draw() * 1000000).toLong
    }

    //draw a sample from a gamme distribution with breeze
    def drawSampleFromGammaBreeze(shape: Double, rate : Double) : Long = {
      //Gamma(shape, scale) with scale = 1 / rate
      val gamma = Gamma(shape, 1 / rate)
      gamma.draw().toLong
    }

  }

}
