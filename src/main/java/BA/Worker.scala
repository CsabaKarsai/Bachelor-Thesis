package BA

import akka.actor.{Actor, Props}

import scala.math._
import scala.util.Random

object Worker {

  def Worker: Props = {
    Props(new Worker)
  }

  class Worker extends Actor {

    override def receive: Receive = {
      case Request(id) => {
        //TODO: alternative to Thread.sleep(), reason: no Messages can be accepted while asleep
        //Thread.sleep((negExNumber(2) * 1000).toLong)
        //Thread.sleep(500)
        var s = System.currentTimeMillis()
        var break = 0
        while(break == 0){
          if(System.currentTimeMillis() - s > 1000){
            break = 1
          }
        }
        sender() ! Response(id)
      }
    }

    def negExNumber(lambda : Double) : Double = {
      val random = new Random(System.currentTimeMillis())
      val randomNumber = random.nextDouble()
      log(1 - randomNumber)  / (-lambda)
    }
  }

}
