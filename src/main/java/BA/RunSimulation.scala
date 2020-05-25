package BA

import io.gatling.app.Gatling
import io.gatling.core.config.GatlingPropertiesBuilder

object RunSimulation {
  def main(args: Array[String]) {

    println("test")

    val simClass = classOf[ThirdSimulation].getName

    val props = new GatlingPropertiesBuilder
    props.simulationClass(simClass)
    Gatling.fromMap(props.build)

  }
}
