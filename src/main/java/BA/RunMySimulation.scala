package BA

import io.gatling.app.Gatling
import io.gatling.core.config.GatlingPropertiesBuilder

object RunMySimulation {
  def main(args: Array[String]) {
    val simClass = classOf[MySimulation].getName

    val props = new GatlingPropertiesBuilder
    props.simulationClass(simClass)
    Gatling.fromMap(props.build)

  }
}
