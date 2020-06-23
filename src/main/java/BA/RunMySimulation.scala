package BA

import io.gatling.app.Gatling
import io.gatling.core.config.GatlingPropertiesBuilder

object RunMySimulation {
  def main(args: Array[String]) {
    //test from laptop
    //test from PC

    val simClass = classOf[MySimulation].getName
    val props = new GatlingPropertiesBuilder
    props.simulationClass(simClass)
    Gatling.fromMap(props.build)

  }
}
