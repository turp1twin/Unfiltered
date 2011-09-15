package unfiltered.netty


/**
 *
 */
object PlannedJarResourcesSpec extends unfiltered.spec.netty.Served {

  def getResourcesUrl = {
    val tmp = getClass.getClassLoader.getResource("resources.jar").toExternalForm
    new java.net.URL("jar:" + tmp + "!/assets")
  }

  def setup = _.resources(getResourcesUrl).handler(unfiltered.netty.cycle.Planify {
    case _ => unfiltered.response.ResponseString("planned")
  })

  "A server with jar resources and plans" should {
    "respond to a resources path" in {
      http(host / "assets/foo.css" as_str) must_==("* { margin:0; }")
    }
    "respond to a plans path" in {
      http(host as_str) must_==("planned")
    }
  }
}