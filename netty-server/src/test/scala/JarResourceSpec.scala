package unfiltered.netty

import org.specs._
import java.net.URL

/**
 *
 */
object JarResourceSpec extends unfiltered.spec.netty.Served {

  import dispatch._
  import unfiltered.netty.{Http => NHttp}

  // todo: roll this into the base spec helper
  def xhttp[T](handler: dispatch.Handler[T]): T = {
    val h = new Http
    try {
      h.x(handler)
    } finally {
      h.shutdown()
    }
  }

  def getResourcesUrl = {
    val tmp = getClass.getClassLoader.getResource("resources.jar").toExternalForm
    new URL("jar:" + tmp + "!/assets")
  }

  implicit def toStatusVerb(req: dispatch.Request) = new {
    def statuscode = dispatch.Handler(req, {
      case (code, _, _) => code
    }, scala.util.control.Exception.nothingCatcher)
  }

  def setup = _.resources(getResourcesUrl, passOnFail = false)

  "A jar resource server" should {
    "respond with a valid file" in {
      http(host / "assets" / "foo.css" as_str) must_== ("* { margin:0; }")
    }
    "respond with an expected Content-Type" in {
      def mustHaveType(path: String, `type` : String) =
        http(host / path >:> {
          h => h
        }) must havePair(("Content-Type", Set(`type`)))
      mustHaveType("assets/foo.html", "text/html")
      mustHaveType("assets/foo.css", "text/css")
      mustHaveType("assets/foo.txt", "text/plain")
      mustHaveType("assets/foo.js", "application/javascript")
    }
    "respond with useful headers" in {
      val headers = http(host / "assets" / "foo.css" >:> {
        h => h
      })
      headers must haveKey("Date")
      headers must haveKey("Expires")
      headers must haveKey("Last-Modified")
      headers must haveKey("Content-Length")
      headers must haveKey("Content-Type")
      headers must haveKey("Cache-Control")
    }
    "respond with NotFound (404) for requests for non-existant files" in {
      xhttp(host / "assets" / "foo.bar" statuscode) must be_==(404)
    }
    "respond with Forbidden (403) for requests for a directory by default" in {
      xhttp(host statuscode) must be_==(403)
    }
    "respond with BadRequest (400) with a non GET request" in {
      xhttp(host.POST / "foo.css" statuscode) must be_==(400)
    }
    "respond with a NotModified (304) with a If-Modified-Since before expiration" in {
      import java.util.{Calendar, Date, GregorianCalendar}
      import java.io.File
      val rsrc = new File(getClass().getResource("/files/foo.css").getFile)
      val cal = new GregorianCalendar()
      cal.setTime(new Date(rsrc.lastModified))
      cal.add(Calendar.MONTH, +1)
      val ifmodsince = Map("If-Modified-Since" -> Dates.format(cal.getTime))
      xhttp(host / "assets" /"foo.css" <:< ifmodsince statuscode) must be_==(304)
      xhttp(host / "assets" / "foo.css" <:< ifmodsince >:> {
        h => h
      }) must notHavePair("Connection" -> "keep-alive")
    }
  }
}