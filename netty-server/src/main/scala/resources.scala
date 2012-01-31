package unfiltered.netty

import java.io.{File, RandomAccessFile}
import java.nio.charset.Charset

import util.control.Exception._

import org.jboss.netty.channel._

object Mimes {
  import javax.activation.MimetypesFileTypeMap

  lazy val underlying = new MimetypesFileTypeMap(getClass.getResourceAsStream("/mime.types"))
  def apply(path: String) = underlying.getContentType(path)
}

object Dates {
  import java.text.SimpleDateFormat
  import java.util.{Date, Locale, TimeZone}

  val HttpDateFormat = "EEE, dd MMM yyyy HH:mm:ss zzz"
  val HttpDateGMTTimezone = "GMT"
  def format(d: Long): String =
    new SimpleDateFormat(HttpDateFormat, Locale.US) {
      setTimeZone(TimeZone.getTimeZone(HttpDateGMTTimezone))
    }.format(d)
  def format(d: Date): String = format(d.getTime)
}

/**
 * Extracts HttpRequest if a retrieval method
 */
object Retrieval {
  import unfiltered.request.{HttpRequest, GET, HEAD}
  def unapply[T](r: HttpRequest[T]) = GET.unapply(r).orElse { HEAD.unapply(r) }
}

object Resources {
  val utf8 = Charset.forName("UTF-8")
  val iso88591 = Charset.forName("ISO-8859-1")
}

/**
 * Resource abstractions, to allow for returning assets from the enclosing jar file (when running as an executable jar),
 * in addition to the filesystem.
 */
trait Resource {
  def exists = false
  def lastModified: Long = -1
  def contentType: String = ""
  def contentLength: Long = -1
  def path: String = ""
  def isFile = false
  def write(secure: Boolean, channel: Channel): ChannelFuture
}

case class EmptyResource() extends Resource {
  def write(secure: Boolean, channel: Channel) =
    new FailedChannelFuture(channel, new IllegalStateException("Cannot write empty resource"))
}

abstract class UrlResource(val url: java.net.URL) extends Resource {
  override def path = url.toExternalForm
  override def contentType = Mimes(path)
}

case class FileResource(theFile: java.io.File) extends UrlResource(theFile.toURI.toURL) {
  import org.jboss.netty.handler.stream.ChunkedFile

  override def exists = theFile.exists && !theFile.isHidden
  override def isFile = theFile.isFile
  override def lastModified = theFile.lastModified
  override def path = theFile.getPath
  override def contentLength = theFile.length

  /**
   * If secure, we cannot use optimized zero copy to write the file..
   */
  def write(secure: Boolean, channel: Channel) = {
    val raf = new RandomAccessFile(theFile, "r")
    val len = raf.length
    if(secure) channel.write(new ChunkedFile(raf, 0, len, 8192))
    else {
      // No encryption - use zero-copy...
      val reg = new DefaultFileRegion(raf.getChannel, 0, len)
      val f = channel.write(reg)
      f.addListener(new ChannelFutureListener {
        def operationComplete(f: ChannelFuture) = reg.releaseExternalResources
      })
      f
    }
  }
}

case class JarFileResource(override val url: java.net.URL) extends UrlResource(url) {
  import org.jboss.netty.handler.stream.ChunkedStream

  private val connection = url.openConnection().asInstanceOf[java.net.JarURLConnection]
  override val exists = allCatch either {
    import scala.collection.JavaConversions._
    val ef = url.toExternalForm
    val sep = ef.indexOf("!/")
    val path = ef.substring(sep + 2)
    connection.getJarFile.entries exists { _.getName.replace('\\', '/') == path && !path.endsWith("/") }
  } match {
    case Left(ex) => false
    case Right(result) => result
  }

  override def lastModified = connection.getLastModified
  override def isFile = exists // Not a directory...
  override def contentLength = connection.getContentLength.toLong
  private def inputStream = Option(connection.getInputStream)

  def write(secure: Boolean, channel: Channel) = inputStream match {
    case Some(is) =>
      val f = channel.write(new ChunkedStream(is))
      f.addListener(new ChannelFutureListener {
        def operationComplete(f: ChannelFuture) = allCatch { is.close }
      })
      f
    case _ => new FailedChannelFuture(channel, new IllegalStateException("Cannot write, no InputStream"))
  }
}

object Resource {
  def apply(base: java.net.URL): Resource = {
    Option(base) match {
      case Some(url) =>
        try {
          val ef = url.toExternalForm
          if (ef startsWith  "jar:file:") JarFileResource(base)
          else if (ef startsWith "file:") FileResource(new File(base.getFile))
          else EmptyResource()
        } catch { case _ => EmptyResource() }
      case _ => EmptyResource()
    }
  }
  def apply(base: java.net.URL, path: String): Resource = {
    Option(base) match {
      case Some(url) => apply(new java.net.URL(url, path))
      case _ => EmptyResource()
    }
  }
}

/**
 * Serves static resources, adaptered from Netty's example code HttpStaticFileServerHandler
 */
case class Resources(base: java.net.URL,
                     cacheSeconds: Int = 60,
                     passOnFail: Boolean = true)
  extends unfiltered.netty.async.Plan with ServerErrorResponse {
  import Resources._

  import unfiltered.request._
  import unfiltered.response._

  import java.util.{Calendar, GregorianCalendar}
  import java.io.FileNotFoundException
  import java.net.URLDecoder

  import org.jboss.netty.channel.ChannelFutureListener
  import org.jboss.netty.handler.codec.http.{HttpHeaders, HttpResponse => NHttpResponse}

  /**
   * Returning Pass here will tell unfiltered to send the request upstream,
   * otherwise this method handles the request itself
   */
  def passOr[T <: NHttpResponse](rf: => ResponseFunction[NHttpResponse])(req: HttpRequest[ReceivedMessage]) =
    if(passOnFail) Pass else req.underlying.respond(rf)

  def forbid = passOr(Forbidden)_

  def notFound = passOr(NotFound)_

  def badRequest = passOr(BadRequest ~> PlainTextContent)_

  def intent = {
    case Retrieval(Path(path)) & req => accessible(path.drop(1)) match {
      case Some(resource) =>
        IfModifiedSince(req) match {
          case Some(since) if (since.getTime >= resource.lastModified) =>
            // close immediately and do not include a content-length header
            // http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
            req.underlying.event.getChannel.write(req.underlying.defaultResponse(
              NotModified ~> Date(Dates.format(new GregorianCalendar().getTime))
            )).addListener(ChannelFutureListener.CLOSE)
          case _ =>
            if(!resource.exists) notFound(req)
            else if(!resource.isFile) forbid(req)
            else try {
              val cal = new GregorianCalendar()
              val heads = Ok ~> ContentLength(resource.contentLength.toString) ~>
                ContentType(Mimes(resource.path)) ~> // note: bin/text/charset not included
                Date(Dates.format(cal.getTime)) ~>
                CacheControl("private, max-age=%d" format cacheSeconds) ~>
                LastModified(Dates.format(resource.lastModified))

              cal.add(Calendar.SECOND, cacheSeconds)

              val chan = req.underlying.event.getChannel
              val writeHeaders = chan.write(req.underlying.defaultResponse(
                heads ~> Expires(Dates.format(cal.getTime))))

              def lastly(future: ChannelFuture) =
                if(!HttpHeaders.isKeepAlive(req.underlying.request)) {
                   future.addListener(ChannelFutureListener.CLOSE)
                }

              if(GET.unapply(req).isDefined && chan.isOpen) lastly(resource.write(req.isSecure, chan))
              else lastly(writeHeaders)
            } catch {
              case e: FileNotFoundException => notFound(req)
            }
        }
      case _ => forbid(req)
    }
    case req => badRequest(req)
  }

  /**
   * Converts a raw uri to a safe system file. Attempts to prevent
   * security holes where resources are accessed with .. paths
   * potentially outside of the root of the web app
   */
  private def accessible(uri: String) =
    (allCatch.opt { URLDecoder.decode(uri, utf8.name()) }
    orElse {
      allCatch.opt { URLDecoder.decode(uri, iso88591.name()) }
    }) match {
      case Some(decoded) =>
        decoded.replace('/', File.separatorChar) match {
          case fpath
            if(fpath.contains(File.separator + ".") ||
               fpath.contains("." + File.separator) ||
               fpath.startsWith(".") ||
               fpath.endsWith(".")) ||
               fpath == "" => None
          case fpath =>
            Some(Resource(base, fpath))
        }
      case _ => None
    }
}
