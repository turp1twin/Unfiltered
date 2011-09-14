package unfiltered.netty

import java.io.{File, RandomAccessFile}
import java.nio.charset.Charset

import util.control.Exception._

import org.jboss.netty.channel._

object Mimes {
  import javax.activation.MimetypesFileTypeMap

  lazy val underlying = new MimetypesFileTypeMap(getClass().getResourceAsStream("/mime.types"))
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

trait ExceptionHandling { self: SimpleChannelUpstreamHandler =>
  import org.jboss.netty.handler.codec.http.HttpVersion._
  import org.jboss.netty.handler.codec.http.HttpResponseStatus._
  import org.jboss.netty.handler.codec.http.{HttpResponse => NHttpResponse, DefaultHttpResponse}
  import org.jboss.netty.handler.codec.frame.TooLongFrameException
  import java.nio.channels.ClosedChannelException
  import unfiltered.response._

  /**
   * Binds a Netty HttpResponse res to Unfiltered's HttpResponse to apply any
   * response function to it.
   */
  private def response[T <: NHttpResponse](res: T)(rf: ResponseFunction[T]) =
    rf(new ResponseBinding(res)).underlying

  /**
   * @return a new Netty DefaultHttpResponse bound to an Unfiltered HttpResponse
   */
  private val defaultResponse = response(new DefaultHttpResponse(HTTP_1_1, BAD_REQUEST))_

  override def exceptionCaught(ctx: ChannelHandlerContext, msg: ExceptionEvent): Unit =
    (msg.getCause, msg.getChannel) match {
      case (e: ClosedChannelException, _) =>
        error("Exception thrown while writing to a closed channel: %s" format e.getMessage)
      case (e: TooLongFrameException, ch) =>
        if(ch.isConnected) {
          ch.write(defaultResponse(BadRequest ~> PlainTextContent))
            .addListener(ChannelFutureListener.CLOSE)
        }
      case (e, ch) =>
        if(ch.isConnected) {
          ch.write(defaultResponse(InternalServerError ~> PlainTextContent))
            .addListener(ChannelFutureListener.CLOSE)
        }
    }
}

/**
 *
 */
object Idempotent {
  import unfiltered.request.{HttpRequest, GET, HEAD}

  def unapply[T](r: HttpRequest[T]) = {
    if(GET :: HEAD :: Nil map(_.unapply(r)) filter(_.isDefined) isEmpty) None
    else Some(r)
  }
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
  def /(path: String): Resource
}

abstract class BaseResource(val url: java.net.URL) extends Resource {
  override def path = url.toExternalForm
  override def contentType = Mimes(path)
  def /(path: String) = Resource(url, path)
}

case class EmptyResource() extends Resource {
  override def /(path: String) = this
  def write(secure: Boolean, channel: Channel) =
    new FailedChannelFuture(channel, new IllegalStateException("Cannot write empty resource"))
}

case class FileResource(theFile: java.io.File) extends BaseResource(theFile.toURI.toURL) {
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
    if(secure) channel.write(new ChunkedFile(raf))
    else {
      // No encryption - use zero-copy...
      val reg = new DefaultFileRegion(raf.getChannel, 0, contentLength)
      val f = channel.write(reg)
      f.addListener(new ChannelFutureProgressListener {
        def operationComplete(f: ChannelFuture) = reg.releaseExternalResources
        def operationProgressed(f: ChannelFuture, amt: Long, cur: Long, total: Long) = {}
      })
      f
    }
  }
}

case class JarFileResource(override val url: java.net.URL) extends BaseResource(url) {
  import org.jboss.netty.handler.stream.ChunkedStream

  private val connection = url.openConnection().asInstanceOf[java.net.JarURLConnection]
  override val exists = allCatch either {
    import scala.collection.JavaConversions._
    val jar = connection.getJarFile
    val ef = url.toExternalForm
    val sep = ef.indexOf("!/")
    val path = ef.substring(sep + 2)
    jar.entries exists { _.getName.replace('\\', '/') == path && !path.endsWith("/") }
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
      f.addListener(new ChannelFutureProgressListener {
        def operationComplete(f: ChannelFuture) = allCatch { is.close }
        def operationProgressed(f: ChannelFuture, amt: Long, cur: Long, total: Long) = {}
      })
      f
    case _ => new FailedChannelFuture(channel, new IllegalStateException("Cannot write, no InputStream"))
  }
}

object Resource {
  def apply(base: java.net.URL): Resource = {
    Option(base) match {
      case Some(url) =>
        val ef = url.toExternalForm
        try {
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

object Resources {
  val utf8 = Charset.forName("UTF-8")
  val iso99591 = Charset.forName("ISO-8859-1")
}

/** Serves static resources.
 *  adaptered from Netty's example code HttpStaticFileServerHandler
 *  The behavior for dirIndexes (listing files under a directory) is not yet implemented and may be removed
 */
case class Resources(base: java.net.URL, cacheSeconds: Int = 60, dirIndexes: Boolean = false, passOnFail: Boolean = false)
  extends unfiltered.netty.channel.Plan with ExceptionHandling {
  import Resources._

  import unfiltered.request._
  import unfiltered.response._

  import java.util.{Calendar, GregorianCalendar}
  import java.io.FileNotFoundException

  import org.jboss.netty.channel.ChannelFutureListener
  import org.jboss.netty.handler.codec.http.{HttpHeaders, HttpResponse => NHttpResponse}

  // todo: why doesn't type variance work here?
  // Returning Pass here will tell unfiltered to send the request upstream, otherwise
  // this method handles the request itself
  def passOr[T <: NHttpResponse](rf: => ResponseFunction[NHttpResponse])(req: HttpRequest[ReceivedMessage]) =
    if(passOnFail) Pass else req.underlying.respond(rf)

  def forbid = passOr(Forbidden)_

  def notFound = passOr(NotFound)_

  def badRequest = passOr(BadRequest ~> PlainTextContent)_

  def intent = {
    case Idempotent(Path(path)) & req => accessible(path.drop(1)) match {
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
              val writeHeaders = chan.write(req.underlying.defaultResponse(heads ~> Expires(Dates.format(cal.getTime))))

              def lastly(future: ChannelFuture) =
                if(!HttpHeaders.isKeepAlive(req.underlying.request)) {
                   future.addListener(ChannelFutureListener.CLOSE)
                }

              /**
               * TODO: what to do if connection is reset by peer after writing the heads but before writing the body?
               */
              if(GET.unapply(req) isDefined) lastly(resource.write(req.isSecure, chan))
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
    (allCatch.opt { java.net.URLDecoder.decode(uri, utf8.name()) }
    orElse {
      allCatch.opt { java.net.URLDecoder.decode(uri, iso99591.name()) }
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
