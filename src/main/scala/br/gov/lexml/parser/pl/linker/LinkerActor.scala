package br.gov.lexml.parser.pl.linker
import akka.actor._
import akka.dispatch._
import akka.event.Logging
import java.io._
import java.lang.Process
import java.lang.ProcessBuilder
import scala.xml._
import scala.xml.parsing._
import scala.io.Source

class LinkerActorException(msg: String) extends Exception(msg)

class LinkerActor extends Actor {
  val log = Logging(context.system,this)
  //self.dispatcher = Dispatchers.newThreadBasedDispatcher(self)
  //val id = "LinkerActor"

  import Actor._

  val cmdPath = "/usr/local/bin/simplelinker"

  final class LinkerProcess() {

    val process = new ProcessBuilder(cmdPath).start

    val reader = new BufferedReader(new InputStreamReader(process.getInputStream()))

    val writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(process.getOutputStream())), true)
  }

  var oprocess: Option[LinkerProcess] = None

  override def preStart() {
    oprocess = Some(new LinkerProcess())
  }
  override def postStop() {
    for { p <- oprocess } {
      try { p.reader.close() } catch { case _ => }
      try { p.writer.close() } catch { case _ => }
      try { p.process.destroy() } catch { case _ => }
    }
  }

  val ws = """\p{javaWhitespace}"""r
  def receive = {
    case msg: Seq[Node] => {
      for { p <- oprocess } {
        println("sending: " + msg)
        val msgTxt = (NodeSeq fromSeq msg).toString.replaceAll("""[\n\r\f]""", "")
        p.writer.println(msgTxt)
        p.writer.flush()
        var l: String = p.reader.readLine()
        //println("read first: " + l)
        if (l == null) {
          throw new LinkerActorException("Connection to linker process down!")
        } else {
          println("received: " + l)
          val r = XhtmlParser(Source.fromString("<result>" + l + "</result>")).head.asInstanceOf[Elem]
          val links: Set[String] = (r \\ "span").collect({ case (e: Elem) => e.attributes.find(_.prefixedKey == "xlink:href").map(_.value.text) }).flatten.toSet
          sender ! ((r.child.toList, links))
        }
      }
    }
    case r => log.warning("received unexpected message: {} ", r)
  }

}
