package br.gov.lexml.parser.pl.linker
import akka.actor._
import scala.language.postfixOps
import akka.dispatch._
import akka.event.Logging
import java.io._
import java.lang.Process
import java.lang.ProcessBuilder
import java.lang.ProcessBuilder.Redirect
import scala.xml._
import scala.xml.parsing._
import scala.io.Source

class LinkerActorException(msg: String) extends Exception(msg)

class LinkerActor extends Actor {
  val log = Logging(context.system,this)
  //self.dispatcher = Dispatchers.newThreadBasedDispatcher(self)
  //val id = "LinkerActor"

  import Actor._
    
  val skipLinker = sys.props.get("lexml.skiplinker").map(_.toBoolean).getOrElse(false)
  val cmdPath = new File(sys.props.getOrElse("lexml.linkertool","/usr/local/bin/linkertool"))

  
  final class LinkerProcess() {

    val process = new ProcessBuilder(cmdPath.getCanonicalPath,
          "--hxml","--xml","--contexto=INLINE").start
    
    val reader = new BufferedReader(new InputStreamReader(process.getInputStream(),"utf-8"))

    val writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(process.getOutputStream(),"utf-8")), true)
  }

  var oprocess: Option[LinkerProcess] = None

  override def preStart() {
    if(!skipLinker && cmdPath.canExecute()) {
      oprocess = Some(new LinkerProcess())
    }    
  }
  override def postStop() {
    for { p <- oprocess } {
      try { p.reader.close() } catch { case _ : Exception => }
      try { p.writer.close() } catch { case _ : Exception => }
      try { p.process.destroy() } catch { case _ : Exception => }
    }
  }

  val ws = """\p{javaWhitespace}"""r
  def receive = {
    case (urnContexto : String, mmsg: Seq[_]) => {           
      val msg = mmsg.collect { case x : Node => x }
      oprocess match { 
        case Some(p) =>      
          val msgTxt = (NodeSeq fromSeq msg).toString
          p.writer.println(urnContexto)
          p.writer.println(msgTxt)
          p.writer.println("###LEXML-END###")
          p.writer.flush()
          val res = {
            val b = new StringBuilder()
            var line = p.reader.readLine()  
            while (line != null && line != "###LEXML-END###") {
              b ++= line
              b ++= System.lineSeparator()     
              line = p.reader.readLine()
            }            
            if(line == null) {
              throw new LinkerActorException("Connection to linker process down!")
            }
            b.toString()
          }          
          val r = XhtmlParser(Source.fromString("<result>" + res + "</result>")).head.asInstanceOf[Elem]
          val links: Set[String] = (r \\ "span").collect({ case (e: Elem) => e.attributes.find(_.prefixedKey == "xlink:href").map(_.value.text) }).flatten.toSet
          sender ! ((r.child.toList, links))        
        case None => 
          sender ! ((msg,Set()))
      }
    } 
    case r => 
      log.warning("received unexpected message: {} ", r)
  } 

}
