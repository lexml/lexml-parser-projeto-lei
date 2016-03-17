

import scala.concurrent.Await

object Test1 extends App {
  import scala.concurrent.duration._
  import scala.collection.JavaConversions._
  import org.apache.commons.io.FileUtils
  import java.io.File
  val data = FileUtils.readFileToByteArray(new File("/var/svn/test/lexml-parser-projeto-lei/tags/lexml-parser-projeto-lei-1.9.7/src/main/resources/PLS201000001.rtf"))
  
  import br.gov.lexml.parser.pl.converter._
  val cg = ConverterGraph.create()
  val conv = cg("text/rtf","application/xhtml+xml").get
  
  val resF = conv(data)
  
  val res = Await.result(resF, 10.seconds)
  
  FileUtils.writeByteArrayToFile(new File("/tmp/res.xhtml"), res)
  println("finished")
}
