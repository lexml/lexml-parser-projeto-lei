import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.javaapi.CollectionConverters

object ParseTest extends App {
  val basePath = Paths.get("/home/joao/tmp/lexml/docx_convertidos")
  private val nameRE = """^([0-9]+)-PUB.docx""".r
  val l =
    Files.list(basePath)
      .toList
      .asScala
      .to(Vector)
      .map(_.toFile)
      .filter(_.getName.endsWith(".docx"))
      .flatMap(f => nameRE.findFirstMatchIn(f.getName).map(m => (m.group(1).toInt,f)))



  val queue = new ConcurrentLinkedQueue[Path]()
}


