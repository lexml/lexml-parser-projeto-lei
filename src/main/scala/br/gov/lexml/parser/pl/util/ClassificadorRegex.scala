package br.gov.lexml.parser.pl.util

import scala.annotation.targetName
import scala.util.matching.*

case class ClassificadoresRegex[T](
    classificadores: List[(Regex, Proc[T])] = Nil
):
  type Classificador = (Regex, Proc[T])
  @targetName("plus")
  def +(r: Regex, p: Proc[T]): ClassificadoresRegex[T] =
    copy(classificadores = classificadores :+ (r, p))

  def classifique(s: String): List[T] =
    for {
      (r, p) <- classificadores
      m <- r.findFirstMatchIn(s)
    } yield {
      p(s, m.subgroups)
    }

object ClassificadoresRegex:
  import java.io._
  type ProcBuilder[T] = (Regex, String) => Proc[T]

  val paramPat = """^(.)(.*)\1,(.+)$""".r
  def fromReader[T](
      r: Reader,
      procBuilder: ProcBuilder[T]
  ): ClassificadoresRegex[T] =
    val br = new BufferedReader(r)
    var c = ClassificadoresRegex[T]()
    var line: String = null
    while
      line = br.readLine();
      line != null
    do
      line match {
        case paramPat(_, pat, msg) =>
          val re = pat.r
          c = c + (pat.r, procBuilder(re, msg))
        case _ => ()
      }
    c

  def fromInputStream[T](
      is: InputStream,
      procBuilder: ProcBuilder[T]
  ): ClassificadoresRegex[T] =
    fromReader(new InputStreamReader(is), procBuilder)

  def fromResource[T](
      resourceName: String,
      procBuilder: ProcBuilder[T]
  ): ClassificadoresRegex[T] =
    fromInputStream(
      classOf[ClassificadoresRegex[_]].getClassLoader
        .getResourceAsStream(resourceName),
      procBuilder
    )
end ClassificadoresRegex

