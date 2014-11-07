package br.gov.lexml.parser.pl.cfg

import java.net.URLClassLoader
import scala.language.postfixOps
import java.net.URL
import java.net.JarURLConnection
import scala.collection.JavaConversions._
import scala.collection.mutable.WeakHashMap
import grizzled.slf4j.Logging

final case class Component(groupId: String, artifactId: String, build: String) {
  override def toString() = "%s:%s:%s" format (groupId, artifactId, build)
}

object ParserComponentConfiguration extends Logging {
  val lexmlParserComponenteNome = "lexml_parser_componente_nome"
  val lexmlParserComponenteBuild = "lexml_parser_componente_build"

  def jarAttributes(u: URL): Map[String, String] = try {
    val url = new URL("jar", "", u.toExternalForm() + "!/")
    logger.info("jarAttributes: u = " + u + ", url = " + url)
    val uc = url.openConnection().asInstanceOf[JarURLConnection]
    for ((k, v) ← uc.getMainAttributes().toMap) yield (
      k.toString.toLowerCase,
      v.asInstanceOf[String])
  } catch {
    case _ : Exception ⇒ Map()
  }

  type Configuration = Map[(String, String), Component]

  private val compsMap: WeakHashMap[ClassLoader, Configuration] = WeakHashMap()

  def componentBuildNumber(groupId: String, artifactId: String, c: Class[_]): Option[String] =
    componentBuildNumber(groupId, artifactId, c.getClassLoader)

  def componentBuildNumber(groupId: String, artifactId: String, cl: ClassLoader): Option[String] =
    scanConfiguration(cl).get((groupId, artifactId)).map(_.build)

  def scanConfiguration(c: Class[_]): Configuration = scanConfiguration(c.getClassLoader)

  def scanConfiguration(cl: ClassLoader): Configuration = synchronized {
    unsyncScanConfiguration(cl)
  }
  private val buildRe = "^[0-9]+"r
  private def unsyncScanConfiguration(cl: ClassLoader): Map[(String, String), Component] = {
    compsMap.get(cl) match {
      case Some(conf) ⇒ conf
      case None ⇒ {
        val thisMap = cl match {
          case ucl: URLClassLoader ⇒ {
            ucl.getURLs.toList.map(jarAttributes).map(m ⇒ (m.get(lexmlParserComponenteNome), m.get(lexmlParserComponenteBuild)) match {
              case (Some(nome), Some(build)) if buildRe.findFirstIn(build).isDefined ⇒ {
                nome.split("/") match {
                  case Array(gid, aid) ⇒ Some(((gid, aid), Component(gid, aid, build)))
                  case _ ⇒ None
                }
              }
              case (Some(nome),Some(build)) => {
                logger.warn("Componente com versão fora da especificação: nome: " + nome + ", build: " + build)
                None
              }
              case _ => None
            }).flatten.toMap
          }
        }
        val parentMap = Option(cl.getParent()).map(unsyncScanConfiguration(_)).getOrElse(Map())
        val conf = (parentMap ++ thisMap)
        compsMap += (cl -> conf)
        conf
      }
    }
  }
}