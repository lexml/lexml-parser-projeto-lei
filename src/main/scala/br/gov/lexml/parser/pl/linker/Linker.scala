package br.gov.lexml.parser.pl.linker

import akka.actor._

import scala.language.postfixOps
import br.gov.lexml.parser.pl.block._
import br.gov.lexml.parser.pl.rotulo._

import scala.xml._
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import grizzled.slf4j.Logger

import scala.concurrent.Await
//import akka.config.Supervision._
import akka.pattern.ask
import akka.routing.SmallestMailboxPool

object Linker {

  val logger: Logger = Logger(this.getClass)

  val system: ActorSystem = ActorSystem("linker")
  
  private val strategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
    case _ : java.io.IOException => Restart
    case _ : Exception => Escalate
  }

  private val linkerRouter = system.actorOf(Props[LinkerActor].withRouter(SmallestMailboxPool(8,
    supervisorStrategy = strategy)))
            
  
  def findLinks(urnContexto : String, ns : Seq[Node]) : (List[String],List[Node]) = {
    logger.info(s"findLinks: urnContexto = $urnContexto, ns=$ns")
    import akka.util.Timeout
    implicit val timeout : Timeout = Timeout(30 seconds)
    val msg = (urnContexto,ns)
    import system.dispatcher
    val f = (linkerRouter ? msg).mapTo[(List[Node],Set[String])] map {
      case (nl,links) => (links.toList,nl)
    }
    logger.info(s"findLinks: waiting for result....")
    val res = Await.result(f,timeout.duration)
    logger.info(s"findLinks: result = $res")
    res
  }

  private def processaAlteracao(a: Alteracao, links: List[URN]): Alteracao = {
    val mr = MatchResult.fromAlteracao(a,links)
    val a1 = a copy (matches = Some(mr))
    mr.first.map(_.updateAlteracao(a1)).getOrElse(a1)
  }

  private def getLinks(d: Dispositivo): List[URN] = d.rotulo match {
    case _: RotuloArtigo => for {
      dd <- d.conteudo.toList.collect {
        case d: Dispositivo => d
      }
      l <- getLinks(dd)
    } yield l
    case _ => d.links.flatMap(URN.fromString)
  }

  def paraCadaAlteracao(bl: List[Block]): List[Block] = {
    def f(d: Dispositivo): Block => Block = {
      case a: Alteracao =>
        val links = getLinks(d)
        processaAlteracao(a, links)
      case dd: Dispositivo => dd.replaceChildren(dd.children.map(f(dd)))
      case x => x
    }
    def g(b: Block) = b match {
      case dd: Dispositivo => dd.replaceChildren(dd.children.map(f(dd)))
      case x => x
    }
    bl.map(g)
  }
}