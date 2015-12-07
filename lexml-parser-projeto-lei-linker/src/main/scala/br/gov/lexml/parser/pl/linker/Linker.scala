package br.gov.lexml.parser.pl.linker

import akka.actor._


import scala.language.postfixOps
import akka.actor.Props
import br.gov.lexml.parser.pl.block._
import br.gov.lexml.parser.pl.rotulo._
import br.gov.lexml.parser.pl.urn._
import scala.xml._
import akka.routing.RoundRobinRouter
import akka.routing.SmallestMailboxRouter
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy._
import scala.concurrent.Await

//import akka.config.Supervision._


object Linker {

  val system = ActorSystem("linker")
  
  import system._

  val strategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
    case _ : java.io.IOException => Restart
    case _ : Exception => Escalate
  }
  
  val linkerRouter = actorOf(Props[LinkerActor].withRouter(SmallestMailboxRouter(nrOfInstances = 8, 
      supervisorStrategy = strategy )))
            
  
  def findLinks(ns : Seq[Node]) : (List[String],List[Node]) = {
    import akka.pattern.ask
    import akka.util.Timeout
    implicit val timeout = Timeout(30 seconds) 
    val f = (linkerRouter ? ns).mapTo[(List[Node],Set[String])] map {
      case (nl,links) ⇒ (links.toList,nl)
    }
    Await.result(f,timeout.duration)
  }

  def processaAlteracao(a: Alteracao, links: List[URN]): Alteracao = {
    val mr = MatchResult.fromAlteracao(a,links)
    val a1 = a copy (matches = Some(mr))
    mr.first.map(_.updateAlteracao(a1)).getOrElse(a1)
  }

  def getLinks(d: Dispositivo): List[URN] = d.rotulo match {
    case _: RotuloArtigo ⇒ for { (dd: Dispositivo) ← d.conteudo.toList.collect { case (d: Dispositivo) ⇒ d }; l ← getLinks(dd) } yield l
    case _ ⇒ d.links.flatMap(URN.fromString(_))
  }

  def paraCadaAlteracao(bl: List[Block]) = {
    def f(d: Dispositivo): Block ⇒ Block = (b: Block) ⇒ b match {
      case a: Alteracao ⇒ {
        val links = getLinks(d)
        processaAlteracao(a, links) 
      }
      case dd: Dispositivo ⇒ dd.replaceChildren(dd.children.map(f(dd)))
      case x ⇒ x
    }
    def g(b: Block) = b match {
      case dd: Dispositivo ⇒ dd.replaceChildren(dd.children.map(f(dd)))
      case x ⇒ x
    }
    bl.map(g)
  }
}