package br.gov.lexml.parser.pl

import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.NodeSeq
import scala.xml.Text
import grizzled.slf4j.Logging
import br.gov.lexml.parser.pl.block.Alteracao
import br.gov.lexml.parser.pl.block.Block
import br.gov.lexml.parser.pl.block.Dispositivo
import br.gov.lexml.parser.pl.block.Image
import br.gov.lexml.parser.pl.block.OL
import br.gov.lexml.parser.pl.block.Omissis
import br.gov.lexml.parser.pl.block.Paragraph
import br.gov.lexml.parser.pl.block.Table
import br.gov.lexml.parser.pl.metadado.Metadado
import br.gov.lexml.parser.pl.rotulo.RotuloPena

object Caracteristicas {
  val POSSUI_TABELA_ARTICULACAO = "possui tabela na articulacao"
  val POSSUI_ALTERACAO = "possui alteracao"
  val POSSUI_TITULO = "possui titulo"
  val POSSUI_PENA = "possui pena"
  val POSSUI_IMAGEM = "possui imagem"
}

case class ProjetoLei(
  metadado: Metadado, preEpigrafe: List[Block], epigrafe: Block, ementa: Block,
  preambulo: List[Paragraph], articulacao: List[Block], otherCaracteristicas: Map[String, Boolean] = Map()) extends Logging {
  import ProjetoLei._
  lazy val toNodeSeq: NodeSeq =
    <projetolei>
      <preEpigrafe>{ NodeSeq fromSeq (preEpigrafe.flatMap(_.toNodeSeq)) }</preEpigrafe>
      <ementa>{ ementa.toNodeSeq }</ementa>
      <preambulo>{ NodeSeq fromSeq preambulo.flatMap(_.toNodeSeq) }</preambulo>
      <articulacao>{ NodeSeq fromSeq (articulacao.flatMap(_.toNodeSeq)) }</articulacao>
    </projetolei>

  lazy val remakeEpigrafe: ProjetoLei =
    this.copy(epigrafe = Paragraph(Text(metadado.epigrafePadrao)))

  lazy val dispositivoCount = {
    def count(b: Block): Int = b match {
      case d: Dispositivo ⇒ 1 + d.subDispositivos.map(count(_)).sum
      case o: Omissis ⇒ 1
      case a: Alteracao ⇒ 1 + a.blocks.map(count(_)).sum
      case _ ⇒ 0
    }
    articulacao.map(count(_)).sum
  }

  import Caracteristicas._
  lazy val caracteristicas: Map[String, Boolean] = (Map(
    POSSUI_TABELA_ARTICULACAO -> existsAnyThat(articulacao, _.isInstanceOf[Table]),
    POSSUI_ALTERACAO -> existsAnyThat(articulacao, _.isInstanceOf[Alteracao]),
    //"possui imagem" -> existsAnyThat(articulacao, _ == Image), 
    POSSUI_TITULO -> existsAnyThatP(articulacao, { case d: Dispositivo ⇒ d.titulo.isDefined }),
    POSSUI_PENA -> existsAnyThatP(articulacao, { case d: Dispositivo ⇒ d.rotulo == RotuloPena }))
    ++ otherCaracteristicas)

}

object ProjetoLei {
  def firstThat(l: List[Block], f: Block ⇒ Option[Block]): Option[Block] = {
    def h(l: List[Block]): Option[Block] = {
      l match {
        case Nil ⇒ None
        case x :: r ⇒ f(x) match {
          case None ⇒ h(r)
          case y ⇒ y
        }
      }
    }
    h(l)
  }

  def existsAnyThat[R](l: List[Block], f: Block ⇒ Boolean): Boolean =
    firstThat(l, _.searchFirst(f)).isDefined

  def existsAnyThatP[R](l: List[Block], f: PartialFunction[Block, Boolean]): Boolean =
    firstThat(l, _.searchFirst(f.lift(_).getOrElse(false))).isDefined
}
