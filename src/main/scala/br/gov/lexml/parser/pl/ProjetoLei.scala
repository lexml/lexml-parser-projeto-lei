package br.gov.lexml.parser.pl

import scala.util.matching.Regex
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.NodeSeq
import scala.xml.Text
import grizzled.slf4j.Logging
import ProjetoLei.existsAnyThat
import ProjetoLei.existsAnyThatP
import br.gov.lexml.parser.pl.block.Alteracao
import br.gov.lexml.parser.pl.block.Block
import br.gov.lexml.parser.pl.block.Dispositivo
import br.gov.lexml.parser.pl.block.Image
import br.gov.lexml.parser.pl.block.OL
import br.gov.lexml.parser.pl.block.Omissis
import br.gov.lexml.parser.pl.block.Paragraph
import br.gov.lexml.parser.pl.block.Table
import br.gov.lexml.parser.pl.errors.ArticulacaoNaoIdentificada
import br.gov.lexml.parser.pl.errors.EmentaAusente
import br.gov.lexml.parser.pl.errors.EpigrafeAusente
import br.gov.lexml.parser.pl.errors.ErroSistema
import br.gov.lexml.parser.pl.errors.ParseException
import br.gov.lexml.parser.pl.errors.ParseProblem
import br.gov.lexml.parser.pl.linker.Linker.findLinks
import br.gov.lexml.parser.pl.linker.Linker
import br.gov.lexml.parser.pl.metadado.Metadado
import br.gov.lexml.parser.pl.output.LexmlRenderer
import br.gov.lexml.parser.pl.rotulo.RotuloPena
import br.gov.lexml.parser.pl.rotulo.niveis
import br.gov.lexml.parser.pl.rotulo.rotuloParser
import br.gov.lexml.parser.pl.text.normalizer
import br.gov.lexml.parser.pl.validation.Validation
import br.gov.lexml.parser.pl.profile.DocumentProfile
import org.apache.commons.io.FileUtils

import java.io.File
import scala.annotation.unused

object Caracteristicas:
  val POSSUI_TABELA_ARTICULACAO = "possui tabela na articulacao"
  val POSSUI_ALTERACAO = "possui alteracao"
  val POSSUI_TITULO = "possui titulo"
  val POSSUI_PENA = "possui pena"
  val POSSUI_IMAGEM = "possui imagem"

case class ProjetoLei(
    metadado: Metadado,
    preEpigrafe: List[Block],
    epigrafe: Block,
    ementa: Option[Block],
    preambulo: List[Paragraph],
    articulacao: List[Block],
    otherCaracteristicas: Map[String, Boolean] = Map()
) extends Logging:
  import ProjetoLei._
  lazy val toNodeSeq: NodeSeq =
    <projetolei>
      <preEpigrafe>{
      NodeSeq fromSeq (preEpigrafe.flatMap(_.toNodeSeq))
    }</preEpigrafe>
			{
      ementa.map(x => <ementa>{x.toNodeSeq}</ementa>).getOrElse(NodeSeq.Empty)
    }      
      <preambulo>{NodeSeq fromSeq preambulo.flatMap(_.toNodeSeq)}</preambulo>
      <articulacao>{
      NodeSeq fromSeq (articulacao.flatMap(_.toNodeSeq))
    }</articulacao>
    </projetolei>

  lazy val remakeEpigrafe: ProjetoLei =
    this.copy(epigrafe = Paragraph(Text(metadado.epigrafePadrao)))
  lazy val dispositivoCount: Int =
    def count(b: Block): Int = b match {
      case d: Dispositivo => 1 + d.subDispositivos.map(count).sum
      case o: Omissis     => 1
      case a: Alteracao   => 1 + a.blocks.map(count).sum
      case _              => 0
    }
    articulacao.map(count).sum

  import Caracteristicas._
  lazy val caracteristicas: Map[String, Boolean] = (Map(
    POSSUI_TABELA_ARTICULACAO -> existsAnyThat(
      articulacao,
      _.isInstanceOf[Table]
    ),
    POSSUI_ALTERACAO -> existsAnyThat(articulacao, _.isInstanceOf[Alteracao]),
    // "possui imagem" -> existsAnyThat(articulacao, _ == Image),
    POSSUI_TITULO -> existsAnyThatP(
      articulacao,
      { case d: Dispositivo => d.titulo.isDefined }
    ),
    POSSUI_PENA -> existsAnyThatP(
      articulacao,
      { case d: Dispositivo => d.rotulo == RotuloPena }
    )
  )
    ++ otherCaracteristicas)

  lazy val asXML: NodeSeq = LexmlRenderer.render(this)
end ProjetoLei

abstract sealed class Marcador extends Ordered[Marcador] with Equals:
  val id: Int
  final override def compare(that: Marcador) : Int = id - that.id
  final override def equals(other: Any): Boolean = other match {
    case that: Marcador => (that canEqual this) && (that.id == this.id)
    case _              => false
  }
  final override def hashCode : Int = id


case object LocalData extends Marcador { val id = 0 }
case object Justificacao extends Marcador { val id = 1 }
case object Anexo extends Marcador { val id = 2 }
case object Legislacao extends Marcador { val id = 3 }
case object Assinatura extends Marcador { val id = 4 }
case object Articulacao extends Marcador { val id = 5 }

case class Marcadores(
    profile: DocumentProfile,
    localData: Boolean = false,
    justificacao: Boolean = false,
    anexo: Boolean = false,
    legislacao: Boolean = false,
    assinatura: Boolean = false
):
  @inline private def oneOf(r: List[Regex]) = (b: Block) =>
    b match {
      case p: Paragraph =>
        r.find(_.findFirstIn(normalizer.normalize(p.text)).isDefined)
          .map(_ => p)
      case _ => None
    }

  @inline private def matchesOneOf(r: List[Regex]) = oneOf(r).andThen(_.isDefined)

  val reMarcadores: Map[Marcador, Block => Boolean] =
    Map[Marcador, List[Regex]](
      LocalData -> profile.regexLocalData,
      Justificacao -> profile.regexJustificativa,
      Anexo -> profile.regexAnexos,
      Legislacao -> profile.regexLegislacaoCitada,
      Assinatura -> profile.regexAssinatura
    ).view.mapValues(matchesOneOf).toMap

  @inline private def reconheceMarcador(b: Block): Option[Marcador] =
    reMarcadores.toList collectFirst { case (n, f) if f(b) => n }

  def reconhece(b: Block): Option[(Marcadores, Marcador)] =
    reconheceMarcador(b) match {
      case None => None
      case Some(m) =>
        m match {
          case LocalData =>
            if (localData) None
            else {
              Some(copy(localData = true), m)
            }
          case Justificacao =>
            if (justificacao) None
            else {
              Some(copy(justificacao = true), m)
            }
          case Anexo =>
            if (anexo) None
            else {
              Some(copy(anexo = true), m)
            }
          case Legislacao =>
            if (legislacao) None
            else {
              Some(copy(legislacao = true), m)
            }
          case Assinatura =>
            if (assinatura) None
            else {
              Some(copy(assinatura = true), m)
            }
          case Articulacao =>
            throw new RuntimeException(
              "reconheceMarcador nunca pode reconhecer a articulação"
            )
        }
    }
  end reconhece

  def span(bl: List[Block]): Map[Marcador, List[Block]] =
    def seek(
        ms: Marcadores,
        m: Marcador,
        accum: List[Block],
        blockMap: Map[Marcador, List[Block]],
        blocks: List[Block]
    ): Map[Marcador, List[Block]] =
      blocks match {
        case Nil => blockMap + ((m, accum.reverse))
        case (b :: bl) =>
          (ms.reconhece(b)) match {
            case None => seek(ms, m, b :: accum, blockMap, bl)
            case Some((ms2, m2)) =>
              seek(ms2, m2, Nil, blockMap + ((m, accum.reverse)), bl)
          }
      }
    seek(this, Articulacao, List(), Map(), bl)
end Marcadores

class ProjetoLeiParser(profile: DocumentProfile) extends Logging {
  import ProjetoLeiParser._

  private def spanEpigrafe(
      bl: List[Block]
  ): Option[(List[Block], Block, List[Block])] =
    val (pre, bl1) = bl.span(doesNotMatchAnyOf(profile.regexEpigrafe1))
    val pre2 = pre.filter(!isEmptyPar(_))
    val (epi, pos) =
      bl1.span(matchesOneOf(profile.regexEpigrafe ++ profile.regexEpigrafe1))
    val pos2 = pos.dropWhile(b =>
      matchesOneOf(profile.regexPosEpigrafe)(b) || isEmptyPar(b)
    )
    val epi2 = epi collect { case p: Paragraph => p }
    epi2 match {
      case Nil => None
      case _ =>
        val npNodes = epi2.headOption.toList.flatMap(_.nodes) ++
          epi2.tail.flatMap(Text(" ") :: _.nodes.toList)
        val np = Paragraph(npNodes)
        Some((pre2, np, pos2))
    }

  private def reconhecePreambulo(
      bl: List[Block]
  ): (List[Block], List[Paragraph], List[Block]) =
    val isPreambulo = matchesOneOf(profile.regexPreambulo)
    val isPosEpigrafe = matchesOneOf(profile.regexPosEpigrafe)
    val isArticulacao: Block => Boolean = {
      case p: Paragraph =>
        rotuloParser.parseRotulo(p.text) match {
          case None => false
          case Some((rotulo, _)) =>
            rotulo.nivel <= niveis.nivel_maximo_aceito_na_raiz
        }
      case x => false
    }
    val (prePreambulo, preAmbuloAndPos) =
      bl.span(x => !isPreambulo(x) && !isArticulacao(x))
    val (preAmbulo1, posPreambulo) = preAmbuloAndPos.span(!isArticulacao(_))
    val preAmbulo = preAmbulo1.filter {
      case p: Paragraph => !isPosEpigrafe(p); case _ => true
    }
    (prePreambulo, preAmbulo.collect { case p: Paragraph => p }, posPreambulo)

  def parseArticulacao(
      bl: List[Block],
      useLinker: Boolean = true,
      urnContexto: String
  ): List[Block] =
    val articulacao0 =
      import java.text.Normalizer
      import Normalizer.Form.NFC
      def norm(x: String) =
        Normalizer.normalize(x, NFC)
      import scala.xml._
      def normalize(n: Node): Node = n match {
        case x: Text => Text(norm(x.text))
        case e: Elem => e.copy(child = e.child.map(normalize))
        case x       => x
      }
      bl.map {
        case p: Paragraph => p.copy(nodes = p.nodes.map(normalize))
        case x            => x
      }

    // Trim paragraphs
    val articulacao1 = articulacao0.map(trimParagraphs)
    // printArticulacao(articulacao1,1)
    val articulacao2 = trimEmptyPars(articulacao1)
    // printArticulacao(articulacao2,2)
    val articulacao3 = Block.reconheceAlteracoes(articulacao2)
    // printArticulacao(articulacao3,3)
    val articulacao4 = Block.reconheceDispositivos(articulacao3)
    // printArticulacao(articulacao4,4)
    val articulacao5 = Block.reconheceOmissis(articulacao4)
    // printArticulacao(articulacao5,5)
    val articulacao6 = Block.identificaTextosAgregadores(articulacao5)
    // printArticulacao(articulacao6,6)
    val articulacao7 = Block.identificaTitulos(articulacao6)
    // printArticulacao(articulacao7,7)
    val articulacao7a = limpaParagrafosVazios(articulacao7)
    // printArticulacao(articulacao7a,7)
    val articulacao8 = Block.organizaDispositivos(articulacao7a)
    // printArticulacao(articulacao8,8)
    val articulacao8_1 = Block.numeraDispositivosGenericos(articulacao8)
    val articulacao8_2 = Block.corrigeRotuloParte(articulacao8_1)
    // val articulacao9 = Block.reconheceOmissisVazio(articulacao8)
    val articulacao9 = Block.limpaParagrafosVazios(articulacao8_2)
    // printArticulacao(articulacao9,9)
    val articulacao9_1 = Block.pushLastOmissis(articulacao9)
    // val articulacao9 = articulacao8
    // printArticulacao(articulacao9,9)
    val articulacao10 = Block.numeraAlteracoes(articulacao9_1)
    // printArticulacao(articulacao10,10)
    val articulacao11 = Block.identificaPaths(articulacao10)
    // printArticulacao(articulacao11,11)
    val articulacao11_1 = Block.numeraDispsGenericos(articulacao11)
    if useLinker then
      val articulacao12 = reconheceLinks(articulacao11_1, urnContexto)
      // printArticulacao(articulacao12,12)
      val articulacao13 = Linker.paraCadaAlteracao(articulacao12)
      // printArticulacao(articulacao13,13)
      articulacao13
    else
      articulacao11_1
    end if
  end parseArticulacao

  def fromBlocks(
      metadado: Metadado,
      blocks: List[Block]
  ): (Option[ProjetoLei], List[ParseProblem]) =
    try {
      val (preEpigrafe, epigrafe, posEpigrafe) =
        if profile.regexEpigrafe.isEmpty then
          (List(), Paragraph(List()), blocks)
        else
          spanEpigrafe(blocks) match {
            case None if !profile.epigrafeObrigatoria =>
              (List(), Paragraph(List()), blocks)
            case Some(p @ (pre, _, _))
                if profile.preEpigrafePermitida || pre.isEmpty =>
              p
            case _ => throw ParseException(EpigrafeAusente)
          }
      val (ementa1, preambulo, posPreambulo) = reconhecePreambulo(posEpigrafe)
      val ementa2 = trimEmptyPars(ementa1)
      val ementa =
        if ementa2.isEmpty ||
            ementa2.exists(isEmptyPar) ||
            ementa2.exists(!isParagraph(_))
        then
          if (profile.ementaAusente) then None
          else throw ParseException(EmentaAusente)
        else
          Some(Block.joinParagraphs(ementa2).head)

      val ms = Marcadores(profile)
      val elementos = ms.span(posPreambulo)
      if !elementos.contains(Articulacao) then
        throw ParseException(ArticulacaoNaoIdentificada)

      val urnContexto = metadado.urnContextoLinker

      val articulacao1 = elementos(Articulacao)
      val articulacao =
        parseArticulacao(articulacao1, urnContexto = urnContexto)
      val possuiImagem =
        (preEpigrafe ++ List(epigrafe) ++ preambulo ++ articulacao1).exists {
          case p: Paragraph => (p.nodes \\ "img").nonEmpty
          case Image        => true
          case _            => false
        }

      import Caracteristicas._

      val otherCaracteristicas =
        Map[String, Boolean](POSSUI_IMAGEM -> possuiImagem)

      val pl = ProjetoLei(
        metadado = metadado,
        preEpigrafe = preEpigrafe,
        epigrafe = epigrafe,
        ementa = ementa.map { x => reconheceLinks(x, urnContexto) },
        preambulo = preambulo,
        articulacao = articulacao,
        otherCaracteristicas = otherCaracteristicas
      )

      val falhas =
        try {
          new Validation().validaEstrutura(articulacao)
        } catch {
          case e: ParseException => e.errors.to(Set)
          case e: Exception      => Set(ErroSistema(e))
        }
      (Some(pl), falhas.toList)
    } catch {
      case e: ParseException => (None, e.errors.toList)
      case e: Exception      => (None, List(ErroSistema(e)))
    }
  }
end ProjetoLeiParser

object ProjetoLeiParser:
  private def isEmptyPar(b: Block): Boolean = b match {
    case Paragraph(_, "") => true
    case _                => false
  }

  private def isParagraph(b: Block): Boolean = b match {
    case Paragraph(_, _) => true
    case _               => false
  }

  private def trimEmptyPars(bl: List[Block]): List[Block] = {
    bl.dropWhile(isEmptyPar).reverse.dropWhile(isEmptyPar).reverse
  }

  private def oneOf(r: List[Regex]) = (b: Block) =>
    b match {
      case p: Paragraph => r.find(_.findFirstIn(p.text).isDefined).map(_ => p)
      case _            => None
    }

  private def matchesOneOf(r: List[Regex]) = oneOf(r).andThen(_.isDefined)

  private def doesNotMatchAnyOf(r: List[Regex]) = oneOf(r).andThen(_.isEmpty)

  def reconheceLinks(b: Block, urnContexto: String): Block = b.mapBlock {
    case d: Dispositivo =>
      d.conteudo match {
        case Some(p: Paragraph) =>
          import br.gov.lexml.parser.pl.linker.Linker.findLinks
          val (links, nl) = findLinks(urnContexto, p.nodes)
          d copy (links = links, conteudo = Some(p copy (nodes = nl)))
        case _ => d
      }
    case p: Paragraph =>
      import br.gov.lexml.parser.pl.linker.Linker.findLinks
      val (_, nl) = findLinks(urnContexto, p.nodes)
      p copy (nodes = nl)
    case x => x
  }

  def reconheceLinks(bl: List[Block], urnContexto: String): List[Block] =
    bl.map(reconheceLinks(_, urnContexto))

  @unused
  def printArticulacao(bl: List[Block], num: Int): Unit =
    println("articulacao" + num + ":")

    def printBlock(b: Block, indent: String = ""): Unit = b match {
      case p: Paragraph =>
        println(
          indent + "P: text = " + p.text + ", fechaAspas = " + p.fechaAspas + ", na = " + p.notaAlteracao + "\n" +
            indent + "   nodes = " + p.toNodeSeq
        )
      case o: Omissis =>
        println(
          indent + "O: fechaAspas = " + o.fechaAspas + ", na =  " + o.notaAlteracao
        )
      case a: Alteracao =>
        println(indent + "A: id = " + a.id + ", base = " + a.baseURN)
        a.blocks.foreach(printBlock(_, indent + "  "))
      case d: Dispositivo =>
        println(
          indent + "D: id = " + d.id + ", rotulo = " + d.rotulo + ", fechaAspas = " + d.fechaAspas + ", na = " + d.notaAlteracao
        )
        d.conteudo.foreach(b => printBlock(b, indent + "  conteudo: "))
        d.children.foreach(printBlock(_, indent + "    "))
      case _: Table => println(indent + "<TABLE>")
      case _: OL    => println(indent + "<OL>")
      case _        => println(indent + "<SOMETHING>")
    }

    bl.foreach(printBlock(_,""))


  def limpaParagrafosVazios(blocks: List[Block]): List[Block] = blocks.filter {
    case p: Paragraph if p.text == "" => false
    case _                            => true
  }

  private def trimParagraphs(b: Block) = b match {
    case p: Paragraph =>
      val t = p.nodes.text
      val start = t.takeWhile(_.isWhitespace).length
      val end = t.reverse.takeWhile(_.isWhitespace).length
      val p1 = if (end > 0) {
        p.cutRight(end)
      } else {
        p
      }
      val p2 = if (start > 0) {
        p.cutLeft(start)
      } else {
        p
      }
      p2
    case x => x
  }
end ProjetoLeiParser

object ProjetoLei:
  def firstThat(l: List[Block], f: Block => Option[Block]): Option[Block] =
    def h(l: List[Block]): Option[Block] =
      l match {
        case Nil => None
        case x :: r =>
          f(x) match {
            case None => h(r)
            case y    => y
          }
      }
    h(l)

  def existsAnyThat[R](l: List[Block], f: Block => Boolean): Boolean =
    firstThat(l, _.searchFirst(f)).isDefined

  def existsAnyThatP[R](
      l: List[Block],
      f: PartialFunction[Block, Boolean]
  ): Boolean =
    firstThat(l, _.searchFirst(f.lift(_).getOrElse(false))).isDefined
end ProjetoLei