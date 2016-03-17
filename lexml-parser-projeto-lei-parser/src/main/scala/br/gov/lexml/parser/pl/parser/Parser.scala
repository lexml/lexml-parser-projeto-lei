package br.gov.lexml.parser.pl.parser

import scala.util.matching.Regex
import scala.xml.NodeSeq.seqToNodeSeq
import scala.xml.NodeSeq
import scala.xml.Node
import scala.xml.Elem
import scala.xml.Text
import grizzled.slf4j.Logging
import br.gov.lexml.parser.pl.ProjetoLei
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
import br.gov.lexml.parser.pl.rotulo._
import org.apache.commons.io.FileUtils
import java.io.File
import br.gov.lexml.parser.pl.block.Unrecognized
import br.gov.lexml.parser.pl.errors.AlteracaoSemFechaAspas

object Caracteristicas {
  val POSSUI_TABELA_ARTICULACAO = "possui tabela na articulacao"
  val POSSUI_ALTERACAO = "possui alteracao"
  val POSSUI_TITULO = "possui titulo"
  val POSSUI_PENA = "possui pena"
  val POSSUI_IMAGEM = "possui imagem"
}



abstract sealed class Marcador extends Ordered[Marcador] with Equals {
  val id: Int
  final override def compare(that: Marcador) = id - that.id
  final override def equals(other: Any): Boolean = other match {
    case that: Marcador ⇒ (that canEqual this) && (that.id == this.id)
    case _ ⇒ false
  }
  final override def hashCode() = id
}

case object LocalData extends Marcador { val id = 0 }
case object Justificacao extends Marcador { val id = 1 }
case object Anexo extends Marcador { val id = 2 }
case object Legislacao extends Marcador { val id = 3 }
case object Assinatura extends Marcador { val id = 4 }
case object Articulacao extends Marcador { val id = 5 }

case class Marcadores(profile: DocumentProfile,
  localData: Boolean = false, justificacao: Boolean = false,
  anexo: Boolean = false, legislacao: Boolean = false,
  assinatura: Boolean = false) {

  def oneOf(r: List[Regex]) = (b: Block) ⇒ b match {
    case p: Paragraph ⇒ r.find(_.findFirstIn(normalizer.normalize(p.text)).isDefined).map(_ ⇒ p)
    case _ ⇒ None
  }

  def matchesOneOf(r: List[Regex]) = oneOf(r).andThen(_.isDefined)

  val reMarcadores: Map[Marcador, Block ⇒ Boolean] = Map[Marcador, List[Regex]](
    LocalData -> profile.regexLocalData,
    Justificacao -> profile.regexJustificativa,
    Anexo -> profile.regexAnexos,
    Legislacao -> profile.regexLegislacaoCitada,
    Assinatura -> profile.regexAssinatura) mapValues matchesOneOf

  def reconheceMarcador(b: Block): Option[Marcador] =
    reMarcadores.toList collectFirst { case (n, f) if f(b) ⇒ n }

  def reconhece(b: Block): Option[(Marcadores, Marcador)] = {
    reconheceMarcador(b) match {
      case None ⇒ None
      case Some(m) ⇒ m match {
        case LocalData ⇒ if (localData) None else {
          Some(copy(localData = true), m)
        }
        case Justificacao ⇒ if (justificacao) None else {
          Some(copy(justificacao = true), m)
        }
        case Anexo ⇒ if (anexo) None else {
          Some(copy(anexo = true), m)
        }
        case Legislacao ⇒ if (legislacao) None else {
          Some(copy(legislacao = true), m)
        }
        case Assinatura ⇒ if (assinatura) None else {
          Some(copy(assinatura = true), m)
        }
        case Articulacao ⇒ throw new RuntimeException("reconheceMarcador nunca pode reconhecer a articulação")
      }
    }
  }

  def span(bl: List[Block]): Map[Marcador, List[Block]] = {
    def seek(ms: Marcadores,
      m: Marcador,
      accum: List[Block],
      blockMap: Map[Marcador, List[Block]],
      blocks: List[Block]): Map[Marcador, List[Block]] =
      blocks match {
        case Nil ⇒ blockMap + ((m, accum.reverse))
        case (b :: bl) ⇒ (ms.reconhece(b)) match {
          case None ⇒ seek(ms, m, b :: accum, blockMap, bl)
          case Some((ms2, m2)) ⇒
            seek(ms2, m2, Nil, blockMap + ((m, accum.reverse)), bl)
        }
      }
    seek(this, Articulacao, List(), Map(), bl)
  }

  val finished = (localData || assinatura) && justificacao && anexo && legislacao
}

class ProjetoLeiParser(profile: DocumentProfile) extends Logging {

  def isEmptyPar(b: Block): Boolean = b match {
    case Paragraph(_, "") ⇒ true
    case _ ⇒ false
  }
  def isParagraph(b: Block): Boolean = b match {
    case Paragraph(_, _) ⇒ true
    case _ ⇒ false
  }
  def trimEmptyPars(bl: List[Block]): List[Block] = {
    bl.dropWhile(isEmptyPar).reverse.dropWhile(isEmptyPar).reverse
  }

  def oneOf(r: List[Regex]) = (b: Block) ⇒ b match {
    case p: Paragraph ⇒ r.find(_.findFirstIn(p.text).isDefined).map(_ ⇒ p)
    case _ ⇒ None
  }

  def matchesOneOf(r: List[Regex]) = oneOf(r).andThen(_.isDefined)

  def doesNotMatchAnyOf(r: List[Regex]) = oneOf(r).andThen(_.isEmpty)

  def spanEpigrafe(bl: List[Block]): Option[(List[Block], Block, List[Block])] = {
 
    val (pre, bl1) = bl.span(doesNotMatchAnyOf(profile.regexEpigrafe))
    val pre2 = pre.filter(!isEmptyPar(_))
    val (epi, pos) = bl1.span(matchesOneOf(profile.regexEpigrafe))
    val pos2 = pos.dropWhile(b ⇒ matchesOneOf(profile.regexPosEpigrafe)(b) || isEmptyPar(b))
    val epi2 = epi collect { case p: Paragraph ⇒ p }
    epi2 match {
      case Nil ⇒ None
      case _ ⇒ {
        val npNodes = epi2.headOption.toList.flatMap(_.nodes) ++
          epi2.tail.flatMap(Text(" ") :: _.nodes.toList)
        val np = Paragraph(npNodes)
        Some((pre2, np, pos2))
      }
    }
  }

  def reconhecePreambulo(bl: List[Block]): (List[Block], List[Paragraph], List[Block]) = {
    val isPreambulo = matchesOneOf(profile.regexPreambulo)
    val isPosEpigrafe = matchesOneOf(profile.regexPosEpigrafe)
    val isArticulacao: Block ⇒ Boolean = {
      case p: Paragraph ⇒ rotuloParser.parseRotulo(p.text) match {
        case None ⇒ false
        case Some((rotulo, _)) ⇒ rotulo.nivel <= niveis.nivel_maximo_aceito_na_raiz
      }
      case x ⇒ false
    }
    val (prePreambulo, preAmbuloAndPos) = bl.span(x ⇒ !isPreambulo(x) && !isArticulacao(x))
    val (preAmbulo1, posPreambulo) = preAmbuloAndPos.span(!isArticulacao(_))
    val preAmbulo = preAmbulo1.filter({ case p: Paragraph ⇒ !isPosEpigrafe(p); case _ ⇒ true })
    (prePreambulo, preAmbulo.collect { case p: Paragraph ⇒ p }, posPreambulo)
  }

  def reconheceLinks(b: Block) = b.mapBlock {
    case d: Dispositivo ⇒ d.conteudo match {
      case Some(p: Paragraph) ⇒ {
        import br.gov.lexml.parser.pl.linker.Linker.findLinks
        val (links, nl) = findLinks(p.nodes)
        d copy (links = links, conteudo = Some(p copy (nodes = nl)))
      }
      case _ ⇒ d
    }
    case p: Paragraph ⇒ {
      import br.gov.lexml.parser.pl.linker.Linker.findLinks
      val (links, nl) = findLinks(p.nodes)
      p copy (nodes = nl)
    }
    case x ⇒ x
  }
  def reconheceLinks(bl: List[Block]): List[Block] = bl.map(reconheceLinks(_))

  def printArticulacao(bl: List[Block], num: Int) = {
    println("articulacao" + num + ":")
    def printBlock(b: Block, indent: String = ""): Unit = b match {
      case p: Paragraph ⇒ println(indent + "P: text = " + p.text + ", fechaAspas = " + p.fechaAspas + ", na = " + p.notaAlteracao + "\n" +
        indent + "   nodes = " + p.toNodeSeq)
      case o: Omissis ⇒ println(indent + "O: fechaAspas = " + o.fechaAspas + ", na =  " + o.notaAlteracao)
      case a: Alteracao ⇒ {
        println(indent + "A: id = " + a.id + ", base = " + a.baseURN)
        a.blocks.foreach(printBlock(_, indent + "  "))
      }
      case d: Dispositivo ⇒ {
        println(indent + "D: id = " + d.id + ", rotulo = " + d.rotulo + ", fechaAspas = " + d.fechaAspas + ", na = " + d.notaAlteracao)
        d.conteudo.foreach(b ⇒ printBlock(b, indent + "  conteudo: "))
        d.children.foreach(printBlock(_, indent + "    "))
      }
      case t: Table ⇒ println(indent + "<TABLE>")
      case o: OL ⇒ println(indent + "<OL>")
      case _ ⇒ println(indent + "<SOMETHING>")
    }
    bl.foreach(printBlock(_))
  }

  def limpaParagrafosVazios(blocks: List[Block]) = blocks.filter {
    case p: Paragraph if p.text == "" ⇒ false
    case _ ⇒ true
  }

  def parseArticulacao(bl: List[Block], useLinker: Boolean = true): List[Block] = {
    val articulacao1 = bl
    //printArticulacao(articulacao1,1)
    val articulacao2 = trimEmptyPars(articulacao1)
    //printArticulacao(articulacao2,2)
    val articulacao3 = ParserTasks.reconheceAlteracoes(articulacao2)
    //printArticulacao(articulacao3,3)
    val articulacao4 = ParserTasks.reconheceDispositivos(articulacao3)
    //printArticulacao(articulacao4,4)      
    val articulacao5 = ParserTasks.reconheceOmissis(articulacao4)
    //printArticulacao(articulacao5,5)
    val articulacao6 = ParserTasks.identificaTextosAgregadores(articulacao5)
    //printArticulacao(articulacao6,6)
    val articulacao7 = ParserTasks.identificaTitulos(articulacao6)
    //printArticulacao(articulacao7,7)
    val articulacao7a = limpaParagrafosVazios(articulacao7)
    //printArticulacao(articulacao7a,7)
    val articulacao8 = ParserTasks.organizaDispositivos(articulacao7a)
    //printArticulacao(articulacao8,8)

    //val articulacao9 = ParserTasks.reconheceOmissisVazio(articulacao8)
    val articulacao9 = ParserTasks.limpaParagrafosVazios(articulacao8)
    //printArticulacao(articulacao9,9)
    val articulacao9_1 = ParserTasks.pushLastOmissis(articulacao9)
    //val articulacao9 = articulacao8
    //printArticulacao(articulacao9,9)
    val articulacao10 = ParserTasks.numeraAlteracoes(articulacao9_1)
    //printArticulacao(articulacao10,10)
    val articulacao11 = ParserTasks.identificaPaths(articulacao10)
    //printArticulacao(articulacao11,11)
    if (useLinker) {
      val articulacao12 = reconheceLinks(articulacao11)
      //printArticulacao(articulacao12,12)
      val articulacao13 = Linker.paraCadaAlteracao(articulacao12)
      //printArticulacao(articulacao13,13)
      articulacao13
    } else {
      articulacao11
    }
  }
  def fromBlocks(metadado: Metadado, blocks: List[Block]): (Option[ProjetoLei], List[ParseProblem]) = {
    
    try {
      val (preEpigrafe, epigrafe, posEpigrafe) =
        if (profile.regexEpigrafe.isEmpty) {
          (List(), Paragraph(List()), blocks)
        } else {
          spanEpigrafe(blocks) match {
            case None if !profile.epigrafeObrigatoria ⇒ (List(), Paragraph(List()), blocks)
            case Some(p @ (pre, _, _)) if profile.preEpigrafePermitida || pre.length == 0 ⇒ p
            case r ⇒ throw ParseException(EpigrafeAusente) 
          }
        }

      val (ementa1, preambulo, posPreambulo) = reconhecePreambulo(posEpigrafe)
      
      val ementa2 = trimEmptyPars(ementa1)
      
      if (ementa2.isEmpty ||
        !(ementa2.filter(isEmptyPar).isEmpty) ||
        !ementa2.filter(!isParagraph(_)).isEmpty) {
        throw ParseException(EmentaAusente)
      }

      val ementa = ParserTasks.joinParagraphs(ementa2)(0)
      val ms = Marcadores(profile)
      val elementos = ms.span(posPreambulo)
      if (!elementos.contains(Articulacao)) {
        throw ParseException(ArticulacaoNaoIdentificada)
      }

      val articulacao1 = elementos(Articulacao)
      val articulacao = parseArticulacao(articulacao1)
      val possuiImagem = (preEpigrafe ++ List(epigrafe) ++ preambulo ++ articulacao1).exists({
        case p: Paragraph ⇒ !(p.nodes \\ "img").isEmpty
        case Image ⇒ true
        case _ ⇒ false
      })

      import Caracteristicas._

      val otherCaracteristicas = Map[String, Boolean](
        POSSUI_IMAGEM -> possuiImagem)

      val pl = ProjetoLei(
        metadado = metadado,
        preEpigrafe = preEpigrafe,
        epigrafe = epigrafe,
        ementa = reconheceLinks(ementa),
        preambulo = preambulo,
        articulacao = articulacao,
        otherCaracteristicas = otherCaracteristicas)
        
      val falhas = try {
        Validation.validaEstrutura(articulacao)
      } catch {
	      case e: ParseException ⇒ {
	        logger.info("Erro de parse: " + e.errors, e)
	        e.errors.to[Set]
	      }
	      case e: Exception ⇒ {
	        logger.info("Erro de sistema: " + e.getMessage, e)
	        Set(ErroSistema(e))
	      }
      }
      (Some(pl), falhas.toList)
    } catch {
      case e: ParseException ⇒ {
        logger.info("Erro de parse: " + e.errors, e)
        (None, e.errors.toList)
      }
      case e: Exception ⇒ {
        logger.info("Erro de sistema: " + e.getMessage, e)
        (None, List(ErroSistema(e)))
      }
    }
  }

}

object ParserTasks {
  
  //val logger = Logger("Block")

  private[this] def collectText(nl: List[Node]): List[Node] = {
    val validLabels = List("i", "b", "span", "sub", "sup")
    def docollect(nl: List[Node], accum: List[Node] = Nil, accum2 : List[Node] = Nil): List[Node] = nl match {
      case Nil if accum.isEmpty => accum2.reverse
      case Nil => (<p>{ NodeSeq fromSeq accum.reverse }</p> :: accum2).reverse
      case (t: Text) :: r ⇒ docollect(r, t :: accum, accum2)
      case (e: Elem) :: r if (validLabels.contains(e.label)) ⇒ docollect(r, e :: accum, accum2)
      case (e : Elem) :: r if accum.isEmpty ⇒ docollect(r,Nil,e :: accum2)
      case (e : Elem) :: r  ⇒ docollect(r,Nil,e :: (<p>{ NodeSeq fromSeq accum.reverse }</p>) :: accum2)
    }
    docollect(nl, Nil)
  }

  private[this] def fromNodes(nodes: List[Node]): List[Block] = {
    nodes.flatMap((n: Node) ⇒ n match {
      case e : Elem if e.label == "p" && !(e.child \\ "p").isEmpty => {
        val newchilds = e.child.to[List].map {
          case e : Elem => e
          case x  => <p>{x}</p>
        }
        fromNodes(newchilds)
      }
      case Elem(_, name, attr, _, cl @ _*) if name == "p" || name == "blockquote" ⇒ {
        List(Paragraph(cl, attr.asAttrMap.withDefault(_ ⇒ "0")("indentation").toDouble,
          attr.asAttrMap.withDefault(_ ⇒ "")("centered").equals("true")))
      }
      case Elem(_, "table", _, _, _*) ⇒ List(Table(n.asInstanceOf[Elem]))
      case e @ Elem(_, "ol", _, _, _*) ⇒ {
        val l1 = (e \ "li").toList.collect({ case Elem(_, "li", _, _, children @ _*) ⇒ children.toList })
        List(OL(l1.map(nl ⇒ fromNodes(collectText(nl)))))
      }
      case Elem(_, "img", _, _, _*) ⇒ List(Image)
      case Elem(_, label, _, _, _*) ⇒ List(Unrecognized(n.asInstanceOf[Elem]))
      case _ => List()
    })
  }

  

  private[this] val reFimAlteracao = """ *(?:\((ac|nr)\))? *(?:”|“|")\.? *(?:\((ac|nr)\)\.?)?$""".r

  private[this] def agrupaAlteracoes(blocks: List[Block]): List[Block] =
    blocks.foldRight[List[Block]](Nil) {
      case (a1: Alteracao, (a2: Alteracao) :: r) ⇒
        Alteracao(a1.blocks ++ a2.blocks, Nil, None) :: r
      case (a1: Alteracao, (p: Paragraph) :: (a2: Alteracao) :: r) if p.text == "" ⇒
        Alteracao(a1.blocks ++ a2.blocks, Nil, None) :: r
      case (b, l) ⇒ b :: l
    }

  def reconheceAlteracoes(blocks: List[Block]): List[Block] = agrupaAlteracoes {
    def procuraFim(blocks: List[Block], acum: List[Block]): (List[Block], Option[String], List[Block]) = {
      blocks match {
        case Nil ⇒ throw new ParseException(AlteracaoSemFechaAspas)
        case (p @ Paragraph(_, t)) :: rest ⇒ {
          val oms = reFimAlteracao.findFirstMatchIn(t)
          oms match {
            case None ⇒ procuraFim(rest, p :: acum)
            case Some(m) ⇒ {
              val len = m.end - m.start
              val na = if (m.group(1) == null) {
                if (m.group(2) == null) { None }
                else { Some(m.group(2)) }
              } else { Some(m.group(1)) }
              val p2 = p.cutRight(len).withFechaAspas
              val acum2 = p2 :: acum
              (acum2.reverse, na, rest)
            }
          }
        }
        case (o @ OL(lis)) :: rest ⇒ lis.lastOption.getOrElse(Nil) match {
          case Nil ⇒ procuraFim(rest, o :: acum)
          case lastLi ⇒ lastLi.last match {
            case p @ Paragraph(_, t) ⇒ {
              val oms = reFimAlteracao.findFirstMatchIn(t)
              oms match {
                case None ⇒ procuraFim(rest, o :: acum)
                case Some(m) ⇒ {
                  val len = m.end - m.start
                  val na = if (m.group(1) == null) {
                    if (m.group(2) == null) { None }
                    else { Some(m.group(2)) }
                  } else { Some(m.group(1)) }
                  val p2 = p.cutRight(len).withFechaAspas
                  val lastLi2 = lastLi.init :+ p2
                  val lis2 = lis.init :+ lastLi2
                  val acum2 = OL(lis2) :: acum
                  (acum2.reverse, na, rest)
                }
              }
            }
          }
        }
        case b :: rest ⇒ procuraFim(rest, b :: acum)
      }
    }
    def alteraUltimo[T](f: PartialFunction[T, T], l: List[T]): List[T] =
      l.reverse match {
        case Nil ⇒ Nil
        case x :: ll ⇒ f lift x match {
          case None ⇒ l
          case Some(y) ⇒ (y :: ll).reverse
        }
      }
    def reconheceInicio(blocks: List[Block], acum: List[Block]): List[Block] = {
      blocks match {

        case (p @ Paragraph(_, t)) :: rest if (t.startsWith("“") || t.startsWith("\"") || t.startsWith("”")) ⇒ {
          val p2 = p.cutLeft(1).withAbreAspas
          val (balt, na, rest2) = procuraFim(p2 :: rest, List[Block]())
          val balt2 = na.map(n ⇒ alteraUltimo[Block]({ case p: Paragraph ⇒ p.withNotaAlteracao(n) }, balt)).getOrElse(balt)
          val alt = Alteracao(balt2)
          reconheceInicio(rest2, alt :: acum)
        }
        case (pp: Paragraph) :: (p @ Paragraph(_, t)) :: rest if t.length == 0 ⇒ reconheceInicio(pp :: rest, acum)
        
        case (o @ OL(lis)) :: rest ⇒ {
          lis.headOption.getOrElse(List()) match {
            case Nil ⇒ reconheceInicio(rest, o :: acum)
            case (p @ Paragraph(_, t)) :: tailLi ⇒ {
              if (t.startsWith("“") || t.startsWith("\"") || t.startsWith("”")) {
                val p2 = p.cutLeft(1).withAbreAspas
                val o2 = OL((p2 :: tailLi) :: lis.tail)
                val (balt, na, rest2) = procuraFim(o2 :: rest, List[Block]())
                val balt2 = na.map(n ⇒ alteraUltimo[Block]({ case p: Paragraph ⇒ p.withNotaAlteracao(n) }, balt)).getOrElse(balt)
                val alt = Alteracao(balt2)
                reconheceInicio(rest2, alt :: acum)
              } else {
                reconheceInicio(rest, o :: acum)
              }
            }
          }
        }
        case b :: rest ⇒ reconheceInicio(rest, b :: acum)
        
        case Nil ⇒ acum.reverse
        
      }
    }
    reconheceInicio(blocks, List[Block]())
  }

  private[this] def spanNivel(nivel: Int, bl: List[Block]): (List[Block], List[Block]) = {
    def proxSpan(l: List[Block]): (List[Block], List[Block]) = {
      val (omissis, posOmissis) = l.span(_.isInstanceOf[Omissis])
      val (nivelSuperior, resto) = posOmissis.span({
        case d: Dispositivo ⇒ d.rotulo.nivel > nivel
        case _ ⇒ false
      })
      if (!nivelSuperior.isEmpty) {
        val (l1, l2) = proxSpan(resto)
        (omissis ++ nivelSuperior ++ l1, l2)
      } else if (nivel == niveis.artigo) {
        (omissis, posOmissis)
      } else {
        (Nil, l)
      }
    }
    proxSpan(bl)
  }

  private[this] def onlyDispositivos(bl : List[Block]) : List[Dispositivo] = bl.collect({ case d : Dispositivo => d })
      
  private[this] def hasAlteracao(d : Dispositivo) : Boolean = d.subDispositivos.exists {
    case d1 : Dispositivo => hasAlteracao(d1)
    case _ : Alteracao => true
    case _ => false
  }
  
  private[this] def hasFechaAspas(b : Block) : Boolean = b match {
    case d : Dispositivo if d.fechaAspas => true
    case d : Dispositivo => d.subDispositivos.exists(hasFechaAspas)
    case o : Omissis if o.fechaAspas => true
    case _ => false
  }
	   
  private[this] def lastIsFechaAspas(b : Block) : Boolean = b match {
    case d : Dispositivo if d.fechaAspas => true
    case d : Dispositivo => d.subDispositivos.lastOption.map(lastIsFechaAspas(_)).getOrElse(false)
    case o : Omissis if o.fechaAspas => true
    case _ => false
  }
	    
  private[this] def hasOmissis(b : Block) : Boolean = b match {
    case _ : Omissis => true
    case d : Dispositivo => (d.rotulo,d.conteudo) match {    
    	case (_,Some(_ : Omissis)) => true
    	case (r,None) if !r.isAgregador && !r.isInstanceOf[RotuloArtigo] => true
    	case (r,Some(p : Paragraph)) if !r.isAgregador && !r.isInstanceOf[RotuloArtigo] && p.text.isEmpty => true    	
    	case _ => d.subDispositivos.exists(hasOmissis)    	
    }
    case _ => false
  }
  
  def organizaDispositivos(blocks: List[Block], dentroAlteracao: Boolean = false): List[Block] = {    
    def agrupa(b: Block, bl: List[Block]): List[Block] = {
      def spanUpToEvidenciaAlteracao(bl : List[Block], rl : List[Block] = Nil) : Option[(List[Block],List[Block])] = bl match {
        case (o: Omissis) :: bl1 ⇒ Some((o :: rl).reverse,bl1)
        case (a : Alteracao) :: bl1 => Some((a :: rl).reverse,bl1)
        case (d : Dispositivo) :: bl1 if !hasAlteracao(d) && (lastIsFechaAspas(d) || hasOmissis(d)) => Some((d :: rl),bl1)
        case (p : Paragraph) :: bl1 if p.text.isEmpty => spanUpToEvidenciaAlteracao(bl1,p :: rl)
        case _ => None
      }
      
      (b, bl) match {
        case (a1: Alteracao, (a2: Alteracao) :: r) ⇒
          (a1 copy (blocks = a1.blocks ++ a2.blocks)) :: r
        
        case (o : Omissis, (a: Alteracao) :: rest) => 
          (a copy(blocks = o :: a.blocks)) :: rest
        
        case (o : Omissis, (o1 : Omissis) :: rest) if !o.fechaAspas && o.notaAlteracao.isEmpty => 
          val abreAspas = o.abreAspas || o1.abreAspas
          val fechaAspas = o1.fechaAspas
          val notaAlteracao = o1.notaAlteracao
          agrupa(o copy (abreAspas = abreAspas, fechaAspas = fechaAspas, notaAlteracao = notaAlteracao),rest)
        
        case (d : Dispositivo, (a: Alteracao) :: rest) if !hasAlteracao(d) && !hasFechaAspas(d) && hasOmissis(d) => 
          (a copy(blocks = d :: a.blocks)) :: rest
                
        case (a: Alteracao, bl) ⇒ spanUpToEvidenciaAlteracao(bl) match {          
          case Some((inAlt,outAlt)) => {
            inAlt.lastOption match {
              case Some(a1 : Alteracao) =>
                agrupa(a copy (blocks = a.blocks ++ inAlt.init),  a1 :: outAlt)
              case Some(_) =>
                agrupa(a copy (blocks = a.blocks ++ inAlt),  outAlt)
              case None => a :: bl
            }            
          }
          case _ => a :: bl
        }        
        case (d: Dispositivo, (p: Paragraph) :: bl) if (d.conteudo.isEmpty && d.subDispositivos.isEmpty && d.rotulo.isAgregador) ⇒ {
          agrupa(d copy (conteudo = Some(p)), bl)
        }
        case (d @ Dispositivo(rotulo, Some(p1: Paragraph), Nil, _, _, _, _, _, _, _), (p: Paragraph) :: bl) if (rotulo.isAgregador) && !d.fechaAspas && d.notaAlteracao.isEmpty && !p.abreAspas ⇒ {
          val prev = if (p1.nodes.text.trim.isEmpty()) { NodeSeq.Empty } else { p1.nodes ++ Text(" ") }
          agrupa(d copy (conteudo = Some(Paragraph(prev ++ p.nodes)),
            fechaAspas = p.fechaAspas, notaAlteracao = p.notaAlteracao), bl)
        }

        case (d: Dispositivo, (p: Paragraph) :: bl) if !d.fechaAspas &&
          d.notaAlteracao.isEmpty &&
          d.subDispositivos.isEmpty &&
          !p.fechaAspas &&
          p.notaAlteracao.isEmpty &&
          d.conteudo.collect({
            case p1: Paragraph ⇒ !p1.text.isEmpty && (
              p1.text.endsWith(",") || p1.text.endsWith(";") || p1.text.endsWith("-") ||
              p.text.headOption.map(Character.isLowerCase(_)).getOrElse(false))
            case _ ⇒ false
          }).getOrElse(false) ⇒ {
          val p1 = d.conteudo.get.asInstanceOf[Paragraph]
          val d1 = d.mapConteudo(_ ⇒ Some(p1.copy(nodes = p1.nodes ++ List(Text(" ")) ++ p.nodes)))
          agrupa(d1 copy (fechaAspas = p.fechaAspas, notaAlteracao = p.notaAlteracao), bl)
        }
        case (d1: Dispositivo, bl2) ⇒ {
          spanNivel(d1.rotulo.nivel, bl2) match {
            case (Nil, (a: Alteracao) :: bl4) ⇒ 
              agrupa(d1 copy (subDispositivos = d1.subDispositivos :+ (a copy(blocks = organizaDispositivos(reconheceOmissisVazio2(a.blocks), true)))), bl4)
            
            case (nivelSuperior @ (_ :: _), resto) ⇒ 
              val dd = nivelSuperior.foldLeft(d1)((d,a) => d copy (subDispositivos = d.subDispositivos :+ a))
              dd :: resto
            
            case _ ⇒ (b :: bl)
            
          }
        }

        case _ ⇒ {
          b :: bl
        }
      }
    }
    blocks.foldRight(List[Block]())(agrupa)
  }
  
  def pushLastOmissis(bl : List[Block]) : List[Block] = {
    def pushit(b : Block) : List[Block] = b match {
      case d : Dispositivo => {
        val cl = pushitL(d.subDispositivos)
        cl.lastOption match {
          case Some(o : Omissis) => List(d copy (subDispositivos = cl.init),o)
          case _ => List(d copy (subDispositivos = cl))
        }
      }
      case x => List(x)
    }
    def pushitL(bl : List[Block]) : List[Block] = bl.flatMap(pushit)
    def onAlteracao(b : Block) : Block = b match {
      case a : Alteracao => a copy (blocks = pushitL(a.blocks))
      case x => x
    }
    def onAlteracaoL(bl : List[Block]) : List[Block] = bl.map(onAlteracao)
    onAlteracaoL(bl)
  }
  
  def limpaParagrafosVazios(bl : List[Block]) : List[Block] = {
    def limpa(b : Block) : Option[Block] = b match { 
      case d : Dispositivo => Some(d copy (subDispositivos = limpaParagrafosVazios(d.subDispositivos)))      
      case p : Paragraph if p.text.isEmpty => None
      case a : Alteracao => Some(a copy (blocks = limpaParagrafosVazios(a.blocks)))
      case x => Some(x)
    }
    bl.flatMap(limpa(_))
  }

  private[this] def splitLi(li: List[Block]): (Option[Paragraph], List[Block]) = li match {
    case (p: Paragraph) :: r ⇒ (Some(p), r)
    case _ ⇒ (None, Nil)
  }

  def reconheceDispositivos(blocks: List[Block]): List[Block] = {
    val f: (List[Block], Block) ⇒ List[Block] = {
      case ((d: Dispositivo) :: prev, OL(lis)) ⇒ {
        val ll = for ((li, i) ← lis.zipWithIndex) yield {
          val (p, liRest) = splitLi(li)
          p match {
            case None ⇒ liRest.reverse
            case Some(pp) ⇒ d.makeSubDispositivo(i + 1, pp) match {
              case None ⇒ Nil
              case Some(b) ⇒ reconheceDispositivos(b :: liRest)
            }
          }

        }
        ll.reverse.flatten ++ (d :: prev)
      }
      case (prev, p: Paragraph) ⇒ p.dispositivoIfPossible.reverse ++ prev
      case (prev, o: OL) ⇒ prev 
      case (prev, a: Alteracao) ⇒ a.mapBlocks(reconheceDispositivos) :: prev
      case (prev, x) ⇒ x :: prev
    }
    blocks.foldLeft(List[Block]())(f).reverse    
  }

  private[this] val isParagraph: Block ⇒ Boolean = { case Paragraph(_, _) ⇒ true; case _ ⇒ false }

  def joinParagraphs(l: List[Block]): List[Block] =
    l.foldRight(List[Block]())((b: Block, l: List[Block]) ⇒ b match {
      case p @ Paragraph(n1, text1) ⇒ {
        val nodes1 = n1.toList
        l match {
          case Paragraph(n2, text2) :: bl ⇒ {
            val nodes2 = n2.toList
            val nodes: List[Node] = if (!text1.isEmpty && !text2.isEmpty) {
              if (!text1.endsWith(" ") && !text2.startsWith(" ")) {
                val revNodes1 = nodes1.reverse
                revNodes1 match {
                  case (Text(lastText1) :: revTail1) ⇒
                    nodes2 match {
                      case (Text(firstText2) :: tail2) ⇒ {
                        revTail1.reverse ++ (Text(lastText1 + " " + firstText2) :: tail2)
                      }
                      case _ ⇒ revTail1.reverse ++ (Text(lastText1 + " ") :: nodes2)
                    }
                  case _ ⇒
                    nodes2 match {
                      case (Text(firstText2) :: tail2) ⇒ {
                        nodes1 ++ (Text(" " + firstText2) :: tail2)
                      }
                      case _ ⇒ nodes1 ++ List(Text(" ")) ++ nodes2
                    }
                }
              } else {
                nodes1 ++ nodes2
              }
            } else {
              nodes1 ++ nodes2
            }
            Paragraph(nodes, p.indentation) :: bl
          }
          case _ ⇒ b :: l
        }
      }
      case _ ⇒ b :: l
    })

  private[this] def validaEstrutura(blocks: List[Block]): Unit = {} //FIXME

  def identificaPaths(blocks: List[Block]): List[Block] = {
    def createPath(tail: List[Rotulo])(b: Block): Block = b match {
      case d: Dispositivo ⇒ {
        val path = d.rotulo match {
          case r: RotuloArtigo ⇒ d.rotulo :: tail.dropWhile(_.isInstanceOf[RotuloAgregador])
          case _ ⇒ d.rotulo :: tail
        }
        d copy (subDispositivos = d.subDispositivos.map(createPath(path)), path = path)
      }
      case (a: Alteracao) ⇒
        a.pos match {
          case None ⇒ throw new RuntimeException("pos is none in Alteracao")
          case Some(num) ⇒ {
            val path = RotuloAlteracao(num) :: tail
            a copy (blocks = a.blocks.map(createPath(path)), path = path)
          }
        }
      case _ ⇒ b
    }
    blocks.map(createPath(List()))
  }

  def reconheceOmissis(blocks: List[Block]): List[Block] = blocks.mapConserve(_.mapBlock(_.recognizeOmissis))

  private[this] def reconheceOmissisVazio(b: Block): Block = {
    def h(ob: Option[Block]) = ob match {
      case None ⇒ Some(Omissis())
      case Some(p: Paragraph) if p.text == "" ⇒ Some(Omissis(p.abreAspas, p.fechaAspas, p.notaAlteracao))
      case x ⇒ x
    }
    b match {
      case d: Dispositivo ⇒ d.rotulo match {
        case _: RotuloArtigo ⇒ d
        case _ ⇒ d.mapConteudo(h)
      }
      case p: Paragraph if p.text.isEmpty ⇒ Omissis(p.abreAspas, p.fechaAspas, p.notaAlteracao)
      case _ ⇒ b
    }
  }
  private[this] def reconheceOmissisVazio2(blocks: List[Block]): List[Block] = blocks.map(_.mapBlock(reconheceOmissisVazio(_)))
  
  private[this] def reconheceOmissisVazio(blocks: List[Block]): List[Block] = {
    def f(b: Block): (List[Block], Boolean) = b match {
      case a: Alteracao ⇒ (List(a.copy(blocks = reconheceOmissisVazio2(a.blocks))), false)
      case x ⇒ (List(x), true)
    }
    blocks.flatMap(_.topDownUntil(f))
  }

  def numeraAlteracoes(blocks: List[Block]): List[Block] = {
    def numera(blp: (List[Block], Int), b: Block) = {
      val (bl, p) = blp
      b match {
        case a: Alteracao ⇒ ((a copy (blocks = numeraAlteracoes(a.blocks), pos = Some(p))) :: bl, p + 1)
        case d: Dispositivo ⇒ ((d copy (subDispositivos = numeraAlteracoes(d.subDispositivos))) :: bl, p)
        case _ ⇒ (b :: bl, p)
      }
    }
    val (rbl, _) = blocks.foldLeft((List[Block](), 1))(numera)
    rbl.reverse
  }

  def identificaTextosAgregadores(blocks: List[Block], nivel: Int = 0): List[Block] = {
    def buscaInicio(bl: List[Block]): (List[Paragraph], List[Block]) = bl match {
      case (p: Paragraph) :: r if p.isEmpty ⇒ buscaInicio(r)
      case (p: Paragraph) :: r ⇒ {
        val (r1, r2) = r.span({ case (p: Paragraph) ⇒ !p.isEmpty; case _ ⇒ false })
        (p :: r1.collect({ case p: Paragraph ⇒ p }), r2)
      }
      case _ ⇒ (Nil, bl)
    }
    def altera(bl: List[Block]): List[Block] = {
      def doit(bl: List[Block], res: List[Block]): List[Block] = bl match {
        case (d: Dispositivo) :: r if d.rotulo.isAgregador ⇒ {
          val (i, rr) = buscaInicio(r)
          val prevList = d.conteudo.collect({ case p: Paragraph ⇒ p }).map(_.nodes).getOrElse(List())
          val nl = (prevList :: i.map(_.nodes)).filter(ns ⇒ !ns.text.trim.isEmpty) match {
            case first :: rest ⇒ first ++ rest.flatMap(Text(" ") ++ _)
            case Nil ⇒ Nil
          }

          val (fa, na) = i.lastOption.map(x ⇒ (x.fechaAspas, x.notaAlteracao)).getOrElse((d.fechaAspas, d.notaAlteracao))
          doit(rr,d.copy(conteudo = Some(Paragraph(nl)), fechaAspas = fa, notaAlteracao = na) :: res)
          
        }
        case (x :: r) ⇒ doit(r, x :: res)
        case Nil ⇒ res.reverse
      }
      doit(bl, Nil)
    }
    altera(blocks.map({
      case b: Dispositivo ⇒ b.replaceChildren(identificaTextosAgregadores(b.children, nivel + 1))
      case b: Alteracao ⇒ b.replaceChildren(identificaTextosAgregadores(b.children, nivel + 1))
      case x ⇒ x
    }))
  }

  def identificaTitulos(blocks: List[Block], nivel: Int = 0): List[Block] = 
    blocks.map({
      case b: Dispositivo ⇒ b.replaceChildren(identificaTitulos(b.children, nivel + 1))
      case b: Alteracao ⇒ b.replaceChildren(identificaTitulos(b.children, nivel + 1))
      case x ⇒ x
    }).foldLeft(List[Block]()) {
      case (bl, d: Dispositivo) if d.titulo.isEmpty ⇒ {
        bl.dropWhile({ case p: Paragraph if p.isEmpty ⇒ true; case _ ⇒ false }) match {
          case (p: Paragraph) :: r if podeSerTitulo(p.text) ⇒ (d copy (titulo = Some(p), abreAspas = d.abreAspas | p.abreAspas)) :: r
          case _ ⇒ d :: bl
        }
      }
      case (l, x) ⇒ x :: l
    }.reverse
    

  private[this] def podeSerTitulo(s: String) = s.toList.find(List[Char](';', '.', ':').contains(_)).isEmpty

}
