package br.gov.lexml.parser.pl.block

import scala.language.postfixOps
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.PCData
import scala.xml.Text

import br.gov.lexml.parser.pl.rotulo.Rotulo
import br.gov.lexml.parser.pl.rotulo.RotuloAlinea
import br.gov.lexml.parser.pl.rotulo.RotuloAlteracao
import br.gov.lexml.parser.pl.rotulo.RotuloArtigo
import br.gov.lexml.parser.pl.rotulo.RotuloCapitulo
import br.gov.lexml.parser.pl.rotulo.RotuloInciso
import br.gov.lexml.parser.pl.rotulo.RotuloItem
import br.gov.lexml.parser.pl.rotulo.RotuloLivro
import br.gov.lexml.parser.pl.rotulo.RotuloParagrafo
import br.gov.lexml.parser.pl.rotulo.RotuloParte
import br.gov.lexml.parser.pl.rotulo.RotuloPena
import br.gov.lexml.parser.pl.rotulo.RotuloSecao
import br.gov.lexml.parser.pl.rotulo.RotuloSubCapitulo
import br.gov.lexml.parser.pl.rotulo.RotuloSubSecao
import br.gov.lexml.parser.pl.rotulo.RotuloSubTitulo
import br.gov.lexml.parser.pl.rotulo.RotuloTitulo
import br.gov.lexml.parser.pl.rotulo.rotuloParser
import br.gov.lexml.parser.pl.text.normalizer
import br.gov.lexml.parser.pl.urn.MatchResult
import grizzled.slf4j.Logging

trait HasId[T] {
  val path: List[Rotulo]
  val overridenId: Option[String] = None
  lazy val id = overridenId.getOrElse(HasId.renderId(path))
  def overrideId(newId: String): T
}

class IdRenderException(msg: String) extends RuntimeException(msg)

object HasId {
  def renderCompId(n: Option[Int]) = n.map(n ⇒ "-" + (n + 1).toString).getOrElse("")

  def renderId(r: Rotulo): String = r match {
    case RotuloArtigo(num, comp, unico) ⇒ "art%d%s%s" format (num, if (num == 1 && unico) { "u" } else { "" }, renderCompId(comp))
    case RotuloParagrafo(None, _, _) ⇒ "cpt"
    case RotuloParagrafo(Some(num), comp, unico) ⇒ "par%d%s%s" format (num, if (num == 1 && unico) { "u" } else { "" }, renderCompId(comp))
    case RotuloInciso(num, comp) ⇒ "inc%d%s" format (num, renderCompId(comp))
    case RotuloAlinea(num, comp) ⇒ "ali%d%s" format (num, renderCompId(comp))
    case RotuloItem(num, comp) ⇒ "ite%d%s" format (num, renderCompId(comp))
    case RotuloPena ⇒ "pena"
    case RotuloParte(Left(_), _) ⇒ throw new IdRenderException("Parte sem número não suportado na renderização")
    case RotuloParte(Right(num), comp) ⇒ "prt%d%s" format (num, renderCompId(comp))
    case RotuloLivro(Left(_), _) ⇒ throw new IdRenderException("Livro sem número não suportado na renderização")
    case RotuloLivro(Right(num), comp) ⇒ "liv%d%s" format (num, renderCompId(comp))
    case RotuloTitulo(num, comp) ⇒ "tit%d%s" format (num, renderCompId(comp))
    case RotuloSubTitulo(num, comp) ⇒ throw new IdRenderException("Sub-título não suportado pelo parser")
    case RotuloCapitulo(num, comp) ⇒ "cap%d%s" format (num, renderCompId(comp))
    case RotuloSubCapitulo(num, comp) ⇒ throw new IdRenderException("Sub-capítulo não suportado pelo parser")
    case RotuloSecao(num, comp) ⇒ "sec%d%s" format (num, renderCompId(comp))
    case RotuloSubSecao(num, comp) ⇒ "sub%d%s" format (num, renderCompId(comp))
    case RotuloAlteracao(num) ⇒ "alt%d" format (num)
    case x => throw new RuntimeException("Lexml Xml renderer. Elemento não esperado:" + x)
  }

  def renderId(path: List[Rotulo]): String = path.reverse.map(renderId).mkString("", "_", "")
}

sealed abstract class Block extends Logging {
  self ⇒
  def mapBlock(f: (Block ⇒ Block)): Block = f(this)
  def flatMapBlock(f: (Block ⇒ List[Block])): List[Block] = f(this)
  lazy val dispositivoIfPossible: List[Block] = List(this)
  lazy val recognizeOmissis: Block = this
  lazy val toNodeSeq: NodeSeq = <Block/>

  def topDownUntil(f: Block ⇒ (List[Block], Boolean)): List[Block] = {
    val (newThis, cont) = f(this)
    if (cont) {
      newThis.map(_.flatMapChildren(_.topDownUntil(f)))
    } else {
      newThis
    }
  }
  def flatMapChildren(f: Block ⇒ List[Block]): Block = {
    replaceChildren(children.flatMap(f))
  }
  def replaceChildren(cl: List[Block]): Block = this
  val children: List[Block] = List()
  def searchFirst[A](f: Block ⇒ Boolean): Option[Block] = {
    def g(b: Block): Option[Block] =
      if (f(b)) { Some(b) } else { h(b.children) }

    def h(bl: List[Block]): Option[Block] = bl match {
      case (b: Block) :: r ⇒ g(b) match {
        case None ⇒ h(r)
        case x ⇒ x
      }
      case Nil ⇒ None
    }
    g(this)
  }
}


class Paragraph(val nodes: Seq[Node], val indentation: Double = 0, val centered: Boolean = false, val abreAspas: Boolean = false, val fechaAspas: Boolean = false,
  val notaAlteracao: Option[String]) extends Block {
  lazy val isEmpty = text.isEmpty && !abreAspas && !fechaAspas && notaAlteracao.isEmpty

  lazy val text: String = normalizer.normalize(unormalizedText)

  lazy val unormalizedText: String = (NodeSeq fromSeq nodes).text

  def splitAt(p: Int): (Paragraph, Paragraph) = {
    val (nodes1, nodes2) = Paragraph.splitAt(p, nodes)
    (Paragraph(nodes1, indentation, centered, abreAspas, false, None),
      Paragraph(nodes2, indentation, centered, false, fechaAspas, notaAlteracao))
  }
  def cutLeft(length: Int): Paragraph = splitAt(length)._2

  def cutRight(length: Int): Paragraph = splitAt(text.length - length)._1

  override def toString(): String = "Paragraph(" + (NodeSeq fromSeq nodes) + ", fechasAspas = " + fechaAspas + ")"

  def copy(nodes: Seq[Node] = nodes, indentation: Double = indentation, centered: Boolean = centered,
    abreAspas: Boolean = abreAspas, fechaAspas: Boolean = fechaAspas,
    notaAlteracao: Option[String] = notaAlteracao) =
    new Paragraph(nodes, indentation, centered, abreAspas, fechaAspas, notaAlteracao)

  lazy val withAbreAspas = copy(abreAspas = true)
  lazy val withFechaAspas = copy(fechaAspas = true)

  def withNotaAlteracao(nota: String) = copy(notaAlteracao = Some(nota))

  override lazy val dispositivoIfPossible: List[Block] = {
    rotuloParser.parseRotulo(text) match {
      case Some((rotulo, pos)) ⇒ {
        val conteudoText = text.substring(pos).trim
        val conteudo: Option[Block] = if (conteudoText.isEmpty) {
          None
        } else if (omissisRe.findFirstIn(conteudoText).isDefined) {
          Some(Omissis())
        } else {
          Some(cutLeft(pos) copy (abreAspas = false, fechaAspas = false,
                                  notaAlteracao = None))
        }
        val res = rotulo match {
          case RotuloArtigo(_, _, _) ⇒
            List(
              Dispositivo(rotulo, None, List(), List(), abreAspas),
              Dispositivo(RotuloParagrafo(), conteudo, List(), List(), false, fechaAspas, notaAlteracao))

          case _ ⇒ List(Dispositivo(rotulo, conteudo, List(), List(), abreAspas, fechaAspas, notaAlteracao))
        }
        res
      }
      case None if (omissisRe.findFirstIn(text).isDefined) ⇒ List(Omissis(abreAspas, fechaAspas, notaAlteracao))
      
      case None ⇒ List(this)
      
    }
  }

  val omissisRe = """^[.… ]*(?:…|\.\.\.|\(\.\.\.\.*\))[…. ]*$"""r

  override lazy val recognizeOmissis: Block =
    text match {
      case omissisRe(_) ⇒ Omissis()
      case _ ⇒ this
    }
  override lazy val toNodeSeq = <p>{ NodeSeq fromSeq nodes }</p>
}

object Paragraph {
  def apply(nodes: Seq[Node], indentation: Double = 0, centered: Boolean = false, abreAspas: Boolean = false, fechaAspas: Boolean = false,
    notaAlteracao: Option[String] = None) =
    new Paragraph(nodes, indentation, centered, abreAspas, fechaAspas, notaAlteracao)
  def unapply(p: Paragraph): Option[(Seq[Node], String)] = Some((p.nodes, p.text))
  
  def splitAt(target: Int, nodes: Seq[Node]): (List[Node], List[Node]) = {
    def breakit(params: (Int, List[Node], List[Node]), n: Node): (Int, List[Node], List[Node]) = {
      val (p, bef, aft) = params
      if (target <= p) { (p, bef, n :: aft) }
      else {
        var len = n.text.length
        if (target >= (p + len)) {
          (p + len, n :: bef, aft)
        } else n match {
          case Text(t) ⇒ {
            val (t1, t2) = t.splitAt(target - p)
            (p + len, Text(t1) :: bef, Text(t2) :: aft)
          }
          case Elem(pref, label, attrs, scope, cl @ _*) ⇒ {
            val (cl1, cl2) = splitAt(target - p, List(cl: _*))
            val e1 = Elem(pref, label, attrs, scope, true, cl1: _*)
            val e2 = Elem(pref, label, attrs, scope, true, cl2: _*)
            (p + len, e1 :: bef, e2 :: aft)
          }
          case PCData(t)=> {
            val (t1, t2) = t.splitAt(target - p)
            (p + len, Text(t1) :: bef, Text(t2) :: aft)
          }
          case x => throw new RuntimeException("ops")
        }
      }
    }
    val (_, bef, aft) = nodes.foldLeft(0, List[Node](), List[Node]())(breakit)
    (bef.reverse, aft.reverse)
  }
}

case class Dispositivo(rotulo: Rotulo,
  conteudo: Option[Block], subDispositivos: List[Block],
  path: List[Rotulo], abreAspas: Boolean = false,
  fechaAspas: Boolean = false,
  notaAlteracao: Option[String] = None,
  titulo: Option[Paragraph] = None,
  links: List[String] = List(),
  _overridenId: Option[String] = None) extends Block with HasId[Dispositivo] {
  override val overridenId = _overridenId
  override def mapBlock(f: (Block ⇒ Block)): Block = {
    f(copy(subDispositivos = subDispositivos.map(_.mapBlock(f))))
  }
  override def flatMapBlock(f: (Block ⇒ List[Block])): List[Block] = {
    f(copy(subDispositivos = subDispositivos.flatMap(f)))
  }
  override def toString(): String = ("Dispositivo(" + rotulo + "," + conteudo
    + ",titulo = " + titulo + ", subDispositivos = " + subDispositivos.mkString("[", ",", "]")
    + ", path = " + path.mkString("<", " ", ">") + ", fechaAspas = " + fechaAspas + ")")
  def mapConteudo(f: (Option[Block] ⇒ Option[Block])): Dispositivo = {
    copy(conteudo = f(conteudo))
  }
  override lazy val recognizeOmissis: Block = mapConteudo(_.map(_.recognizeOmissis))

  /*def addSubDispositivo(b: Block) = {
    copy(subDispositivos = subDispositivos :+ b)
  }*/

  override lazy val toNodeSeq =
    <dispositivo>
      { rotulo.toNodeSeq }
      <path>{ NodeSeq fromSeq path.flatMap(_.toNodeSeq) }</path>
      { NodeSeq fromSeq conteudo.map(b ⇒ <conteudo>{ b.toNodeSeq }</conteudo>).toSeq }
      <subdispositivos>{ NodeSeq fromSeq (subDispositivos.flatMap(_.toNodeSeq)) }</subdispositivos>
    </dispositivo>
  override def replaceChildren(cl: List[Block]): Dispositivo = {
    copy(subDispositivos = cl)
  }
  override val children: List[Block] = subDispositivos
  def makeSubDispositivo(n: Int, p: Paragraph): Option[Dispositivo] = rotulo.subRotulo(n).map(r ⇒
    Dispositivo(r, Some(p), List(), List()))
  def overrideId(newId: String) = copy(_overridenId = Some(newId))
}

case class Omissis(abreAspas: Boolean = false, fechaAspas: Boolean = false, notaAlteracao: Option[String] = None) extends Block {
  override def toString(): String = "Omissis(fechasAspas = " + fechaAspas + ")"
  override lazy val toNodeSeq = <Omissis/>
}

case class OL(val lis: List[List[Block]]) extends Block {
  override def toString(): String = "OL " + lis.map(_.mkString("[", ",", "]")).mkString("{", ";", "}")
  override lazy val toNodeSeq = (<ol>
                                   {
                                     for (cl ← lis) yield {
                                       <li>{ cl }</li>
                                     }
                                   }
                                 </ol>)
  override val children = lis.flatten
}

case class Table(elem: Elem) extends Block {
  override def toString(): String = "Table"
  override lazy val toNodeSeq = elem
}

case class Alteracao(blocks: List[Block], path: List[Rotulo] = List[Rotulo](), pos: Option[Int] = None, matches: Option[MatchResult] = None, _overridenId: Option[String] = None,
  baseURN: Option[String] = None) extends Block with HasId[Alteracao] {
  override def toString(): String = "Alteracao" + blocks.mkString("[", ",", ";") + ", matches = " + matches + "]"
  override def mapBlock(f: (Block ⇒ Block)): Block = {
    f(Alteracao(blocks.map(_.mapBlock(f)), path, pos))
  }
  override def flatMapBlock(f: (Block ⇒ List[Block])): List[Block] = {
    val d = Alteracao(blocks.flatMap(f), path, pos)
    f(d)
  }
  def mapBlocks(f: List[Block] ⇒ List[Block]): Alteracao = {
    copy(blocks = f(blocks))
  }
  override lazy val toNodeSeq =
    <alteracao>
      { NodeSeq fromSeq pos.map(n ⇒ <pos>{ n }</pos>).toSeq }
      <path>{ NodeSeq fromSeq path.flatMap(_.toNodeSeq) }</path>
      <blocks>{ NodeSeq fromSeq (blocks.flatMap(_.toNodeSeq)) }</blocks>
    </alteracao>
  override def replaceChildren(cl: List[Block]): Alteracao = mapBlocks(_ ⇒ cl)

  override val children: List[Block] = blocks

  override val overridenId = _overridenId
  def overrideId(newId: String) = copy(_overridenId = Some(newId))
}
case class Unrecognized(elem: Seq[Node]) extends Block {
  override def toString(): String = "Unrecognized(" + elem + ")"
  override lazy val toNodeSeq = <unrecognized>{ NodeSeq fromSeq elem }</unrecognized>
}

case object Image extends Block

object Block {

}
