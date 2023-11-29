package br.gov.lexml.parser.pl.block

import br.gov.lexml.parser.pl.errors.{AlteracaoSemFechaAspas, ParseException}
import br.gov.lexml.parser.pl.linker.MatchResult
import br.gov.lexml.parser.pl.rotulo.*
import br.gov.lexml.parser.pl.text.normalizer
import grizzled.slf4j.Logging

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.matching.Regex
import scala.xml.*

trait HasId[T]:
  def path: List[Rotulo]
  val overridenId: Option[String] = None
  lazy val id: String = overridenId.getOrElse(HasId.renderId(path))

  def overrideId(newId: String): T


class IdRenderException(msg: String) extends Exception(msg)

object HasId:
  def renderCompId(n: Option[Int]): String =
    n.map(n => "-" + (n + 1).toString).getOrElse("")

  private implicit class Unico(un: Boolean):
    def unicoChar: String = if (un) { "u" }
    else { "" }


  def renderId(r: Rotulo): String = r match {
    case RotuloArtigo(num, comp, unico) =>
      "art%d%s%s" format (num, if (num == 1 && unico) {
        "u"
      } else {
        ""
      }, renderCompId(comp))
    case RotuloParagrafo(None, _, _) => "cpt"
    case RotuloParagrafo(Some(num), comp, unico) =>
      "par%d%s%s" format (num, if (num == 1 && unico) {
        "u"
      } else {
        ""
      }, renderCompId(comp))
    case RotuloInciso(num, comp) => "inc%d%s" format (num, renderCompId(comp))
    case RotuloAlinea(num, comp) => "ali%d%s" format (num, renderCompId(comp))
    case RotuloItem(num, comp)   => "ite%d%s" format (num, renderCompId(comp))
    case RotuloPena              => "pena"
    case r: RotuloDispositivoGenerico => s"dpg${r.num}"
    case r @ RotuloParte(Left(_), _, _, _, _) =>
      throw new IdRenderException(
        "Parte sem número não suportado na renderização: " + r
      )
    case RotuloParte(Right(num), comp, unico, _, _) =>
      "prt%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloLivro(Left(rot), _, _) =>
      throw new IdRenderException(
        s"Livro sem número não suportado na renderização: $rot"
      )
    case RotuloLivro(Right(num), comp, unico) =>
      "liv%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloTitulo(num, comp, unico) =>
      "tit%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloSubTitulo(num, comp, unico) =>
      "stt%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloCapitulo(num, comp, unico) =>
      "cap%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloSubCapitulo(_, _, _) =>
      throw new IdRenderException("Sub-capítulo não suportado pelo parser")
    case RotuloSecao(num, comp, unico) =>
      "sec%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloSubSecao(num, comp, unico) =>
      "sub%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloAlteracao(num) => "alt%d" format num
    case x =>
      throw new RuntimeException(
        "Lexml Xml renderer. Elemento não esperado:" + x
      )
  }

  def renderId(path: List[Rotulo]): String =
    path.reverse.map(renderId).mkString("", "_", "")
end HasId


sealed abstract class Block extends Logging:
  self =>
  def mapBlock(f: Block => Block): Block = f(this)

  def flatMapBlock(f: Block => List[Block]): List[Block] = f(this)

  lazy val dispositivoIfPossible: List[Block] = List(this)
  lazy val recognizeOmissis: Block = this
  lazy val toNodeSeq: NodeSeq = <Block/>

  def topDownUntil(f: Block => (List[Block], Boolean)): List[Block] =
    val (newThis, cont) = f(this)
    if (cont) {
      newThis.map(_.flatMapChildren(_.topDownUntil(f)))
    } else {
      newThis
    }

  def flatMapChildren(f: Block => List[Block]): Block =
    replaceChildren(children.flatMap(f))

  def replaceChildren(cl: List[Block]): Block = this

  val children: List[Block] = List()

  def searchFirst(f: Block => Boolean): Option[Block] =
    def g(b: Block): Option[Block] =
      if (f(b)) {
        Some(b)
      } else {
        h(b.children)
      }

    @tailrec
    def h(bl: List[Block]): Option[Block] = bl match {
      case (b: Block) :: r =>
        g(b) match {
          case None => h(r)
          case x    => x
        }
      case Nil => None
    }

    g(this)

end Block


class Paragraph(
    val nodes: Seq[Node],
    val indentation: Double = 0,
    val centered: Boolean = false,
    val abreAspas: Boolean = false,
    val fechaAspas: Boolean = false,
    val notaAlteracao: Option[String]
) extends Block:
  lazy val isEmpty: Boolean =
    text.isEmpty && !abreAspas && !fechaAspas && notaAlteracao.isEmpty

  lazy val text: String = normalizer.normalize(unormalizedText)

  lazy val unormalizedText: String = (NodeSeq fromSeq nodes).text.trim

  def splitAt(p: Int): (Paragraph, Paragraph) =
    val (nodes1, nodes2) = Block.splitAt(p, nodes)
    (
      Paragraph(
        nodes1,
        indentation,
        centered,
        abreAspas,
        fechaAspas = false,
        None
      ),
      Paragraph(
        nodes2,
        indentation,
        centered,
        abreAspas = false,
        fechaAspas = fechaAspas,
        notaAlteracao
      )
    )

  def cutLeft(length: Int): Paragraph = splitAt(length)._2

  def cutRight(length: Int): Paragraph = splitAt(text.length - length)._1

  override def toString: String =
    "Paragraph(" + (NodeSeq fromSeq nodes) + ", fechasAspas = " + fechaAspas + ")"

  def copy(
      nodes: Seq[Node] = nodes,
      indentation: Double = indentation,
      centered: Boolean = centered,
      abreAspas: Boolean = abreAspas,
      fechaAspas: Boolean = fechaAspas,
      notaAlteracao: Option[String] = notaAlteracao
  ) =
    new Paragraph(
      nodes,
      indentation,
      centered,
      abreAspas,
      fechaAspas,
      notaAlteracao
    )

  lazy val withAbreAspas: Paragraph = copy(abreAspas = true)
  lazy val withFechaAspas: Paragraph = copy(fechaAspas = true)

  def withNotaAlteracao(nota: String): Paragraph =
    copy(notaAlteracao = Some(nota))

  override lazy val dispositivoIfPossible: List[Block] =
    rotuloParser.parseRotulo(text) match {
      case Some((rotulo, pos)) =>
        val conteudoText = text.substring(pos).trim
        val conteudo: Option[Block] =
          if conteudoText.isEmpty then None
          else if omissisRe.findFirstIn(conteudoText).isDefined then Some(Omissis())
          else Some(
                cutLeft(pos) copy (abreAspas = false, fechaAspas = false,
                notaAlteracao = None)
              )
        rotulo match {
          case RotuloArtigo(_, _, _) =>
            List(
              Dispositivo(rotulo, None, List(), List(), abreAspas),
              Dispositivo(
                RotuloParagrafo(),
                conteudo,
                List(),
                List(),
                abreAspas = false,
                fechaAspas = fechaAspas,
                notaAlteracao
              )
            )

          case _ =>
            List(
              Dispositivo(
                rotulo,
                conteudo,
                List(),
                List(),
                abreAspas,
                fechaAspas,
                notaAlteracao
              )
            )
        }
      case None if omissisRe.findFirstIn(text).isDefined =>
        List(Omissis(abreAspas, fechaAspas, notaAlteracao))
      case None => List(this)
   }
  end dispositivoIfPossible

  val omissisRe: Regex = """^[.… ]*(?:…|\.\.\.|\(\.\.\.+\))[…. ]*$""" r

  override lazy val recognizeOmissis: Block =
    text match {
      case omissisRe(_) => Omissis()
      case _            => this
    }
  override lazy val toNodeSeq: Elem = <p>
    {NodeSeq fromSeq nodes}
  </p>
end Paragraph


object Paragraph:
  def apply(
      nodes: Seq[Node],
      indentation: Double = 0,
      centered: Boolean = false,
      abreAspas: Boolean = false,
      fechaAspas: Boolean = false,
      notaAlteracao: Option[String] = None
  ) =
    new Paragraph(
      nodes,
      indentation,
      centered,
      abreAspas,
      fechaAspas,
      notaAlteracao
    )

  def unapply(p: Paragraph): Option[(Seq[Node], String)] = Some(
    (p.nodes, p.text)
  )

case class Dispositivo(
    rotulo: Rotulo,
    conteudo: Option[Block],
    subDispositivos: List[Block],
    path: List[Rotulo],
    abreAspas: Boolean = false,
    fechaAspas: Boolean = false,
    notaAlteracao: Option[String] = None,
    titulo: Option[Paragraph] = None,
    links: List[String] = List(),
    _overridenId: Option[String] = None
) extends Block
    with HasId[Dispositivo]:
  override val overridenId: Option[String] = _overridenId

  override def mapBlock(f: Block => Block): Block =
    f(copy(subDispositivos = subDispositivos.map(_.mapBlock(f))))


  override def flatMapBlock(f: Block => List[Block]): List[Block] =
    f(copy(subDispositivos = subDispositivos.flatMap(f)))

  override def toString: String =
    s"""Dispositivo($rotulo,$conteudo,titulo = $titulo, subDispositivos = ${subDispositivos.mkString("[",",","]")}
       |, path = ${path.mkString("<"," ",">")}, fechaAspas = $fechaAspas)""".stripMargin

  def mapConteudo(f: Option[Block] => Option[Block]): Dispositivo =
    copy(conteudo = f(conteudo))


  override lazy val recognizeOmissis: Block = mapConteudo(
    _.map(_.recognizeOmissis)
  )

  /*def addSubDispositivo(b: Block) = {
    copy(subDispositivos = subDispositivos :+ b)
  }*/

  override lazy val toNodeSeq: Elem =
    <dispositivo>
      {rotulo.toNodeSeq}<path>
      {NodeSeq fromSeq path.flatMap(_.toNodeSeq)}
    </path>{
      NodeSeq fromSeq conteudo
        .map(b => <conteudo>
      {b.toNodeSeq}
    </conteudo>)
        .toSeq
    }<subdispositivos>
      {NodeSeq fromSeq subDispositivos.flatMap(_.toNodeSeq)}
    </subdispositivos>
    </dispositivo>

  override def replaceChildren(cl: List[Block]): Dispositivo =
    copy(subDispositivos = cl)


  override val children: List[Block] = subDispositivos

  def makeSubDispositivo(n: Int, p: Paragraph): Option[Dispositivo] =
    rotulo.subRotulo(n).map(r => Dispositivo(r, Some(p), List(), List()))

  def overrideId(newId: String): Dispositivo = copy(_overridenId = Some(newId))
end Dispositivo


case class Omissis(
    abreAspas: Boolean = false,
    fechaAspas: Boolean = false,
    notaAlteracao: Option[String] = None
) extends Block:
  override def toString: String = "Omissis(fechasAspas = " + fechaAspas + ")"

  override lazy val toNodeSeq: Elem = <Omissis/>


case class OL(lis: List[List[Block]]) extends Block:
  override def toString: String =
    "OL " + lis.map(_.mkString("[", ",", "]")).mkString("{", ";", "}")

  override lazy val toNodeSeq: Elem = <ol>
    {
    for (cl <- lis) yield {
      <li>
        {cl}
      </li>
    }
  }
  </ol>
  override val children: List[Block] = lis.flatten


case class Table(elem: Elem) extends Block:
  override def toString: String = "Table"

  override lazy val toNodeSeq: Elem = elem


case class Alteracao(
    blocks: List[Block],
    path: List[Rotulo] = List[Rotulo](),
    pos: Option[Int] = None,
    matches: Option[MatchResult] = None,
    _overridenId: Option[String] = None,
    baseURN: Option[String] = None
) extends Block
    with HasId[Alteracao]:
  override def toString: String = "Alteracao" + blocks.mkString(
    "[",
    ",",
    ";"
  ) + ", matches = " + matches + "]"

  override def mapBlock(f: Block => Block): Block =
    f(Alteracao(blocks.map(_.mapBlock(f)), path, pos))

  override def flatMapBlock(f: Block => List[Block]): List[Block] =
    val d = Alteracao(blocks.flatMap(f), path, pos)
    f(d)

  def mapBlocks(f: List[Block] => List[Block]): Alteracao =
    copy(blocks = f(blocks))

  override lazy val toNodeSeq: NodeSeq =
    <alteracao>
      {
      NodeSeq fromSeq pos
        .map(n => <pos>
      {n}
    </pos>)
        .toSeq
    }<path>
      {NodeSeq fromSeq path.flatMap(_.toNodeSeq)}
    </path>
      <blocks>
        {NodeSeq fromSeq blocks.flatMap(_.toNodeSeq)}
      </blocks>
    </alteracao>

  override def replaceChildren(cl: List[Block]): Alteracao = mapBlocks(_ => cl)

  override val children: List[Block] = blocks

  override val overridenId: Option[String] = _overridenId

  def overrideId(newId: String): Alteracao = copy(_overridenId = Some(newId))
end Alteracao

case class Unrecognized(elem: Seq[Node]) extends Block:
  override def toString: String = "Unrecognized(" + elem + ")"

  override lazy val toNodeSeq: NodeSeq = <unrecognized>
    {NodeSeq fromSeq elem}
  </unrecognized>

case object Image extends Block

object Block extends Block:

  def collectText(nl: List[Node]): List[Node] = {
    val validLabels = List("i", "b", "span", "sub", "sup")

    @tailrec
    def docollect(
        nl: List[Node],
        accum: List[Node] = Nil,
        accum2: List[Node] = Nil
    ): List[Node] = nl match {
      case Nil if accum.isEmpty => accum2.reverse
      case Nil => (<p>
        {NodeSeq fromSeq accum.reverse}
      </p> :: accum2).reverse
      case (t: Text) :: r => docollect(r, t :: accum, accum2)
      case (e: Elem) :: r if validLabels.contains(e.label) =>
        docollect(r, e :: accum, accum2)
      case (e: Elem) :: r if accum.isEmpty => docollect(r, Nil, e :: accum2)
      case (e: Elem) :: r =>
        docollect(
          r,
          Nil,
          e :: <p>
        {NodeSeq fromSeq accum.reverse}
      </p> :: accum2
        )
    }

    docollect(nl, Nil)
  }

  def fromNodes(nodes: List[Node]): List[Block] =
    nodes.flatMap((n: Node) =>
      n match {
        case e: Elem if e.label == "p" && (e.child \\ "p").nonEmpty =>
          val newchilds = e.child.toList.map {
            case e: Elem => e
            case x => <p>
            {x}
          </p>
          }
          fromNodes(newchilds)
        case Elem(_, name, attr, _, cl*)
            if name == "p" || name == "blockquote" =>
          List(
            Paragraph(
              cl,
              attr.asAttrMap.withDefault(_ => "0")("indentation").toDouble,
              attr.asAttrMap.withDefault(_ => "")("centered").equals("true")
            )
          )
        case Elem(_, "table", _, _, _*) => List(Table(n.asInstanceOf[Elem]))
        case e @ Elem(_, "ol", _, _, _*) =>
          val l1 = (e \ "li").toList.collect({
            case Elem(_, "li", _, _, children*) => children.toList
          })
          List(OL(l1.map(nl => fromNodes(collectText(nl)))))
        case Elem(_, "img", _, _, _*) => List(Image)
        case Elem(_, _, _, _, _*) => List(Unrecognized(n.asInstanceOf[Elem]))
        case _                    => List()
      }
    )

  def splitAt(target: Int, nodes: Seq[Node]): (List[Node], List[Node]) =
    def breakit(
        params: (Int, List[Node], List[Node]),
        n: Node
    ): (Int, List[Node], List[Node]) =
      val (p, bef, aft) = params
      if target <= p then
        (p, bef, n :: aft)
      else
        val len = n.text.length
        if target >= (p + len) then
          (p + len, n :: bef, aft)
        else
          n match {
            case a: scala.xml.Atom[_] =>
              a.data match {
                case t: String =>
                  val (t1, t2) = t.splitAt(target - p)
                  (p + len, Text(t1) :: bef, Text(t2) :: aft)
                case x =>
                  throw new RuntimeException(
                    s"unexpected scala.xml object class: ${x.getClass.getName}, object: $x"
                  )
              }
            case Elem(pref, label, attrs, scope, cl*) =>
              val (cl1, cl2) = splitAt(target - p, List(cl: _*))
              val e1 = Elem(pref, label, attrs, scope, true, cl1: _*)
              val e2 = Elem(pref, label, attrs, scope, true, cl2: _*)
              (p + len, e1 :: bef, e2 :: aft)

            case x =>
              throw new RuntimeException(
                s"unexpected scala.xml object class: ${x.getClass.getName}, object: $x"
              )
          }

    val (_, bef, aft) = nodes.foldLeft(0, List[Node](), List[Node]())(breakit)
    (bef.reverse, aft.reverse)
  end splitAt

  val reFimAlteracao: Regex =
    """ *(?:\((ac|nr)\))? *(?:”|“|"|'')(?: *\((ac|nr)\.?\))?$""".r

  def agrupaAlteracoes(blocks: List[Block]): List[Block] =
    blocks.foldRight[List[Block]](Nil) {
      case (a1: Alteracao, (a2: Alteracao) :: r) =>
        Alteracao(a1.blocks ++ a2.blocks, Nil, None) :: r
      case (a1: Alteracao, (p: Paragraph) :: (a2: Alteracao) :: r)
          if p.text == "" =>
        Alteracao(a1.blocks ++ a2.blocks, Nil, None) :: r
      case (b, l) => b :: l
    }

  def reconheceAlteracoes(blocks: List[Block]): List[Block] = agrupaAlteracoes {
    @tailrec
    def procuraFim(
                    blocks: List[Block],
                    acum: List[Block]
                  ): (List[Block], Option[String], List[Block]) =
      blocks match {
        case Nil =>
          throw ParseException(
            AlteracaoSemFechaAspas.in(
              acum.reverse.take(3).collect({ case p: Paragraph => p.text }): _*
            )
          )
        case (p@Paragraph(_, t)) :: rest =>
          val oms = reFimAlteracao.findFirstMatchIn(t)
          oms match {
            case None =>
              procuraFim(rest, p :: acum)
            case Some(m) =>
              val len = m.end - m.start
              val na = Option(m.group(1)).orElse(Option(m.group(2)))
              val p2 = p.cutRight(len).withFechaAspas
              val acum2 = p2 :: acum
              (acum2.reverse, na, rest)
          }
        case (o@OL(lis)) :: rest =>
          lis.lastOption.getOrElse(Nil) match {
            case Nil => procuraFim(rest, o :: acum)
            case lastLi =>
              lastLi.last match {
                case p@Paragraph(_, t) =>
                  val oms = reFimAlteracao.findFirstMatchIn(t)
                  oms match {
                    case None => procuraFim(rest, o :: acum)
                    case Some(m) =>
                      val len = m.end - m.start
                      val na = Option(m.group(1)).orElse(Option(m.group(2)))
                      val p2 = p.cutRight(len).withFechaAspas
                      val lastLi2 = lastLi.init :+ p2
                      val lis2 = lis.init :+ lastLi2
                      val acum2 = OL(lis2) :: acum
                      (acum2.reverse, na, rest)
                  }
              }
          }
        case b :: rest =>
          procuraFim(rest, b :: acum)
      }
    end procuraFim

    def alteraUltimo[T](f: PartialFunction[T, T], l: List[T]): List[T] =
      l.reverse match {
        case Nil => Nil
        case x :: ll =>
          f lift x match {
            case None => l
            case Some(y) => (y :: ll).reverse
          }
      }

    @tailrec
    def reconheceInicio(blocks: List[Block], acum: List[Block]): List[Block] =
      blocks match {
        case (p@Paragraph(_, t)) :: rest
          if t.startsWith("“") || t.startsWith("\"") || t.startsWith("”") || t
            .startsWith("''") =>
          val l = p.nodes.text.takeWhile(_.isWhitespace).length + 1
          val p2 = p.cutLeft(l).withAbreAspas
          val (balt, na, rest2) = procuraFim(p2 :: rest, List[Block]())
          val balt2 = na
            .map(n =>
              alteraUltimo[Block](
                { case p: Paragraph => p.withNotaAlteracao(n) },
                balt
              )
            )
            .getOrElse(balt)
          val alt = Alteracao(balt2)
          reconheceInicio(rest2, alt :: acum)
        case (pp: Paragraph) :: Paragraph(_, t) :: rest if t.length == 0 =>
          reconheceInicio(pp :: rest, acum)

        case (o@OL(lis)) :: rest =>
          lis.headOption.getOrElse(List()) match {
            case Nil => reconheceInicio(rest, o :: acum)
            case (p@Paragraph(_, t)) :: tailLi =>
              if t.startsWith("“") || t.startsWith("\"") || t.startsWith("”") then
                val l = p.nodes.text.takeWhile(_.isWhitespace).length + 1
                val p2 = p.cutLeft(l).withAbreAspas
                val o2 = OL((p2 :: tailLi) :: lis.tail)
                val (balt, na, rest2) = procuraFim(o2 :: rest, List[Block]())
                val balt2 = na
                  .map(n =>
                    alteraUltimo[Block](
                      { case p: Paragraph => p.withNotaAlteracao(n) },
                      balt
                    )
                  )
                  .getOrElse(balt)
                val alt = Alteracao(balt2)
                reconheceInicio(rest2, alt :: acum)
              else
                reconheceInicio(rest, o :: acum)
          }
        case b :: rest => reconheceInicio(rest, b :: acum)
        case Nil => acum.reverse
      }
    end reconheceInicio

    reconheceInicio(blocks, List[Block]())
  }

  def spanNivel(nivel: Int, bl: List[Block]): (List[Block], List[Block]) =
    def proxSpan(l: List[Block]): (List[Block], List[Block]) =
      val (omissis, posOmissis) = l.span(_.isInstanceOf[Omissis])
      val (nivelSuperior, resto) = posOmissis.span({
        case d: Dispositivo => d.rotulo.nivel > nivel
        case _              => false
      })
      if nivelSuperior.nonEmpty then
        val (l1, l2) = proxSpan(resto)
        (omissis ++ nivelSuperior ++ l1, l2)
      else if nivel == niveis.artigo then (omissis, posOmissis)
      else  (Nil, l)
    proxSpan(bl)
  end spanNivel

  def onlyDispositivos(bl: List[Block]): List[Dispositivo] = bl.collect({
    case d: Dispositivo => d
  })

  def hasAlteracao(d: Dispositivo): Boolean = d.subDispositivos.exists {
    case d1: Dispositivo => hasAlteracao(d1)
    case _: Alteracao    => true
    case _               => false
  }

  def hasFechaAspas(b: Block): Boolean = b match {
    case d: Dispositivo if d.fechaAspas => true
    case d: Dispositivo             => d.subDispositivos.exists(hasFechaAspas)
    case o: Omissis if o.fechaAspas => true
    case _                          => false
  }

  def lastIsFechaAspas(b: Block): Boolean = b match {
    case d: Dispositivo if d.fechaAspas => true
    case d: Dispositivo => d.subDispositivos.lastOption.exists(lastIsFechaAspas)
    case o: Omissis if o.fechaAspas => true
    case _                          => false
  }

  def hasOmissis(b: Block): Boolean = b match {
    case _: Omissis => true
    case d: Dispositivo =>
      (d.rotulo, d.conteudo) match {
        case (_, Some(_: Omissis)) => true
        case (r, None) if !r.isAgregador && !r.isInstanceOf[RotuloArtigo] =>
          true
        case (r, Some(p: Paragraph))
            if !r.isAgregador && !r
              .isInstanceOf[RotuloArtigo] && p.text.isEmpty =>
          true
        case _ => d.subDispositivos.exists(hasOmissis)
      }
    case _ => false
  }

  def organizaDispositivos(
      blocks: List[Block],
      dentroAlteracao: Boolean = false
  ): List[Block] =
    @tailrec
    def agrupa(b: Block, bl: List[Block]): List[Block] =
      @tailrec
      def spanUpToEvidenciaAlteracao(
          bl: List[Block],
          rl: List[Block] = Nil
      ): Option[(List[Block], List[Block])] = bl match {
        case (o: Omissis) :: bl1   => Some((o :: rl).reverse, bl1)
        case (a: Alteracao) :: bl1 => Some((a :: rl).reverse, bl1)
        case (d: Dispositivo) :: bl1
            if !hasAlteracao(d) && (lastIsFechaAspas(d) || hasOmissis(d)) =>
          Some(d :: rl, bl1)
        case (p: Paragraph) :: bl1 if p.text.isEmpty =>
          spanUpToEvidenciaAlteracao(bl1, p :: rl)
        case _ => None
      }

      (b, bl) match {
        case (a1: Alteracao, (a2: Alteracao) :: r) =>
          (a1 copy (blocks = a1.blocks ++ a2.blocks)) :: r

        case (o: Omissis, (a: Alteracao) :: rest) =>
          (a copy (blocks = o :: a.blocks)) :: rest

        case (o: Omissis, (o1: Omissis) :: rest)
            if !o.fechaAspas && o.notaAlteracao.isEmpty =>
          val abreAspas = o.abreAspas || o1.abreAspas
          val fechaAspas = o1.fechaAspas
          val notaAlteracao = o1.notaAlteracao
          agrupa(
            o copy (abreAspas = abreAspas, fechaAspas =
              fechaAspas, notaAlteracao = notaAlteracao),
            rest
          )

        case (d: Dispositivo, (a: Alteracao) :: rest)
            if !hasAlteracao(d) && !hasFechaAspas(d) && hasOmissis(d) =>
          (a copy (blocks = d :: a.blocks)) :: rest

        case (a: Alteracao, bl) =>
          spanUpToEvidenciaAlteracao(bl) match {
            case Some((inAlt, outAlt)) =>
              inAlt.lastOption match {
                case Some(a1: Alteracao) =>
                  agrupa(a copy (blocks = a.blocks ++ inAlt.init), a1 :: outAlt)
                case Some(_) =>
                  agrupa(a copy (blocks = a.blocks ++ inAlt), outAlt)
                case None => a :: bl
              }
            case _ => a :: bl
          }
        case (d: Dispositivo, (p: Paragraph) :: bl)
            if d.conteudo.isEmpty && d.subDispositivos.isEmpty && d.rotulo.isAgregador =>
          agrupa(d copy (conteudo = Some(p)), bl)
        case (
              d @ Dispositivo(
                rotulo,
                Some(p1: Paragraph),
                Nil, _, _, _, _, _, _,_),
              (p: Paragraph) :: bl
            )
            if rotulo.isAgregador && !d.fechaAspas && d.notaAlteracao.isEmpty && !p.abreAspas =>
          val prev = if p1.nodes.text.trim.isEmpty then NodeSeq.Empty else p1.nodes ++ Text(" ")
          agrupa(
            d copy (conteudo = Some(Paragraph(prev ++ p.nodes)),
            fechaAspas = p.fechaAspas, notaAlteracao = p.notaAlteracao),
            bl
          )

        case (d: Dispositivo, (p: Paragraph) :: bl)
            if !d.fechaAspas &&
              d.notaAlteracao.isEmpty &&
              d.subDispositivos.isEmpty &&
              !p.fechaAspas &&
              p.notaAlteracao.isEmpty &&
              d.conteudo
                .collect({
                  case p1: Paragraph =>
                    p1.text.nonEmpty && (p1.text.endsWith(",") || p1.text
                      .endsWith(";") || p1.text.endsWith("-") ||
                      p.text.headOption.exists(Character.isLowerCase))
                  case _ => false
                })
                .getOrElse(false) =>
          val p1 = d.conteudo.get.asInstanceOf[Paragraph]
          val d1 = d.mapConteudo(_ =>
            Some(p1.copy(nodes = p1.nodes ++ List(Text(" ")) ++ p.nodes))
          )
          agrupa(
            d1 copy (fechaAspas = p.fechaAspas, notaAlteracao =
              p.notaAlteracao),
            bl
          )
        case (d1: Dispositivo, bl2) =>
          spanNivel(d1.rotulo.nivel, bl2) match {
            case (Nil, (a: Alteracao) :: bl4) =>
              agrupa(
                d1 copy (subDispositivos =
                  d1.subDispositivos :+ (a copy (blocks = organizaDispositivos(
                    reconheceOmissisVazio2(a.blocks),
                    dentroAlteracao = true
                  )))),
                bl4
              )

            case (nivelSuperior @ _ :: _, resto) =>
              val dd = nivelSuperior.foldLeft(d1)((d, a) =>
                d copy (subDispositivos = d.subDispositivos :+ a)
              )
              dd :: resto

            case _ => b :: bl

          }
        case _ => b :: bl
      }
    end agrupa

    blocks.foldRight(List[Block]())(agrupa)
  end organizaDispositivos

  def pushLastOmissis(bl: List[Block]): List[Block] = {
    def pushit(b: Block): List[Block] = b match {
      case d: Dispositivo =>
        val cl = pushitL(d.subDispositivos)
        cl.lastOption match {
          case Some(o: Omissis) => List(d copy (subDispositivos = cl.init), o)
          case _                => List(d copy (subDispositivos = cl))
        }
      case x => List(x)
    }

    def pushitL(bl: List[Block]): List[Block] = bl.flatMap(pushit)

    def onAlteracao(b: Block): Block = b match {
      case a: Alteracao => a copy (blocks = pushitL(a.blocks))
      case x            => x
    }

    def onAlteracaoL(bl: List[Block]): List[Block] = bl.map(onAlteracao)

    onAlteracaoL(bl)
  }

  def numeraDispositivosGenericos(bl: List[Block]): List[Block] =
    var pos = 0
    def numera(b: Block): List[Block] =
      b.flatMapChildren(numera) match {
        case d: Dispositivo =>
          d.rotulo match {
            case r: RotuloDispositivoGenerico =>
              val d1 = d.copy(rotulo = r.copy(num = pos))
              pos = pos + 1
              List(d1)
            case _ => List(d)
          }
        case _ => List(b)
      }
    bl.flatMap(numera)
  end numeraDispositivosGenericos

  def limpaParagrafosVazios(bl: List[Block]): List[Block] =
    def limpa(b: Block): Option[Block] = b match {
      case d: Dispositivo =>
        Some(
          d copy (subDispositivos = limpaParagrafosVazios(d.subDispositivos))
        )
      case p: Paragraph if p.text.isEmpty => None
      case a: Alteracao =>
        Some(a copy (blocks = limpaParagrafosVazios(a.blocks)))
      case x => Some(x)
    }
    bl.flatMap(limpa)
  end limpaParagrafosVazios


  def splitLi(li: List[Block]): (Option[Paragraph], List[Block]) = li match {
    case (p: Paragraph) :: r => (Some(p), r)
    case _                   => (None, Nil)
  }

  def reconheceDispositivos(blocks: List[Block]): List[Block] =
    val f: (List[Block], Block) => List[Block] = {
      case ((d: Dispositivo) :: prev, OL(lis)) =>
        val ll = for ((li, i) <- lis.zipWithIndex) yield {
          val (p, liRest) = splitLi(li)
          p match {
            case None => liRest.reverse
            case Some(pp) =>
              d.makeSubDispositivo(i + 1, pp) match {
                case None    => Nil
                case Some(b) => reconheceDispositivos(b :: liRest)
              }
          }

        }
        ll.reverse.flatten ++ (d :: prev)
      case (prev, p: Paragraph) => p.dispositivoIfPossible.reverse ++ prev
      case (prev, _: OL)        => prev
      case (prev, a: Alteracao) => a.mapBlocks(reconheceDispositivos) :: prev
      case (prev, x)            => x :: prev
    }
    blocks.foldLeft(List[Block]())(f).reverse
  end reconheceDispositivos

  val isParagraph: Block => Boolean = {
    case Paragraph(_, _) => true;
    case _               => false
  }

  def joinParagraphs(l: List[Block]): List[Block] =
    l.foldRight(List[Block]())((b: Block, l: List[Block]) =>
      b match {
        case p @ Paragraph(n1, text1) =>
          val nodes1 = n1.toList
          l match {
            case Paragraph(n2, text2) :: bl =>
              val nodes2 = n2.toList
              val nodes: List[Node] = if text1.nonEmpty && text2.nonEmpty then
                if !text1.endsWith(" ") && !text2.startsWith(" ") then
                  val revNodes1 = nodes1.reverse
                  revNodes1 match {
                    case Text(lastText1) :: revTail1 =>
                      nodes2 match {
                        case Text(firstText2) :: tail2 =>
                          revTail1.reverse ++ (Text(
                            lastText1 + " " + firstText2
                          ) :: tail2)
                        case _ =>
                          revTail1.reverse ++ (Text(lastText1 + " ") :: nodes2)
                      }
                    case _ =>
                      nodes2 match {
                        case Text(firstText2) :: tail2 =>
                          nodes1 ++ (Text(" " + firstText2) :: tail2)
                        case _ => nodes1 ++ List(Text(" ")) ++ nodes2
                      }
                  }
                else  nodes1 ++ nodes2
              else nodes1 ++ nodes2

              Paragraph(nodes, p.indentation) :: bl
            case _ => b :: l
          }
        case _ => b :: l
      }
    )
  end joinParagraphs

  def validaEstrutura(): Unit = {} // FIXME

  def identificaPaths(blocks: List[Block]): List[Block] =
    def createPath(tail: List[Rotulo])(b: Block): Block = b match {
      case d: Dispositivo =>
        val path = d.rotulo match {
          case _: RotuloArtigo =>
            d.rotulo :: tail.dropWhile(_.isInstanceOf[RotuloAgregador])
          case _ => d.rotulo :: tail
        }
        d copy (subDispositivos =
          d.subDispositivos.map(createPath(path)), path = path)
      case a: Alteracao =>
        a.pos match {
          case None => throw new RuntimeException("pos is none in Alteracao")
          case Some(num) =>
            val path = RotuloAlteracao(num) :: tail
            a copy (blocks = a.blocks.map(createPath(path)), path = path)
        }
      case _ => b
    }

    blocks.map(createPath(List()))
  end identificaPaths


  def numeraDispsGenericos(blocks: List[Block]): List[Block] =
    val blocks1 = blocks
      .foldLeft((List[Block](), 1)) {
        case ((l, n), d: Dispositivo) =>
          val subDisps = numeraDispsGenericos(d.subDispositivos)
          val (d1, n1) = d.rotulo match {
            case r: RotuloDispositivoGenerico =>
              r.num match {
                case 0 =>
                  val r1 = r.copy(num = n)
                  (d.copy(rotulo = r1, path = r1 :: d.path.tail), n + 1)
                case nn => (d, math.max(nn + 1, n))
              }
            case _ => (d, n + 1)
          }
          (d1.copy(subDispositivos = subDisps) :: l, n1)
        case ((l, n), a: Alteracao) =>
          val subDisps = numeraDispsGenericos(a.blocks)
          (a.copy(blocks = subDisps) :: l, n)
        case ((l, n), x) => (x :: l, n)
      }
      ._1
      .reverse
    blocks1
  end numeraDispsGenericos

  def reconheceOmissis(blocks: List[Block]): List[Block] =
    blocks.mapConserve(_.mapBlock(_.recognizeOmissis))

  def reconheceOmissisVazio(b: Block): Block =
    def h(ob: Option[Block]) = ob match {
      case None => Some(Omissis())
      case Some(p: Paragraph) if p.text == "" =>
        Some(Omissis(p.abreAspas, p.fechaAspas, p.notaAlteracao))
      case x => x
    }

    b match {
      case d: Dispositivo =>
        d.rotulo match {
          case _: RotuloArtigo => d
          case _               => d.mapConteudo(h)
        }
      case p: Paragraph if p.text.isEmpty =>
        Omissis(p.abreAspas, p.fechaAspas, p.notaAlteracao)
      case _ => b
    }
  end reconheceOmissisVazio

  def reconheceOmissisVazio2(blocks: List[Block]): List[Block] =
    blocks.map(_.mapBlock(reconheceOmissisVazio))

  def reconheceOmissisVazio(blocks: List[Block]): List[Block] =
    def f(b: Block): (List[Block], Boolean) = b match {
      case a: Alteracao =>
        (List(a.copy(blocks = reconheceOmissisVazio2(a.blocks))), false)
      case x => (List(x), true)
    }

    blocks.flatMap(_.topDownUntil(f))
  end reconheceOmissisVazio


  def numeraAlteracoes(blocks: List[Block]): List[Block] =
    def numera(blp: (List[Block], Int), b: Block) =
      val (bl, p) = blp
      b match {
        case a: Alteracao =>
          (
            (a copy (blocks = numeraAlteracoes(a.blocks), pos = Some(p))) :: bl,
            p + 1
          )
        case d: Dispositivo =>
          (
            (d copy (subDispositivos =
              numeraAlteracoes(d.subDispositivos))) :: bl,
            p
          )
        case _ => (b :: bl, p)
      }
    end numera

    val (rbl, _) = blocks.foldLeft((List[Block](), 1))(numera)
    rbl.reverse
  end numeraAlteracoes

  def identificaTextosAgregadores(
      blocks: List[Block],
      nivel: Int = 0
  ): List[Block] =
    @tailrec
    def buscaInicio(bl: List[Block]): (List[Paragraph], List[Block]) =
      bl match {
        case (p: Paragraph) :: r if p.isEmpty => buscaInicio(r)
        case (p: Paragraph) :: r =>
          val (r1, r2) = r.span({
            case p: Paragraph => !p.isEmpty; case _ => false
          })
          (p :: r1.collect({ case p: Paragraph => p }), r2)
        case _ => (Nil, bl)
      }

    def altera(bl: List[Block]): List[Block] =
      @tailrec
      def doit(bl: List[Block], res: List[Block]): List[Block] = bl match {
        case (d: Dispositivo) :: r if d.rotulo.isAgregador =>
          val (i, rr) = buscaInicio(r)
          val prevList = d.conteudo
            .collect({ case p: Paragraph => p })
            .map(_.nodes)
            .getOrElse(List())
          val nl = (prevList :: i.map(_.nodes)).filter(ns =>
            ns.text.trim.nonEmpty
          ) match {
            case first :: rest => first ++ rest.flatMap(Text(" ") ++ _)
            case Nil           => Nil
          }

          val (fa, na) = i.lastOption
            .map(x => (x.fechaAspas, x.notaAlteracao))
            .getOrElse((d.fechaAspas, d.notaAlteracao))
          doit(
            rr,
            d.copy(
              conteudo = Some(Paragraph(nl)),
              fechaAspas = fa,
              notaAlteracao = na
            ) :: res
          )
        case x :: r => doit(r, x :: res)
        case Nil    => res.reverse
      }

      doit(bl, Nil)
    end altera

    altera(blocks.map({
      case b: Dispositivo =>
        b.replaceChildren(identificaTextosAgregadores(b.children, nivel + 1))
      case b: Alteracao =>
        b.replaceChildren(identificaTextosAgregadores(b.children, nivel + 1))
      case x => x
    }))
  end identificaTextosAgregadores

  def identificaTitulos(blocks: List[Block], nivel: Int = 0): List[Block] =
    blocks
      .map({
        case b: Dispositivo =>
          b.replaceChildren(identificaTitulos(b.children, nivel + 1))
        case b: Alteracao =>
          b.replaceChildren(identificaTitulos(b.children, nivel + 1))
        case x => x
      })
      .foldLeft(List[Block]()) {
        case (bl, d: Dispositivo) if d.titulo.isEmpty =>
          bl.dropWhile({
            case p: Paragraph if p.isEmpty => true; case _ => false
          }) match {
            case (p: Paragraph) :: r if podeSerTitulo(p.text) =>
              (d copy (titulo = Some(p), abreAspas =
                d.abreAspas | p.abreAspas)) :: r
            case _ => d :: bl
          }
        case (l, x) => x :: l
      }
      .reverse

  def podeSerTitulo(s: String): Boolean =
    !s.toList.exists(List[Char](';', '.', ':').contains(_))

  def corrigeRotuloParte(blocks: List[Block]): List[Block] =
    val blocks1 = blocks
      .foldLeft((Vector[Block](), 0)) {
        case ((blks, partePos), d: Dispositivo) =>
          d.rotulo match {
            case RotuloParte(Right(n), _, _, _, _) => (blks :+ d, n)
            case r @ RotuloParte(Left(txt), _, _, _, _) =>
              val d1 = d.copy(rotulo =
                r.copy(num = Right(partePos + 1), rotulo = Some(txt))
              )
              (blks :+ d1, partePos + 1)
            case _ => (blks :+ d, partePos)
          }
        case ((blks, n), x) => (blks :+ x, n)
      }
      ._1
    blocks1
      .map { b =>
        b.replaceChildren(corrigeRotuloParte(b.children))
      }
      .to(List)
  end corrigeRotuloParte
end Block