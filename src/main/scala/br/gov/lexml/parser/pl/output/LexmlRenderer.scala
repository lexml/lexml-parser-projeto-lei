package br.gov.lexml.parser.pl.output

import br.gov.lexml.parser.pl.metadado.Metadado
import br.gov.lexml.parser.pl.rotulo.*
import br.gov.lexml.parser.pl.block.*
import br.gov.lexml.parser.pl.ProjetoLei
import br.gov.lexml.parser.pl.rotulo.rotuloParser.{Fem, Genero}

import scala.annotation.tailrec
import scala.xml.*
import scala.collection.immutable.List

object LexmlRenderer:
  private def rename(label: String, n: NodeSeq): NodeSeq = n match {
    case Elem(prefix, _, attributes, scope, child*) =>
      Elem(prefix, label, attributes, scope, true, child*)
    case _ => n
  }

  @inline
  private def elemLabel(r: Rotulo): String = r match {
    case _: RotuloArtigo              => "Artigo"
    case RotuloParagrafo(None, _, _)  => "Caput"
    case _: RotuloParagrafo           => "Paragrafo"
    case _: RotuloInciso              => "Inciso"
    case _: RotuloAlinea              => "Alinea"
    case _: RotuloItem                => "Item"
    case _: RotuloDispositivoGenerico => "DispositivoGenerico"
    case RotuloPena                   => "Pena"
    case _: RotuloParte               => "Parte"
    case _: RotuloLivro               => "Livro"
    case _: RotuloTitulo              => "Titulo"
    case _: RotuloSubTitulo           => "Subtitulo"
    case _: RotuloCapitulo            => "Capitulo"
    case _: RotuloSubCapitulo         => "Subcapitulo"
    case _: RotuloSecao               => "Secao"
    case _: RotuloSubSecao            => "Subsecao"
    case _: RotuloAlteracao           => "Alteracao"
  }

  @inline private def renderNumeral(num: Int): String =
    if num > 1000 then "%s.%03d".format(renderNumeral(num / 1000), num % 1000) else num.toString


  @inline private def renderOrdinal(num: Int): String =
    renderNumeral(num) + (if num < 10 then "º" else "")

  @inline private def renderComp(onum: Option[Int]): String =
    onum.map(n => "-" + renderAlphaSeq(n).toUpperCase).getOrElse("")

  @inline private def renderRomano(num: Int): String =
    def rom(cX: String, cV: String, cI: String, d: Int): String = d match {
      case 0           => ""
      case 9           => cI + cX
      case 4           => cI + cV
      case _ if d >= 5 => cV + (cI * (d - 5))
      case _           => cI * d
    }
    ("M" * (num / 1000)) + rom("M", "D", "C", (num / 100) % 10) + rom(
      "C",
      "L",
      "X",
      (num / 10) % 10
    ) + rom("X", "V", "I", num % 10)

  @inline private def renderAlphaSeq(num: Int): String =
    def rend(n: Int): String = n match {
      case 0 => ""
      case _ =>
        val nn = n - 1
        rend(nn / 26) + ('a' + (nn % 26)).asInstanceOf[Char]
    }
    rend(num + 1)

  private class RenderException(msg: String) extends RuntimeException(msg)

  private implicit class Unico(un: Boolean):
    @inline def unicoChar : String = if un then "u" else ""
    @inline def unicoMajStr(alt: => String): String = if un then "ÚNICO" else alt
    @inline def unicaMajStr(alt: => String): String = if un then "ÚNICA" else alt
    @inline def unicoMinStr(alt: => String): String = if un then "Único" else alt
    @inline def unicaMinStr(alt: => String): String = if un then "Única" else alt

  private val ordDez: Map[Int, String] = Map(
    1 -> "décim",
    2 -> "vigésim",
    3 -> "trigésim",
    4 -> "quadragésim",
    5 -> "quinquagésim",
    6 -> "sexagésim",
    7 -> "septuagésim",
    8 -> "octagésim",
    9 -> "nonagésim"
  )
  private val ordUnid: Map[Int, String] = Map(
    1 -> "primeir",
    2 -> "segund",
    3 -> "terceir",
    4 -> "quart",
    5 -> "quint",
    6 -> "sext",
    7 -> "sétim",
    8 -> "oitav",
    9 -> "non"
  )
  private def renderOrdinalExtenso(
      num: Int,
      g: Genero,
      allCaps: Boolean = false,
      firstCaps: Boolean = false
  ): String =
    val s = g.select("o", "a")
    if num > 99 then
      throw new RuntimeException(
        "Não há suporte à renderização de ordinais por extenso acima de 99: " + num
      )
    else if num < 1 then
      throw new RuntimeException(
        "Não há suporte à renderização de ordinais por extenso menores do que 1: " + num
      )
    end if
    val d = num / 10
    val u = num % 10
    val ds = if d > 0 then ordDez(d) + s else ""
    val us = if u > 0 then ordUnid(u) + s else ""
    val rs1 = if ds != "" && us != "" then ds + "-" + us else ds + us
    if allCaps then rs1.toUpperCase()
    else if firstCaps then
      rs1.substring(0, 1).toUpperCase + rs1.substring(1)
    else rs1
  end renderOrdinalExtenso

  private def renderRotuloEither(r: Rotulo): Either[String, String] = r match {
    case RotuloArtigo(1, None, true) => Left("Artigo único. ")
    case RotuloArtigo(num, comp, _) =>
      Left(
        "Art. " + renderOrdinal(num) + renderComp(comp) +
          (if num >= 10 || comp.isDefined then "." else "")
      )
    case RotuloParagrafo(None, _, _)          => Right("Caput")
    case RotuloParagrafo(Some(1), None, true) => Left("Parágrafo único.")
    case RotuloParagrafo(Some(num), comp, _) =>
      Left(
        "§ " + renderOrdinal(num) + renderComp(comp)
          + (if num >= 10 || comp.isDefined then "." else "")
      )
    case RotuloInciso(num, comp) =>
      Left(renderRomano(num).toUpperCase + renderComp(comp) + " –")
    case RotuloAlinea(num, comp) =>
      Left(renderAlphaSeq(num - 1).toLowerCase + renderComp(comp) + ")")
    case RotuloItem(num, comp)              => Left(s"$num${renderComp(comp)}.")
    case RotuloPena                         => Left("Pena –")
    case d: RotuloDispositivoGenerico       => Left(s"${d.nomeRotulo} –")
    case RotuloParte(Left(rot), _, _, _, _) => Left("PARTE " + rot)
    case RotuloParte(_, _, _, _, Some(rot)) => Left("PARTE " + rot)
    case RotuloParte(_, _, true, _, _)      => Left("PARTE ÚNICA")
    case RotuloParte(Right(num), comp, _, true, _) =>
      Left(
        "PARTE " + renderOrdinalExtenso(
          num = num,
          g = Fem,
          allCaps = true
        ).toUpperCase + renderComp(comp)
      )
    case RotuloParte(Right(num), comp, _, _, _) =>
      Left(renderRomano(num).toUpperCase + renderComp(comp))
    case RotuloLivro(Left(rot), _, _) =>
      throw new RenderException(
        s"Livro sem número não suportado na renderização: $rot"
      )
    case RotuloLivro(Right(num), comp, unico) =>
      Left(
        "LIVRO " + unico.unicoMajStr(
          renderRomano(num).toUpperCase + renderComp(comp)
        )
      )
    case RotuloTitulo(num, comp, unico) =>
      Left("TÍTULO " + unico.unicoMajStr(renderRomano(num) + renderComp(comp)))
    case RotuloSubTitulo(num, comp, unico) =>
      Left(
        "SUBTÍTULO " + unico.unicoMajStr(renderRomano(num) + renderComp(comp))
      )
    case RotuloCapitulo(num, comp, unico) =>
      Left(
        "CAPÍTULO " + unico.unicoMajStr(renderRomano(num) + renderComp(comp))
      )
    case RotuloSubCapitulo(num, comp, unico) =>
      Left(
        "SUB-CAPÍTULO " + unico.unicoMajStr(
          renderRomano(num) + renderComp(comp)
        )
      )
    case RotuloSecao(num, comp, unica) =>
      Left("Seção " + unica.unicaMinStr(renderRomano(num) + renderComp(comp)))
    case RotuloSubSecao(num, comp, unica) =>
      Left(
        "Subseção " + unica.unicaMinStr(renderRomano(num) + renderComp(comp))
      )
    case RotuloAlteracao(num) => Right("Alteracao " + num)
    case x =>
      throw new RuntimeException(
        "Lexml Xml renderer. Elemento não esperado:" + x
      )
  }

  private def renderRotulo(r: Rotulo): Option[String] =
    renderRotuloEither(r).fold(Some(_), _ => None)

  def renderRotulo2(r: Rotulo): String = renderRotuloEither(r).merge

  def renderCompId(n: Option[Int]) : String =
    n.map(n => "-" + (n + 1).toString).getOrElse("")

  def renderId(r: Rotulo): String = r match {
    case RotuloArtigo(num, comp, unico) =>
      s"art$num${if num == 1 && unico then "u" else ""}${renderCompId(comp)}"
    case RotuloParagrafo(None, _, _) => "cpt"
    case RotuloParagrafo(Some(num), comp, unico) =>
      s"par$num${if num == 1 && unico then "u" else ""}${renderCompId(comp)}"
    case RotuloInciso(num, comp) => s"inc$num${renderCompId(comp)}"
    case RotuloAlinea(num, comp) => s"ali$num${renderCompId(comp)}"
    case RotuloItem(num, comp)   => s"ite$num${renderCompId(comp)}"
    case RotuloPena              => "pena"
    case RotuloParte(Left(rot), _, _, _, _) =>
      throw new RenderException(
        s"Parte sem número não suportado na renderização: $rot"
      )
    case RotuloParte(Right(num), comp, unico, _, _) =>
      s"prt$num${unico.unicoChar}${renderCompId(comp)}"
    case RotuloLivro(Left(rot), _, _) =>
      throw new RenderException(
        s"Livro sem número não suportado na renderização: $rot"
      )
    case RotuloLivro(Right(num), comp, unico) =>
      s"liv$num${unico.unicoChar}${renderCompId(comp)}"
    case RotuloTitulo(num, comp, unico) =>
      s"tit$num${unico.unicoChar}${renderCompId(comp)}"
    case RotuloSubTitulo(num, comp, unico) =>
      s"stt$num${unico.unicoChar}${renderCompId(comp)}"
    case RotuloCapitulo(num, comp, unico) =>
      s"cap$num${unico.unicoChar}${renderCompId(comp)}"
    case RotuloSubCapitulo(_, _, _) =>
      throw new RenderException("Sub-capítulo não suportado pelo parser")
    case RotuloSecao(num, comp, unico) =>
      s"sec$num${unico.unicoChar}${renderCompId(comp)}"
    case RotuloSubSecao(num, comp, unico) =>
      s"sub$num${unico.unicoChar}${renderCompId(comp)}"
    case RotuloAlteracao(num) => s"alt$num"
    case x =>
      throw new RuntimeException(
        s"Lexml Xml renderer. Elemento não esperado: $x"
      )
  }

  def renderId(path: List[Rotulo]): String =
    path.reverse.map(renderId).mkString("", "_", "")

  @inline private def renderBlocks(bl: Seq[Block], idPai: String): NodeSeq =
    NodeSeq fromSeq bl.foldLeft((List[Node](), 0))(render(idPai))._1.reverse

  private def cleanBs(ns: NodeSeq): NodeSeq =
    ns.flatMap {
      case e: Elem if e.label == "b" => cleanBs(e.child)
      case e: Elem                   => e copy (child = cleanBs(e.child))
      case n                         => NodeSeq fromSeq n
    }

  @inline @tailrec private def cleanTopBIs(ns: NodeSeq): NodeSeq = (ns.length, ns.headOption) match {
    case (1, Some(e: Elem)) if e.label == "b" || e.label == "i" =>
      cleanTopBIs(e.child)
    case _ => ns
  }

  private def addBaseURN(b: Option[String]): Elem => Elem = (e: Elem) =>
    b match {
      case None    => e
      case Some(u) => e % new PrefixedAttribute("xml", "base", u, Null)
    }

  private def addXlinkHref(b: Option[String]): Elem => Elem = (e: Elem) =>
    b match {
      case None    => e
      case Some(u) => e % new PrefixedAttribute("xlink", "href", u, Null)
    }

  private def addXlinkHref(id: String): Elem => Elem = addXlinkHref(
    id.lastIndexOf("_alt") match {
      case n if n > 0 =>
        val id2 = id.substring(n + 4).dropWhile(!Character.isLetter(_))
        id2.length match {
          case 0 => None
          case _ => Some(id2)
        }
      case _ => None
    }
  )

  def render(idPai: String): ((List[Node], Int), Block) => (List[Node], Int) =
    case ((nl, omissisCount), b) =>
      val el = b match {
        case d: Dispositivo =>
          val xml = rename(
            elemLabel(d.rotulo),
            addXlinkHref(d.id)(<Dispositivo id={d.id} textoOmitido={
              d.conteudo match {
                case _ if d.rotulo.isAgregador          => null
                case Some(_: Omissis)                   => "s"
                case Some(p: Paragraph) if p.text == "" => "s"
                case _                                  => null
              }
            } abreAspas={
              if d.abreAspas then "s" else null
            } fechaAspas={
              if d.fechaAspas then "s" else null
            } notaAlteracao={d.notaAlteracao.map(_.toUpperCase).orNull}
          	nome={
              d.rotulo match {
                case dd: RotuloDispositivoGenerico => dd.nomeRotulo
                case _                             => null
              }
            }>
            {d.links.map(l => Comment(s"Link:$l"))}
            {
              d.titulo
                .map(tit =>
                  <TituloDispositivo>{cleanBs(tit.nodes)}</TituloDispositivo>
                )
                .getOrElse(NodeSeq.Empty)
            }
            {
              renderRotulo(d.rotulo) match {
                case Some(r) => <Rotulo>{r}</Rotulo>
                case _       => NodeSeq.Empty
              }
            }
            {
              d.conteudo
                .filter(!_.isInstanceOf[Omissis])
                .map(b => {
                  val e = b.toNodeSeq.asInstanceOf[Elem]
                  val e2 = e.copy(child = cleanTopBIs(e.child))
                  if (d.rotulo.isAgregador) {
                    rename("NomeAgrupador", e2)
                  } else { e2 }
                })
                .getOrElse(NodeSeq.Empty)
            }
            {renderBlocks(d.subDispositivos, d.id + "_")}
          </Dispositivo>)
          ).toList
          xml
        case a: Alteracao =>
          addBaseURN(a.baseURN)(<Alteracao id={a.id}>              
              {NodeSeq fromSeq renderBlocks(a.blocks, a.id + "_").toList}
            </Alteracao>).toList
        case Omissis(abreAspas, fechaAspas, notaAlteracao) =>
          List(<Omissis id={idPai + "omi" + (omissisCount + 1)} abreAspas={
            if abreAspas then "s" else null
          } fechaAspas={
            if fechaAspas then "s" else null
          } notaAlteracao={notaAlteracao.map(_.toUpperCase).orNull}/>)
        case p: Paragraph =>
          List(
            <p>{NodeSeq fromSeq p.nodes}</p>
          )
        case Table(elem) => List(elem)
        case _           => List[Node]()
      }
      (
        el ++ nl,
        omissisCount + (b match { case _: Omissis => 1; case _ => 0 })
      )
  end render

  def renderArticulacao(blocks: List[Block]): Elem = <Articulacao>{
    renderBlocks(blocks, "")
  }</Articulacao>

  @inline private def renderParagraphWithoutP(b: Block) = b match {
    case p: Paragraph => p.nodes
    case _            => NodeSeq.Empty
  }

  @inline private def renderMetadado(m: Metadado): NodeSeq =
    <Metadado>
      <Identificacao URN={m.urn}/>
    </Metadado>
  @inline private def renderParteInicial(pl: ProjetoLei): NodeSeq =
    <ParteInicial>
      <Epigrafe id="epigrafe">{renderParagraphWithoutP(pl.epigrafe)}</Epigrafe>
		  {
      pl.ementa
        .map(x =>
          <Ementa id="ementa">{cleanTopBIs(renderParagraphWithoutP(x))}</Ementa>
        )
        .getOrElse(NodeSeq.Empty)
    }
      <Preambulo id="preambulo">{
      NodeSeq fromSeq pl.preambulo.flatMap(p => cleanBs(p.toNodeSeq))
    }</Preambulo>
    </ParteInicial>

  import scala.xml.Utility.trim

  def render(pl: ProjetoLei): Elem =
    val norma =
      <Norma>
          {renderParteInicial(pl)}
          {renderArticulacao(pl.articulacao)}
        </Norma>
    // FIXME: tirar o comentário para habilitar
    val encloseInProjetoNorma = true // pl.metadado.isProjetoNorma
    val inner = if (encloseInProjetoNorma) {
      <ProjetoNorma>{norma}</ProjetoNorma>
    } else { norma }
    <LexML xmlns="http://www.lexml.gov.br/1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.lexml.gov.br/1.0 ../xsd/lexml-br-rigido.xsd">
      <!--       xsi:schemaLocation="http://www.lexml.gov.br/1.0 http://projeto.lexml.gov.br/esquemas/lexml-br-rigido.xsd" > -->
      {renderMetadado(pl.metadado)}
      {inner}      
    </LexML>
end LexmlRenderer

