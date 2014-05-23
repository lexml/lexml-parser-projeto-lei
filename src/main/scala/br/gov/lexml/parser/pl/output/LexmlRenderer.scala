package br.gov.lexml.parser.pl.output

import br.gov.lexml.parser.pl.metadado.Metadado
import br.gov.lexml.parser.pl.rotulo._
import br.gov.lexml.parser.pl.block._
import br.gov.lexml.parser.pl.ProjetoLei
import scala.xml._
import scala.collection.immutable.List

class LexmlRenderer {
}

object LexmlRenderer {
  def rename(label: String, n: NodeSeq): NodeSeq = n match {
    case Elem(prefix, _, attributes, scope, child @ _*) ⇒
      Elem(prefix, label, attributes, scope, true, child: _*)
    case _ ⇒ n
  }

  def elemLabel(r: Rotulo): String = r match {
    case _: RotuloArtigo ⇒ "Artigo"
    case RotuloParagrafo(None, _, _) ⇒ "Caput"
    case _: RotuloParagrafo ⇒ "Paragrafo"
    case _: RotuloInciso ⇒ "Inciso"
    case _: RotuloAlinea ⇒ "Alinea"
    case _: RotuloItem ⇒ "Item"
    case RotuloPena ⇒ "Pena"
    case _: RotuloParte ⇒ "Parte"
    case _: RotuloLivro ⇒ "Livro"
    case _: RotuloTitulo ⇒ "Titulo"
    case _: RotuloSubTitulo ⇒ throw new RenderException("Sub-título não suportado pelo parser")
    case _: RotuloCapitulo ⇒ "Capitulo"
    case _: RotuloSubCapitulo ⇒ throw new RenderException("Sub-capítulo não suportado pelo parser")
    case _: RotuloSecao ⇒ "Secao"
    case _: RotuloSubSecao ⇒ "Subsecao"
    case _: RotuloAlteracao ⇒ "Alteracao"
  }

  def renderNumeral(num: Int): String = {
    if (num > 1000) { "%s.%03d".format(renderNumeral(num / 1000), num % 1000) }
    else { num.toString }
  }

  def renderOrdinal(num: Int): String = renderNumeral(num) + (if (num < 10) "º" else "")

  def renderComp(onum: Option[Int]): String = onum.map(n ⇒ "-" + renderAlphaSeq(n).toUpperCase).getOrElse("")

  def renderRomano(num: Int): String = {
    def rom(cX: String, cV: String, cI: String, d: Int): String = d match {
      case 0 ⇒ ""
      case 9 ⇒ cI + cX
      case 4 ⇒ cI + cV
      case _ if d >= 5 ⇒ cV + (cI * (d - 5))
      case _ ⇒ cI * d
    }
    ("M" * (num / 1000)) + rom("M", "D", "C", (num / 100) % 10) + rom("C", "L", "X", (num / 10) % 10) + rom("X", "V", "I", num % 10)
  }

  def renderAlphaSeq(num: Int): String = {
    def rend(n: Int): String = n match {
      case 0 ⇒ ""
      case _ ⇒ {
        val nn = n - 1
        rend(nn / 26) + ('a' + (nn % 26)).asInstanceOf[Char]
      }
    }
    rend(num + 1)
  }

  class RenderException(msg: String) extends RuntimeException(msg)

  def renderRotuloEither(r: Rotulo): Either[String, String] = r match {
    case RotuloArtigo(1, None, true) ⇒ Left("Artigo único. ")
    case RotuloArtigo(num, comp, _) ⇒ Left("Art. " + renderOrdinal(num) + renderComp(comp) + (if (num >= 10 || comp.isDefined) "." else ""))
    case RotuloParagrafo(None, _, _) ⇒ Right("Caput")
    case RotuloParagrafo(Some(1), None, true) ⇒ Left("Parágrafo único.")
    case RotuloParagrafo(Some(num), comp, _) ⇒ Left("§ " + renderOrdinal(num) + renderComp(comp) + (if (num >= 10 || comp.isDefined) "." else ""))
    case RotuloInciso(num, comp) ⇒ Left(renderRomano(num).toUpperCase + renderComp(comp) + " –")
    case RotuloAlinea(num, comp) ⇒ Left(renderAlphaSeq(num - 1).toLowerCase + renderComp(comp) + ")")
    case RotuloItem(num, comp) ⇒ Left(num.toString + "–")
    case RotuloPena ⇒ Left("Pena –")
    case RotuloParte(Left(_), _) ⇒ throw new RenderException("Parte sem número não suportado na renderização")
    case RotuloParte(Right(num), comp) ⇒ Left("PARTE " + renderRomano(num).toUpperCase + renderComp(comp))
    case RotuloLivro(Left(_), _) ⇒ throw new RenderException("Livro sem número não suportado na renderização")
    case RotuloLivro(Right(num), comp) ⇒ Left("LIVRO " + renderRomano(num).toUpperCase + renderComp(comp))
    case RotuloTitulo(num, comp) ⇒ Left("TÍTULO " + renderRomano(num) + renderComp(comp))
    case RotuloSubTitulo(num, comp) ⇒ Left("SUB-TÍTULO " + renderRomano(num) + renderComp(comp))
    case RotuloCapitulo(num, comp) ⇒ Left("CAPÍTULO " + renderRomano(num) + renderComp(comp))
    case RotuloSubCapitulo(num, comp) ⇒ Left("SUB-CAPÍTULO " + renderRomano(num) + renderComp(comp))
    case RotuloSecao(num, comp) ⇒ Left("SEÇÃO " + renderRomano(num) + renderComp(comp))
    case RotuloSubSecao(num, comp) ⇒ Left("SUBSEÇÃO " + renderRomano(num) + renderComp(comp))
    case RotuloAlteracao(num) ⇒ Right("Alteracao " + num)
  }

  def renderRotulo(r: Rotulo): Option[String] = renderRotuloEither(r).fold((Some(_)), _ ⇒ None)

  def renderRotulo2(r: Rotulo): String = renderRotuloEither(r).merge

  def renderCompId(n: Option[Int]) = n.map(n ⇒ "-" + (n + 1).toString).getOrElse("")

  def renderId(r: Rotulo): String = r match {
    case RotuloArtigo(num, comp, unico) ⇒ "art%d%s%s" format (num, if (num == 1 && unico) { "u" } else { "" }, renderCompId(comp))
    case RotuloParagrafo(None, _, _) ⇒ "cpt"
    case RotuloParagrafo(Some(num), comp, unico) ⇒ "par%d%s%s" format (num, if (num == 1 && unico) { "u" } else { "" }, renderCompId(comp))
    case RotuloInciso(num, comp) ⇒ "inc%d%s" format (num, renderCompId(comp))
    case RotuloAlinea(num, comp) ⇒ "ali%d%s" format (num, renderCompId(comp))
    case RotuloItem(num, comp) ⇒ "ite%d%s" format (num, renderCompId(comp))
    case RotuloPena ⇒ "pena"
    case RotuloParte(Left(_), _) ⇒ throw new RenderException("Parte sem número não suportado na renderização")
    case RotuloParte(Right(num), comp) ⇒ "prt%d%s" format (num, renderCompId(comp))
    case RotuloLivro(Left(_), _) ⇒ throw new RenderException("Livro sem número não suportado na renderização")
    case RotuloLivro(Right(num), comp) ⇒ "liv%d%s" format (num, renderCompId(comp))
    case RotuloTitulo(num, comp) ⇒ "tit%d%s" format (num, renderCompId(comp))
    case RotuloSubTitulo(num, comp) ⇒ throw new RenderException("Sub-título não suportado pelo parser")
    case RotuloCapitulo(num, comp) ⇒ "cap%d%s" format (num, renderCompId(comp))
    case RotuloSubCapitulo(num, comp) ⇒ throw new RenderException("Sub-capítulo não suportado pelo parser")
    case RotuloSecao(num, comp) ⇒ "sec%d%s" format (num, renderCompId(comp))
    case RotuloSubSecao(num, comp) ⇒ "sub%d%s" format (num, renderCompId(comp))
    case RotuloAlteracao(num) ⇒ "alt%d" format (num)
  }

  def renderId(path: List[Rotulo]): String = path.reverse.map(renderId).mkString("", "_", "")

  def renderBlocks(bl: Seq[Block], idPai: String): NodeSeq =
    NodeSeq fromSeq (bl.foldLeft((List[Node](), 0))(render(idPai)))._1.reverse

  /*def formatBlocksAlteracao(nl : List[Node]) : List[Node] = {
		def addAttribute(name : String, value : String) = (n : Node) => 
			n match {
				case e : Elem => e % (new UnprefixedAttribute(name,value,Null))
				case _ => n
			}
		val addFechaAspas = addAttribute("fechaAspas","s")
		val addAbreAspas = addAttribute("abreAspas","s")
		def addNotaRedacao(nota : String) = addAttribute("notaAlteracao",nota)
		def addIfPresent(n : Node, onota : Option[String]) : Node = onota.map(nota => addNotaRedacao(nota)(n)).getOrElse(n)
		
		nl match {
			case List(n) => List(addIfPresent(addFechaAspas(addAbreAspas(n)),nota))
			case n1 :: (nl : List[Node]) => 
				addAbreAspas(n1) :: (nl.dropRight(1) ++ List((addIfPresent(addFechaAspas(nl.last),nota)))).toList 				
			case List() => nl
		} 
	}*/

  def cleanBs(ns: NodeSeq): NodeSeq = {
    ns.flatMap(n ⇒ (n match {
      case e: Elem if (e.label == "b") ⇒ cleanBs(e.child)
      case e: Elem ⇒ e copy (child = cleanBs(e.child))
      case n ⇒ NodeSeq fromSeq n
    }): NodeSeq)
  }
  def cleanTopBIs(ns: NodeSeq): NodeSeq = (ns.length,ns.headOption) match {
    case (1,Some(e : Elem)) if e.label == "b" || e.label == "i" => cleanTopBIs(e.child)
    case _ => ns    
  }

  def addBaseURN(b: Option[String]): Elem ⇒ Elem = (e: Elem) ⇒ b match {
    case None ⇒ e
    case Some(u) ⇒ e % (new PrefixedAttribute("xml", "base", u, Null))
  }

  def addXlinkHref(b: Option[String]): Elem ⇒ Elem = (e: Elem) ⇒ b match {
    case None ⇒ e
    case Some(u) ⇒ e % (new PrefixedAttribute("xlink", "href", u, Null))
  }

  def addXlinkHref(id: String): Elem ⇒ Elem = addXlinkHref(id.lastIndexOf("_alt") match {
    case n if n > 0 ⇒ {
      val id2 = id.substring(n + 4).dropWhile(!Character.isLetter(_))
      id2.length match {
        case 0 ⇒ None
        case _ ⇒ Some(id2)
      }
    }
    case _ ⇒ None
  })

  def render(idPai: String): ((List[Node], Int), Block) ⇒ (List[Node], Int) = {
    case ((nl, omissisCount), b) ⇒ {
      val el = b match {
        case d: Dispositivo ⇒ rename(elemLabel(d.rotulo), addXlinkHref(d.id)(
          <Dispositivo id={ d.id } textoOmitido={
            d.conteudo match {
              case Some(_: Omissis) ⇒ "s"
              case Some(p: Paragraph) if p.text == "" ⇒ "s"
              case _ ⇒ null
            }
          } abreAspas={ if (d.abreAspas) { "s" } else { null } } fechaAspas={ if (d.fechaAspas) { "s" } else { null } } notaAlteracao={ d.notaAlteracao.map(_.toUpperCase).orNull }>
            { d.links.map(l ⇒ Comment("Link: " + l)) }
            { d.titulo.map(tit ⇒ <TituloDispositivo>{ cleanBs(tit.nodes) }</TituloDispositivo>).getOrElse(NodeSeq.Empty) }
            { renderRotulo(d.rotulo) match { case Some(r) ⇒ <Rotulo>{ r }</Rotulo>; case _ ⇒ NodeSeq.Empty } }
            {
              d.conteudo.filter(!_.isInstanceOf[Omissis]).map(b ⇒ {
                val e = b.toNodeSeq.asInstanceOf[Elem]
                val e2 = e.copy(child = cleanTopBIs(e.child))
                if (d.rotulo.isAgregador) {
                  rename("NomeAgrupador", e2)
                } else { e2 }
              }).getOrElse(NodeSeq.Empty)
            }
            { renderBlocks(d.subDispositivos, d.id + "_") }
          </Dispositivo>)).toList

        case a: Alteracao ⇒ {
          addBaseURN(a.baseURN)(
            <Alteracao id={ a.id }>
              {
                a.matches.map(x ⇒ Comment("Matches: " + x)).getOrElse(NodeSeq.Empty)
              }
              { NodeSeq fromSeq (renderBlocks(a.blocks, a.id + "_").toList) }
            </Alteracao>).toList
        }
        case Omissis(abreAspas, fechaAspas, notaAlteracao) ⇒
          List(<Omissis id={ idPai + "omi" + (omissisCount + 1) } abreAspas={ if (abreAspas) { "s" } else { null } } fechaAspas={ if (fechaAspas) { "s" } else { null } } notaAlteracao={ notaAlteracao.map(_.toUpperCase).orNull }/>)
        case p: Paragraph ⇒ List(
          /*<p abreAspas={ if (p.abreAspas) { "s" } else { null } } 
                       fechaAspas={ if (p.abreAspas) { "s" } else { null } } 
                       notaAlteracao={ p.notaAlteracao.map(_.toUpperCase).orNull } > */
          <p>{ NodeSeq fromSeq p.nodes }</p>)
        case Table(elem) ⇒ List(elem)
        case _ ⇒ List[Node]()
      }
      ((el ++ nl).toList, omissisCount + (b match { case _: Omissis ⇒ 1; case _ ⇒ 0 }))
    }
  }

  def renderArticulacao(blocks: List[Block]): Elem = <Articulacao>{ renderBlocks(blocks, "") }</Articulacao>

  def renderParagraphWithoutP(b: Block) = b match {
    case p: Paragraph ⇒ p.nodes
    case _ ⇒ NodeSeq.Empty
  }

  def renderMetadado(m: Metadado): NodeSeq =
    <Metadado>
      <Identificacao URN={ m.urn }/>
    </Metadado>
  def renderParteInicial(pl: ProjetoLei): NodeSeq =
    <ParteInicial>
      <Epigrafe id="epigrafe">{ renderParagraphWithoutP(pl.epigrafe) }</Epigrafe>
      <Ementa id="ementa">{ cleanTopBIs(renderParagraphWithoutP(pl.ementa)) }</Ementa>
      <Preambulo id="preambulo">{ NodeSeq fromSeq pl.preambulo.flatMap(p ⇒ cleanBs(p.toNodeSeq)) }</Preambulo>
    </ParteInicial>

  import scala.xml.Utility.trim

  def render(pl: ProjetoLei): Elem = (
    <LexML xmlns="http://www.lexml.gov.br/1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.lexml.gov.br/1.0 ../xsd/lexml-br-rigido.xsd">
      <!--       xsi:schemaLocation="http://www.lexml.gov.br/1.0 http://projeto.lexml.gov.br/esquemas/lexml-br-rigido.xsd" > -->
      { renderMetadado(pl.metadado) }
      <ProjetoNorma>
        <Norma>
          { renderParteInicial(pl) }
          { renderArticulacao(pl.articulacao) }
        </Norma>
      </ProjetoNorma>
    </LexML>)
}