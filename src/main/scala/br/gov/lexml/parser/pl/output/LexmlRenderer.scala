package br.gov.lexml.parser.pl.output

import br.gov.lexml.parser.pl.metadado.Metadado
import br.gov.lexml.parser.pl.rotulo._
import br.gov.lexml.parser.pl.block._
import br.gov.lexml.parser.pl.ProjetoLei
import br.gov.lexml.parser.pl.rotulo.rotuloParser.{Fem, Genero}

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
    case _: RotuloDispositivoGenerico => "DispositivoGenerico"
    case RotuloPena ⇒ "Pena"
    case _: RotuloParte ⇒ "Parte"
    case _: RotuloLivro ⇒ "Livro"
    case _: RotuloTitulo ⇒ "Titulo"
    case _: RotuloSubTitulo ⇒ "Subtitulo"
    case _: RotuloCapitulo ⇒ "Capitulo"
    case _: RotuloSubCapitulo ⇒ "Subcapitulo"
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

  private implicit class Unico(un : Boolean) {
    def unicoChar = if (un) { "u" } else { "" }
    def unicoMajStr(alt : => String) : String =
      if(un) { "ÚNICO" } else { alt }
    def unicaMajStr(alt : => String) : String =
      if(un) { "ÚNICA" } else { alt }
    def unicoMinStr(alt : => String) : String =
      if(un) { "Único" } else { alt }
    def unicaMinStr(alt : => String) : String =
      if(un) { "Única" } else { alt }
  }

  val ordDez : Map[Int,String] = Map(
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
  val ordUnid : Map[Int,String] = Map(
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
  def renderOrdinalExtenso(num : Int, g : Genero,allCaps : Boolean = false, firstCaps : Boolean = false): String = {
    val s = g.select("o","a")
    if(num > 99) { throw new RuntimeException("Não há suporte à renderização de ordinais por extenso acima de 99: " + num)}
    if(num < 1)  { throw new RuntimeException("Não há suporte à renderização de ordinais por extenso menores do que 1: " + num)}
    val d = num / 10
    val u = num % 10
    val ds = if(d > 0) { ordDez(d) + s} else {""}
    val us = if(u > 0) { ordUnid(u) + s} else {""}
    val rs1 = if(ds != "" && us != "") { ds + "-" + us} else { ds + us }
    if(allCaps) { rs1.toUpperCase() }
    else if(firstCaps) {
      rs1.substring(0,1).toUpperCase + rs1.substring(1)
    } else {
      rs1
    }
  }

  def renderRotuloEither(r: Rotulo): Either[String, String] = r match {
    case RotuloArtigo(1, None, true) ⇒ Left("Artigo único. ")
    case RotuloArtigo(num, comp, _) ⇒ Left("Art. " + renderOrdinal(num) + renderComp(comp) + (if (num >= 10 || comp.isDefined) "." else ""))
    case RotuloParagrafo(None, _, _) ⇒ Right("Caput")
    case RotuloParagrafo(Some(1), None, true) ⇒ Left("Parágrafo único.")
    case RotuloParagrafo(Some(num), comp, _) ⇒ Left("§ " + renderOrdinal(num) + renderComp(comp) + (if (num >= 10 || comp.isDefined) "." else ""))
    case RotuloInciso(num, comp) ⇒ Left(renderRomano(num).toUpperCase + renderComp(comp) + " –")
    case RotuloAlinea(num, comp) ⇒ Left(renderAlphaSeq(num - 1).toLowerCase + renderComp(comp) + ")")
    case RotuloItem(num, comp) ⇒ Left(num.toString + ".")
    case RotuloPena ⇒ Left("Pena –")
    case d : RotuloDispositivoGenerico ⇒ Left(s"${d.nomeRotulo} –")
    case RotuloParte(Left(rot),_,_,_,_) ⇒ Left("PARTE " + rot)
    case RotuloParte(_,_,_,_,Some(rot)) ⇒ Left("PARTE " + rot)
    case RotuloParte(_,_,true,_,_) ⇒ Left("PARTE ÚNICA")
    case RotuloParte(Right(num),comp,_,true,_) ⇒
      Left("PARTE " + renderOrdinalExtenso(num = num,g = Fem,allCaps = true).toUpperCase + renderComp(comp))
    case RotuloParte(Right(num),comp,_,_,_) ⇒
      Left(renderRomano(num).toUpperCase + renderComp(comp))
    case RotuloLivro(Left(rot), _, _) ⇒ throw new RenderException(s"Livro sem número não suportado na renderização: ${rot}")
    case RotuloLivro(Right(num), comp,unico) ⇒ Left("LIVRO " + unico.unicoMajStr(renderRomano(num).toUpperCase + renderComp(comp)))
    case RotuloTitulo(num, comp, unico) ⇒ Left("TÍTULO " + unico.unicoMajStr(renderRomano(num) + renderComp(comp)))
    case RotuloSubTitulo(num, comp, unico) ⇒ Left("SUBTÍTULO " + unico.unicoMajStr(renderRomano(num) + renderComp(comp)))
    case RotuloCapitulo(num, comp, unico) ⇒ Left("CAPÍTULO " + unico.unicoMajStr(renderRomano(num) + renderComp(comp)))
    case RotuloSubCapitulo(num, comp, unico) ⇒ Left("SUB-CAPÍTULO " + unico.unicoMajStr(renderRomano(num) + renderComp(comp)))
    case RotuloSecao(num, comp, unica) ⇒ Left("Seção " + unica.unicaMinStr(renderRomano(num) + renderComp(comp)))
    case RotuloSubSecao(num, comp, unica) ⇒ Left("Subseção " + unica.unicaMinStr(renderRomano(num) + renderComp(comp)))
    case RotuloAlteracao(num) ⇒ Right("Alteracao " + num)
    case x => throw new RuntimeException("Lexml Xml renderer. Elemento não esperado:" + x)
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
    case RotuloParte(Left(rot), _,_,_,_) ⇒ throw new RenderException(s"Parte sem número não suportado na renderização: ${rot}")
    case RotuloParte(Right(num), comp,unico,_,_) ⇒ "prt%d%s%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloLivro(Left(rot), _,_) ⇒ throw new RenderException(s"Livro sem número não suportado na renderização: ${rot}")
    case RotuloLivro(Right(num), comp,unico) ⇒ "liv%d%s%s" format (num, renderCompId(comp))
    case RotuloTitulo(num, comp,unico) ⇒ "tit%d%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloSubTitulo(num, comp,unico) ⇒ "stt%d%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloCapitulo(num, comp,unico) ⇒ "cap%d%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloSubCapitulo(num, comp,_) ⇒ throw new RenderException("Sub-capítulo não suportado pelo parser")
    case RotuloSecao(num, comp,unico) ⇒ "sec%d%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloSubSecao(num, comp,unico) ⇒ "sub%d%s" format (num, unico.unicoChar, renderCompId(comp))
    case RotuloAlteracao(num) ⇒ "alt%d" format (num)
    case x => throw new RuntimeException("Lexml Xml renderer. Elemento não esperado:" + x)
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
        case d: Dispositivo ⇒           
          val xml = rename(elemLabel(d.rotulo), addXlinkHref(d.id)(
            <Dispositivo id={ d.id } textoOmitido={
              d.conteudo match {
                case _ if d.rotulo.isAgregador => null
                case Some(_: Omissis) ⇒ "s"
                case Some(p: Paragraph) if p.text == "" ⇒ "s"
                case _ ⇒ null
              }
            } abreAspas={ if (d.abreAspas) { "s" } else { null } } fechaAspas={ if (d.fechaAspas) { "s" } else { null } } notaAlteracao={ d.notaAlteracao.map(_.toUpperCase).orNull }
          	nome={d.rotulo match {
                    case dd : RotuloDispositivoGenerico => dd.nomeRotulo
                    case _ => null 
                  }}>
            { d.links.map(l ⇒ Comment("Link: " + l)) }
            { d.titulo.map(tit ⇒ <TituloDispositivo>{ cleanBs(tit.nodes) }</TituloDispositivo>).getOrElse(NodeSeq.Empty) }
            { renderRotulo(d.rotulo) match { 
                case Some(r) ⇒ <Rotulo>{ r }</Rotulo>                
                case _ ⇒ NodeSeq.Empty 
                } }
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
          xml
        case a: Alteracao ⇒ {
          addBaseURN(a.baseURN)(
            <Alteracao id={ a.id }>              
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
		  {pl.ementa.map(x => <Ementa id="ementa">{ cleanTopBIs(renderParagraphWithoutP(x)) }</Ementa>).getOrElse(NodeSeq.Empty)}
      <Preambulo id="preambulo">{ NodeSeq fromSeq pl.preambulo.flatMap(p ⇒ cleanBs(p.toNodeSeq)) }</Preambulo>
    </ParteInicial>

  import scala.xml.Utility.trim

  def render(pl: ProjetoLei): Elem = {
    val norma =  (  
        <Norma>
          { renderParteInicial(pl) }
          { renderArticulacao(pl.articulacao) }
        </Norma>
      )
    //FIXME: tirar o comentário para habilitar
    val encloseInProjetoNorma = true //pl.metadado.isProjetoNorma 		
    val inner = if (encloseInProjetoNorma) {
      <ProjetoNorma>{norma}</ProjetoNorma>
    } else { norma }
    (        
    <LexML xmlns="http://www.lexml.gov.br/1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.lexml.gov.br/1.0 ../xsd/lexml-br-rigido.xsd">
      <!--       xsi:schemaLocation="http://www.lexml.gov.br/1.0 http://projeto.lexml.gov.br/esquemas/lexml-br-rigido.xsd" > -->
      { renderMetadado(pl.metadado) }
      { inner }      
    </LexML>)
  }
}