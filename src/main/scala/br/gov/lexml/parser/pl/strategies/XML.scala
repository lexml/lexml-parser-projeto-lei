package br.gov.lexml.parser.pl.strategies

import org.kiama.==>
import org.kiama.rewriting.Rewriter._
import br.gov.lexml.parser.pl.terms.XML.AST._
import org.kiama.rewriting.Strategy

/**
 * Estratégias de transformação do XHTML de entrada
 *
 * @author João Rafael Moraes Nicola
 */
object XML {
  
  
  lazy val matchBlockElement : Strategy = null /*{
    val blockElements = Set("p", "ol", "li", "table", "thead", "tbody", "tfoot",
      "tr", "td")
  }*/
  
  /**
   * Filtra os elementos do XHTML de entrada, mantendo apenas os elementos que interessam ao processo de conversão.
   *
   * A estratégia reescreve, de baixo para cima, as listas de de elementos presentes na árvore (o atributos `contents`
   * de objetos `XElem`), mantendo os elementos cujos rotulos estão definidos na variável `keepIntactElements`,
   * substituindo os elementos cujos rótulos estão definidos na variável `explodedElements` e removendo interamente os
   * outros.
   */
  lazy val filterAndExplode: Strategy = {
    /**
     * Lista de elementos a serem preservados.
     */
    val elementsToKeep = Set("p", "ol", "li", "table", "thead", "tbody", "tfoot",
      "tr", "td", "span", "b", "i", "sup", "sub")

    /**
     * Lista de elementos a serem renomeados para <p>
     */
    val elementsToRename = Set("blockquote", "h1", "h2", "h3", "h4")

    /**
     * Lista de elementos a serve explodidos (substituídos por seus conteúdos).
     */
    val elementsToExplode = Set("div", "body", "html")

    /**
     * Elementos inline
     */
    val inlineElements = Set("span", "sup", "sub", "i", "b")

    /**
     * Envelopa sequências de elementos inline em conteúdos mistos (inline e bloco)
     * de elementos de bloco
     */
    def wrapAnonymousInlineInP(el: List[XObject]): List[XObject] = {
      def isInline(x: XObject) = x match {
        case _: XText ⇒ true
        case e: XElem ⇒ inlineElements.contains(e.name)
      }
      def wrapIfNeeded: ((List[XObject], List[XObject])) ⇒ List[XObject] = {
        case (Nil, notInline) ⇒ notInline
        case (inline, Nil) ⇒ inline
        case (inline, notInline) ⇒ XElem("p", "", Map(), inline) :: notInline
      }
      wrapIfNeeded(el.foldRight((List[XObject](), List[XObject]())) {
        case (o, (inline, notInline)) if isInline(o) ⇒ (o :: inline, notInline)
        case (o, (Nil, notInline)) ⇒ (Nil, o :: notInline)
        case (o, (inline, notInline)) ⇒ (Nil, o :: XElem("p", "", Map(), inline) :: notInline)
      })
    }

    everywherebu {
      rule {
        case e: XElem if elementsToExplode.contains(e.name) ⇒
          e copy (contents = wrapAnonymousInlineInP(e.contents))
        case l: List[XObject] if !l.isEmpty ⇒ l flatMap {
          case e: XElem if elementsToKeep.contains(e.name) ⇒ List(e)
          case e: XElem if elementsToRename.contains(e.name) ⇒ List(e copy (name = "p"))
          case e: XElem if elementsToExplode.contains(e.name) ⇒ e.contents
          case _: XElem ⇒ List()
          case x ⇒ List(x)
        }
        case x ⇒ x
      }
    } <* rule {
      case (x: List[XObject]) ⇒ wrapAnonymousInlineInP(x)
    }
  }

/*  lazy val cleanAtributes: Strategy = {
    def styleMap(m : Map[String,String]) = m.get("style","").map(_.split(";").toList.map(_.split(":").toList.map(_.trim)).collect {
      case List(x) => (x,"")
      case List(x,y) => (x,y)
    }) toMap
    def onlyIn(s : Set[String])(m : Map[String,String]) = m.filterKeys(s.contains(_))
    def onlyBoldAndItalic(m : Map[String,String]) = styleMap(m).collect({
      case (styleName,value) => boldAndItalic(styleName,value) 
    })
    val restrict1 = Map[String,Set[String]](
        "table" -> Set("rows","cols"),
        "td" -> Set("rowspan","colspan"),
        "
    ) 
    
    
    def boldAndItalic(m : Map[String,String]) = {
      val m1 = styleMap(m)
      val isBold = m1.getOrElse("font-weight","") match {
        case "bold" => true
        case "bolder" => true
        case _ => false
      }
      val isItalics
    }
    everywheretd { 
      rule {      
      	case XElem("span","",m,cl) => (m.get("style").map) match { 
      		if(m.get)
      	}
  }
  }
  */
}

