package br.gov.lexml.parser.pl.terms

import org.kiama.==>
import org.kiama.attribution.Attributable
import org.kiama.attribution.Attribution._

object XHTML {

  object AST {
    type StyleMap = Map[String, String]

    abstract sealed class XAttributes extends Product with Attributable {
      val clazz: Option[String]
      val style: StyleMap
    }

    case class XParagraphAttributes(clazz: Option[String] = None, style: StyleMap = Map()) extends XAttributes
    case class XOLAttributes(clazz: Option[String] = None, style: StyleMap = Map()) extends XAttributes
    case class XLIAttributes(clazz: Option[String] = None, style: StyleMap = Map()) extends XAttributes
    case class XTableAttributes(clazz: Option[String] = None, style: StyleMap = Map()) extends XAttributes
    case class XSpanAttributes(clazz: Option[String] = None, style: StyleMap = Map()) extends XAttributes
    case class XCellAttributes(clazz: Option[String] = None, style: StyleMap = Map(), rowspan: Int = 1, colspan: Int = 1) extends XAttributes

    trait InlineContainer extends Product with Attributable {
      val elements: List[InlineElement]
    }

    abstract sealed class BlockElement extends Product with Attributable

    case class XParagraph(attributes: XParagraphAttributes, elements: List[InlineElement]) extends BlockElement with InlineContainer
    case class XOL(attributes: XOLAttributes, items: List[XLI]) extends BlockElement
    case class XLI(attributes: XLIAttributes, items: List[BlockElement]) extends BlockElement
    case class XTable(attributes: XTableAttributes, head: List[XTR], foot: List[XTR], body: List[XTR]) extends BlockElement

    case class XTR(cells: List[XTD]) extends Attributable

    case class XTD(attributes: XCellAttributes, elements: List[InlineElement]) extends InlineContainer

    abstract sealed class InlineElement extends Product with Attributable
    case class XText(text: String) extends InlineElement
    case class XSpan(attributes: XSpanAttributes, elements: List[InlineElement]) extends InlineElement
    case class XBold(elements: List[InlineElement]) extends InlineElement with InlineContainer
    case class XItalic(elements: List[InlineElement]) extends InlineElement with InlineContainer
    case class XSubscript(elements: List[InlineElement]) extends InlineElement with InlineContainer
    case class XSuperscript(elements: List[InlineElement]) extends InlineElement with InlineContainer
  }

  import AST._

}
