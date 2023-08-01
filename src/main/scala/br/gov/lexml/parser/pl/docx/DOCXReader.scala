package br.gov.lexml.parser.pl.docx

import javax.xml.stream.XMLInputFactory
import br.gov.lexml.parser.pl.misc.XMLStreamUtils._

import scala.annotation.tailrec
import br.gov.lexml.parser.pl.misc.CollectionUtils._
import javax.xml.stream.events._


object DOCXReader {
  type Elem = (String,String)

  final case class TextStyle(bold : Boolean = false, italics : Boolean = false, 
    subscript : Boolean = false, superscript : Boolean = false) {

    override def toString : String = {
      val label =
        (if (bold) {"B"} else "") +
        (if (italics) {"I"} else "") +
        (if (superscript) { "⌃"} else "") +
          (if (subscript) { "⌄"} else "")
      s"<$label>"
    }
  }
  
  val emptyStyle: TextStyle = TextStyle()

  abstract sealed class Segment {
    import scala.xml._
    def toXML : Seq[Node]
  } 
  
  final case class TextSegment(style : TextStyle, text : String) extends Segment {
    import scala.xml._
	  override def toXML: Seq[Node] =
      TextSegment.styles(style)(Text(text))

    override def toString : String = {
      val head = if(style != emptyStyle) { style.toString } else ""
      "〈" + head + text + "〉"
    }
  }

  object TextSegment {
    import scala.xml._
    def encloseIf(label : String)(cond : TextStyle => Boolean)(style : TextStyle)(nodes : Seq[Node]) : Seq[Node] =
      if(cond(style)) {
        val el = Elem(prefix = null,
                      label = label,
                      attributes = Null,
                      scope = TopScope,
                      minimizeEmpty = true,
                      child = nodes : _*)
        Seq(el)
      } else nodes

    val italicsIf = encloseIf("i")(_.italics) _
    val boldIf = encloseIf("b")(_.bold) _
    val supIf = encloseIf("sup")(_.superscript) _
    val subIf = encloseIf("sub")(_.subscript) _

    def styles(style : TextStyle)(nodes : Seq[Node]) : Seq[Node] =
      boldIf(style) {
        italicsIf(style) {
          supIf(style) {
            subIf(style) {
              nodes
            }
          }
        }
      }
  }
  
  case object Space extends Segment {
    import scala.xml._
    override def toXML = Seq(Text(" "))
    override def toString = "⎵"
  }
  
  case object Tab extends Segment {
    import scala.xml._
    override def toXML = Seq(Text(" "))
    override def toString = "⇥"
  }
  
  //From scalaz
  def intersperse[A](as: List[A], a: A): List[A] = {
    @tailrec
    def intersperse0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil => accum
      case x :: Nil => x :: accum
      case h :: t => intersperse0(a :: h :: accum, t)
    }
    intersperse0(Nil, as).reverse
  }
  def breakText(style : TextStyle, text : String) : IndexedSeq[Segment] = {
    val txt = text.
    			replaceAll("[\\u00A0\\s]"," ").    			
    			replaceAll("\\s\\s+"," ")        		
    val hd = if(txt.startsWith(" ")) { IndexedSeq(Space) } else { IndexedSeq() }    			
    val tl = if(txt.endsWith(" ")) { IndexedSeq(Space) } else { IndexedSeq() }
    hd ++ intersperse(List(txt.split(" ").toIndexedSeq :_*).map(t => TextSegment(style,t)),Space).toIndexedSeq ++ tl
  }  
  
  final case class XElem(ns : Option[String], label : String, attributes : Map[(String,String),String] = Map()) {
    override def toString = {
      val nsTxt = ns match {
        case Some(XElem.wNs) => "():"
        case Some(n) => s"($n):"
        case None => ""
      }
      nsTxt + label
    }
  }
  
  object XElem {
    def fromEvent(ev : StartElement): XElem = {
      val ns = Option(ev.getName.getNamespaceURI)
      import scala.jdk.CollectionConverters._
      val attrIt : Iterator[Attribute] = ev.getAttributes.asScala.collect { case x : Attribute => x }
      val attrSeq : Seq[Attribute] = attrIt.toSeq
      val attrPairs : Seq[((String,String),String)] = attrSeq.map { (att: javax.xml.stream.events.Attribute) =>
        ((att.getName.getNamespaceURI, att.getName.getLocalPart), att.getValue)
      }
      val attrs = attrPairs.toMap
      XElem(ns,ev.getName.getLocalPart,attrs)
    }
    
    val wNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

  }
  
  final case class Context(style : TextStyle = TextStyle(), stack : List[(XElem,TextStyle)] = List(), segments : IndexedSeq[Segment] = IndexedSeq()) {
    
    def enter(elem : XElem) : Context = copy(stack = (elem,style) :: stack)
    def leave(keepStyle : Option[TextStyle => TextStyle] = None) : Context = {
    	val newStyle = keepStyle.map(f => f(style)).getOrElse(stack.head._2)    	  
    	copy(style = newStyle, stack = stack.tail)
    }

    def head: Option[XElem] = stack.headOption.map(_._1)
    def add(s : Segment*) : Context = copy(segments = segments ++ s)
    override def toString =
      s"⟦${ if (style != emptyStyle) { style.toString + ":" } else "" } ${segments.mkString("")} STACK: ${stack.map { case (e,style) => style.toString + e.toString }.mkString("⟪"," ","⟫")}⟧"
  }
  
  import XElem._
  private def processEvent(ctx : Context, event : XMLEvent) : Context = {
    (event,ctx.head) match {
      case (ev : StartElement,_) => ctx.enter(fromEvent(ev))
      case (ev : EndElement,Some(e)) if e.ns == Some(XElem.wNs) =>
        e.label match {
          case "i" => ctx.leave(Some(s => s.copy(italics = true)))
          case "b" => ctx.leave(Some(s => s.copy(bold = true)))
          case "pPr" | "rPr" => ctx.leave(Some(x => x))
          case "tab" => ctx.add(Tab)
          case "vertAlign" =>
            e.attributes.get((XElem.wNs,"val")) match {
              case Some("superscript") => ctx.leave(Some(s => s.copy(superscript = true)))
              case Some("subscript") => ctx.leave(Some(s => s.copy(subscript = true)))
              case x => sys.error(s"Unexpcted vertAlign value: $x")
            }
          case _ => ctx.leave()
        }
      case (ev : Characters,_) =>
        val brokenText = breakText(ctx.style,ev.getData)
        ctx.add(brokenText:_*)
      case (ev : EntityReference,_) =>
        br.gov.lexml.parser.pl.util.Entities.entities.get(ev.getName).
          map(c => ctx.add(breakText(ctx.style,"" + c):_*)).getOrElse(ctx)
      case _ => ctx
    }
  }

  private def collectText(evs : Iterable[XMLEvent]) = {
    val segs1 = evs.foldLeft(Context())(processEvent).segments 
	  val segs2 = collapseBy(segs1) {
    	case (Space,Space) => Space
    	case (Space,Tab) => Tab
    	case (Tab,Space) => Tab
    	case (Tab,Tab) => Tab
    	case (TextSegment(s1,t1),TextSegment(s2,t2)) 
    		if s1 == s2 => TextSegment(s1,t1 ++ t2)
  	}
  	val segs3 = collapseBy3(segs2)({
      case (TextSegment(s1, t1), Space, TextSegment(s2, t2)) if s1 == s2 =>
        TextSegment(s1, t1 + " " + t2)
    })
  	
  	val segs4 = segs3.headOption match {
  	  case Some(x) if !x.isInstanceOf[TextSegment] => segs3.tail
  	  case _ => segs3
  	}
  	val segs5 = segs4.lastOption match {
  	  case Some(x) if !x.isInstanceOf[TextSegment] => segs4.init
  	  case _ => segs4
  	}
  	segs5  	  
  }
  
  import java.io.InputStream
  import java.util.zip._

  def readDOCX(s: InputStream): Option[scala.xml.Elem] = {
    val zis = new ZipInputStream(s)
    var entry = zis.getNextEntry
    while (entry != null && entry.getName != "word/document.xml") {
      entry = zis.getNextEntry
    }
    if (entry != null) {
      val reader = XMLInputFactory.newFactory().createXMLEventReader(zis, "UTF-8")
      import scala.jdk.CollectionConverters._
      val events = LazyList.from(reader.asScala.collect { case e: XMLEvent => e })
      val pars = collectPars(events)
      val textContents = pars.map(collectText)
      val collapsed = collapseBy(textContents) {
        case (l1, l2) if l1.isEmpty && l2.isEmpty => l1
      }
      val ps = collapsed.map(segs =>
        <p>
          {segs.flatMap(_.toXML)}
        </p>)
      Some(<html><body>{ps}</body></html>)
    }  else {
      None
    }
  }
}


