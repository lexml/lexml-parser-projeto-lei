package br.gov.lexml.parser.pl.docx

import scala.io.Source
import scala.xml.pull._
import br.gov.lexml.parser.pl.misc.XMLStreamUtils._
import scala.annotation.tailrec
import br.gov.lexml.parser.pl.misc.CollectionUtils._


object DOCXReader {
  type Elem = (String,String)
  
  final case class TextStyle(bold : Boolean = false, italics : Boolean = false, 
    subscript : Boolean = false, superscript : Boolean = false) {
    import scala.xml._
    def wrap(nodes : Node*) : Seq[Node] = {
      val styleStr = Map(
          "font-style: italic" -> italics,
          "font-weight: bold" -> bold
          ).filter(_._2).keys.mkString(";")
      if(styleStr.isEmpty) {
        nodes
      } else {
        Seq(<span style={styleStr}>{nodes}</span>)
      }
    }
  }
  
  val emptyStyle = TextStyle()

  abstract sealed class Segment {
    import scala.xml._
    def toXML : Seq[Node]
  } 
  
  final case class TextSegment(style : TextStyle, text : String) extends Segment {
    import scala.xml._
	override def toXML = style.wrap(Text(text))
  }
  
  case object Space extends Segment {
    import scala.xml._
    override def toXML = Seq(Text(" "))
  }
  
  case object Tab extends Segment {
    import scala.xml._
    override def toXML = Seq(Text(" "))
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
  def breakText(style : TextStyle, ev : EvText) : IndexedSeq[Segment] = {
    val txt = ev.text.
    			replaceAll("[\\u00A0\\s]"," ").    			
    			replaceAll("\\s\\s+"," ")        		
    val hd = if(txt.startsWith(" ")) { IndexedSeq(Space) } else { IndexedSeq() }    			
    val tl = if(txt.endsWith(" ")) { IndexedSeq(Space) } else { IndexedSeq() }
    hd ++ intersperse(List(txt.split(" ") :_*).map(t => TextSegment(style,t)),Space).toIndexedSeq ++ tl
  }  
  
  final case class XElem(ns : Option[String], label : String)
  
  object XElem {
    def fromEvent(ev : EvElemStart) = {
      val ns = Option(ev.pre).flatMap(p => Option(ev.scope.getURI(p)))
      XElem(ns,ev.label)
    }
    
    private[this] val wNs = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    
    private[this] def wElem(label : String) = XElem(Some(wNs),label)
    
    val parPr = wElem("pPr")
    val runPr = wElem("rPr")
    val italic = wElem("i")
    val tab = wElem("tab")
    val parStyle = wElem("pStyle")
    val bold = wElem("b")
  }
  
  final case class Context(style : TextStyle = TextStyle(), stack : List[(XElem,TextStyle)] = List(), segments : IndexedSeq[Segment] = IndexedSeq()) {    
    def changeStyle(f : TextStyle => TextStyle) : Context = 
      copy(style = f(style))
    
    def enter(elem : XElem) = copy(stack = (elem,style) :: stack) 
    def leave(keepStyle : Option[TextStyle => TextStyle] = None) = {
    	val newStyle = keepStyle.map(f => f(style)).getOrElse(stack.head._2)    	  
    	copy(style = newStyle, stack = stack.tail)
    }
    def head = stack.headOption.map(_._1)
    def add(s : Segment*) = copy(segments = segments ++ s)
  }
  
  import XElem._
  def processEvent(ctx : Context, ev : XMLEvent) : Context = (ev,ctx.head) match {
    case (ev : EvElemStart,_) => ctx.enter(fromEvent(ev))
    case (ev : EvElemEnd,Some(e)) 
    	if e == italic => ctx.leave(Some(s => s.copy(italics = true)))
    case (ev : EvElemEnd,Some(e))
    	if e == bold => ctx.leave(Some(s => s.copy(bold = true)))
    case (ev : EvElemEnd,Some(e)) 
    	if e == parPr || e == runPr => ctx.leave(Some(x => x))
    case (ev : EvElemEnd,Some(e))
    	if e == tab => ctx.add(Tab)
    case (ev : EvElemEnd,_) => ctx.leave()  
    case (ev : EvText,_) => ctx.add(breakText(ctx.style,ev):_*)
    case _ => ctx
  }
  
  def collectText(evs : Traversable[XMLEvent]) = {
    val segs1 = evs.foldLeft(Context())(processEvent).segments 
	val segs2 = collapseBy(segs1) {
    	case (Space,Space) => Space
    	case (Space,Tab) => Tab
    	case (Tab,Space) => Tab
    	case (Tab,Tab) => Tab
    	case (TextSegment(s1,t1),TextSegment(s2,t2)) 
    		if s1 == s2 => TextSegment(s1,t1 ++ t2)
  	}
  	val segs3 = collapseBy3(segs2) ({
  	  case (TextSegment(s1,t1),Space,TextSegment(s2,t2)) if s1 == s2 =>
  	    	TextSegment(s1,t1 + " " + t2)
  	}).toIndexedSeq
  	
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
  
  def readDOCX(s : InputStream) = {
    val zis = new ZipInputStream(s)
    var entry = zis.getNextEntry    
    while(entry != null && entry.getName != "word/document.xml") {
      entry = zis.getNextEntry
    }
    if(entry != null) {      
      val src = Source.fromInputStream(zis, "utf-8")
      val reader = new XMLEventReader(src)
      val events = reader.toStream
      val pars = collectPars(events) 
      val textContents = pars.map(collectText)
      val collapsed = collapseBy(textContents) {
  	  	case (l1,l2) if l1.isEmpty && l2.isEmpty => l1
  	  }      
      Some(<html>
        <body>
         {collapsed.map(segs => <p>{segs.flatMap(_.toXML)}</p>)}
      	</body>
      </html>)      
    } else {      
      None
    }
  }
}


