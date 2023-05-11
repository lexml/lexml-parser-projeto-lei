package br.gov.lexml.parser.pl.misc

import javax.xml.stream.events._

object XMLStreamUtils {
    import CollectionUtils._
    
	private def collectElement(ns : String, label : String)(s : LazyList[XMLEvent]) : LazyList[Seq[XMLEvent]] = {
	    def isElemStart : XMLEvent => Boolean = {
	      case ev : StartElement =>
					ev.getName.getNamespaceURI == ns && ev.getName.getLocalPart == label
	      case _ => false
	    }
	
	    val sMarked = mapWith(s)((0,None : Option[Int]))({
	        case ((level, None), ev : StartElement) if isElemStart(ev) => ((level+1,Some(level+1)),(false,ev))
	        case ((level, elStart), ev : StartElement) => ((level+1,elStart),(elStart.isDefined,ev))
	        case ((level, Some(elLevel)), ev : EndElement) if level == elLevel => ((level-1,None),(false,ev))
	        case ((level, elStart), ev : EndElement) => ((level-1,elStart),(elStart.isDefined,ev))
	        case ((level, elStart), ev) => ((level,elStart),(elStart.isDefined,ev))
	    })

	    val segs = segmentBy(sMarked) {
	       case ((v1,_),(v2,_)) => v1 != v2
	    }

	    segs.filter(_.head._1).map(_.map(_._2))
	}

	val collectPars: LazyList[XMLEvent] => LazyList[Seq[XMLEvent]] =
		collectElement("http://schemas.openxmlformats.org/wordprocessingml/2006/main", "p")(_)
}