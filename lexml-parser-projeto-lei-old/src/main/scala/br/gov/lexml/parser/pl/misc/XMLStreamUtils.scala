package br.gov.lexml.parser.pl.misc

import scala.xml.pull._

object XMLStreamUtils {
    import CollectionUtils._
    
	def collectElement(ns : String, label : String)(s : Stream[XMLEvent]) : Stream[Seq[XMLEvent]] = {
	    def isElemStart : XMLEvent => Boolean = {
	      case ev : EvElemStart => {
	        val prefix = ev.scope.getPrefix(ns)
	        ev.pre == prefix && ev.label == label
	      }
	      case _ => false
	    }
	
	    val sMarked = mapWith(s)((0,None : Option[Int]))({
	        case ((level, None), ev : EvElemStart) if isElemStart(ev) => ((level+1,Some(level+1)),(false,ev))
	        case ((level, elStart), ev : EvElemStart) => ((level+1,elStart),(elStart.isDefined,ev))
	        case ((level, Some(elLevel)), ev : EvElemEnd) if level == elLevel => ((level-1,None),(false,ev))
	        case ((level, elStart), ev : EvElemEnd) => ((level-1,elStart),(elStart.isDefined,ev))
	        case ((level, elStart), ev) => ((level,elStart),(elStart.isDefined,ev))
	    })

	    val segs = segmentBy(sMarked) {
	       case ((v1,_),(v2,_)) => v1 != v2
	    }

	    segs.filter(_.head._1).map(_.map(_._2))
	}

    val collectPars = collectElement("http://schemas.openxmlformats.org/wordprocessingml/2006/main","p")(_)
}