package br.gov.lexml.parser.pl.util

import scala.xml._
import scala.util.matching._

object Embolden {
  def embolden(re : Regex, nodes : Seq[Node]) : Seq[Node] = {
    import scala.collection.immutable.{SortedMap,SortedSet}
    val rawText = NodeSeq.fromSeq(nodes).text
    val matches0 = re.findAllMatchIn(rawText).map(m => (m.start,m.end - m.start)).to(SortedMap)
    def walk(ns : Seq[Node], matches : SortedMap[Int,Int] = matches0, curPos : Int = 0,processed : Seq[Node] = Seq()) : (Seq[Node],Int,SortedMap[Int,Int]) = {
      if (ns.isEmpty || matches.isEmpty) {
        (processed ++ ns, curPos + (NodeSeq fromSeq ns).text.length,matches)
      } else {
        val n = ns.head
        val nl = n.text.length
        matches.head match {
          case (_,0) => walk(ns,matches.tail,curPos,processed)
          case (mstart,msize) if mstart < curPos =>
            walk(ns,matches.tail + (curPos -> math.max(msize - (curPos - mstart),0)),curPos,processed)
          case (mstart,_) if mstart >= (curPos + nl) =>
            walk(ns.tail,matches,curPos + nl,processed :+ n)
          case (mstart,msize) =>
            n match {
              case Text(txt) =>
                val p = mstart - curPos
                val before = txt.substring(0,p)
                val i = math.min(msize, nl - p)
                val inside = txt.substring(p, p + i)
                val after = txt.substring(p + i)
                val s1 = if (before.isEmpty) { Seq() } else { Seq(Text(before)) }
                val s2 = if (inside.isEmpty)  { Seq() } else {
                  Seq(<b>{inside}</b>)
                }
                val s3 = if (after.isEmpty)  { Seq() } else { Seq(Text(after)) }
                val matches1 = if (i < msize) { matches.tail + ((curPos + p + i) -> (msize - i))} else { matches.tail }
                walk(s3 ++ ns.tail, matches1, curPos + p + i, processed ++ s1 ++ s2)
              case e : Elem =>
                val (nn,curPos1,matches1) = walk(e.child,matches,curPos)
                walk(ns.tail, matches1, curPos1, processed :+ e.copy(child = nn))
              case _ => throw new RuntimeException(s"Not expecting: n = $n")
            }
        }
      }
    }
    val (nodes1,_,_) = walk(nodes)
    nodes1
  }
}
