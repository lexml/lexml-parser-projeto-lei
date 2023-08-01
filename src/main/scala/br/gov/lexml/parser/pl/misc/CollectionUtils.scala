package br.gov.lexml.parser.pl.misc

import collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.Option.option2Iterable
import scala.collection.BuildFrom

object CollectionUtils {

	def mapWith[A,B,C,M[A] <: Iterable[A],That](s : M[A])(z : B)(f : (B,A) => (B,C))(implicit bf: BuildFrom[M[A], C, That]) = {
	    val s1 = s.scanLeft((z, None : Option[C]))({
	        case ((zz,_),x) =>
	          val (b,c) = f(zz,x)
	          (b,Some(c))
	    }).drop(1).unzip._2.flatten
	    val builder = bf.newBuilder(s)
	    builder ++= s1
	    builder.result()
	}
		
	def segmentBy[A,M[A] <: Iterable[A],That](s : M[A])(f : (A,A) => Boolean)(implicit bf : BuildFrom[M[A], Seq[A], That]) : That = {
	  val b = bf.newBuilder(s)
	  
	  var last : Option[A] = None
	  var curSeq : Seq[A] = Seq()
	
	  def seek(x : A) = (last,x) match {	  
	      case (None,x) =>
	        last = Some(x)
	        curSeq = Seq(x)
	      case (Some(y),x) if f(y,x) =>
	         b += curSeq
	         curSeq = Seq(x)
	         last = Some(x)
	      case (_,x) =>
	        curSeq = curSeq :+ x
	        last = Some(x)
	    }
	  
	  s.foreach(seek)
	  b.result()
	}
	
	def collapseBy[A,M[A] <: Iterable[A],That](s : M[A])(f : PartialFunction[(A,A),A])(implicit bf : BuildFrom[M[A], A, That]) : That = {
	  val b = bf.newBuilder(s)
	  
	  var last : Option[A] = None
	
	  def seek(x : A) : Unit = (last,x) match {
	      case (None,x) =>
	        last = Some(x)
	      case (Some(y),x) => f.lift((y,x)) match {
	        case Some(c) =>
	          last = Some(c)
	        case None =>
	          last = Some(x)
	          b += y
	      }
	    }
	  s.foreach(seek)
	  last.foreach(b += _)
	  b.result()
	}
	
	def collapseBy3[A,M[A] <: Iterable[A],That](s : M[A])(f : PartialFunction[(A,A,A),A])(implicit bf : BuildFrom[M[A], A, That]) : That = {
	  val b = bf.newBuilder(s)
	  
	  var last : List[A] = List()
	
	  def seek(x : A) : Unit = (last,x) match {	      
	      case (List(p2,p1),p3) => f.lift((p1,p2,p3)) match {
	        case Some(c) =>
	          last = List(c)	          

	        case None =>
	          last = List(p3,p2)
	          b += p1
	      }
	      case (l,x) =>
	        last = x :: l
	    }
	  s.foreach(seek)
	  b ++= last.reverse
	  b.result()
	}
}

