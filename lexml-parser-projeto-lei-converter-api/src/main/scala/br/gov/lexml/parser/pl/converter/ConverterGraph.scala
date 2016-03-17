package br.gov.lexml.parser.pl.converter

import br.gov.lexml.parser.pl.converter.graph.Graph
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

final case class EdgeData(value : Double, converter : Converter)

class ConverterGraph()(implicit ec : ExecutionContext) {  
  val graph : Graph[EdgeData,String] = Graph.empty          
      
  def +=(converter : Converter) {        
    val edges = for {
      (accept,cost) <- converter.acceptMimeTypes
      (provide,value) <- converter.provideMimeTypes     
    } {      
      graph.connect(accept,EdgeData(cost/value,converter),provide)
    }
  }
  def ++=(converters : Seq[Converter]) = converters.foreach(this += _)
  def apply(fromMimeType : String, toMimeType : String) : Option[Array[Byte] => Future[Array[Byte]]] = {
    val conv = graph.dijkstra(fromMimeType, toMimeType){case (_,e,_) => e.value }.foldLeft[Converter](Converter.identity(fromMimeType)) {
      case (c,(f,ed,t)) => c.chain(f,ed.converter)
    }
    Some((in : Array[Byte]) => conv.convert(fromMimeType,in,toMimeType))    
  }
}

object ConverterGraph {
  def create(
      generalExecutionContext : ExecutionContext = scala.concurrent.ExecutionContext.global,
      externalExecutionContext : ExecutionContext = scala.concurrent.ExecutionContext.global) = {
    val cg = new ConverterGraph()(generalExecutionContext)
    val convs = Converters.configConverters()(externalExecutionContext)
    cg ++= convs
    cg
  }
}
