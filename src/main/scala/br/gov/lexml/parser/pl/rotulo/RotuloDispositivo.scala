package br.gov.lexml.parser.pl.rotulo

import scala.util.matching._
import scala.xml._

sealed abstract class Rotulo extends AnyRef with Ordered[Rotulo] {
  val nivel: Int
  lazy val toNodeSeq: NodeSeq = <Rotulo nivel={ nivel.toString }/>
  val elemLabel: String
  val isDispositivo: Boolean
  val isAgregador: Boolean
  val compBase: Option[List[Int]]
  val proposicao: String

  def subRotulo(n: Int): Option[Rotulo] = None

  final def compare(r: Rotulo) =  {
      val ord = implicitly[Ordering[Iterable[Int]]]
      ord.compare((nivel :: compBase.getOrElse(List())).to[Iterable],
        (r.nivel :: r.compBase.getOrElse(List())).to[Iterable]
      )
  }


  final override lazy val hashCode: Int = {
    compBase match {
      case Some(l) ⇒ ((nivel + 41) :: l).foldLeft(41)({ case (x, y) ⇒ 41 * (x + y) })
      case _ ⇒ super.hashCode
    }
  }

  final override def equals(that: Any) = that match {
    case r: Rotulo ⇒ compare(r) == 0
    case _ ⇒ false
  }

  def consecutivoContinuo(r: Rotulo): Boolean = false
  def canBeFirst : Boolean  
}

trait RotuloDispositivo {
  val isDispositivo = true
  val isAgregador = false
}

trait RotuloAgregador {
  val isDispositivo = false
  val isAgregador = true
}

trait PodeSerUnico {
  val unico: Boolean
  val numRotulosQuandoTemUnico = 1
}

object niveis {
  val artigo = 110
  val paragrafo = 120
  val inciso = 130
  val alinea = 140
  val item = 150
  val pena = 160
  val parte = 10
  val livro = 20
  val titulo = 30
  val subtitulo = 40
  val capitulo = 50
  val subcapitulo = 60
  val secao = 70
  val subsecao = 80
  val alteracao = 1000

  val niveis = Set(artigo, paragrafo, inciso, alinea, item, pena, parte, livro, titulo, subtitulo, capitulo, subcapitulo, secao, subsecao, alteracao)
  val agrupadores = niveis.filter(_<=artigo)
  
  val nivel_maximo_aceito_na_raiz = artigo
  
  val niveisSubNiveis : Map[Int,Set[Int]] = Map(
        artigo -> Set(paragrafo)
      , paragrafo -> Set(inciso,alteracao,pena)
      , inciso -> Set(alinea,pena)
      , alinea -> Set(item,pena)
      , item -> Set(pena)
      , parte -> Set (livro,titulo,capitulo,secao,artigo)
      , livro -> Set (titulo,capitulo,secao,artigo)
      , titulo -> Set (capitulo,secao,artigo)
      , capitulo -> Set (subcapitulo,secao,artigo)
      , secao -> Set (subsecao,artigo)
      , subsecao -> Set (artigo)
      , alteracao -> agrupadores
  )
  val niveisSubNiveisTrans = {    
    var m : Map[Int,Set[Int]] = Map()
    var visited : Set[Int] = Set()   
    def visit(nivel : Int) : Set[Int] = {
      m.get(nivel) match {
        case None if visited(nivel) =>
          throw new RuntimeException("Error: circular reference for " + nivel)
        case None =>
          val l = niveisSubNiveis.getOrElse(nivel,Set())
          visited = visited + nivel
          val ll = ((l - alteracao).flatMap(visit))
          val res = ll ++ l
          m = m + (nivel -> res)
          res
        case Some(res) => res
      }
    }
    niveisSubNiveis.keys.foreach(visit)
    m
  }
  def nivelSubNivelValido(r : Rotulo, sr : Rotulo) = niveisSubNiveis.get(r.nivel).map(s => s.contains(sr.nivel)).getOrElse(false)
  def nivelSubNivelValidoTrans(r : Rotulo, sr : Rotulo) = niveisSubNiveisTrans.get(r.nivel).map(s => s.contains(sr.nivel)).getOrElse(false)
}

trait WithNumComp extends Rotulo {
  val num : Int
  val comp : Option[Int]
  override def canBeFirst : Boolean = num == 1 && comp.isEmpty 
}

case class RotuloArtigo(num: Int, comp: Option[Int] = None, unico: Boolean = false)
  extends Rotulo with RotuloDispositivo with PodeSerUnico {
  val nivel = niveis.artigo
  override lazy val toNodeSeq = <RotuloArtigo num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Artigo"
  val compBase = Some(num :: comp.toList)
  val proposicao = "do"
  override def subRotulo(n: Int) = Some(RotuloParagrafo(Some(n)))
  override def consecutivoContinuo(r: Rotulo) = 
    r match {
      case RotuloArtigo(num1, comp1, _) if num1 == num ⇒ (comp, comp1) match {
        case (None, Some(1)) ⇒ true
        case (Some(n1), Some(n2)) ⇒ n2 == n1 + 1
        case _ ⇒ false
      }
      case RotuloArtigo(num1, comp1, _) ⇒ num1 == num + 1 && comp1.isEmpty
      case r: Rotulo ⇒ r.nivel < nivel
    }
    
  override def canBeFirst = true
}

case class RotuloParagrafo(num: Option[Int] = None, comp: Option[Int] = None, unico: Boolean = false) extends Rotulo with RotuloDispositivo with PodeSerUnico {
  val nivel = niveis.paragrafo
  override lazy val toNodeSeq = num match {
    case None ⇒ <RotuloCaput/>
    case Some(n) ⇒ <RotuloParagrafo num={ n.toString } comp={ comp.mkString("", "", "") }/>
  }
  val elemLabel = num match { case Some(_) ⇒ "Paragrafo"; case _ ⇒ "Caput" }
  val compBase = Some(num.getOrElse(-1) :: comp.toList)
  val proposicao = "do"
  override def subRotulo(n: Int) = Some(RotuloInciso(n))
  override def consecutivoContinuo(r: Rotulo) =    
    r match {
      case RotuloParagrafo(None,_,_) => false
      case RotuloParagrafo(Some(1),None,_) => num.isEmpty
      case RotuloParagrafo(Some(num1),None,_) => num.map(num1 == _+ 1).getOrElse(false)
      case RotuloParagrafo(Some(num1),Some(0),_) => num.map(num1 == _).getOrElse(false)
      case RotuloParagrafo(Some(num1),Some(comp1),_) => num.map(num1 == _).getOrElse(false) && comp.map(comp1 == _ + 1).getOrElse(false)
      case _ => false
    }
  
  override val numRotulosQuandoTemUnico = 2
  override def canBeFirst = num.isEmpty && comp.isEmpty
}

trait HasRegularContinuity[T <: Rotulo] extends Rotulo with WithNumComp {
  val num: Int
  val comp: Option[Int]
  override def consecutivoContinuo(r: Rotulo) = r match {
    case rr: HasRegularContinuity[T] if (num == rr.num) ⇒ (comp, rr.comp) match {
      case (None, Some(0)) ⇒ true
      case (Some(n1), Some(n2)) ⇒ n2 == n1 + 1
      case _ ⇒ false
    }
    case rr: HasRegularContinuity[T] if (num + 1 == rr.num) ⇒ rr.comp.isEmpty
    case _ ⇒ false
  }
}

trait NoMatterContinuity extends Rotulo {
  override def consecutivoContinuo(r: Rotulo) = true
}

case class RotuloInciso(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloDispositivo with HasRegularContinuity[RotuloInciso] {
  val nivel = niveis.inciso
  override lazy val toNodeSeq = <RotuloInciso num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Inciso"
  val compBase = Some(num :: comp.toList)
  val proposicao = "do"
  override def subRotulo(n: Int) = Some(RotuloAlinea(n))
}
case class RotuloAlinea(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloDispositivo with HasRegularContinuity[RotuloAlinea] {
  val nivel = niveis.alinea
  override lazy val toNodeSeq = <RotuloAlinea num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Alinea"
  val compBase = Some(num :: comp.toList)
  val proposicao = "da"
  override def subRotulo(n: Int) = Some(RotuloItem(n))
}
case class RotuloItem(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloDispositivo with HasRegularContinuity[RotuloItem] {
  val nivel = niveis.item
  override lazy val toNodeSeq = <RotuloItem num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Item"
  val compBase = Some(num :: comp.toList)
  val proposicao = "do"
}

case object RotuloPena extends Rotulo with RotuloDispositivo {
  val nivel = niveis.pena
  override lazy val toNodeSeq = <RotuloPena/>
  val elemLabel = "Pena"
  val compBase = Some(List())
  val proposicao = "da"
  override def canBeFirst = true
}

trait WithEitherNumComp extends Rotulo {
  val num : Either[String,Int]
  val comp : Option[Int]
  override def canBeFirst = (num,comp) match {
    case (Left(_),None) => true
    case (Right(1),None) => true
    case _ => false
  }
}

case class RotuloParte(num: Either[String, Int], comp: Option[Int] = None) 
extends Rotulo with RotuloAgregador with NoMatterContinuity with WithEitherNumComp {
  val nivel = niveis.parte
  override lazy val toNodeSeq = <RotuloParte num={ num.fold(x ⇒ x, x ⇒ x.toString) } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Parte"
  val compBase = num.fold(_ ⇒ None, n ⇒ Some(n :: comp.toList))
  val proposicao = "da"  
}
case class RotuloLivro(num: Either[String, Int], comp: Option[Int] = None) extends Rotulo with RotuloAgregador with NoMatterContinuity with WithEitherNumComp {
  val nivel = niveis.livro
  override lazy val toNodeSeq = <RotuloLivro num={ num.fold(x ⇒ x, x ⇒ x.toString) } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Livro"
  val compBase = num.fold(_ ⇒ None, n ⇒ Some(n :: comp.toList))
  val proposicao = "do"
}
case class RotuloTitulo(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloAgregador with HasRegularContinuity[RotuloTitulo] {
  val nivel = niveis.titulo
  override lazy val toNodeSeq = <RotuloTitulo num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Titulo"
  val compBase = Some(num :: comp.toList)
  val proposicao = "do"
}
case class RotuloSubTitulo(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloAgregador with HasRegularContinuity[RotuloSubTitulo] {
  val nivel = niveis.subtitulo
  override lazy val toNodeSeq = <RotuloSubtitulo num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "SubTitulo"
  val compBase = Some(num :: comp.toList)
  val proposicao = "do"
}
case class RotuloCapitulo(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloAgregador with HasRegularContinuity[RotuloCapitulo] {
  val nivel = niveis.capitulo
  override lazy val toNodeSeq = <RotuloCapitulo num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Capitulo"
  val compBase = Some(num :: comp.toList)
  val proposicao = "do"
}
case class RotuloSubCapitulo(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloAgregador with HasRegularContinuity[RotuloSubCapitulo] {
  val nivel = niveis.subcapitulo
  override lazy val toNodeSeq = <RotuloSubCapitulo num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "SubCapitulo"
  val compBase = Some(num :: comp.toList)
  val proposicao = "do"
}
case class RotuloSecao(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloAgregador with HasRegularContinuity[RotuloSecao] {
  val nivel = niveis.secao
  override lazy val toNodeSeq = <RotuloSecao num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "Secao"
  val compBase = Some(num :: comp.toList)
  val proposicao = "da"
}
case class RotuloSubSecao(num: Int, comp: Option[Int] = None) extends Rotulo with RotuloAgregador with HasRegularContinuity[RotuloSubSecao] {
  val nivel = niveis.subsecao
  override lazy val toNodeSeq = <RotuloSubSecao num={ num.toString } comp={ comp.mkString("", "", "") }/>
  val elemLabel = "SubSecao"
  val compBase = Some(num :: comp.toList)
  val proposicao = "da"
}

case class RotuloAlteracao(num: Int) extends Rotulo with NoMatterContinuity {
  val nivel = niveis.alteracao
  override lazy val toNodeSeq = <RotuloAlteracao num={ num.toString }/>
  val elemLabel = "Alteracao"
  val isDispositivo = false
  val isAgregador = false
  val compBase = Some(List(num))
  val proposicao = "da"
  override def canBeFirst = num == 1
}

