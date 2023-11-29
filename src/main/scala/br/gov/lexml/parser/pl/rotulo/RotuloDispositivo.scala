package br.gov.lexml.parser.pl.rotulo

import scala.util.matching.*
import scala.xml.*

sealed abstract class Rotulo extends AnyRef with Ordered[Rotulo]:
  val nivel: Int
  lazy val toNodeSeq: NodeSeq = <Rotulo nivel={nivel.toString}/>
  val elemLabel: String
  val isDispositivo: Boolean
  val isAgregador: Boolean
  val compBase: Option[List[Int]]
  val proposicao: String
  val proposicaoEm: String

  def subRotulo(n: Int): Option[Rotulo] = None

  final def compare(r: Rotulo) : Int =
    import Ordering.Implicits._
    val ord = implicitly[Ordering[Seq[Int]]]
    ord.compare(
      (nivel :: compBase.getOrElse(List())).to(Seq),
      (r.nivel :: r.compBase.getOrElse(List())).to(Seq)
    )

  final override lazy val hashCode: Int =
    compBase match {
      case Some(l) =>
        ((nivel + 41) :: l).foldLeft(41)({ case (x, y) => 41 * (x + y) })
      case _ => super.hashCode
    }

  final override def equals(that: Any) : Boolean = that match {
    case r: Rotulo => compare(r) == 0
    case _         => false
  }

  def consecutivoContinuo(r: Rotulo): Boolean = false
  def canBeFirst: Boolean
end Rotulo

trait RotuloFeminino extends Rotulo:
    override final val proposicao : String = "da"
    override final val proposicaoEm : String = "na"

trait RotuloMasculino extends Rotulo:
  override final val proposicao : String = "do"
  override final val proposicaoEm : String = "no"

trait RotuloDispositivo:
  val isDispositivo = true
  val isAgregador = false

trait PodeSerUnico:
  val unico: Boolean
  val numRotulosQuandoTemUnico = 1

trait RotuloAgregador extends PodeSerUnico:
  override final val numRotulosQuandoTemUnico = 1
  val isDispositivo = false
  val isAgregador = true

object niveis:
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

  val niveis : Set[Int] = Set(
    artigo,
    paragrafo,
    inciso,
    alinea,
    item,
    pena,
    parte,
    livro,
    titulo,
    subtitulo,
    capitulo,
    subcapitulo,
    secao,
    subsecao,
    alteracao
  )
  private val agrupadores : Set[Int] = niveis.filter(_ <= artigo)

  val nivel_maximo_aceito_na_raiz : Int = artigo

  private val niveisSubNiveis: Map[Int, Set[Int]] = Map(
    artigo -> Set(paragrafo),
    paragrafo -> Set(inciso, alteracao, pena),
    inciso -> Set(alinea, pena),
    alinea -> Set(item, pena),
    item -> Set(pena),
    parte -> Set(livro, titulo, capitulo, secao, artigo),
    livro -> Set(titulo, capitulo, secao, artigo),
    titulo -> Set(capitulo, secao, artigo, subtitulo),
    subtitulo -> Set(capitulo, secao, artigo),
    capitulo -> Set(subcapitulo, secao, artigo),
    secao -> Set(subsecao, artigo),
    subsecao -> Set(artigo),
    alteracao -> agrupadores
  )
  private val niveisSubNiveisTrans = {
    var m: Map[Int, Set[Int]] = Map()
    var visited: Set[Int] = Set()
    def visit(nivel: Int): Set[Int] = {
      m.get(nivel) match {
        case None if visited(nivel) =>
          throw new RuntimeException("Error: circular reference for " + nivel)
        case None =>
          val l = niveisSubNiveis.getOrElse(nivel, Set())
          visited = visited + nivel
          val ll = (l - alteracao).flatMap(visit)
          val res = ll ++ l
          m = m + (nivel -> res)
          res
        case Some(res) => res
      }
    }
    niveisSubNiveis.keys.foreach(visit)
    m
  }
  def nivelSubNivelValido(r: Rotulo, sr: Rotulo): Boolean =
    r.isInstanceOf[RotuloAlteracao] ||
    niveisSubNiveis
      .get(r.nivel).exists(s => s.contains(sr.nivel))

  def nivelSubNivelValidoTrans(r: Rotulo, sr: Rotulo): Boolean = niveisSubNiveisTrans
    .get(r.nivel).exists(s => s.contains(sr.nivel))

end niveis


sealed trait WithNumComp extends Rotulo:
  val num: Int
  val comp: Option[Int]
  override def canBeFirst: Boolean = num == 1 && comp.isEmpty

case class RotuloArtigo(
    num: Int,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloDispositivo
    with PodeSerUnico
    with RotuloMasculino:
  val nivel: Int = niveis.artigo
  override lazy val toNodeSeq: Elem = <RotuloArtigo num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "Artigo"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

  override def subRotulo(n: Int): Option[RotuloParagrafo] = Some(RotuloParagrafo(Some(n)))
  override def consecutivoContinuo(r: Rotulo): Boolean =
    r match {
      case RotuloArtigo(num1, comp1, _) if num1 == num =>
        (comp, comp1) match {
          case (None, Some(0))      => true
          case (Some(n1), Some(n2)) => n2 == n1 + 1
          case _                    => false
        }
      case RotuloArtigo(num1, comp1, _) => num1 == num + 1 && comp1.isEmpty
      case r: Rotulo                    => r.nivel < nivel
    }

  override def canBeFirst = true
end RotuloArtigo

case class RotuloParagrafo(
    num: Option[Int] = None,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloDispositivo
    with PodeSerUnico
    with RotuloMasculino:
  val nivel: Int = niveis.paragrafo
  override lazy val toNodeSeq: Elem = num match {
    case None => <RotuloCaput/>
    case Some(n) =>
      <RotuloParagrafo num={n.toString} comp={comp.mkString("", "", "")}/>
  }
  val elemLabel: String = num match { case Some(_) => "Paragrafo"; case _ => "Caput" }
  val compBase: Option[List[Int]] = Some(num.getOrElse(-1) :: comp.toList)

  override def subRotulo(n: Int): Option[RotuloInciso] = Some(RotuloInciso(n))
  override def consecutivoContinuo(r: Rotulo): Boolean =
    r match {
      case RotuloParagrafo(None, _, _)       => false
      case RotuloParagrafo(Some(1), None, _) => num.isEmpty
      case RotuloParagrafo(Some(num1), None, _) =>
        num.exists(num1 == _ + 1)
      case RotuloParagrafo(Some(num1), Some(0), _) =>
        num.contains(num1)
      case RotuloParagrafo(Some(num1), Some(comp1), _) =>
        num.contains(num1) && comp.exists(comp1 == _ + 1)
      case _ => false
    }

  override val numRotulosQuandoTemUnico = 2
  override def canBeFirst: Boolean = num.isEmpty && comp.isEmpty
end RotuloParagrafo

sealed trait HasRegularContinuity[T <: Rotulo] extends Rotulo with WithNumComp:
  val num: Int
  val comp: Option[Int]
  override def consecutivoContinuo(r: Rotulo): Boolean = r match {
    case rr: HasRegularContinuity[T] if num == rr.num =>
      (comp, rr.comp) match {
        case (None, Some(0))      => true
        case (Some(n1), Some(n2)) => n2 == n1 + 1
        case _                    => false
      }
    case rr: HasRegularContinuity[T] if num + 1 == rr.num => rr.comp.isEmpty
    case _                                                  => false
  }

sealed trait ContinuityUnnecessary extends Rotulo:
  override def consecutivoContinuo(r: Rotulo) = true

case class RotuloInciso(num: Int, comp: Option[Int] = None)
    extends Rotulo
    with RotuloDispositivo
    with HasRegularContinuity[RotuloInciso]
    with RotuloMasculino:
  val nivel: Int = niveis.inciso
  override lazy val toNodeSeq: Elem = <RotuloInciso num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "Inciso"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)
  override def subRotulo(n: Int): Option[Rotulo] = Some(RotuloAlinea(n))

case class RotuloAlinea(num: Int, comp: Option[Int] = None)
    extends Rotulo
    with RotuloDispositivo
    with HasRegularContinuity[RotuloAlinea]
    with RotuloFeminino:
  val nivel: Int = niveis.alinea
  override lazy val toNodeSeq: Elem = <RotuloAlinea num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "Alinea"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

  override def subRotulo(n: Int): Option[RotuloItem] = Some(RotuloItem(n))
  override def consecutivoContinuo(r: Rotulo): Boolean = r match {
    case rr: RotuloAlinea
        if (num == 'j'.toInt - 'a'.toInt + 1) && (rr.num == num + 2) =>
      rr.comp.isEmpty
    case _ => super.consecutivoContinuo(r)
  }
end RotuloAlinea

case class RotuloItem(num: Int, comp: Option[Int] = None)
    extends Rotulo
    with RotuloDispositivo
    with HasRegularContinuity[RotuloItem]
    with RotuloMasculino:
  val nivel: Int = niveis.item
  override lazy val toNodeSeq: Elem = <RotuloItem num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "Item"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

case object RotuloPena
  extends Rotulo
    with RotuloDispositivo
    with RotuloFeminino:
  val nivel: Int = niveis.pena
  override lazy val toNodeSeq: Elem = <RotuloPena/>
  val elemLabel = "Pena"
  val compBase: Option[List[Nothing]] = Some(List())
  override def canBeFirst = true

case class RotuloDispositivoGenerico(
    nomeRotulo: String,
    num: Int = 0,
    nivel: Int = niveis.pena,
    proposicao: String = "da",
    proposicaoEm: String = "na",
    override val canBeFirst: Boolean = true
) extends Rotulo
    with RotuloDispositivo
    with HasRegularContinuity[RotuloDispositivoGenerico]:
  override lazy val toNodeSeq: Elem = <RotuloGenerico nome={nomeRotulo} nivel={
    nivel.toString
  }/>
  val elemLabel = "DispositivoGenerico"
  val compBase: Option[List[Int]] = Some(List(num))
  val comp: Option[Int] = None

sealed trait WithEitherNumComp extends Rotulo:
  val num: Either[String, Int]
  val comp: Option[Int]
  override def canBeFirst: Boolean = (num, comp) match {
    case (Left(_), None)  => true
    case (Right(1), None) => true
    case _                => false
  }

case class RotuloParte(
    num: Either[String, Int],
    comp: Option[Int] = None,
    unico: Boolean = false,
    ordinalExtenso: Boolean = false,
    rotulo: Option[String] = None
) extends Rotulo
    with RotuloAgregador
    with ContinuityUnnecessary
    with WithEitherNumComp
    with RotuloFeminino:
  val nivel: Int = niveis.parte
  override lazy val toNodeSeq: Elem = <RotuloParte num={
    num.fold(x => x, x => x.toString)
  } comp={comp.mkString("", "", "")}/>
  val elemLabel = "Parte"
  val compBase: Option[List[Int]] = num.fold(_ => None, n => Some(n :: comp.toList))

case class RotuloLivro(
    num: Either[String, Int],
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloAgregador
    with ContinuityUnnecessary
    with WithEitherNumComp
    with RotuloMasculino:
  val nivel: Int = niveis.livro
  override lazy val toNodeSeq: Elem = <RotuloLivro num={
    num.fold(x => x, x => x.toString)
  } comp={comp.mkString("", "", "")}/>
  val elemLabel = "Livro"
  val compBase: Option[List[Int]] = num.fold(_ => None, n => Some(n :: comp.toList))

case class RotuloTitulo(
    num: Int,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloAgregador
    with HasRegularContinuity[RotuloTitulo]
    with RotuloMasculino:
  val nivel: Int = niveis.titulo
  override lazy val toNodeSeq: Elem = <RotuloTitulo num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "Titulo"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

case class RotuloSubTitulo(
    num: Int,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloAgregador
    with HasRegularContinuity[RotuloSubTitulo]
    with RotuloMasculino:
  val nivel: Int = niveis.subtitulo
  override lazy val toNodeSeq: Elem = <RotuloSubtitulo num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "SubTitulo"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

case class RotuloCapitulo(
    num: Int,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloAgregador
    with HasRegularContinuity[RotuloCapitulo]
    with RotuloMasculino:
  val nivel: Int = niveis.capitulo
  override lazy val toNodeSeq: Elem = <RotuloCapitulo num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "Capitulo"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)
  override def consecutivoContinuo(r: Rotulo): Boolean =
    super.consecutivoContinuo(r) || (r match {
      case RotuloSubTitulo(1, None, _) => true
      case _                           => false
    })

case class RotuloSubCapitulo(
    num: Int,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloAgregador
    with HasRegularContinuity[RotuloSubCapitulo]
    with RotuloMasculino:
  val nivel: Int = niveis.subcapitulo
  override lazy val toNodeSeq: Elem = <RotuloSubCapitulo num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "SubCapitulo"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

case class RotuloSecao(
    num: Int,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloAgregador
    with HasRegularContinuity[RotuloSecao]
    with RotuloFeminino:
  val nivel: Int = niveis.secao
  override lazy val toNodeSeq: Elem = <RotuloSecao num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "Secao"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

case class RotuloSubSecao(
    num: Int,
    comp: Option[Int] = None,
    unico: Boolean = false
) extends Rotulo
    with RotuloAgregador
    with HasRegularContinuity[RotuloSubSecao]
    with RotuloFeminino:
  val nivel: Int = niveis.subsecao
  override lazy val toNodeSeq: Elem = <RotuloSubSecao num={num.toString} comp={
    comp.mkString("", "", "")
  }/>
  val elemLabel = "SubSecao"
  val compBase: Option[List[Int]] = Some(num :: comp.toList)

case class RotuloAlteracao(num: Int)
  extends Rotulo
    with ContinuityUnnecessary
    with RotuloFeminino:
  val nivel: Int = niveis.alteracao
  override lazy val toNodeSeq: Elem = <RotuloAlteracao num={num.toString}/>
  val elemLabel = "Alteracao"
  val isDispositivo = false
  val isAgregador = false
  val compBase: Option[List[Int]] = Some(List(num))
  override def canBeFirst: Boolean = num == 1
