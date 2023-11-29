package br.gov.lexml.parser.pl.validation

import br.gov.lexml.parser.pl.errors.*

import scala.language.postfixOps
import scala.collection.immutable.Set
import br.gov.lexml.parser.pl.output.LexmlRenderer
import br.gov.lexml.parser.pl.block.Dispositivo
import br.gov.lexml.parser.pl.rotulo.Rotulo
import br.gov.lexml.parser.pl.block.Block
import br.gov.lexml.parser.pl.block.Alteracao
import br.gov.lexml.parser.pl.block.Paragraph
import br.gov.lexml.parser.pl.rotulo.*

import scala.xml.NodeSeq
import scala.util.matching.Regex
import br.gov.lexml.parser.pl.util.ClassificadoresRegex
import br.gov.lexml.parser.pl.block.OL

import scala.xml.Node
import br.gov.lexml.parser.pl.block.Omissis
import br.gov.lexml.parser.pl.block.Table

import scala.annotation.{tailrec, targetName}

final case class Path(rl: List[Rotulo]) extends Ordered[Path]:
  import LexmlRenderer.{renderRotulo2 => render}
  private def mkTexto(rl: List[Rotulo]): String =
      rl match {
        case Nil => ""
        case r1 :: rll =>
          rll match {
            case r2 :: _ if r1.isDispositivo && r2.isAgregador =>
              render(r1) + " (" + r2.proposicaoEm + " " + mkTexto(rll) + ")"
            case r2 :: _ =>
              render(r1) + " " + r2.proposicao + " " + mkTexto(rll)
            case Nil => render(r1)
          }
      }

  lazy val txt : String = mkTexto(rl)

  override def toString: String = txt

  @targetName("plus")
  def +(r: Rotulo) : Path = Path(rl :+ r)

  def compare(p: Path): Int =
    @tailrec
    def comp(l1: Seq[Rotulo], l2: Seq[Rotulo]): Int = (l1, l2) match {
      case (a :: al, b :: bl) =>
        a.compare(b) match {
          case 0 => comp(al, bl)
          case x => x
        }
      case (Nil, Nil) => 0
      case (_, Nil)   => 1
      case (Nil, _)   => -1
    }
    comp(rl, p.rl)

  override def equals(o: Any): Boolean = o match {
    case p: Path => compare(p) == 0
    case _       => false
  }
  override lazy val hashCode: Int =
    rl.foldLeft(41)((x, y) => 41 * (x + y.hashCode))
end Path

trait ToContext[-T]:
  def toContext(t: T): Seq[String]

class Validation:
  private type ValidationRule[P] = PartialFunction[P, Set[ParseProblem]]

  private val es = Set[ParseProblem]()

  private var ctx: Seq[String] = Seq()

  def in[B: ToContext, A](ctx1: B*)(x: => A): A =
    val tc = implicitly[ToContext[B]]
    val old_ctx = ctx
    ctx = ctx1.flatMap(tc.toContext) ++ ctx
    try {
      x
    } catch {
      case ex: ParseException =>
        ex.errors.foreach(_.in(ctx*))
        throw ex
    } finally {
      ctx = old_ctx
    }

  @inline private def withContext(p: ParseProblem): ParseProblem =
    p.in(ctx*)

  given tcAny: ToContext[Any] = (_: Any) => Seq()

  given tcString: ToContext[String] = (t: String) => Seq(t)

  given tcBlock: ToContext[Block] = {
    case p: Paragraph => Seq(p.text)
    case a: Alteracao =>
      Seq(s"Na alteração '${a.blocks.flatMap(tcBlock.toContext).take(1)}'")
    case d: Dispositivo =>
      d.path.map(LexmlRenderer.renderRotulo2)
    case o: Omissis => Seq(s"No omissis $o")
    case x =>
      throw new RuntimeException(s"toContext: bloco nao esperado: $x")
  }

  @inline private def verificaTodos[P](vl: ValidationRule[P]*)(using tc: ToContext[P]): ValidationRule[P] =
    p => in(p) {
      vl.map((f: ValidationRule[P]) => f.lift(p).getOrElse(es))
        .foldLeft(es)(_ | _)
    }

  private def paraTodoConjuntoDeIrmaos(
      vl: ValidationRule[(Path, List[Rotulo])]*
  ): ValidationRule[List[Block]] =
    case bl: List[Block] =>
      val vf = verificaTodos(vl*)

      def verifica(
          p: Path,
          bl: List[Block],
          res: Set[ParseProblem]
      ): Set[ParseProblem] =
        val ds = bl.collect({ case d: Dispositivo => d })
        val resHere = vf.lift(p, ds.map(_.rotulo)).getOrElse(es)
        val resSubdisps = ds.map { d =>
          in(d) {
            verifica(p + d.rotulo, d.subDispositivos, es)
          }
        }
        resHere | resSubdisps.foldLeft(es) { _ | _ }
      verifica(Path(List()), bl, es)
  end paraTodoConjuntoDeIrmaos

  private def paraTodoIrmaoConsecutivo(
      vl: ValidationRule[(Path, Rotulo, Rotulo)]*
  ): ValidationRule[(Path, List[Rotulo])] =
    case (pai: Path, rl: List[Rotulo]) =>
      rl match {
        case r1 :: (rll @ _ :: _) =>
          val vf = verificaTodos(vl*)
          rll
            .foldLeft((r1, es))({ case ((r1, s), r2) =>
              (r2, s | vf.lift(pai, r1, r2).getOrElse(es))
            })
            ._2
        case _ => es
      }

  private def paraTodoParagrafo(
      vl: ValidationRule[Seq[Node]]*
  ): ValidationRule[List[Block]] =
    val vf = verificaTodos(vl*)

    def rule: ValidationRule[Block] =
      case d: Dispositivo =>
        in(d) {
          recurse(d.conteudo.toList ++ d.subDispositivos ++ d.titulo.toList)
        }
      case a: Alteracao =>
        in(a) {
          recurse(a.blocks)
        }
      case p: Paragraph =>
        in(p) {
          vf(p.nodes)
        }

    def recurse(l: List[Block]): Set[ParseProblem] =
      l.collect(rule).foldLeft(Set[ParseProblem]())(_ union _)

    val r: ValidationRule[List[Block]] =
      (bl: List[Block]) => recurse(bl)
    r
  end paraTodoParagrafo


  private def paraTodaAlteracao(
      vl: ValidationRule[Alteracao]*
  ): ValidationRule[List[Block]] =
    val vf = verificaTodos(vl*)

    def rule: ValidationRule[Block] = {
      case d: Dispositivo =>
        in(d) {
          recurse(d.conteudo.toList ++ d.subDispositivos ++ d.titulo.toList)
        }
      case a: Alteracao =>
        in(a) {
          vf(a)
        }
    }

    def recurse(l: List[Block]): Set[ParseProblem] =
      l.collect(rule).foldLeft(Set[ParseProblem]())(_ union _)

    val r: ValidationRule[List[Block]] =
      (bl: List[Block]) => recurse(bl)
    r
  end paraTodaAlteracao

  private def paraTodoDispositivo(
      vl: ValidationRule[Dispositivo]*
  ): ValidationRule[List[Block]] =
    (bl: List[Block]) =>
      val vf = verificaTodos(vl*)
      val r = bl.collect { case d: Dispositivo =>
        in(d) {
          vf.lift(d)
            .getOrElse(Set[ParseProblem]())
            .union(paraTodoDispositivo(vf)(d.children))
        }
      }
      r.foldLeft(Set[ParseProblem]())(_ union _)

  private def paraTodoPaiOpcional_e_Filho(
      vl: ValidationRule[(Option[Block], Block)]*
  ): ValidationRule[List[Block]] =
    val vf = verificaTodos(vl*)

    def verifica(b: Block, pai: Option[Block]): Set[ParseProblem] =
      val e1 = vf.lift((pai, b)).getOrElse(Set())
      val cl = b match {
        case d: Dispositivo =>
          in(d) {
            d.subDispositivos
          }
        case a: Alteracao =>
          in(a) {
            a.blocks
          }
        case _ => List()
      }
      val e2 =
        cl.map(verifica(_, Some(b))).foldLeft(Set[ParseProblem]())(_ union _)
      e1 union e2
    end verifica

    val r: ValidationRule[List[Block]] =
      (bl: List[Block]) =>
        bl.map(verifica(_, None)).foldLeft(Set[ParseProblem]())(_ union _)
    r
  end paraTodoPaiOpcional_e_Filho

  private val semTabelasPorEnquanto: ValidationRule[(Option[Block], Block)] = {
    case (Some(d: Dispositivo), _: Table) =>
      Set(ElementoNaoSuportado("tabela", Some(Path(d.path).txt)))
    case (_, _: Table) => Set(ElementoNaoSuportado("tabela"))
  }

  private val noTopoSoDispositivos: ValidationRule[(Option[Block], Block)] = {
    case (None, p: Paragraph) =>
      Set(ElementoArticulacaoNaoReconhecido("", "Text Paragraph: " + p.text))
  }

  private def paraTodosOsCaminhos(vl: ValidationRule[List[(Path, Block)]]*): ValidationRule[List[Block]] =
    val vf = verificaTodos(vl*)

    def collectPaths(b: Block): List[(Path, Block)] = b match {
      case d: Dispositivo =>
        (Path(d.path), d) :: d.subDispositivos.flatMap(collectPaths)
      case a: Alteracao => (Path(a.path), a) :: a.blocks.flatMap(collectPaths)
      case _              => List()
    }

    val r: ValidationRule[List[Block]] =
      (bl: List[Block] )=>
        vf.lift(bl.flatMap(collectPaths)).getOrElse(Set())
    r

  private def paraTodo[T: ToContext](vl: ValidationRule[T]*): ValidationRule[List[T]] =
    val vf = verificaTodos(vl*)
    val r: ValidationRule[List[T]] =
      (l: List[T]) => l.collect(vf).foldLeft(Set[ParseProblem]())(_ union _)
    r

  private def procDuplicado(data: (Path, List[Block])): Option[ParseProblem] =
    data match {
      case (p, bl) if bl.length > 1 =>
        val itsOk =
          if p.rl.exists(x => x.nivel == niveis.alteracao)
             && bl.length == 2
          then
            (bl.head, bl(1)) match {
              case (d1: Dispositivo, d2: Dispositivo) =>
                (d1.rotulo, d2.rotulo) match {
                  case (
                        RotuloParagrafo(Some(1), None, true),
                        RotuloParagrafo(Some(1), None, false)
                      ) =>
                    true
                  case _ => false
                }
              case _ => false
            }
          else false
        if !itsOk then
          val c = bl.map(tcBlock.toContext).map(_.mkString(":")).mkString(", ")
          in(c) {
            Some(withContext(RotuloDuplicado(p.txt)))
          }
        else
          None
      case _ => None
    }

  private val naoPodeHaverCaminhoDuplicado: ValidationRule[List[(Path, Block)]] =
    (l: List[(Path, Block)]) =>
      l.groupBy(_._1).view.mapValues(_.map(_._2)).toList.flatMap(procDuplicado).toSet

  private val somenteOmissisOuDispositivoEmAlteracao: ValidationRule[Alteracao] =
    (a: Alteracao) =>
      in(a) {
        a
          .blocks
          .collect { case p: Paragraph =>
            in(p) {
              withContext(
                ElementoArticulacaoNaoReconhecido(
                  Path(a.path).txt,
                  s"Text Paragraph: ${p.text}"
                )
              )
            }
          }
          .toSet
      }

  private val somenteUmRotuloUnico: ValidationRule[(Path, List[Rotulo])] =
    (p, l) =>
      val possuiUnico = l.collect {
        case t: PodeSerUnico if t.unico => t
      }
      (for {
        r <- possuiUnico
        l1 = for { rr <- l; if rr != r; if rr.getClass == r.getClass } yield rr
        if l1.length > r.numRotulosQuandoTemUnico
      } yield {
        withContext(RotuloUnicoNaoUnico((p + r).txt, (p + l1.head).txt))
      }).toSet

  private val niveisSubNiveisValidos: ValidationRule[(Path, Block)] = {
    case (Path(rl @ x :: xs), bl)
        if x.isInstanceOf[RotuloAlteracao] &&
          xs.exists(_.isInstanceOf[RotuloAlteracao]) =>
      in(bl) {
        Set(withContext(PosicaoInvalida(Path(rl).txt).in()))
      }
    case (Path(rl @ x :: y :: xs), bl)
        if xs.exists(_.isInstanceOf[RotuloAlteracao]) && !niveis
          .nivelSubNivelValidoTrans(y, x) =>
      in(bl) {
        Set(withContext(PosicaoInvalida(Path(rl).txt).in()))
      }
    case (Path(rl @ x :: y :: xs), bl)
        if !xs.exists(_.isInstanceOf[RotuloAlteracao])
          && !x.isInstanceOf[RotuloAlteracao]
          && !y.isInstanceOf[RotuloAlteracao]
          && !niveis.nivelSubNivelValido(y, x) =>
      in(bl) {
        Set(withContext(PosicaoInvalida(Path(rl).txt).in()))
      }
  }

  private def alineasSoDebaixoDeIncisos: ValidationRule[(Path, Block)] =
    @tailrec def check(rl: List[Rotulo]): Boolean = rl match {
      case (_: RotuloInciso) :: (_: RotuloAlinea) :: r => check(r)
      case (_: RotuloAlinea) :: _                      => true
      case _ :: r                                      => check(r)
      case Nil                                         => false
    }

    def check2(rl: List[Rotulo]): Boolean =
      val rl1 = rl.reverse.takeWhile(!_.isInstanceOf[RotuloAlteracao])
      check(rl1)

    {
      case (Path(rl), bl) if check2(rl) =>
        // println(s"alineasSoDebaixoDeIncisos (2) rl=${rl}, bl=${bl}")
        in(bl) {
          Set(withContext(PosicaoInvalida(Path(rl).txt)))
        }
    }
  end alineasSoDebaixoDeIncisos

  private lazy val listaNegraTextosDispositivos
      : ClassificadoresRegex[Dispositivo => TextoInvalido] =

    def procBuilder(r: Regex, msg: String) = (t: String, _: List[String]) =>
      (d: Dispositivo) =>
        in(d) {
          TextoInvalido(Path(d.path).txt, r.pattern.pattern(), t, msg)
        }

    ClassificadoresRegex.fromResource(
      "lista-negra-textos-dispositivos.txt",
      procBuilder
    )

  private def somenteDispositivosComTextoValido: ValidationRule[Dispositivo] =
    case d: Dispositivo =>
      in(d) {
        d.conteudo match {
          case Some(p: Paragraph) =>
            val l = listaNegraTextosDispositivos.classifique(p.text)
            Set[ParseProblem](l.map(_(d))*)
          case _ => Set()
        }
      }

  private def todosOsPares[T](l: Seq[T]): List[(T, T)] = l match {
    case v1 :: (l2 @ v2 :: _) => (v1, v2) :: todosOsPares[T](l2)
    case _                      => Nil
  }

  private val naoPodeHaveOlLi: ValidationRule[Seq[Node]] =
    (ns: Seq[Node]) =>
      val ols = ((NodeSeq fromSeq ns) \\ "ol").length
      val lis = ((NodeSeq fromSeq ns) \\ "li").length
      if (ols == 0) && (lis == 0) then Set() else Set(PresencaEnumeracao)

  private def ordemInvertida(r1: Rotulo, r2: Rotulo): Boolean =
    (r1.nivel == r2.nivel) && (r1 > r2)

  private def numeracaoContinua: ValidationRule[(Path, List[Rotulo])] =
    case (p: Path, _) if p.rl.exists(_.isInstanceOf[RotuloAlteracao]) =>
      Set[ParseProblem]()
    case (p: Path, rl: List[Rotulo]) =>
      val m = rl.foldRight(Map[Int, List[Rotulo]]()) {
        case (_: RotuloAlteracao, m) => m
        case (r: Rotulo, m) =>
          m + (r.nivel -> (r :: m.getOrElse(r.nivel, List())))
      }
      val l = for {
        case (_, h :: _) <- m
        l1: List[ParseProblem] =
          if !h.canBeFirst then
            val t = (p + h).txt
            val d = withContext(DispositivoInicialNumeracaoInvalida(t))
            List(d)
          else List()
        l2 = todosOsPares(rl).collect {
          case (r1, r2)
              if !ordemInvertida(r1, r2) && !r1.consecutivoContinuo(r2) =>
            DispositivosDescontinuos((p + r1).txt, (p + r2).txt)
        }
        e <- l1 ++ l2
      } yield {
        e
      }
      l.toSet
  end numeracaoContinua

  private val naoDevemHaverParagrafosNoMeio: ValidationRule[Dispositivo] = {
    case d: Dispositivo
        if d.subDispositivos
          .exists(x => x.isInstanceOf[Paragraph] || x.isInstanceOf[OL]) =>
      Set(
        ElementoArticulacaoNaoReconhecido(
          Path(d.path).txt,
          d.subDispositivos collect {
            case p: Paragraph => s"Text Paragraph: ${p.text}"
            case o: OL        => s"OL: ${o.toNodeSeq.text}"
          }*
        )
      )
  }

  private val omissisSoEmAlteracao: ValidationRule[(Option[Block], Block)] = {
    case (Some(_: Alteracao), _: Omissis) => Set()
    case (Some(d: Dispositivo), _: Omissis)
        if d.path.exists(_.isInstanceOf[RotuloAlteracao]) =>
      Set()
    case (Some(d: Dispositivo), _: Omissis) =>
      in(d) {
        Set(withContext(OmissisForaAlteracao(Some(Path(d.path).txt))))
      }
    case (c, _: Omissis) =>
      in(c.to(Seq)*) {
        Set(withContext(OmissisForaAlteracao()))
      }
    case _ => Set()
  }

  private val regras: ValidationRule[List[Block]] = verificaTodos(
    {
      case l: List[Block] if l.isEmpty => Set(ArticulacaoNaoIdentificada)
    }: ValidationRule[List[Block]],
    paraTodoConjuntoDeIrmaos(
      paraTodoIrmaoConsecutivo {
        case (p, r1, r2) if r1 == r2 =>
          Set(withContext(RotuloRepetido((p + r1).txt)))
        case (p, r1, r2) if ordemInvertida(r1, r2) =>
          Set(withContext(OrdemInvertida((p + r1).txt, (p + r2).txt)))
      },
      somenteUmRotuloUnico,
      numeracaoContinua
    ),
    paraTodosOsCaminhos(
      paraTodo(niveisSubNiveisValidos, alineasSoDebaixoDeIncisos)(tcAny),
      naoPodeHaverCaminhoDuplicado
    ),
    paraTodoDispositivo(
      naoDevemHaverParagrafosNoMeio,
      somenteDispositivosComTextoValido
    ),
    paraTodoParagrafo(naoPodeHaveOlLi),
    paraTodaAlteracao(somenteOmissisOuDispositivoEmAlteracao),
    paraTodoPaiOpcional_e_Filho(
      omissisSoEmAlteracao,
      semTabelasPorEnquanto,
      noTopoSoDispositivos
    )
  )

  def validaEstrutura(bl: List[Block]): Set[ParseProblem] =
    regras.lift(bl).getOrElse(es)

  def validaComSchema(ns: NodeSeq): Set[ParseProblem] =
    import scala.xml.*
    import _root_.br.gov.lexml.schema.validator.Validador
    import _root_.br.gov.lexml.schema.validator.TipoSchema

    val validador = Validador.getInstance

    def validate(xml: String): Set[ParseProblem] =
      import TipoSchema._
      val res = validador.valide(RIGIDO, xml)
      import scala.jdk.javaapi.CollectionConverters._
      (asScala(res.errosFatais) ++ asScala(res.erros))
        .map(ErroValidacaoSchema(_))
        .toSet

    import scala.xml.Utility.serialize
    validate(serialize(ns.head, minimizeTags = MinimizeMode.Always).toString)
end Validation

