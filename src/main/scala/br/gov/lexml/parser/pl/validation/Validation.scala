package br.gov.lexml.parser.pl.validation

import br.gov.lexml.parser.pl.errors._
import scala.language.postfixOps
import scala.collection.immutable.Set
import br.gov.lexml.parser.pl.output.LexmlRenderer
import br.gov.lexml.parser.pl.block.Dispositivo
import br.gov.lexml.parser.pl.rotulo.Rotulo
import br.gov.lexml.parser.pl.block.Block
import br.gov.lexml.parser.pl.block.Alteracao
import br.gov.lexml.parser.pl.rotulo._
import scala.xml.NodeSeq
import scala.util.matching.Regex
import br.gov.lexml.parser.pl.util.ClassificadoresRegex
import br.gov.lexml.parser.pl.block.OL
import scala.xml.Node
import br.gov.lexml.parser.pl.block.Paragraph
import br.gov.lexml.parser.pl.block.Omissis
import br.gov.lexml.parser.pl.block.Table

final case class Path(rl: List[Rotulo]) extends Ordered[Path] {
  import LexmlRenderer.{ renderRotulo2 => render }
  def mkTexto(rl: List[Rotulo]): String = {    
    def mkTexto1(rl : List[Rotulo]) : String = {    
      rl match {  
        case Nil => ""
        case r1 :: rll => rll match {
          case r2 :: _ if r1.isDispositivo && r2.isAgregador => render(r1) + " (" + r2.proposicaoEm + " " + mkTexto1(rll) + ")"
          case r2 :: _ => render(r1) + " " + r2.proposicao + " " + mkTexto1(rll)
          case Nil => render(r1)
        }
      }
    }
    mkTexto1(rl)
  }

  lazy val txt = mkTexto(rl)

  override def toString: String = txt

  def +(r: Rotulo) = Path(rl :+ r)

  def compare(p: Path): Int = {
    def comp(l1: Seq[Rotulo], l2: Seq[Rotulo]): Int = (l1, l2) match {
      case (a :: al, b :: bl) => a.compare(b) match {
        case 0 => comp(al, bl)
        case x => x
      }
      case (Nil, Nil) => 0
      case (_, Nil) => 1
      case (Nil, _) => -1
    }
    comp(rl, p.rl)
  }
  override def equals(o: Any): Boolean = o match {
    case p: Path => compare(p) == 0
    case _ => false
  }
  override lazy val hashCode: Int = rl.foldLeft(41)((x, y) => 41 * (x + y.hashCode))
  val empty = rl.isEmpty
}


trait ToContext[-T] {
  def toContext(t : T) : Seq[String]
}

class Validation {

  type ValidationRule[P] = PartialFunction[P, Set[ParseProblem]]

  val es = Set[ParseProblem]()

  var ctx: Seq[String] = Seq()

  def in[B: ToContext, A](ctx1: B*)(x: => A): A = {
    val tc = implicitly[ToContext[B]]
    val old_ctx = ctx
    ctx = ctx1.flatMap(tc.toContext) ++ ctx
    try {
      x
    } catch {
      case ex: ParseException =>
        ex.errors.foreach(_.in(ctx: _*))
        throw ex
    }
    finally {
      ctx = old_ctx
    }
  }

  def withContext(p: ParseProblem): ParseProblem = {
    p.in(ctx: _*)
  }

  def withContext(p: Set[ParseProblem]): Set[ParseProblem] = p.map(withContext)

  private val tcAny: ToContext[Any] = new ToContext[Any] {
    def toContext(t: Any) = {
      Seq()
    }
  }

  implicit val tcString: ToContext[String] = new ToContext[String] {
    def toContext(t: String) = Seq(t)
  }

  implicit val tcBlock: ToContext[Block] = new ToContext[Block] {
    def toContext(b: Block): Seq[String] = b match {
      case p: Paragraph => Seq(p.text)
      case a: Alteracao =>
        Seq("Na alteração '" + a.blocks.flatMap(toContext).take(1) + "'")
      case d: Dispositivo =>
        d.path.map(LexmlRenderer.renderRotulo2)
      case o: Omissis => Seq("No omissis " + o)
      case x => throw new RuntimeException("toContext: bloco nao esperado: " + x)

    }
  }

  def verificaTodos[P](vl: ValidationRule[P]*)(implicit tc: ToContext[P]): ValidationRule[P] = {
    case p =>
      in(p) {
        vl.map((f: ValidationRule[P]) => f.lift(p).getOrElse(es)).foldLeft(es)(_ | _)
      }
  }

  def paraTodoConjuntoDeIrmaos(vl: ValidationRule[(Path, List[Rotulo])]*): ValidationRule[List[Block]] = {
    case (bl: List[Block]) => {
      val vf = verificaTodos(vl: _*)(tcAny)

      def verifica(p: Path, bl: List[Block], res: Set[ParseProblem]): Set[ParseProblem] = {
        val ds = bl.collect({ case d: Dispositivo => d })
        val resHere = vf.lift(p, ds.map(_.rotulo)).getOrElse(es)
        val resSubdisps = ds.map { d => in(d) {
          verifica(p + d.rotulo, d.subDispositivos, es)
        }
        }
        resHere | resSubdisps.foldLeft(es) {
          _ | _
        }
      }

      verifica(Path(List()), bl, es)
    }
  }

  def paraTodoIrmaoConsecutivo(vl: ValidationRule[(Path, Rotulo, Rotulo)]*): ValidationRule[(Path, List[Rotulo])] = {
    case (pai: Path, rl: List[Rotulo]) => rl match {
      case (r1 :: (rll@(_ :: _))) => {
        val vf = verificaTodos(vl: _*)(tcAny)
        rll.foldLeft((r1, es))({ case ((r1, s), r2) => (r2, s | vf.lift(pai, r1, r2).getOrElse(es)) })._2
      }
      case _ => es
    }
  }

  def paraTodoParagrafo(vl: ValidationRule[Seq[Node]]*): ValidationRule[List[Block]] = {
    val vf = verificaTodos(vl: _*)(tcAny)

    def rule: ValidationRule[Block] = {
      case d: Dispositivo => in(d) {
        recurse(d.conteudo.toList ++ d.subDispositivos ++ d.titulo.toList)
      }
      case a: Alteracao => in(a) {
        recurse(a.blocks)
      }
      case p: Paragraph => in(p) {
        vf(p.nodes)
      }
    }

    def recurse(l: List[Block]): Set[ParseProblem] = l.collect(rule).foldLeft(Set[ParseProblem]())(_ union _)

    val r: ValidationRule[List[Block]] = {
      case bl: List[Block] => recurse(bl)
    }
    r
  }

  def paraTodaAlteracao(vl: ValidationRule[Alteracao]*): ValidationRule[List[Block]] = {
    val vf = verificaTodos(vl: _*)

    def rule: ValidationRule[Block] = {
      case d: Dispositivo => in(d) {
        recurse(d.conteudo.toList ++ d.subDispositivos ++ d.titulo.toList)
      }
      case a: Alteracao => in(a) {
        vf(a)
      }
    }

    def recurse(l: List[Block]): Set[ParseProblem] = l.collect(rule).foldLeft(Set[ParseProblem]())(_ union _)

    val r: ValidationRule[List[Block]] = {
      case bl: List[Block] => recurse(bl)
    }
    r
  }

  def paraTodoDispositivo(vl: ValidationRule[Dispositivo]*): ValidationRule[List[Block]] = {
    case (bl: List[Block]) => {
      val vf = verificaTodos(vl: _*)
      val r = bl.collect {
        case d: Dispositivo => in(d) {
          vf.lift(d).getOrElse(Set[ParseProblem]()).union(
            paraTodoDispositivo(vf)(d.children))
        }
      }
      r.foldLeft(Set[ParseProblem]())(_ union _)
    }
  }

  def paraTodoPaiOpcional_e_Filho(vl: ValidationRule[(Option[Block], Block)]*) = {
    val vf = verificaTodos(vl: _*)(tcAny)

    def verifica(b: Block, pai: Option[Block]): Set[ParseProblem] = {
      val e1 = vf.lift((pai, b)).getOrElse(Set())
      val cl = b match {
        case d: Dispositivo => in(d) {
          d.subDispositivos
        }
        case a: Alteracao => in(a) {
          a.blocks
        }
        case _ => List()
      }
      val e2 = cl.map(verifica(_, Some(b))).foldLeft(Set[ParseProblem]())(_ union _)
      e1 union e2
    }

    val r: ValidationRule[List[Block]] = {
      case bl: List[Block] => bl.map(verifica(_, None)).foldLeft(Set[ParseProblem]())(_ union _)
    }
    r
  }

  val semTabelasPorEnquanto: ValidationRule[(Option[Block], Block)] = {
    case (Some(d: Dispositivo), _: Table) => Set(ElementoNaoSuportado("tabela", Some(Path(d.path).txt)))
    case (_, _: Table) => Set(ElementoNaoSuportado("tabela"))
  }

  val noTopoSoDispositivos: ValidationRule[(Option[Block], Block)] = {
    case (None, p: Paragraph) => {
      Set(ElementoArticulacaoNaoReconhecido("", "Text Paragraph: " + p.text))
    }
  }

  def paraTodosOsCaminhos(vl: ValidationRule[List[(Path, Block)]]*) = {
    val vf = verificaTodos(vl: _*)(tcAny)

    def collectPaths(b: Block): List[(Path, Block)] = b match {
      case (d: Dispositivo) => (Path(d.path), d) :: d.subDispositivos.flatMap(collectPaths)
      case (a: Alteracao) => (Path(a.path), a) :: a.blocks.flatMap(collectPaths)
      case _ => List()
    }

    val r: ValidationRule[List[Block]] = {
      case bl: List[Block] => vf.lift(bl.flatMap(collectPaths)).getOrElse(Set())
    }
    r
  }

  def paraTodo[T : ToContext](vl: ValidationRule[T]*) = {
    val vf = verificaTodos(vl: _*)
    val r: ValidationRule[List[T]] = {
      case l: List[T] => l.collect(vf).foldLeft(Set[ParseProblem]())(_ union _)
    }
    r
  }

  def toMultiSet[T](i: Iterable[T]): Map[T, Int] = {
    i.foldLeft(Map[T, Int]()) {
      case (m, x) => m + (x -> (m.getOrElse(x, 0) + 1))
    }
  }

  def procDuplicado(data : (Path,List[Block])) : Option[ParseProblem] = data match {
    case (p, bl) if bl.length > 1 =>
      val itsOk = if (p.rl.exists(x => x.nivel == niveis.alteracao)
        && bl.length == 2) {
        (bl(0), bl(1)) match {
          case (d1: Dispositivo, d2: Dispositivo) => (d1.rotulo, d2.rotulo) match {
            case (RotuloParagrafo(Some(1), None, true),
            RotuloParagrafo(Some(1), None, false)) => true
            case _ => false
          }
          case _ => false
        }
      } else { false }
      if (!itsOk) {
        val c = bl.map(tcBlock.toContext).map(_.mkString(":")).mkString(", ")
        in(c) {
          Some(withContext(RotuloDuplicado(p.txt)))
        }
      } else {
        None
      }
    case _ => None
  }
  val naoPodeHaverCaminhoDuplicado: ValidationRule[List[(Path, Block)]] = {
    case l: List[(Path, Block)] => {
      l.groupBy(_._1).mapValues(_.map(_._2)).toList.flatMap(procDuplicado).toSet
    }
  }

  val somenteOmissisOuDispositivoEmAlteracao: ValidationRule[Alteracao] = {
    case a: Alteracao => in(a) {
      a.blocks collect {
        case p: Paragraph =>
          in(p) {
            withContext(ElementoArticulacaoNaoReconhecido(Path(a.path).txt,
              "Text Paragraph: " + p.text))
          }
      } toSet
    }
  }

  val somenteUmRotuloUnico: ValidationRule[(Path, List[Rotulo])] = {
    case (p, l) => {
      val possuiUnico = l.collect({
        case t: PodeSerUnico if t.unico => t
      })
      (for {
        r <- possuiUnico
        l1 = for {rr <- l; if rr != r; if rr.getClass == r.getClass} yield rr
        if l1.length > r.numRotulosQuandoTemUnico
      } yield {
        withContext(RotuloUnicoNaoUnico((p + r).txt, (p + l1.head).txt))
      }).toSet
    }
  }

  def lastPair[T](l: List[T]): Option[(T, T)] = l.reverse match {
    case (x :: y :: _) => Some(x, y)
    case _ => None
  }

  val niveisSubNiveisValidos: ValidationRule[(Path, Block)] = {
    case (Path(rl@(x :: xs)), bl)
      if x.isInstanceOf[RotuloAlteracao] &&
        xs.exists(_.isInstanceOf[RotuloAlteracao]) => in(bl) {
      //System.err.println(s"niveisSubNiveisValidos (caso 0): x=$x, rl=$rl")
      Set(withContext(PosicaoInvalida(Path(rl).txt).in()))
    }
    case (Path(rl@(x :: y :: xs)), bl)
      if xs.exists(_.isInstanceOf[RotuloAlteracao]) && !niveis.nivelSubNivelValidoTrans(y, x) => {
      in(bl) {
        //System.err.println(s"niveisSubNiveisValidos (caso 1): x=$x, y=$y, xs=$xs, rl=$rl")
        Set(withContext(PosicaoInvalida(Path(rl).txt).in()))
      }
    }
    case (Path(rl@(x :: y :: xs)), bl)
      if !xs.exists(_.isInstanceOf[RotuloAlteracao])
        && !x.isInstanceOf[RotuloAlteracao]
        && !y.isInstanceOf[RotuloAlteracao]
        && !niveis.nivelSubNivelValido(y, x) => {
      in(bl) {
        //System.err.println(s"niveisSubNiveisValidos (caso 2): x=$x, y=$y, xs=$xs, rl=$rl")
        Set(withContext(PosicaoInvalida(Path(rl).txt).in()))
      }
    }
  }

  def alineasSoDebaixoDeIncisos: ValidationRule[(Path, Block)] = {
    def check(rl: List[Rotulo]): Boolean = rl match {
      case (_: RotuloInciso) :: (_: RotuloAlinea) :: r => check(r)
      case (_: RotuloAlinea) :: _ => true
      case _ :: r => check(r)
      case Nil => false
    }

    def check2(rl: List[Rotulo]): Boolean = {
      val rl1 = rl.reverse.takeWhile(!_.isInstanceOf[RotuloAlteracao])
      check(rl1)
    }

    {
      case (Path(rl), bl) if check2(rl) =>
        //println(s"alineasSoDebaixoDeIncisos (2) rl=${rl}, bl=${bl}")
        in(bl) {
          Set(withContext(PosicaoInvalida(Path(rl).txt)))
        }
    }
  }

  lazy val listaNegraTextosDispositivos: ClassificadoresRegex[Dispositivo => TextoInvalido] = {
    import br.gov.lexml.parser.pl.util.Proc
    def procBuilder(r: Regex, msg: String) = (t: String, _: List[String]) =>
      (d: Dispositivo) =>
        in(d) {
          TextoInvalido(Path(d.path).txt, r.pattern.pattern(), t, msg)
        }

    ClassificadoresRegex.fromResource("lista-negra-textos-dispositivos.txt", procBuilder)
  }

  import br.gov.lexml.parser.pl.block.Paragraph

  def somenteDispositivosComTextoValido: ValidationRule[Dispositivo] = {
    case d: Dispositivo =>
      in(d) {
        d.conteudo match {
          case Some(p: Paragraph) => {
            val l = listaNegraTextosDispositivos.classifique(p.text)
            Set[ParseProblem](l.map(_ (d)): _*)
          }
          case _ => Set()
        }
      }
  }

  def todosOsPares[T](l: Seq[T]): List[(T, T)] = l match {
    case v1 :: (l2@(v2 :: r)) => (v1, v2) :: todosOsPares[T](l2)
    case _ => Nil
  }

  val naoPodeHaveOlLi: ValidationRule[Seq[Node]] = {
    case (ns: Seq[Node]) => {
      val ols = ((NodeSeq fromSeq ns) \\ "ol").length
      val lis = ((NodeSeq fromSeq ns) \\ "li").length
      if ((ols == 0) && (lis == 0)) {
        Set()
      } else {
        Set(PresencaEnumeracao)
      }
    }
  }


  def ordemInvertida(r1: Rotulo, r2: Rotulo): Boolean = (r1.nivel == r2.nivel) && (r1 > r2)

  def numeracaoContinua: ValidationRule[(Path, List[Rotulo])] = {
    case (p: Path, _) if p.rl.exists(_.isInstanceOf[RotuloAlteracao]) => Set[ParseProblem]()
    case (p: Path, rl: List[Rotulo]) => {
      val m = (rl :\ Map[Int, List[Rotulo]]()) {
        case (r: RotuloAlteracao, m) => m
        case (r: Rotulo, m) => m + (r.nivel -> (r :: m.getOrElse(r.nivel, List())))
      }
      val l = for {
        (_, rl1@(h :: _)) <- m
        l1: List[ParseProblem] = if (!h.canBeFirst) {
          val t = (p + h).txt.toString
          val d = withContext(DispositivoInicialNumeracaoInvalida(t))
          List(d)
        } else {
          List()
        }
        l2 = todosOsPares(rl).collect {
          case (r1, r2) if !ordemInvertida(r1, r2) && !r1.consecutivoContinuo(r2) =>
            DispositivosDescontinuos((p + r1).txt, (p + r2).txt)
        }
        e <- l1 ++ l2
      } yield {
        e
      }
      l.toSet
    }
  }

  val naoDevemHaverParagrafosNoMeio: ValidationRule[Dispositivo] = {
    case d: Dispositivo
      if d.subDispositivos.exists(x => x.isInstanceOf[Paragraph] || x.isInstanceOf[OL]) => {
      Set(ElementoArticulacaoNaoReconhecido(
        Path(d.path).txt,
        (d.subDispositivos collect {
          case p: Paragraph => "Text Paragraph: " + p.text
          case o: OL => "OL: " + o.toNodeSeq.text
        }): _*))
    }
  }

  val omissisSoEmAlteracao: ValidationRule[(Option[Block], Block)] = {
    case (Some(_: Alteracao), _: Omissis) => Set()
    case (Some(d: Dispositivo), _: Omissis) if d.path.exists(_.isInstanceOf[RotuloAlteracao]) => Set()
    case (Some(d: Dispositivo), _: Omissis) =>
      in(d) {
        Set(withContext(OmissisForaAlteracao(Some(Path(d.path).txt))))
      }
    case (c, _: Omissis) =>
      in(c.to(Seq): _*) {
        Set(withContext(OmissisForaAlteracao()))
      }
    case _ => Set()
  }

  val regras: ValidationRule[List[Block]] = verificaTodos(
    {
      case l: List[Block] if l.isEmpty => Set(ArticulacaoNaoIdentificada)
    }: ValidationRule[List[Block]],
    paraTodoConjuntoDeIrmaos(
      paraTodoIrmaoConsecutivo({
        case (p, r1, r2) if r1 == r2 =>
          Set(withContext(RotuloRepetido((p + r1).txt)))
        case (p, r1, r2) if ordemInvertida(r1, r2) =>
          Set(withContext(OrdemInvertida((p + r1).txt, (p + r2).txt)))
      }),
      somenteUmRotuloUnico,
      numeracaoContinua),
    paraTodosOsCaminhos(
      paraTodo(niveisSubNiveisValidos, alineasSoDebaixoDeIncisos)(tcAny),
      naoPodeHaverCaminhoDuplicado),
    paraTodoDispositivo(naoDevemHaverParagrafosNoMeio, somenteDispositivosComTextoValido),
    paraTodoParagrafo(naoPodeHaveOlLi),
    paraTodaAlteracao(somenteOmissisOuDispositivoEmAlteracao),
    paraTodoPaiOpcional_e_Filho(omissisSoEmAlteracao, semTabelasPorEnquanto, noTopoSoDispositivos))(tcAny)

  def validaEstrutura(bl: List[Block]): Set[ParseProblem] =
    regras.lift(bl).getOrElse(es)

  def validaComSchema(ns: NodeSeq): Set[ParseProblem] = {
    import _root_.javax.xml.stream._
    import scala.xml._
    import _root_.br.gov.lexml.schema.validator.Validador
    import _root_.br.gov.lexml.schema.validator.TipoSchema

    val validador = Validador.getInstance

    lazy val resolver = new XMLResolver() {
      override def resolveEntity(publicID: String, systemID: String, baseURI: String, namespace: String): AnyRef = {
        //println("resolver: requesting publicId = " + publicID + ", systemID : " + systemID + ", baseURI: " + baseURI)
        //Result can be: InputStream, XMLStreamReader, XMLEventReader or null for default
        null
      }
    }

    lazy val xmlInputFactory: XMLInputFactory = {
      val i = XMLInputFactory.newInstance
      import XMLInputFactory._
      i.setProperty(IS_NAMESPACE_AWARE, true)
      i.setProperty(IS_SUPPORTING_EXTERNAL_ENTITIES, true)
      i.setProperty(RESOLVER, resolver)
      i
    }

    def unchain(ex: Throwable): List[String] =
      ex.getMessage() :: Option(ex.getCause()).toList.
        flatMap(unchain(_))

    def validate(xml: String): Set[ParseProblem] = {
      import TipoSchema._
      val res = validador.valide(RIGIDO, xml)
      import scala.jdk.javaapi.CollectionConverters._
      (asScala(res.errosFatais) ++ asScala(res.erros)).map(ErroValidacaoSchema(_)).toSet
    }
    import scala.xml.Utility.serialize
    validate(serialize(ns.head, minimizeTags = MinimizeMode.Always).toString)
  }
}
