package br.gov.lexml.parser.pl.linker

import java.util.Date
import br.gov.lexml.parser.pl.block.*
import br.gov.lexml.parser.pl.output.*

import scala.annotation.targetName

case class URN(
    ano: Int,
    mesdia: Option[(Int, Int)],
    fragment: String,
    urn: String
) extends Ordered[URN]:
  lazy val base = urn.takeWhile(_ != '!')
  override def compare(u: URN) =
    List(
      ano.compare(u.ano),
      mesdia.map(_._1).getOrElse(0).compare(u.mesdia.map(_._1).getOrElse(0)),
      mesdia.map(_._2).getOrElse(0).compare(u.mesdia.map(_._2).getOrElse(0)),
      fragment.compare(u.fragment),
      urn.compare(u.urn)
    ).dropWhile(_ == 0).headOption.getOrElse(0)

object URN:
  val urnRe =
    """^urn:lex:br:[^:]*:[^:]*:(\d\d\d\d(?:-\d\d-\d\d)?);[^!]*(!.*)?$""".r
  val dataCompletaRe = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
  def fromString(urn: String) =
    urn match {
      case urnRe(date, fragment) => {
        date match {
          case dataCompletaRe(ano, mes, dia) =>
            Some(
              URN(
                ano.toInt,
                Some((mes.toInt, dia.toInt)),
                if fragment == null || fragment.isEmpty then  ""
                else fragment.substring(1) ,
                urn
              )
            )
          case _ =>
            Some(
              URN(
                date.toInt,
                None,
                if fragment == null || fragment.isEmpty then  ""
                else fragment.substring(1),
                urn
              )
            )
        }
      }
      case _ => None
    }

  def matchAll(urns: List[URN], frags: List[String]): List[UMatch] =
    for {
      urn <- urns
      frag <- frags.map(_.replaceAll("1u", "1"))
    } yield {
      val ufrag = urn.fragment
      val ulen = ufrag.length
      val flen = frag.length
      val altMatch = NeighborMatch
        .check(ufrag, frag)
        .map(NeighborMatch(urn, frag, _))
        .getOrElse(NoMatch(urn, frag))
      if ulen == flen then
        if ufrag == frag then ExactMatch(urn)
        else altMatch
      else if flen > ulen then
        if frag startsWith ufrag then PrefixMatch(urn, frag)
        else altMatch
      else
        if ufrag endsWith frag then SuffixMatch(urn, frag)
        else
          val firstComp = frag.takeWhile(_ != '_')
          val idx = ufrag.indexOf(firstComp)
          if (idx > 0) { PartialMatch(urn, frag, ufrag.substring(0, idx)) }
          else if (idx == 0) { SuperPrefixMatch(urn, frag) }
          else { altMatch }
    }
end URN

case class MatchCount(
    val exact: Int = 0,
    val prefix: Int = 0,
    val suffix: Int = 0,
    val superPrefix: Int = 0,
    val partial: Int = 0,
    val neighbor: Int = 0,
    val noMatch: Int = 0
):
  @targetName("plus")
  def +(m: UMatch): MatchCount = m match {
    case _: ExactMatch       => copy(exact = exact + 1)
    case _: PrefixMatch      => copy(prefix = prefix + 1)
    case _: SuffixMatch      => copy(suffix = suffix + 1)
    case _: SuperPrefixMatch => copy(superPrefix = superPrefix + 1)
    case _: PartialMatch     => copy(partial = partial + 1)
    case _: NeighborMatch    => copy(neighbor = neighbor + 1)
    case _: NoMatch          => copy(noMatch = noMatch + 1)
  }
  def compareTo(mc: MatchCount): Int =
    List(
      exact compareTo mc.exact,
      prefix compareTo mc.prefix,
      suffix compareTo mc.suffix,
      superPrefix compareTo mc.superPrefix,
      partial compareTo mc.partial,
      neighbor compareTo mc.neighbor,
      noMatch compareTo mc.noMatch
    ).dropWhile((0 == _)).headOption.getOrElse(0)
end MatchCount

case class MatchData(
    val urn: URN,
    val count: MatchCount = MatchCount(),
    val matches: Map[String, UMatch] = Map()
):
  @targetName("plus")
  def +(m: UMatch): MatchData =
    if (m.urn == urn) then
      copy(count = count + m, matches = matches + (m.fragment -> m))
    else this

  lazy val mapId: String => Option[String] =
    matches.view
      .mapValues(m => m.complement + m.fragment)
      .filter(x => x._1 != x._2)
      .lift
  def updateAlteracao(a: Alteracao): Alteracao =
    val baseId = a.id + "_"
    val blen = baseId.length
    def applyBlock(b: Block): Block = b match {
      case d: Dispositivo => {
        val d1 = mapId(d.id.substring(blen)) match {
          case None        => d
          case Some(newId) => d overrideId (baseId + newId)
        }
        d1 replaceChildren (applyBlocks(d.children))
      }
      case a: Alteracao =>
        throw new RuntimeException(
          "Nao pode haver alteracao dentro de alteracao"
        )
      case x => x
    }
    def applyBlocks(bl: List[Block]): List[Block] = bl.map(applyBlock(_))
    a.mapBlocks(applyBlocks(_)).copy(baseURN = Some(urn.base))

  lazy val anyNonMatch = matches.values.exists(_.isInstanceOf[NoMatch])
end MatchData

case class CompareKey(count: MatchCount, ano: Int, mesDia: Option[(Int, Int)])
    extends Ordered[CompareKey]:
  def compareMesDia(md1: Option[(Int, Int)], md2: Option[(Int, Int)]) =
    (md1, md2) match {
      case (None, _) => 1
      case (_, None) => -1
      case (Some((m1, d1)), Some((m2, d2))) =>
        List(m1 compareTo m2, d1 compareTo d2)
          .dropWhile((0 == _))
          .headOption
          .getOrElse(0)
    }
  override def compare(d: CompareKey): Int =
    List(
      d.count compareTo count,
      ano compareTo d.ano,
      compareMesDia(mesDia, d.mesDia)
    ).dropWhile((0 == _)).headOption.getOrElse(0)
end CompareKey

case class MatchByBase(
    base: String,
    ano: Int,
    mesdia: Option[(Int, Int)] = None,
    fragMap: Map[String, UMatch] = Map()
):
  @targetName("plus")
  def +(u: UMatch): MatchByBase =
    if u.urn.base == base then
      val bestMatch = fragMap.get(u.fragment) match {
        case None     => u
        case Some(u1) => u1 max u
      }
      this.copy(base: String, fragMap = fragMap + (u.fragment -> bestMatch))
    else this
  lazy val count: MatchCount = fragMap.values.foldLeft(MatchCount())(_ + _)
  lazy val allMatched: Boolean = fragMap.values.forall(!_.isInstanceOf[NoMatch])

  def updateAlteracao(a: Alteracao): Alteracao =
    val baseId = a.id + "_"
    val blen = baseId.length
    def applyBlock(b: Block): Block = b match {
      case d: Dispositivo =>
        val oid = d.id.substring(blen)
        val nid = oid.replaceAll("1u", "1")
        val d1 = mapId(nid) match {
          case None => d
          case Some(newId) =>
            val nid2 = baseId + newId
            val n = nid2.lastIndexOf(nid)
            val rid = nid2.patch(n, oid, oid.length)
            d overrideId (rid)
        }
        d1 replaceChildren (applyBlocks(d.children))
      case a: Alteracao =>
        throw new RuntimeException(
          s"Nao pode haver alteracao dentro de alteracao: ${a.id}"
        )
      case x => x
    }
    def applyBlocks(bl: List[Block]): List[Block] = bl.map(applyBlock(_))
    a.mapBlocks(applyBlocks(_)).copy(baseURN = Some(base))
  end updateAlteracao

  lazy val mapId: String => Option[String] =
    fragMap.view
      .mapValues(ma => ma.complement + ma.fragment)
      .filter(x => x._1 != x._2)
      .lift
end MatchByBase

case class MatchResult(val m: Map[String, MatchByBase] = Map()):

  @targetName("plus")
  def +(u: URN) =
    copy(m =
      m + (u.base -> m.getOrElse(u.base, MatchByBase(u.base, u.ano, u.mesdia)))
    )

  @targetName("plus")
  def +(mt: UMatch): MatchResult =
    copy(m =
      m + (mt.urn.base -> (m.getOrElse(
        mt.urn.base,
        MatchByBase(mt.urn.base, mt.urn.ano, mt.urn.mesdia)
      ) + mt))
    )
  lazy val rank: List[MatchByBase] =
    val (l1, l2) = m.values.partition(_.allMatched)
    def s(l: List[MatchByBase]) =
      l.sortBy(md => CompareKey(md.count, md.ano, md.mesdia))
    s(l1.toList) ++ s(l2.toList)

  lazy val first: Option[MatchByBase] = rank.headOption
end MatchResult

object MatchResult:
  def fromAlteracao(a: Alteracao, links: List[URN]): MatchResult =
    import br.gov.lexml.parser.pl.output.LexmlRenderer.renderId
    def getIds(b: Block): List[String] =
      b match {
        case d: Dispositivo => { d.id :: d.subDispositivos.flatMap(getIds) }
        case _              => Nil
      }
    val altIdLength = renderId(a.path).length() + 1
    val allIds =
      a.blocks
        .flatMap(getIds)
        .map(s =>
          if (s.length >= altIdLength) { s.substring(altIdLength) }
          else { s }
        )
    val matches = URN.matchAll(links, allIds)

    val mr = links.foldLeft(MatchResult())(_ + _)
    matches.foldLeft(mr)(_ + _)

abstract sealed class UMatch extends Ordered[UMatch]:
  val urn: URN
  val fragment: String
  val complement = ""
  val subComplement = ""
  def level: Int
  def compare(um: UMatch): Int =
    List(
      level.compare(um.level),
      urn.compare(um.urn),
      fragment.compare(um.fragment),
      complement.compare(um.complement),
      subComplement.compare(um.subComplement)
    ).dropWhile(_ == 0).headOption.getOrElse(0)

  def max(um: UMatch): UMatch =
    if um > this then um else this

case class ExactMatch(val urn: URN) extends UMatch:
  val fragment: String = urn.fragment
  def level = 10

case class PrefixMatch(val urn: URN, val fragment: String) extends UMatch:
  def level = 9

case class SuffixMatch(val urn: URN, val fragment: String) extends UMatch:
  override val complement: String =
    urn.fragment.substring(0, urn.fragment.length - fragment.length)
  def level = 8

case class SuperPrefixMatch(val urn: URN, val fragment: String) extends UMatch:
  def level = 7

case class NeighborMatch(
    val urn: URN,
    val fragment: String,
    override val subComplement: String
) extends UMatch:
  override val complement: String = urn.fragment + subComplement + "_"
  def level = 6

object NeighborMatch:
  private def lastComponent(s: String) =
    s.reverse.takeWhile(_ != '_').reverse.takeWhile(Character.isLetter)
  private def firstComponent(s: String) = s.takeWhile(Character.isLetter)
  val compatiblePairs: List[(String, String, String)] = List(
    ("art", "inc", "cpt"),
    ("art", "par", ""),
    ("par", "inc", ""),
    ("inc", "ali", ""),
    ("ali", "ite", ""),
    ("prt", "liv", ""),
    ("liv", "tit", ""),
    ("tit", "cap", ""),
    ("cap", "sec", ""),
    ("sec", "sub", "")
  )
  def check(refFrag: String, idFrag: String): Option[String] =
    val c1 = lastComponent(refFrag)
    val c2 = firstComponent(idFrag)
    val l = for {
      (cc1, cc2, comp) <- compatiblePairs
      if cc1 == c1 && cc2 == c2
    } yield { comp }
    l.headOption

  def level = 5
end NeighborMatch

case class PartialMatch(
    val urn: URN,
    val fragment: String,
    override val complement: String
) extends UMatch:
  def level = 4

case class NoMatch(val urn: URN, val fragment: String) extends UMatch:
  def level = 0