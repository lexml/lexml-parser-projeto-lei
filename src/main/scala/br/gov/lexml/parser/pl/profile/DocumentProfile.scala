package br.gov.lexml.parser.pl.profile
import scala.util.matching.Regex
import scala.language.postfixOps

trait RegexProfile {
  def regexLocalData : List[Regex] = List()
	def regexJustificativa : List[Regex] = List()
	def regexAnexos : List[Regex] = List()
	def regexLegislacaoCitada : List[Regex] = List()
	def regexAssinatura : List[Regex] = List()
	def regexEpigrafe : List[Regex] = List()  
	def regexPosEpigrafe : List[Regex] = List()
	def epigrafeObrigatoria : Boolean = true
	def preEpigrafePermitida : Boolean = true
	def regexPreambulo : List[Regex] = List()
	def ementaAusente : Boolean = false
	final def regProfileAsMap : Map[String,Any] = Map(
	    "localData" -> regexLocalData.map(_.pattern.pattern()),
	    "justificacativa" -> regexJustificativa.map(_.pattern.pattern()),
	    "anexos" -> regexAnexos.map(_.pattern.pattern()),
	    "legislacaoCitada" -> regexLegislacaoCitada.map(_.pattern.pattern()),
	    "assinatura" -> regexAssinatura.map(_.pattern.pattern()),
	    "epigrafe" -> regexEpigrafe.map(_.pattern.pattern()),
	    "posEpigrafe" -> regexPosEpigrafe.map(_.pattern.pattern()),
	    "preambulo" -> regexPreambulo.map(_.pattern.pattern()),
	    "epigrafeObrigatoria" -> epigrafeObrigatoria,
	    "preEpigrafePermitida" -> preEpigrafePermitida,
	    "ementaAusente" -> ementaAusente	    
	    )
}

trait EpigrafeOpcional extends RegexProfile {
  override def epigrafeObrigatoria = false
}

trait PreEpigrafeProibida extends RegexProfile {
  override def preEpigrafePermitida = false
}

trait EmentaAusente extends RegexProfile {
  override def ementaAusente = true
}


trait DefaultRegexProfile extends RegexProfile {
	override def regexLocalData: List[Regex] = super.regexLocalData ++ List(
	      "^sala da sessao"r,
	      "^sala das sessoes"r,
	      "^sala das comissoes"r,
	      "^sala da comissao"r,
	      "^camara dos deputados"r,
	      "^senado federal"r,
	      "^brasilia,"r
	)
	override def regexJustificativa: List[Regex] = super.regexJustificativa ++ List(
	    "^justificacao"r,
	    "^j u s t i f i c a c a o"r,
	    "^justificativa"r,
	    "^j u s t i f i c a t i v a"r
	)
	override def regexAnexos: List[Regex] = super.regexAnexos ++ List(
	    "^anexo"r,
	    "a n e x o"r
	)
	override def regexLegislacaoCitada: List[Regex] = super.regexLegislacaoCitada ++ List(
	    "^legislacao citada"r,
	    "^l e g i s l a c a o c i t a d a"r
	)
	override def regexAssinatura: List[Regex] = super.regexAssinatura ++ List("^senadora? "r)
	override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List(
          """^\s*(red\d+;+)?(projeto( de)? (lei|decreto legislativo)|(proposta|projeto) de emenda|pec|projeto de resolu)""".r
         ,"""^(n[oº°˚]|complementar)"""r
    )
    
    override def regexPosEpigrafe: List[Regex] = super.regexPosEpigrafe ++ List(
        """^\s*(\(.*\)|autora?:.*|autoria d.*|(d[oa] )?senador.*)\s*$""".r
    )
    
    override def regexPreambulo: List[Regex] = super.regexPreambulo ++ List(
        "^o (congress+o nacional|senado federal) (decret[oa]|resolve|promulg[oa])"r,
        """^[ao] president[ae] (da republica|do senado)"""r,
        "^[ao] vice-president[ae] da republica"r,
        "^as? mesas?"r,
        """^faco saber que [oa] president[ea] da republica""".r
       )
}

trait TipoNormaProfile {
  def urnFragTipoNorma : String
  def epigrafeHead : String  
  def epigrafeTail : String = ""
  val isProjetoNorma : Boolean = false
    
  import org.clapper.scalasti._
    
  def epigrafeTemplateCode : String = """<epigrafeHead> <epigrafeRepr> <epigrafeTail>"""
  def epigrafeSemIdTemplateCode : String = """<epigrafeHead> Nº LEXML_EPIGRAFE_NUMERO de LEXML_EPIGRAFE_DATA <epigrafeTail>"""
  
  final def epigrafeTemplate = ST(epigrafeTemplateCode)

  final def epigrafeSemIdTemplate = ST(epigrafeSemIdTemplateCode)

  final def tipoNormaProfileAsMap : Map[String,Any] = Map(
      "urnFragTipoNorma" -> urnFragTipoNorma,
      "epigrafeHead" -> epigrafeHead,
      "epigrafeTail" -> epigrafeTail,
      "isProjetoNorma" -> isProjetoNorma,
      "epigrafeTemplateCode" -> epigrafeTemplateCode,
      "epigrafeSemIdTemplateCode" -> epigrafeSemIdTemplateCode	    
	    )
}

trait AutoridadeProfile {
  def urnFragAutoridade : String
  def autoridadeEpigrafe : Option[String] = None
  final def autoridadeProfileAsMap : Map[String,Any] = Map(
      "urnFragAutoridade" -> urnFragAutoridade,
      "autoridadeEpigrafe" -> autoridadeEpigrafe	    
	    )
}

trait LocalidadeProfile {
  def urnFragLocalidade : Option[String] = None
  final def localidadeProfileAsMap : Map[String,Any] = Map(
      "urnFragLocalidaed" -> urnFragLocalidade	    
	    )
}

trait LocalidadeBR extends LocalidadeProfile {
  override def urnFragLocalidade = Some("br")
}

trait DocumentProfile extends RegexProfile with TipoNormaProfile with AutoridadeProfile with LocalidadeProfile {
  lazy val subTipoNorma: Option[String] = urnFragTipoNorma.split(";") match {
    case Array(_,st) => Some(st)
    case _ => None
  }
  def +(o : Overrides): DocumentProfileOverride =
      DocumentProfileOverride(this).replaceOverrides(o)
  final def asMap : Map[String,Any] = Map(
     "tipoNorma" -> tipoNormaProfileAsMap,
     "autoridade" -> autoridadeProfileAsMap,
     "localidade" -> localidadeProfileAsMap,
     "regex" -> regProfileAsMap
  )
  override def toString() = {
    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)    
    pw.println(super.toString() + ":")
    def printMap(m : Map[String,Any], indent : String) {
      m.foreach { case (k,v) =>
        pw.print(indent + k + ":")
        v match {
          case mm : Map[String,Any] =>
            pw.println()
            printMap(mm,indent + "  ")
          case _ => pw.println(" " + v)
        }
      }
    }
    printMap(asMap,"  ")
    pw.close()
    sw.toString()
  }
}

trait Overrides {
  val overrideRegexLocalData: Option[List[Regex]]
  val overrideRegexJustificativa: Option[List[Regex]]
  val overrideRegexAnexos: Option[List[Regex]]
  val overrideRegexLegislacaoCitada: Option[List[Regex]]
  val overrideRegexAssinatura: Option[List[Regex]]
  val overrideRegexEpigrafe: Option[List[Regex]]
  val overrideRegexPosEpigrafe: Option[List[Regex]]
  val overrideEpigrafeObrigatoria: Option[Boolean]
  val overridePreEpigrafePermitida: Option[Boolean]
  val overrideRegexPreambulo: Option[List[Regex]]
  val overrideUrnFragTipoNorma: Option[String]
  val overrideEpigrafeHead: Option[String]
  val overrideEpigrafeTail: Option[String]
  val overrideUrnFragAutoridade: Option[String]
  val overrideAutoridadeEpigrafe: Option[Option[String]]
  val overrideUrnFragLocalidade : Option[Option[String]]
  val overrideEmentaAusente : Option[Boolean]
}

final case class OverridesData(
  overrideRegexLocalData: Option[List[Regex]] = None,
  overrideRegexJustificativa: Option[List[Regex]] = None,
  overrideRegexAnexos: Option[List[Regex]] = None,
  overrideRegexLegislacaoCitada: Option[List[Regex]] = None,
  overrideRegexAssinatura: Option[List[Regex]] = None,
  overrideRegexEpigrafe: Option[List[Regex]] = None,
  overrideRegexPosEpigrafe: Option[List[Regex]] = None,
  overrideEpigrafeObrigatoria: Option[Boolean] = None,
  overridePreEpigrafePermitida: Option[Boolean] = None,
  overrideRegexPreambulo: Option[List[Regex]] = None,
  overrideUrnFragTipoNorma: Option[String] = None,
  overrideEpigrafeHead: Option[String] = None,
  overrideEpigrafeTail: Option[String] = None,
  overrideUrnFragAutoridade: Option[String] = None,
  overrideAutoridadeEpigrafe: Option[Option[String]] = None,
  overrideUrnFragLocalidade : Option[Option[String]] = None,
  overrideEmentaAusente : Option[Boolean] = None
) extends Overrides

final case class DocumentProfileOverride(base : DocumentProfile,
  overrideRegexLocalData: Option[List[Regex]] = None,
  overrideRegexJustificativa: Option[List[Regex]] = None,
  overrideRegexAnexos: Option[List[Regex]] = None,
  overrideRegexLegislacaoCitada: Option[List[Regex]] = None,
  overrideRegexAssinatura: Option[List[Regex]] = None,
  overrideRegexEpigrafe: Option[List[Regex]] = None,
  overrideRegexPosEpigrafe: Option[List[Regex]] = None,
  overrideEpigrafeObrigatoria: Option[Boolean] = None,
  overridePreEpigrafePermitida: Option[Boolean] = None,
  overrideRegexPreambulo: Option[List[Regex]] = None,
  overrideUrnFragTipoNorma: Option[String] = None,
  overrideEpigrafeHead: Option[String] = None,
  overrideEpigrafeTail: Option[String] = None,
  overrideUrnFragAutoridade: Option[String] = None,
  overrideAutoridadeEpigrafe: Option[Option[String]] = None,
  overrideUrnFragLocalidade : Option[Option[String]] = None,
  overrideEmentaAusente : Option[Boolean] = None
) extends DocumentProfile with Overrides {
  override def regexLocalData : List[Regex] = overrideRegexLocalData.getOrElse(base.regexLocalData)
  override def regexJustificativa: List[Regex] = overrideRegexJustificativa.getOrElse(base.regexJustificativa)
  override def regexAnexos: List[Regex] = overrideRegexAnexos.getOrElse(base.regexAnexos)
  override def regexLegislacaoCitada: List[Regex] = overrideRegexLegislacaoCitada.getOrElse(base.regexLegislacaoCitada)
  override def regexAssinatura: List[Regex] = overrideRegexAssinatura.getOrElse(base.regexAssinatura)
  override def regexEpigrafe: List[Regex] = overrideRegexEpigrafe.getOrElse(base.regexEpigrafe)
  override def regexPosEpigrafe: List[Regex] = overrideRegexPosEpigrafe.getOrElse(base.regexPosEpigrafe)
  override def epigrafeObrigatoria: Boolean = overrideEpigrafeObrigatoria.getOrElse(base.epigrafeObrigatoria)
  override def preEpigrafePermitida: Boolean = overridePreEpigrafePermitida.getOrElse(base.preEpigrafePermitida)
  override def regexPreambulo: List[Regex] = overrideRegexPreambulo.getOrElse(base.regexPreambulo)
  override def urnFragTipoNorma: String = overrideUrnFragTipoNorma.getOrElse(base.urnFragTipoNorma)
  override def epigrafeHead: String = overrideEpigrafeHead.getOrElse(base.epigrafeHead)
  override def epigrafeTail: String = overrideEpigrafeTail.getOrElse(base.epigrafeTail)
  override def urnFragAutoridade: String = overrideUrnFragAutoridade.getOrElse(base.urnFragAutoridade)
  override def autoridadeEpigrafe: Option[String] = overrideAutoridadeEpigrafe.getOrElse(base.autoridadeEpigrafe)
  override def urnFragLocalidade : Option[String] = overrideUrnFragLocalidade.getOrElse(base.urnFragLocalidade)
  override def ementaAusente : Boolean = overrideEmentaAusente.getOrElse(base.ementaAusente)
  
  val hasOverride : Boolean = this.productIterator.exists { _.isInstanceOf[Some[_]] }
  
  override def +(o : Overrides): DocumentProfileOverride = copy(
    overrideRegexLocalData = o.overrideRegexLocalData.orElse(overrideRegexLocalData),
    overrideRegexJustificativa = o.overrideRegexJustificativa.orElse(overrideRegexJustificativa),
    overrideRegexAnexos = o.overrideRegexAnexos.orElse(overrideRegexAnexos),
    overrideRegexLegislacaoCitada = o.overrideRegexLegislacaoCitada.orElse(overrideRegexLegislacaoCitada),
    overrideRegexAssinatura = o.overrideRegexAssinatura.orElse(overrideRegexAssinatura),
    overrideRegexEpigrafe = o.overrideRegexEpigrafe.orElse(overrideRegexEpigrafe),
    overrideRegexPosEpigrafe = o.overrideRegexPosEpigrafe.orElse(overrideRegexPosEpigrafe),
    overrideEpigrafeObrigatoria = o.overrideEpigrafeObrigatoria.orElse(overrideEpigrafeObrigatoria),
    overridePreEpigrafePermitida = o.overridePreEpigrafePermitida.orElse(overridePreEpigrafePermitida),
    overrideRegexPreambulo = o.overrideRegexPreambulo.orElse(overrideRegexPreambulo),
    overrideUrnFragTipoNorma = o.overrideUrnFragTipoNorma.orElse(overrideUrnFragTipoNorma),
    overrideEpigrafeHead = o.overrideEpigrafeHead.orElse(overrideEpigrafeHead),
    overrideEpigrafeTail = o.overrideEpigrafeTail.orElse(overrideEpigrafeTail),
    overrideUrnFragAutoridade = o.overrideUrnFragAutoridade.orElse(overrideUrnFragAutoridade),
    overrideAutoridadeEpigrafe = o.overrideAutoridadeEpigrafe.orElse(overrideAutoridadeEpigrafe),
    overrideUrnFragLocalidade  = o.overrideUrnFragLocalidade .orElse(overrideUrnFragLocalidade),
    overrideEmentaAusente = o.overrideEmentaAusente.orElse(overrideEmentaAusente)
  )
  
  def replaceOverrides(o : Overrides): DocumentProfileOverride = copy(
    overrideRegexLocalData = o.overrideRegexLocalData,
    overrideRegexJustificativa = o.overrideRegexJustificativa,
    overrideRegexAnexos = o.overrideRegexAnexos,
    overrideRegexLegislacaoCitada = o.overrideRegexLegislacaoCitada,
    overrideRegexAssinatura = o.overrideRegexAssinatura,
    overrideRegexEpigrafe = o.overrideRegexEpigrafe,
    overrideRegexPosEpigrafe = o.overrideRegexPosEpigrafe,
    overrideEpigrafeObrigatoria = o.overrideEpigrafeObrigatoria,
    overridePreEpigrafePermitida = o.overridePreEpigrafePermitida,
    overrideRegexPreambulo = o.overrideRegexPreambulo,
    overrideUrnFragTipoNorma = o.overrideUrnFragTipoNorma,
    overrideEpigrafeHead = o.overrideEpigrafeHead,
    overrideEpigrafeTail = o.overrideEpigrafeTail,
    overrideUrnFragAutoridade = o.overrideUrnFragAutoridade,
    overrideAutoridadeEpigrafe = o.overrideAutoridadeEpigrafe,
    overrideUrnFragLocalidade  = o.overrideUrnFragLocalidade,
    overrideEmentaAusente = o.overrideEmentaAusente
  )
}



object DocumentProfileRegister {  
  type Autoridade = String
  type TipoNorma = String
  type Localidade = Option[String]
  var profiles : Map[(Localidade,Autoridade,TipoNorma),DocumentProfile] = Map()
  def register(profile : DocumentProfile): Unit = { profiles = profiles + ((profile.urnFragLocalidade,profile.urnFragAutoridade,profile.urnFragTipoNorma) -> profile) }
  def getProfile(autoridade : Autoridade, tipoNorma : TipoNorma, localidade : Option[String] = Some("br")) : Option[DocumentProfile] = profiles.get((localidade,autoridade,tipoNorma))
  def profileByAutoridadeSigla(autoridade : Autoridade, sigla : String): Option[DocumentProfile] =
    profiles.filterKeys({ case (_,aut,tn) => aut == autoridade && tn.endsWith(";" + sigla)}).values.headOption
  def autoridades: Set[Autoridade] = profiles.keySet.map(_._2)
  def tiposDeNormasPorAutoridade(autoridade : String) : Set[String] = for { (_,aut,tn) <- profiles.keySet ; if aut == autoridade } yield tn
  def tiposDeNormas : Set[String] = profiles.keySet.map(_._2)
  def byUrnFrag(urnFrag : String) : Option[DocumentProfile] = urnFrag.split(":").toList match {
    case autoridade :: tipoNorma :: _ => getProfile(autoridade,tipoNorma)
    case _ => None
  }   
  
  def dumpProfiles(pw : java.io.PrintWriter = new java.io.PrintWriter(System.err)) {
      profiles.to[Vector].sortBy { case (x,y) => x }.foreach { 
      case ((loc,aut,tn),prof) =>
        System.err.println(s"localidade: ${loc}, autoridade=${aut}, tipoNorma: ${tn}")
        System.err.println(prof)
        System.err.println()
    }
  }
  
  val builtins: List[DocumentProfile] = List[DocumentProfile](
      ProjetoDeLeiDoSenadoNoSenado,
      ProjetoDeLeiDaCamaraNoSenado,
      ProjetoDeLeiDoCongressoNacional,
      PropostaEmendaConstitucionalNoSenado,
      ProjetoDeResolucaoDoSenado,
      ProjetoDeDecretoLegislativoDoSenadoNoSenado,
      ProjetoDeDecretoLegislativoDaCamaraNoSenado,
      ProjetoDeDecretoLegislativoDaCamaraNovaNomenclatura,
      ProjetoDeDecretoLegislativoDoSenadoNovaNomenclatura,
      ProjetoDeLeiComplementarDoSenadoNoSenado,
      ProjetoDeLeiComplementarDaCamaraNoSenado,
      ProjetoDeLeiNaCamara,
      ProjetoDeLeiNovaNomenclatura,
      ProjetoDeLeiComplementarNaCamara,
      ProjetoDeLeiComplementarNovaNomenclatura,
      ProjetoDeResolucaoNaCamara,
      MedidaProvisoriaNoCongresso,
      MedidaProvisoriaFederal,
      Lei,
      LeiComplementar,
      LeiDelegada,
      DecretoLei,
      Decreto,
      DecretoLegislativoDoCongresso,
      DecretoLegislativoFederal,
      EmendaConstitucional,
      RegimentoComumCongresso,
      RegimentoInternoCamara,
      RegimentoInternoSenado,
      ResolucaoDaCamara,
      ResolucaoDoCongresso,
      ResolucaoDoSenado,
      ProjetoDeLeiDoSenadoNoSenado,
      ConstituicaoFederal
  )
  builtins foreach register
}

trait DoSenadoProfile extends AutoridadeProfile with LocalidadeBR {
  override def urnFragAutoridade = "senado.federal"
  override def autoridadeEpigrafe = Some("DO SENADO FEDERAL")     
}

trait DaCamaraProfile extends AutoridadeProfile with LocalidadeBR {
  override def urnFragAutoridade = "camara.deputados"
  override def autoridadeEpigrafe = Some("DA CÂMARA DOS DEPUTADOS")  
}

trait DoCongressoProfile extends AutoridadeProfile with LocalidadeBR {
  override def urnFragAutoridade = "congresso.nacional"
  override def autoridadeEpigrafe = Some("DO CONGRESSO NACIONAL")
}

trait FederalProfile extends AutoridadeProfile with LocalidadeBR {
  override def urnFragAutoridade = "federal"
  override def autoridadeEpigrafe = Some("FEDERAL")  
}

trait ConstituicaoProfile extends TipoNormaProfile {
  override def urnFragTipoNorma = "constituicao"
  override def epigrafeHead = "CONSTITUICAO"
  override def epigrafeTail = ""
  override def epigrafeTemplateCode : String = """CONSTITUIÇÃO"""
  override def epigrafeSemIdTemplateCode : String = """CONSTITUIÇÃO"""    
}

trait ConstituicaoFederalProfile extends ConstituicaoProfile with FederalProfile with EmentaAusente {     
  override def epigrafeTemplateCode : String = """CONSTITUIÇÃO DA REPÚBLICA FEDERATIVA DO BRASIL"""
  override def epigrafeSemIdTemplateCode : String = """CONSTITUIÇÃO DA REPÚBLICA FEDERATIVA DO BRASIL"""    
}

trait NormaProfile extends DocumentProfile with DefaultRegexProfile

trait ResolucaoProfile extends NormaProfile {
  override def urnFragTipoNorma: String = "resolucao"
  override def epigrafeHead: String = "RESOLUÇÃO"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^resolucao"r)
}

trait DecretoLegislativoProfile extends NormaProfile {
  override def urnFragTipoNorma = "decreto.legislativo"
  override def epigrafeHead = "DECRETO LEGISLATIVO"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^decreto"r)   
}

trait RegimentoInternoProfile extends NormaProfile {
  override def urnFragTipoNorma = "regimento.interno"
  override def epigrafeHead = "REGIMENTO INTERNO"
  override def regexEpigrafe : List[Regex] = List("^regimento *(interno|comum)$"r)
  override def ementaAusente = true
}


trait NormaFederalProfile extends NormaProfile with FederalProfile

object Lei extends NormaFederalProfile {
  override def urnFragTipoNorma = "lei"
  override def epigrafeHead = "LEI"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^lei"r)
}

object LeiComplementar extends NormaFederalProfile {
  override def urnFragTipoNorma = "lei.complementar"
  override def epigrafeHead = "LEI COMPLEMENTAR"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^lei complementar"r)
}

object LeiDelegada extends NormaFederalProfile {
  override def urnFragTipoNorma = "lei.delegada"
  override def epigrafeHead = "LEI DELEGADA"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^lei delegada"r)
}

object DecretoLei extends NormaFederalProfile {
  override def urnFragTipoNorma = "decreto.lei"
  override def epigrafeHead = "DECRETO-LEI"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^decreto-lei"r)
}

object Decreto extends NormaFederalProfile {
  override def urnFragTipoNorma = "decreto"
  override def epigrafeHead = "DECRETO"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^decreto"r)
  override def epigrafeTemplateCode : String = """DECRETO Nº <numeroComComplemento>, DE <dataExtenso>"""    
  //FIXME: Decreto não numerado  
}

object DecretoLegislativoFederal extends DecretoLegislativoProfile with FederalProfile {
  override def epigrafeTemplateCode : String = """DECRETO LEGISLATIVO Nº <numeroComComplemento>, DE <ano>"""
}

object DecretoLegislativoDoCongresso extends DecretoLegislativoProfile with DoCongressoProfile {
  override def epigrafeTemplateCode : String = """DECRETO LEGISLATIVO Nº <numeroComComplemento>, DE <ano>-CN"""
} 

object EmendaConstitucional extends DocumentProfile with DefaultRegexProfile with FederalProfile {
  override def urnFragTipoNorma = "emenda.constitucional"
  override def epigrafeHead = "EMENDA CONSTITUCIONAL"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^emenda constitucional"r)
  override def epigrafeTemplateCode : String = """EMENDA CONSTITUCIONAL Nº <numeroComComplemento>"""
}

object ResolucaoDaCamara extends ResolucaoProfile with DaCamaraProfile

object ResolucaoDoCongresso extends ResolucaoProfile with DoCongressoProfile {
    override def epigrafeTemplateCode : String = """RESOLUCAO Nº <numeroComComplemento>, DE <ano>-CN"""
}

object ResolucaoDoSenado extends ResolucaoProfile with DoSenadoProfile {
  override def epigrafeTemplateCode : String = """RESOLUCAO Nº <numeroComComplemento>, DE <ano>"""
}

object RegimentoInternoSenado extends RegimentoInternoProfile with DoSenadoProfile {
  override def epigrafeTemplateCode : String = """REGIMENTO INTERNO DO SENADO FEDERAL"""
  override def epigrafeSemIdTemplateCode : String = """REGIMENTO INTERNO DO SENADO FEDERAL"""
}

object RegimentoInternoCamara extends RegimentoInternoProfile with DaCamaraProfile {
  override def epigrafeTemplateCode : String = """REGIMENTO INTERNO DA CÂMARA DOS DEPUTADOS"""
  override def epigrafeSemIdTemplateCode : String = """REGIMENTO INTERNO DA CÂMARA DOS DEPUTADOS"""
}

object RegimentoComumCongresso extends RegimentoInternoProfile with DoCongressoProfile {
  override def epigrafeTemplateCode : String = """REGIMENTO COMUM DO CONGRESSO NACIONAL"""
  override def epigrafeSemIdTemplateCode : String = """REGIMENTO COMUM DO CONGRESSO NACIONAL"""
}

trait ProjetoNorma {
  self : TipoNormaProfile =>
    override val isProjetoNorma = true
}

object ProjetoDeLeiDoSenadoNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with ProjetoNorma  {
  override def urnFragTipoNorma = "projeto.lei;pls"
  override def epigrafeHead = "PROJETO DE LEI DO SENADO"
}

object ProjetoDeLeiDaCamaraNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.lei;plc"
  override def epigrafeHead = "PROJETO DE LEI DA CÂMARA"
  override def regexEpigrafe = List()
  //override def regexPosEpigrafe = List()
}

object ProjetoDeLeiDoCongressoNacional extends DocumentProfile with DefaultRegexProfile with DoCongressoProfile with EpigrafeOpcional  with ProjetoNorma{
  override def urnFragTipoNorma = "projeto.lei;pln"
  override def epigrafeHead = "PROJETO DE LEI DO CONGRESSO"
  override def regexEpigrafe = List()
  //override def regexPosEpigrafe = List()
}

object PropostaEmendaConstitucionalNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile  with ProjetoNorma {
  override def urnFragTipoNorma = "proposta.emenda.constitucional;pec"
  override def epigrafeHead = "PROPOSTA DE EMENDA CONSTITUCIONAL"
}

object ProjetoDeResolucaoDoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.resolucao;prs"
  override def epigrafeHead = "PROJETO DE RESOLUÇÃO DO SENADO"
}

object ProjetoDeDecretoLegislativoDoSenadoNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with PreEpigrafeProibida with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.decreto.legislativo;pds"
  override def epigrafeHead = "PROJETO DE DECRETO LEGISLATIVO (SF)"
} 

object ProjetoDeDecretoLegislativoDaCamaraNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.decreto.legislativo;pdc"
  override def epigrafeHead = "PROJETO DE DECRETO LEGISLATIVO"
}

object ProjetoDeDecretoLegislativoDaCamaraNovaNomenclatura extends DocumentProfile with DefaultRegexProfile with DaCamaraProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.decreto.legislativo;pdl"
  override def epigrafeHead = "PROJETO DE DECRETO LEGISLATIVO"
}

object ProjetoDeDecretoLegislativoDoSenadoNovaNomenclatura extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.decreto.legislativo;pdl"
  override def epigrafeHead = "PROJETO DE DECRETO LEGISLATIVO"
}

object ProjetoDeLeiComplementarDoSenadoNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.lei.complementar;pls"
  override def epigrafeHead = "PROJETO DE LEI DO SENADO"
  override def epigrafeTail = " - COMPLEMENTAR"
}

object ProjetoDeLeiComplementarDaCamaraNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.lei.complementar;plc"
  override def epigrafeHead = "PROJETO DE LEI DA CÂMARA"
  override def epigrafeTail = " - COMPLEMENTAR"
}

object ProjetoDeLeiNaCamara extends DocumentProfile with DefaultRegexProfile with DaCamaraProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.lei;pl"
  override def epigrafeHead = "PROJETO DE LEI"
}

object ProjetoDeLeiNovaNomenclatura extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma: String = "projeto.lei;pl"
  override def epigrafeHead: String = "PROJETO DE LEI"
}

object ProjetoDeLeiComplementarNovaNomenclatura extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.lei.complementar;plp"
  override def epigrafeHead = "PROJETO DE LEI COMPLEMENTAR"
}


object ProjetoDeLeiComplementarNaCamara extends DocumentProfile with DefaultRegexProfile with DaCamaraProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.lei.complementar;plp"
  override def epigrafeHead = "PROJETO DE LEI COMPLEMENTAR"
}

object ProjetoDeResolucaoNaCamara extends DocumentProfile with DefaultRegexProfile with DaCamaraProfile with EpigrafeOpcional  with ProjetoNorma {
  override def urnFragTipoNorma = "projeto.resolucao"
  override def epigrafeHead = "PROJETO DE RESOLUÇÃO"
}

object MedidaProvisoriaNoCongresso extends DocumentProfile with DefaultRegexProfile with DoCongressoProfile  with ProjetoNorma {
  override def urnFragTipoNorma = "medida.provisoria;mpv"
  override def epigrafeHead = "MEDIDA PROVISÓRIA"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^medida provisoria"r)
  //FIXME: Medida Provisória com Sequencial
}

object MedidaProvisoriaFederal extends DocumentProfile with DefaultRegexProfile with FederalProfile {
  override def urnFragTipoNorma = "medida.provisoria"
  override def epigrafeHead = "MEDIDA PROVISÓRIA"
  override def regexEpigrafe: List[Regex] = super.regexEpigrafe ++ List("^medida provisoria"r)
  //FIXME: Medida Provisória com Sequencial  
}

object ConstituicaoFederal extends DocumentProfile with ConstituicaoFederalProfile with DefaultRegexProfile {  
	override def regexLocalData: List[Regex] = super.regexLocalData ++ List(
	      "^sala da sessao"r,
	      "^sala das sessoes"r,
	      "^sala das comissoes"r,
	      "^sala da comissao"r,
	      "^camara dos deputados"r,
	      "^senado federal"r,
	      "^brasilia,"r
	)
	override def regexJustificativa: List[Regex] = List()
	override def regexAnexos: List[Regex] = super.regexAnexos ++ List(
	    "^anexo"r,
	    "a n e x o"r
	)
	override def regexLegislacaoCitada: List[Regex] = List()	
	override def regexEpigrafe: List[Regex] = List(
	         """^constituicao (da republica federativa|federal) do brasil"""r          
    )
       
  override def regexPreambulo: List[Regex] = List(
        "^preambulo$"r
       )
  override def epigrafeTemplateCode : String = """Constituição da República Federativa do Brasil."""
}
