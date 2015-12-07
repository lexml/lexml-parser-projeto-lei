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
}

trait EpigrafeOpcional extends RegexProfile {
  override def epigrafeObrigatoria = false
}

trait PreEpigrafeProibida extends RegexProfile {
  override def preEpigrafePermitida = false
}


trait DefaultRegexProfile extends RegexProfile {
	override def regexLocalData = super.regexLocalData ++ List(
	      "^sala da sessao"r,
	      "^sala das sessoes"r,
	      "^sala das comissoes"r,
	      "^sala da comissao"r,
	      "^camara dos deputados"r,
	      "^senado federal"r,
	      "^brasilia,"r
	)
	override def regexJustificativa = super.regexJustificativa ++ List(
	    "^justificacao"r,
	    "^j u s t i f i c a c a o"r,
	    "^justificativa"r,
	    "^j u s t i f i c a t i v a"r
	)
	override def regexAnexos = super.regexAnexos ++ List(
	    "^anexo"r,
	    "a n e x o"r
	)
	override def regexLegislacaoCitada = super.regexLegislacaoCitada ++ List(
	    "^legislacao citada"r,
	    "^l e g i s l a c a o c i t a d a"r
	)
	override def regexAssinatura = super.regexAssinatura ++ List("^senadora? "r)
	override def regexEpigrafe = super.regexEpigrafe ++ List(
          """^\s*(red\d+;+)?(projeto( de)? (lei|decreto legislativo)|(proposta|projeto) de emenda|pec|projeto de resolu)""".r
         ,"""^(n[oº°˚]|complementar)"""r
    )
    
    override def regexPosEpigrafe = super.regexPosEpigrafe ++ List(
        """^\s*(\(.*\)|autora?:.*|autoria d.*|(d[oa] )?senador.*)\s*$""".r
    )
    
    override def regexPreambulo = super.regexPreambulo ++ List(
        "^o (congress+o nacional|senado federal) (decret[oa]|resolve|promulg[oa])"r,
        "^[ao] president[ae] da republica"r,
        "^[ao] vice-president[ae] da republica"r,
        "^as? mesas?"r
       )
}

trait TipoNormaProfile {
  def urnFragTipoNorma : String
  def epigrafeHead : String  
  def epigrafeTail : String = ""
}

trait AutoridadeProfile {
  def urnFragAutoridade : String
  def autoridadeEpigrafe : Option[String] = None
}

trait DocumentProfile extends RegexProfile with TipoNormaProfile with AutoridadeProfile {
  lazy val subTipoNorma = urnFragTipoNorma.split(";") match {
    case Array(_,st) => Some(st)
    case _ => None
  }
}

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
  overrideAutoridadeEpigrafe: Option[Option[String]] = None    
) extends DocumentProfile {
  override final def regexLocalData : List[Regex] = overrideRegexLocalData.getOrElse(base.regexLocalData)
  override final def regexJustificativa: List[Regex] = overrideRegexJustificativa.getOrElse(base.regexJustificativa)
  override final def regexAnexos: List[Regex] = overrideRegexAnexos.getOrElse(base.regexAnexos)
  override final def regexLegislacaoCitada: List[Regex] = overrideRegexLegislacaoCitada.getOrElse(base.regexLegislacaoCitada)
  override final def regexAssinatura: List[Regex] = overrideRegexAssinatura.getOrElse(base.regexAssinatura)
  override final def regexEpigrafe: List[Regex] = overrideRegexEpigrafe.getOrElse(base.regexEpigrafe)
  override final def regexPosEpigrafe: List[Regex] = overrideRegexPosEpigrafe.getOrElse(base.regexPosEpigrafe)
  override final def epigrafeObrigatoria: Boolean = overrideEpigrafeObrigatoria.getOrElse(base.epigrafeObrigatoria)
  override final def preEpigrafePermitida: Boolean = overridePreEpigrafePermitida.getOrElse(base.preEpigrafePermitida)
  override final def regexPreambulo: List[Regex] = overrideRegexPreambulo.getOrElse(base.regexPreambulo)
  override final def urnFragTipoNorma: String = overrideUrnFragTipoNorma.getOrElse(base.urnFragTipoNorma)
  override final def epigrafeHead: String = overrideEpigrafeHead.getOrElse(base.epigrafeHead)
  override final def epigrafeTail: String = overrideEpigrafeTail.getOrElse(base.epigrafeTail)
  override final def urnFragAutoridade: String = overrideUrnFragAutoridade.getOrElse(base.urnFragAutoridade)
  override final def autoridadeEpigrafe: Option[String] = overrideAutoridadeEpigrafe.getOrElse(base.autoridadeEpigrafe)
  final val hasOverride : Boolean = this.productIterator.exists { _.isInstanceOf[Some[_]] }
}

object DocumentProfileRegister {  
  type Autoridade = String
  type TipoNorma = String
  var profiles : Map[(Autoridade,TipoNorma),DocumentProfile] = Map()
  def register(profile : DocumentProfile) = { profiles = profiles + ((profile.urnFragAutoridade,profile.urnFragTipoNorma) -> profile) }
  def getProfile(autoridade : Autoridade, tipoNorma : TipoNorma) : Option[DocumentProfile] = profiles.get((autoridade,tipoNorma))
  def profileByAutoridadeSigla(autoridade : Autoridade, sigla : String) =
    profiles.filterKeys({ case (aut,tn) => aut == autoridade && tn.endsWith(";" + sigla)}).values.headOption
  def autoridades = profiles.keySet.map(_._1)
  def tiposDeNormasPorAutoridade(autoridade : String) : Set[String] = for { (aut,tn) <- profiles.keySet ; if aut == autoridade } yield tn
  def tiposDeNormas : Set[String] = profiles.keySet.map(_._2)
  def byUrnFrag(urnFrag : String) : Option[DocumentProfile] = urnFrag.split(":").toList match {
    case autoridade :: tipoNorma :: _ => getProfile(autoridade,tipoNorma)
    case _ => None
  }   
  val builtins = List[DocumentProfile](
      ProjetoDeLeiDoSenadoNoSenado,
      ProjetoDeLeiDaCamaraNoSenado,
      PropostaEmendaConstitucionalNoSenado,
      ProjetoDeResolucaoDoSenado,
      ProjetoDeDecretoLegislativoDoSenadoNoSenado,
      ProjetoDeDecretoLegislativoDaCamaraNoSenado,
      ProjetoDeLeiComplementarDoSenadoNoSenado,
      ProjetoDeLeiComplementarDaCamaraNoSenado,
      ProjetoDeLeiNaCamara,
      ProjetoDeLeiComplementarNaCamara,
      ProjetoDeResolucaoNaCamara,
      MedidaProvisoriaNoCongresso,
      MedidaProvisoriaFederal
  )
  builtins foreach register
}

trait DoSenadoProfile extends AutoridadeProfile {
  override def urnFragAutoridade = "senado.federal"
  override def autoridadeEpigrafe = Some("DO SENADO FEDERAL")     
}

trait DaCamaraProfile extends AutoridadeProfile {
  override def urnFragAutoridade = "camara.deputados"
  override def autoridadeEpigrafe = Some("DA CÂMARA DOS DEPUTADOS")  
}

trait DoCongressoProfile extends AutoridadeProfile {
  override def urnFragAutoridade = "congresso.nacional"
  override def autoridadeEpigrafe = Some("DO CONGRESSO NACIONAL")
}

trait FederalProfile extends AutoridadeProfile {
  override def urnFragAutoridade = "federal"
  override def autoridadeEpigrafe = Some("FEDERAL")  
}

object ProjetoDeLeiDoSenadoNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile  {
  override def urnFragTipoNorma = "projeto.lei;pls"
  override def epigrafeHead = "PROJETO DE LEI DO SENADO"
} 

object ProjetoDeLeiDaCamaraNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional {
  override def urnFragTipoNorma = "projeto.lei;plc"
  override def epigrafeHead = "PROJETO DE LEI DA CÂMARA"
  override def regexEpigrafe = List()
  //override def regexPosEpigrafe = List()
}

object PropostaEmendaConstitucionalNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile {
  override def urnFragTipoNorma = "proposta.emenda.constitucional;pec"
  override def epigrafeHead = "PROPOSTA DE EMENDA CONSTITUCIONAL"
}

object ProjetoDeResolucaoDoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile {
  override def urnFragTipoNorma = "projeto.resolucao;prs"
  override def epigrafeHead = "PROJETO DE RESOLUÇÃO DO SENADO"
}

object ProjetoDeDecretoLegislativoDoSenadoNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with PreEpigrafeProibida with EpigrafeOpcional {
  override def urnFragTipoNorma = "projeto.decreto.legislativo;pds"
  override def epigrafeHead = "PROJETO DE DECRETO LEGISLATIVO (SF)"
} 

object ProjetoDeDecretoLegislativoDaCamaraNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional {
  override def urnFragTipoNorma = "projeto.decreto.legislativo;pdc"
  override def epigrafeHead = "PROJETO DE DECRETO LEGISLATIVO"
}

object ProjetoDeLeiComplementarDoSenadoNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile {
  override def urnFragTipoNorma = "projeto.lei.complementar;pls"
  override def epigrafeHead = "PROJETO DE LEI DO SENADO"
  override def epigrafeTail = " - COMPLEMENTAR"
}

object ProjetoDeLeiComplementarDaCamaraNoSenado extends DocumentProfile with DefaultRegexProfile with DoSenadoProfile with EpigrafeOpcional {
  override def urnFragTipoNorma = "projeto.lei.complementar;plc"
  override def epigrafeHead = "PROJETO DE LEI DA CÂMARA"
  override def epigrafeTail = " - COMPLEMENTAR"
}

object ProjetoDeLeiNaCamara extends DocumentProfile with DefaultRegexProfile with DaCamaraProfile with EpigrafeOpcional {
  override def urnFragTipoNorma = "projeto.lei;pl"
  override def epigrafeHead = "PROJETO DE LEI"
}

object ProjetoDeLeiComplementarNaCamara extends DocumentProfile with DefaultRegexProfile with DaCamaraProfile with EpigrafeOpcional {
  override def urnFragTipoNorma = "projeto.lei.complementar;plp"
  override def epigrafeHead = "PROJETO DE LEI COMPLEMENTAR"
}

object ProjetoDeResolucaoNaCamara extends DocumentProfile with DefaultRegexProfile with DaCamaraProfile with EpigrafeOpcional {
  override def urnFragTipoNorma = "projeto.resolucao"
  override def epigrafeHead = "PROJETO DE RESOLUÇÃO"
}

object MedidaProvisoriaNoCongresso extends DocumentProfile with DefaultRegexProfile with DoCongressoProfile {
  override def urnFragTipoNorma = "medida.provisoria;mpv"
  override def epigrafeHead = "MEDIDA PROVISÓRIA"
  override def regexEpigrafe = super.regexEpigrafe ++ List("^medida provisoria"r)  
}

object MedidaProvisoriaFederal extends DocumentProfile with DefaultRegexProfile with FederalProfile {
  override def urnFragTipoNorma = "medida.provisoria"
  override def epigrafeHead = "MEDIDA PROVISÓRIA"
  override def regexEpigrafe = super.regexEpigrafe ++ List("^medida provisoria"r)  
}