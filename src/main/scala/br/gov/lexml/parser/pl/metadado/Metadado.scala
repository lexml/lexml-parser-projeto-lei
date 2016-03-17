package br.gov.lexml.parser.pl.metadado

import br.gov.lexml.parser.pl.block.Paragraph
import br.gov.lexml.parser.pl.ProjetoLei
import scala.xml.NodeSeq
import java.util.Calendar
import java.util.Date
import java.io.Writer
import br.gov.lexml.parser.pl.cfg.ParserComponentConfiguration
import br.gov.lexml.parser.pl.profile.DocumentProfile
import java.util.Locale

object ReplacementConstants {
	val LEXML_METADADO_NUMERO = "LEXML_METADADO_NUMERO"
	val LEXML_METADADO_ANO = "LEXML_METADADO_ANO"
	val LEXML_METADADO_VERSAO = "LEXML_METADADO_VERSAO"
	val LEXML_METADADO_COMPLEMENTO = "LEXML_METADADO_COMPLEMENTO"	
	val LEXML_EPIGRAFE_NUMERO = "LEXML_EPIGRAFE_NUMERO"
	val LEXML_EPIGRAFE_DATA = "LEXML_EPIGRAFE_DATA"
	val LEXML_URN_ID = "LEXML_URN_ID"
				
}

object Fixer {
	import ReplacementConstants._
	val regex = List(
			LEXML_METADADO_NUMERO, LEXML_METADADO_ANO, LEXML_METADADO_VERSAO
		,	LEXML_METADADO_COMPLEMENTO, LEXML_EPIGRAFE_NUMERO, LEXML_EPIGRAFE_DATA
		,	LEXML_URN_ID ).mkString("","|","").r
	
	def makeRepMap(id : Id) : Map[String,String] = {
		import id._
		Map(
			LEXML_METADADO_NUMERO -> (num.toString + complemento.map(Metadado.renderComplemento(_)).getOrElse(""))
		,	LEXML_METADADO_ANO -> anoOuData.fold(x => x,data => data.ano).toString
		,	LEXML_METADADO_VERSAO -> versao.metadadoRepr
		,	LEXML_METADADO_COMPLEMENTO -> complemento.map(Metadado.renderComplemento(_)).getOrElse("")
		,	LEXML_EPIGRAFE_NUMERO -> Metadado.renderNumero(num)
		,	LEXML_EPIGRAFE_DATA -> anoOuData.fold(_.toString,_.extenso)
		,	LEXML_URN_ID -> urnRepr
			)
	}
	def replace(in : CharSequence, repMap : Map[String,String]) = regex.replaceAllIn(in,m => repMap(m.matched))
}

case class Data(ano : Int, mes : Int, dia : Int) {
	lazy val urnRepr = "%04d-%02d-%02d".format(ano,mes,dia)
	val nomeMes = List(
			"janeiro","fevereiro","março","abril","maio","junho",
			"julho","agosto","setembro","outubro","novembro","dezembro"
			)
	lazy val extenso = dia + " de " + nomeMes(mes-1) + " de " + ano		
}

object Data {
  import java.util.{Date,Calendar}
  def fromDate(d : Date) : Data = {
    val c = Calendar.getInstance()
    c.setTime(d)
    Data(c.get(Calendar.YEAR),c.get(Calendar.MONTH)+1,c.get(Calendar.DAY_OF_MONTH))
  }
}

case class Timestamp(ano : Int, mes : Int, dia : Int, hora : Int, minuto : Int) {
	lazy val txt = "%04d-%02d-%02dt%02d.%02d".format(ano,mes,dia,hora,minuto)
}

object Timestamp {
  def now() = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    import Calendar._
    Timestamp(cal.get(YEAR),cal.get(MONTH)+1,cal.get(DAY_OF_MONTH),cal.get(HOUR),cal.get(MINUTE))
  }
}

case class Versao(dataEvento : Option[Data], evento : String, timestamp : Option[Timestamp]) {
	lazy val  urnRepr = "@" + metadadoRepr 
	lazy val metadadoRepr = dataEvento.map(_.urnRepr).getOrElse("data.evento") + ";" + evento + timestamp.map(s => ";" + s.txt).getOrElse("")
}

object Versao {
	def versaoInicial() = Versao(None,"leitura",Some(Timestamp.now()))
}

case class Id(num : Int, complemento : Option[Int] = None, anoOuData : Either[Int,Data],  
		versao : Versao) {
    lazy val anoOuDataUrn = anoOuData match {
      case Left(ano) => "%04d" format ano
      case Right(data) => data.urnRepr
    }
	lazy val urnRepr = anoOuDataUrn + ";" + num +
		complemento.map(Metadado.renderComplemento(_)).getOrElse("") + versao.urnRepr	
	lazy val epigrafeRepr : String = "Nº " + Metadado.renderNumero(num) + complemento.map(Metadado.renderComplemento(_)).getOrElse("") + (", DE %04d" format
		anoOuData.fold(ano => ano,data => data.ano))
}

object Id {
  val re = """(\d\d\d\d)(?:-(\d\d)-(\d\d))?;(\d+)(?:-(\d+))?(?:@(data\.evento|(\d\d\d\d)-(\d\d)-(\d\d));([a-z.]+);(\d\d\d\d)-(\d\d)-(\d\d)t(\d\d)\.(\d\d))?""".r
  def fromUrnFrag (urnFrag : String) : Option[Id] = urnFrag match {
    case re(idAno,idMes,idDia,idNum,idComp,idDataEvento,deAno,deMes,deDia,nomeEvento,tsAno,tsMes,tsDia,tsHora,tsMin) => {
      val dataId = if (idMes == null) { Left(idAno.toInt) } else { Right(Data(idAno.toInt,idMes.toInt,idDia.toInt)) }
      val num = idNum.toInt
      val comp = Option(idComp).map(_.toInt)
      val versao = {
    	val dataEvento = idDataEvento match {
          case "data.evento" => None
          case _ => Some(Data(deAno.toInt,deMes.toInt,deDia.toInt))
      	}
      	val ts = if(tsAno == null) { None } else {
    	  Some(Timestamp(tsAno.toInt,tsMes.toInt,tsDia.toInt,tsHora.toInt,tsMin.toInt))
      	}
      	Versao(dataEvento,nomeEvento,ts)
      }
      Some(Id(num,comp,dataId,versao))
    }
    case _ => None
  }
}


case class Metadado(profile : DocumentProfile, localidade : String = "br", id : Option[Id] = None, hashFonte : Option[Array[Byte]]) {	
	lazy val urn = s"urn:lex:$localidade:${profile.urnFragAutoridade}:${profile.urnFragTipoNorma}:${id.map(_.urnRepr).getOrElse("LEXML_URN_ID")}"
	lazy val epigrafePadrao = 
	  s"${profile.epigrafeHead} ${id.map(_.epigrafeRepr).getOrElse("Nº LEXML_EPIGRAFE_NUMERO de LEXML_EPIGRAFE_DATA")}${profile.epigrafeTail}"
		/*
		 * val t = tipoProjetoLei match {
				case TipoPLS() => "PROJETO DE LEI DO SENADO Nº %s de %d"
				case TipoPLC() => "PROJETO DE LEI DO CÂMARA DOS DEPUTADOS Nº %s de %d"
			}
		 */
	def toXMLmetadadoEditor(pl : ProjetoLei) : NodeSeq = ( 
		<Proposicao>
		<URN href={urn}/>				
		<Ementa>{(NodeSeq fromSeq pl.ementa.asInstanceOf[Paragraph].nodes).text }</Ementa>
		<VersaoParser>{ParserComponentConfiguration.componentBuildNumber("br.gov.lexml.parser.pl","parser-projeto-lei",
		    			getClass).map(_.toString).getOrElse("")}</VersaoParser>
		<HashFonte alg="SHA-1">{hashFonte.map(_.map(b => "%02x".format(b.toInt&0xff)).mkString("","","")).getOrElse("")}</HashFonte>
		<PartesProposicao>
		
		<ParteProposicao id="1">
		<Tipo>lcp95-strict-proposicao-texto</Tipo>
		<Rotulo>Texto</Rotulo>
		<Descricao>Texto da Proposição</Descricao>
		<NomeArquivo>texto.xml</NomeArquivo>
		<QtdeDispositivos>{pl.dispositivoCount}</QtdeDispositivos>
		</ParteProposicao>
		</PartesProposicao>		
		</Proposicao>)
	
	lazy val zipDirName = id match {
	  case Some(i) => {
	    val sigla = profile.subTipoNorma.getOrElse("").toUpperCase	    
	    val ano = i.anoOuData.fold(ano => ano.toString,data => data.ano)
	    sigla + "_" + i.num + "_" + i.complemento.map(Metadado.renderComplemento(_)).getOrElse("") +
	    			"_" + ano + "_" + i.versao.dataEvento.map(_.urnRepr).getOrElse("data.evento") +
	    			"_" + i.versao.evento
	  }
	  case None => throw new RuntimeException("%s_0_0_0_0_leitura" format (profile.subTipoNorma.getOrElse("")))		
	}
	
}

object Metadado {
    lazy val pt_BR = Locale.getAvailableLocales.view.filter(_.getCountry == "BR").head
/*	def sigla(a : Autoridade, t : TipoNorma) = (a,t) match {
		case (ASenado,TNProjetoLei) => "PLS"
		case (ACamaraDeputados,TNProjetoLei) => "PLC"
		case (ASenado,TNProjetoLeiComplementar) => "PLS"
		case (ACamaraDeputados,TNProjetoLeiComplementar) => "PLC"
	} */
	def renderComplemento(n : Int) : String = "-" + n.toString
	def renderNumero(n : Int) : String = String.format(pt_BR,"%,d",n.asInstanceOf[java.lang.Integer])
	
}