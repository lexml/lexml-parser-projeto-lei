package br.gov.lexml.parser.pl.errors

abstract class ParserException(msg : String) extends Exception(msg) with Product

final case class ParseException(val errors : ParseProblem*) extends ParserException("Erros durante o parse: " + errors)

abstract class ProblemCategory(val code : Int, val description : String) {
  override final def toString() = "[" + code + ": " + description + "]"
}
  
case object PC_TecnicaLegislativa extends ProblemCategory(1,"Erro de técnica legislativa")

case object PC_LimitacaoParser extends ProblemCategory(2,"Limitação técnica do parser")
  
case object PC_LimitacaoEditor extends ProblemCategory(3,"Limitação técnica do editor")

case object PC_XMLSchema extends ProblemCategory(3,"Erro de validação do esquema XML")

case object PC_ErroGeralParser extends ProblemCategory(4,"Erro geral do parser")

abstract class ProblemType(val code : Int, val description : String, val category : ProblemCategory) {
  override final def toString() = "[" + code + ": " +  description + ", categoria: " + category + "]"
}

case object TNiveisDiferentes extends ProblemType(1,"Dispositivos incompatíveis agrupados no mesmo nível.",PC_TecnicaLegislativa)

case object TOrdemInvertida extends ProblemType(2,"Dispositivos com ordem invertida.",PC_TecnicaLegislativa)

case object TDispositivosDescontinuos extends ProblemType(3, "Dispositivos com descontinuidade.",PC_TecnicaLegislativa)

case object TDispositivoInicialNumeracaoInvalida extends ProblemType(4,"Dispositivo inicial com numeração inválida",PC_TecnicaLegislativa)

case object TRotuloRepetido extends ProblemType(5, "Rótulo repetido",PC_TecnicaLegislativa)

case object TRotuloUnicoNaoUnico extends ProblemType(6, "Rótulo único não aparece sozinho.",PC_TecnicaLegislativa)

case object TPosicaoInvalida extends ProblemType(7,"Dispositivo em posição inválida",PC_TecnicaLegislativa)

case object TEpigrafeAusente extends ProblemType(8,"Epígrafe ausente ou fora do padrão", PC_TecnicaLegislativa)

case object TArticulacaoNaoIdentificada extends ProblemType(9, "Articulação não identificada", PC_TecnicaLegislativa)

case object TErroValidacaoSchema extends ProblemType(10, "Erro de validação do esquema XML", PC_XMLSchema) 

case object TTextoInvalido extends ProblemType(11,"Dispositivo com texto inválido.",PC_TecnicaLegislativa)

case object TEmentaAusente extends ProblemType(12,"Ementa ausente ou fora do padrão.",PC_TecnicaLegislativa)

case object TErroSistema extends ProblemType(14,"Erro de sistema", PC_ErroGeralParser)

case object TFalhaConversao extends ProblemType(15,"Falha na conversão primária", PC_ErroGeralParser)

case object TElementoArticulacaoNaoReconhecido extends ProblemType(16, "Elemento da articulação não reconhecido", PC_TecnicaLegislativa)

case object TAlteracaoSemFechaAspas extends ProblemType(17,"Alteração sem fecha-aspas", PC_TecnicaLegislativa)

case object TPresencaEnumeracao extends ProblemType(18, "O uso de listas não é suportado", PC_ErroGeralParser)

case object TRotuloDuplicado extends ProblemType(19,"Rótulo duplicado", PC_TecnicaLegislativa)

case object TOmissisForaAlteracao extends ProblemType(20,"Omissis fora da alteração", PC_TecnicaLegislativa)

case object TElementoNaoSuportado extends ProblemType(21,"Elemento não suportado", PC_LimitacaoEditor)

abstract class ParseProblem(val problemType : ProblemType, val msg : Option[String], val pos : String*) {
  override final def toString() = "[" + message + " " + pos.mkString("(",",",")") + " | " + problemType + "]"
  final def message = msg.getOrElse(problemType.description)
}

/*
 Problemas 
 */

final case class NiveisDiferentes(id1: String, id2: String) extends 
	ParseProblem(TNiveisDiferentes,None,id1,id2)

final case class OrdemInvertida(id1: String, id2: String) extends 
	ParseProblem(TOrdemInvertida,None,id1,id2)
  
final case class DispositivosDescontinuos(id1: String, id2: String) extends 
	ParseProblem(TDispositivosDescontinuos, None,id1,id2)

final case class DispositivoInicialNumeracaoInvalida(id : String) extends 
	ParseProblem(TDispositivoInicialNumeracaoInvalida,None,id)

final case class RotuloRepetido(id : String) extends ParseProblem(TRotuloRepetido,None, id)

final case class RotuloUnicoNaoUnico(idUnico : String, idNaoUnico : String) extends 
	ParseProblem(TRotuloUnicoNaoUnico,None,idUnico,idNaoUnico)


final case class PosicaoInvalida(id : String) extends ParseProblem(TPosicaoInvalida,None, id)

final case class TextoInvalido(id : String, padrao : String, texto : String, mesg : String ) extends ParseProblem(
    TTextoInvalido, Some("Texto inválido. Padrão: " + padrao + ", texto: "  + texto + ", mensagem de erro: " + mesg),id)

case object ArticulacaoNaoIdentificada extends ParseProblem(TArticulacaoNaoIdentificada,None)

case object EpigrafeAusente extends ParseProblem(TEpigrafeAusente,None)

case object EmentaAusente extends ParseProblem(TEmentaAusente,None)

final case class ErroSistema(ex : Exception) extends ParseProblem(TErroSistema,Some("Erro de sistema: " + ex.getMessage)) {
  ex.printStackTrace
}
 


case object FalhaConversaoPrimaria extends ParseProblem(TFalhaConversao,None)
      
final case class ErroValidacaoSchema(ex: org.xml.sax.SAXParseException) extends 
	ParseProblem(TErroValidacaoSchema,
	  Some("Erro de validação do esquema XML. systemid: %s, publicid:%s, lineno: %d, column:%d, error: %s" format (
			ex.getSystemId(), ex.getPublicId(), ex.getLineNumber(), 
			ex.getColumnNumber(), ErroValidacaoSchema.unchain(ex).mkString("", " => ", "")))
    ) 

object ErroValidacaoSchema {
	def unchain(ex: Throwable): List[String] =
		ex.getMessage() :: Option(ex.getCause()).toList.
		flatMap(unchain(_))  
}

final case class ElementoArticulacaoNaoReconhecido(path : String, textos : String*) 
	extends ParseProblem(TElementoArticulacaoNaoReconhecido,
			Some("Elementos não reconhecidos na articulação: " +
			     textos.mkString(" \n")),path)
	    

case object AlteracaoSemFechaAspas extends ParseProblem(TAlteracaoSemFechaAspas,None)

case object PresencaEnumeracao extends ParseProblem(TPresencaEnumeracao, None) 

case class RotuloDuplicado(id : String) extends ParseProblem(TRotuloDuplicado,None,id)

case class OmissisForaAlteracao(id : Option[String] = None) extends ParseProblem(TOmissisForaAlteracao,
    id.map("Omissis fora de alteração em : " + _),id.toSeq : _*)

case class ElementoNaoSuportado(elemento : String, id : Option[String] = None) extends ParseProblem(
    TElementoNaoSuportado,
    Some("Elemento não suportado: " + elemento + id.map(", em " + _).getOrElse("")),id.toSeq : _*)
