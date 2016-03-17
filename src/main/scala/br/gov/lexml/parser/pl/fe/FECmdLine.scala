package br.gov.lexml.parser.pl.fe

case class CmdLineOpts(
    metadado : Option[Metadado] = None, 
    tipoNorma : String = "lei",
    autoridade : String = "federal",
    localidade : String = "br",
    numero : Option[Int] = None,
    complemento : Option[String] = None,
    a
    )

object FECmdLine extends App {
  
}