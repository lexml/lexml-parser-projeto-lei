package br.gov.lexml.parser.pl

import java.io.File
import br.gov.lexml.parser.pl.metadado.Metadado
import br.gov.lexml.parser.pl.profile.ProjetoDeLeiDoSenadoNoSenado
import java.io.FileInputStream
import java.io.BufferedInputStream
import br.gov.lexml.parser.pl.output.LexmlRenderer

//case class Metadado(profile : DocumentProfile, id : Option[Id] = None, hashFonte : Option[Array[Byte]]) {  

/*
case class OptConfig(baseProfile : DocumentProfile = ProjetoDeLeiDoSenadoNoSenado,
                     id : Option[Id]md : Metadado, inFile : Option[File] = None, outFile : Option[File] = None)
*/

object ParserMain {
  def main(args : Array[String]) {
    val df = ProjetoDeLeiDoSenadoNoSenado
    val md = Metadado(df, None, None)  
    val is = new BufferedInputStream(new FileInputStream(args(0)))
    val pp = ParserParams(is,md)
    val (or,errs) = ParserFrontEnd.parseProjetoLei(pp)
    if(!errs.isEmpty) {
      System.err.println("Erros encontrados:")
      errs.foreach(e => System.err.println("\t" + e))
    }
    or foreach { res =>
      val rpl = LexmlRenderer.render(res)
      println(rpl)
    }
  }
}