package br.gov.lexml.parser.pl

import java.io.File
import scala.xml.NodeSeq
import java.security.MessageDigest
import java.security.DigestInputStream
import java.util.zip.ZipEntry
import br.gov.lexml.parser.pl.block.Paragraph
import java.util.zip.ZipOutputStream
import java.io.ByteArrayOutputStream
import br.gov.lexml.parser.pl.block.Block
import br.gov.lexml.parser.pl.xhtml.AbiwordConverter
import br.gov.lexml.parser.pl.xhtml.Success
import br.gov.lexml.parser.pl.xhtml.Failure
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor
import br.gov.lexml.parser.pl.metadado.Metadado
import java.io.InputStream
import br.gov.lexml.parser.pl.output._
import br.gov.lexml.parser.pl.errors.ParseProblem
import br.gov.lexml.parser.pl.errors.ParseException
import br.gov.lexml.parser.pl.errors.FalhaConversaoPrimaria
import scala.io.Source


case class ParserParams(inRTF: InputStream, md: Metadado)

object ParserFrontEnd {

  val versaoParser = 1

  def parseProjetoLei(params: ParserParams): (Option[ProjetoLei], List[ParseProblem]) = {
    val ParserParams(inRTF, md) = params
    val inRTF2 = new DigestInputStream(inRTF, MessageDigest.getInstance("MD5"))
    val xhtmlRes = XHTMLProcessor.pipeline(inRTF2, new AbiwordConverter())
    try { params.inRTF.close() } catch { case _ : Exception ⇒ }
    val xhtml = xhtmlRes match {
      case Failure ⇒ {
        throw ParseException(FalhaConversaoPrimaria)
      }
      case Success(xhtml) ⇒ xhtml
    }
    val blocks = Block fromNodes xhtml    
    val (mpl, msgs) = new ProjetoLeiParser(params.md.profile).fromBlocks(md copy (hashFonte = Some(inRTF2.getMessageDigest.digest())), blocks)
    (mpl.map(_.remakeEpigrafe), msgs)
  }

  def parseAndBuild(params: ParserParams): (Option[(ProjetoLei, Array[Byte])], List[ParseProblem]) = {
    val (mpl, msgs) = parseProjetoLei(params)
    val r = mpl.map(pl ⇒ {
      val bos = new ByteArrayOutputStream()
      val zos = new ZipOutputStream(bos)
      zos.setLevel(9)
      zos.setComment(pl.epigrafe.asInstanceOf[Paragraph].text)
      zos.putNextEntry(new ZipEntry( /*dirName + "/" + */ "proposicao.xml"))
      zos.write(pl.metadado.toXMLmetadadoEditor(pl).toString.getBytes("utf-8"))
      zos.closeEntry()
      zos.putNextEntry(new ZipEntry( /* dirName + "/" + */ "texto.xml"))
      val rpl = LexmlRenderer.render(pl)
      zos.write(rpl.toString.getBytes("utf-8"))
      zos.closeEntry()
      zos.close()
      (pl, bos.toByteArray)
    })
    (r, msgs)
  }  
  
  def main(args : Array[String]) {
    
  }
}

class ArticulacaoParser {
  import java.util.{List => JList}  
  import xml.Text
  import br.gov.lexml.parser.pl.profile.ProjetoDeLeiDoSenadoNoSenado
  def parseJList(l : JList[String]) = {
    import collection.JavaConversions._
    parseList(l.toList)
  }
  def parseList(l : List[String]) = {
    val blocks = l.map((x : String) => Paragraph(Seq(Text(x))))
    val parser = new ProjetoLeiParser(ProjetoDeLeiDoSenadoNoSenado)
    val articulacao = parser.parseArticulacao(blocks,false)
    LexmlRenderer.renderArticulacao(articulacao).toString
  }
  def parse(f : File) = parseList(Source.fromFile(f).getLines().toList)
}