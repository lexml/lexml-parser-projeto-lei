package br.gov.lexml.parser.pl.docx

import java.io.ByteArrayInputStream
import br.gov.lexml.parser.pl.converter.Converter

final class DOCXConverter(otherConverter : Converter) extends Converter {
  override def convert(srcExtension: String, srcData: Array[Byte], dstExtension: String): Array[Byte] = {
	  (srcExtension,dstExtension) match {
	    case ("docx","xhtml") =>  
	      DOCXReader.readDOCX(new ByteArrayInputStream(srcData)).
	      		get.toString.getBytes	      
	    
	    case _ => otherConverter.convert(srcExtension,srcData,dstExtension)
	  }
  }
}