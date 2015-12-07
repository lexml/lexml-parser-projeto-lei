package br.gov.lexml.parser.pl.converter

import java.io.File
import org.apache.commons.io.filefilter.PrefixFileFilter
import org.apache.commons.io.FileUtils
import java.io.FileFilter

trait Converter {
  def convert(srcExtension: String, srcData: Array[Byte], dstExtension: String): Array[Byte]
  def deleteByPrefix(dir : File, prefix : String) =
        dir.listFiles(new PrefixFileFilter(prefix) : FileFilter).foreach(f => FileUtils.deleteQuietly(f))
  def acceptMimeTypes : Set[String]
  def provideMimeTypes : Set[String]
}

trait ConversionChain