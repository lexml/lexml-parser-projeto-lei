package br.gov.lexml.parser.pl.xhtml

import java.io.CharArrayReader
import java.io.FileReader
import java.io.BufferedReader
import org.apache.commons.io.IOUtils
import org.apache.commons.io.filefilter.FileFilterUtils
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.IOException
import java.io.FileOutputStream
import java.io.FileInputStream
import java.io.BufferedOutputStream
import java.io.BufferedInputStream
import java.io.InputStream
import java.io.OutputStream
import java.io.File
import scala.xml._
import scala.util.matching.Regex
import grizzled.slf4j.Logging
import org.apache.commons.io.FileUtils
import java.io.InputStreamReader
import java.io.ByteArrayInputStream
import java.io.StringReader
import scala.xml.parsing.NoBindingFactoryAdapter
import org.apache.commons.io.filefilter.PrefixFileFilter
import java.io.FileFilter
import scala.xml.parsing.XhtmlParser
import scala.io.BufferedSource
import scala.xml.Source
import scala.xml.Source
import scala.xml.Source
import scala.xml.Source
import br.gov.lexml.parser.pl.docx.DOCXReader
import scala.io.Codec

abstract class XHTMLProcessorResult
case object Failure extends XHTMLProcessorResult
case class Success(result: List[Node]) extends XHTMLProcessorResult

object TextUtils {
  def fixXHTML(data: Array[Byte]) = new String(data,"utf-8")
    //.replaceFirst("<!DOCTYPE (html|HTML)[^>]*>", "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"" + XHTMLProcessor.dtdUrl + "\">")
    .replaceAll("\u0007", "")
    .replaceAll("\u001f", "")
    .replace(0x92: Char, '`')
    .replaceAll("&#146;", "`")
    .replace(0x202d: Char, ' ')
    .replace(0x202c: Char, ' ')
    .replace('–', '-')
    .getBytes("utf-8")
}

trait Converter {
  def convert(srcExtension: String, srcData: Array[Byte], dstExtension: String): Array[Byte]
  def deleteByPrefix(dir : File, prefix : String) =
        dir.listFiles(new PrefixFileFilter(prefix) : FileFilter).foreach(f => FileUtils.deleteQuietly(f))
}

final class DOCXConverter(otherConverter : Converter) extends Converter {
  override def convert(srcExtension: String, srcData: Array[Byte], dstExtension: String): Array[Byte] = {
	  (srcExtension,dstExtension) match {
	    case ("docx","xhtml") =>  
	      DOCXReader.readDOCX(new ByteArrayInputStream(srcData)).
	      		get.toString.getBytes("utf-8")   
	    
	    case _ => otherConverter.convert(srcExtension,srcData,dstExtension)
	  }
  }
}

final class AbiwordConverter(val removeTemporaryFiles: Boolean = true) extends Converter with Logging {

  import TextUtils._

  def noPostProc(data: Array[Byte]) = data

  override def convert(srcExtension: String, srcData: Array[Byte], dstExtension: String) = {
    logger.info("abiword.convert: starting: srcExtension = " + srcExtension + ", dstExtension = " + dstExtension)
    val srcFile = File.createTempFile("lexml-parser-pl", "." + srcExtension)
    logger.info("abiword.convert: srcFile = " + srcFile)
    val baseName = srcFile.getName.substring(0, srcFile.getName.length - srcExtension.length - 1)
    val baseDir = srcFile.getParentFile
    val destFile = new File(baseDir, baseName + "." + dstExtension)
    logger.info("abiword.convert: destFile = " + destFile)
    val (params, postProc) = dstExtension match {
      case "xhtml" ⇒ (List("--to=xhtml", "--exp-props=html4: no; declare-xml: yes; use-awml:no; embed-css: yes; embed-images: yes"), fixXHTML(_))
      case "pdf" ⇒ (List("--to=pdf"), noPostProc(_))
      case _ ⇒ throw new RuntimeException("Abiword Converter does not support extension: " + dstExtension)
    }
    logger.info("abiword.convert: params = " + params)
    try {
      //val srcPath = srcFile.getCanonicalPath
      FileUtils.writeByteArrayToFile(srcFile, srcData)
      val cmd: Array[String] = (("/usr/bin/abiword" :: params) :+ srcFile.getPath).toArray
      logger.info("running " + cmd.mkString(" "))
      val p = Runtime.getRuntime.exec(cmd, Array[String](), srcFile.getParentFile)
      logger.info("returned from abiword")
      p.waitFor
      postProc(FileUtils.readFileToByteArray(destFile))
    } finally {
      if (removeTemporaryFiles) {        
        deleteByPrefix(baseDir,baseName)        
      }
    }
  }
}

final class OpenOfficeConverter(val removeTemporaryFiles: Boolean = true) extends Converter with Logging {

  val pyodconverter = "/usr/local/bin/docconverter"

  import TextUtils._

  def htmlPostProc(data: Array[Byte]) = {
    val data2 = fixXHTML(data)
    val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
    val parser = parserFactory.newSAXParser()
    val source = new InputSource(new ByteArrayInputStream(data2))
    val adapter = new NoBindingFactoryAdapter
    val e = adapter.loadXML(source, parser)
    e.toString.getBytes("utf-8")
  }

  def noPostProc(data: Array[Byte]) = data

  override def convert(srcExtension: String, srcData: Array[Byte], dstExtension: String) = {
    logger.info("oo.convert: starting: srcExtension = " + srcExtension + ", dstExtension = " + dstExtension)
    val dstExtension2 = dstExtension match {
      case "xhtml" ⇒ "html"
      case x ⇒ x
    }
    val srcFile = File.createTempFile("lexml-parser-pl", "." + srcExtension)
    logger.info("oo.convert: srcFile = " + srcFile)
    val baseName = srcFile.getName.substring(0, srcFile.getName.length - srcExtension.length - 1)
    val baseDir = srcFile.getParentFile
    val destFile = new File(baseDir, baseName + "." + dstExtension2)
    logger.info("oo.convert: destFile = " + destFile)
    val postProc = dstExtension match {
      case "xhtml" ⇒ htmlPostProc(_)
      case _ ⇒ noPostProc(_)
    }

    try {

      FileUtils.writeByteArrayToFile(srcFile, srcData)
      val cmd: Array[String] = Array(pyodconverter, srcFile.getPath, destFile.getPath)
      val p = Runtime.getRuntime.exec(cmd, Array[String](), srcFile.getParentFile)
      p.waitFor
      val res = postProc(FileUtils.readFileToByteArray(destFile))
      //FileUtils.writeByteArrayToFile(new File(destFile.getParentFile,destFile.getName + ".res"),res)
      res
    } finally {
      
      if (removeTemporaryFiles) {
        deleteByPrefix(baseDir,baseName)        
      }
    }
  }
}

object XHTMLProcessor extends Logging {

  val accept : Set[String] = Set(
      "text/plain",
      "text/html",
      "application/rtf",
      "text/rtf",
      "application/msword",
      "application/vnd.oasis.opendocument.text", 
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      )
  
  import TextUtils._

/*  lazy val dtdFile: File = {
    val f = File.createTempFile("xhtml11-", ".dtd")
    f.deleteOnExit()
    f
  }
  lazy val dtdUrl: String = {
    val is = getClass.getClassLoader().getResourceAsStream("xhtml11.dtd")
    val os = new BufferedOutputStream(new FileOutputStream(dtdFile))
    IOUtils.copy(is, os)
    IOUtils.closeQuietly(is)
    IOUtils.closeQuietly(os)
    dtdFile.getCanonicalFile.toURI.toURL.toString
  }*/

  //val converter : Converter = new AbiwordConverter
  val defaultConverter: Converter = new DOCXConverter(new AbiwordConverter)

  def changeChildren[T <: Seq[Node]](f: Seq[Node] ⇒ Seq[Node]) = (e: T) ⇒ {
    e match {
      case Elem(pref, name, attrs, scope, children @ _*) ⇒
        Elem(pref, name, attrs, scope, true, (f(children)): _*).asInstanceOf[T]
      case _ ⇒ e
    }
  }

  //  var removeTemporaryFiles = true
  //  def fixXHTML(xhtml: String) = xhtml
  //    .replaceFirst("<!DOCTYPE html[^>]*>", "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"" + XHTMLProcessor.dtdUrl + "\">")
  //    .replaceAll("\007", "")
  //    .replaceAll("\037", "")
  //    .replace(0x92: Char, '`')
  //    .replaceAll("&#146;", "`")
  //    .replace(0x202d: Char, ' ')
  //    .replace(0x202c: Char, ' ')
  //
  //
  //  
  //  def abiwordConversion(extension : String) =
  //	  	(extension,convertUsingAbiword2(extension)(_))
  //	  	
  //  def convertUsingAbiword2(extension : String)(source : Array[Byte]) =
  //	  convertUsingAbiword(source,extension)
  //    
  //  def convertUsingAbiword(source: Array[Byte], extension: String): Elem = {
  //    val srcFile = File.createTempFile("lexml-parser-pl", "." + extension)
  //    val xhtmlFile = new File(srcFile.getCanonicalPath.replaceFirst(extension + "$", "xhtml"))
  //    try {
  //      //val srcPath = srcFile.getCanonicalPath
  //      FileUtils.writeByteArrayToFile(srcFile, source)
  //      val cmd: Array[String] = Array(
  //        "/usr/bin/abiword", "--to=xhtml", srcFile.getName, "--exp-props=html4: no; declare-xml: yes; use-awml:no; embed-css: no; embed-images: no")
  //      val p = Runtime.getRuntime.exec(cmd, Array[String](), srcFile.getParentFile)
  //      p.waitFor
  //
  //      val xhtmlData = fixXHTML(FileUtils.readFileToString(xhtmlFile))
  //
  //      val reader = new StringReader(xhtmlData)
  //
  //      try {
  //        if (!xhtmlFile.exists || xhtmlFile.length < 300) {
  //          throw new RuntimeException("HTML de saida do Abiword vazio")
  //        }
  //        else { XML.load(reader) }
  //      } finally {
  //        IOUtils.closeQuietly(reader)
  //      }    
  //    } finally {
  //      if (removeTemporaryFiles) {
  //        srcFile.delete
  //        xhtmlFile.delete
  //        val f = new File(xhtmlFile.getPath() + "_files")
  //        if (f.exists() && f.isDirectory()) {
  //          f.listFiles().foreach(_.delete)
  //          f.delete
  //        }
  //      }
  //    }
  //  }

  type SourceProcessor = (Array[Byte], Converter) ⇒ Elem

  def xhtmlLoader(in : Array[Byte]) : Elem = xhtmlLoader(new ByteArrayInputStream(in)) 
	  
  def xhtmlLoader(is : InputStream) : Elem = 
	  XhtmlParser(new BufferedSource(is)(Codec.UTF8)).collectFirst({ case e : Elem => e }).get
	      
  def externalConvertToXhtml(extension: String) = (extension, (data: Array[Byte], converter : Converter) ⇒ {
    System.setProperty("file.encoding", "utf-8")
    val converted = converter.convert(extension, data, "xhtml")
    val r = xhtmlLoader(converted)
    ((r \\ "html").toSeq.collect { case e : Elem => e }) . head
    
  })
  
  val sourceProcessorMap: Map[String, (String, SourceProcessor)] = Map(
    ("text/plain", ("txt", (source: Array[Byte], _ : Converter) ⇒ {
      val text = fixXHTML(source)

      import scala.collection.JavaConversions._
      val lines = IOUtils.readLines(new InputStreamReader(
          new ByteArrayInputStream(text),"utf-8")).toList
      def toPars(l: List[String], r: List[String] = Nil, s: List[String] = Nil): List[String] = l match {
        case Nil ⇒ s match { case Nil ⇒ r; case _ ⇒ s.mkString("", " ", "") :: r }
        case (x :: xs) if x.trim.length == 0 ⇒ toPars(xs, s.mkString("", " ", "") :: r)
        case (x :: xs) ⇒ toPars(xs, r, x :: s)
      }
      val pars = toPars(lines).reverse
      <html><body><div>{ pars.map(p ⇒ <p>{ p }</p>) }</div></body></html>
    })),
    ("application/xhtml+xml", ("xhtml", (source: Array[Byte],_ : Converter) ⇒ {
      val text = fixXHTML(source)
      xhtmlLoader(text)
    })),
    ("text/html", ("html", (source: Array[Byte],_ : Converter) ⇒ {
      val text = fixXHTML(source)
      xhtmlLoader(text)
    })),
    ("application/rtf", externalConvertToXhtml("rtf")),
    ("text/rtf", externalConvertToXhtml("rtf")),
    ("application/msword", externalConvertToXhtml("doc")),
    ("application/vnd.oasis.opendocument.text", externalConvertToXhtml("odt")),
    ("application/vnd.openxmlformats-officedocument.wordprocessingml.document", externalConvertToXhtml("docx")))

  def convertSrcToXHTML(source: Array[Byte], mimeType: String,converter : Converter): Option[Elem] =
    sourceProcessorMap.get(mimeType).map(_._2(source,converter))

  def convertRTFtoXHTML(rtfSource: InputStream, converter : Converter): Option[Elem] =
    convertSrcToXHTML(IOUtils.toByteArray(rtfSource), "text/rtf",converter)

  def selectBaseElems(root: Elem): List[Elem] = {
    val body = (root \\ "body").head.asInstanceOf[Elem]
   
    val belems = root.child.to[List].dropWhile ((n : Node) => n match {
      case e : Elem => e.label != "body" 
      case _ => true 
    })
      
    
    def getAttr(n: Node, attr: String) = n match {
      case (e: Elem) ⇒ e.attributes.get(attr).map(_.text.toLowerCase)
      case x ⇒ None
    }
    def getIdOrType(n: Node) = getAttr(n, "id").orElse(getAttr(n, "type")).getOrElse("")
    val childs = trim(belems)
    val childs1 = childs.filter((n: Node) ⇒ { val x = getIdOrType(n) ; x != "header" && x != "footer" })      
    val (cl1, cl2) = childs1.span({ case (e: Elem) ⇒ e.label == "table"; case _ ⇒ false })
    val childs3 = (cl1 \\ "*").filter { 
      case e : Elem => e.label == "p" || e.label == "h1" || e.label == "h2" || e.label == "h3"
      case _ => false
    } ++ cl2
    val r = wrapText(childs3.toList).collect { case e : Elem => e }
    r
  }

  def chooseDivs(divs: List[Elem]) = divs

  val parLabels = Set("p", "h1", "h2", "h3", "h4", "blockquote")
  val isValidElem: PartialFunction[Node, Node] = (x: Node) ⇒
    x match {
      case e @ Elem(_, "table", _, _, _@ _*) ⇒ e
      case Elem(pref, "ol", _, scope, children @ _*) ⇒ Elem(pref, "ol", Null, scope, true, children.collect(isValidElem): _*)
      case Elem(pref, "li", _, scope, children @ _*) ⇒ Elem(pref, "li", Null, scope, true, children.collect(isValidElem orElse isContent): _*)
      case Elem(pref, label, attrs, scope, children @ _*) if (parLabels.contains(label)) ⇒ Elem(pref, "p", attrs, scope, true, children: _*)
    }
  val isContent: PartialFunction[Node, Node] = ({
    case t: Text ⇒ t
    case e @ Elem(_, "span", _, _, _*) ⇒ e
  }: PartialFunction[Node, Node]).orElse(isValidElem)

  def wrapText(nl: List[Node]): List[Node] = {
    val blockElems = List("table","thead","tbody","th","tr","td","p","blockquote","center","div","img")
    def docollect(nl: List[Node], accum: List[Node] = Nil, accum2 : List[Node] = Nil): List[Node] = nl match {
      case Nil if trim(accum).isEmpty => accum2.reverse
      case Nil => (<p>{ NodeSeq fromSeq accum.reverse }</p> :: accum2).reverse
      case (t: Text) :: r ⇒ docollect(r, t :: accum, accum2)
      case (e: Elem) :: r if (!blockElems.contains(e.label)) ⇒ docollect(r, e :: accum, accum2)
      case (e : Elem) :: r if trim(accum).isEmpty ⇒ docollect(r,Nil,e :: accum2)
      case (e : Elem) :: r  ⇒ docollect(r,Nil,e :: (<p>{ NodeSeq fromSeq accum.reverse }</p>) :: accum2)
    }
    docollect(nl, Nil)
  }
  
  def trimLeft(nl: List[Node]) = nl.dropWhile({ case t: Text ⇒ t.text.trim.isEmpty; case _ ⇒ false })
  def trim(nl: List[Node]) = trimLeft(trimLeft(nl).reverse).reverse
  val explodedBlockElements = Set("div", "center")
  val explodedInlineElements = Set("font")
  def explodeDivs(divs: List[Elem]) = {
    def explode(n: Node): List[Node] = n match {
      case e: Elem if explodedBlockElements.contains(e.label) ⇒ wrapText(e.child.toList).flatMap(explode)
      case e: Elem if explodedInlineElements.contains(e.label) ⇒ e.child.toList.flatMap(explode)
      case e: Elem if e.label == "td" ⇒ 
      	 trim(e.child.toList) match {
            case List(e2: Elem) if e2.label == "p" ⇒ List(e copy (child = e2.child.toList.flatMap(explode)))
            case _ ⇒ List(e copy (child = e.child.toList.flatMap(explode)))
         }
      case e: Elem ⇒ List(e copy (child = e.child.toList.flatMap(explode)))
      case x ⇒ List(x)
    }
    divs.flatMap(explode).collect(isValidElem)
  }

  def changeElem(f: Elem ⇒ Elem) = (n: Node) ⇒ n match {
    case Elem(_, _, _, _, _*) ⇒ f(n.asInstanceOf[Elem])
    case _ ⇒ n
  }

  def mapToAttributes(m: Map[String, String]) = m.foldRight(Null.asInstanceOf[MetaData])(
    (kv, md) ⇒ {
      val (k, v) = kv;
      new UnprefixedAttribute(k, v, md);
    })

  def cleanAttributes: Seq[Node] ⇒ Seq[Node] = bottomUp((n: Node) ⇒ {

    val changeAttrs = (f: (Map[String, String] ⇒ Map[String, String])) ⇒
      (e: Node) ⇒ e match {
        case Elem(pref, name, attrs, scope, children @ _*) ⇒
          Elem(pref, name, mapToAttributes(f(attrs.asAttrMap)), scope, true, children: _*)
      }

    val allowedStyles = Set[String](
      "font-weight:bold", "font-weight:bolder", "font-weight:normal", "font-style:italic", "font-style:oblique", "font-style:normal",
      "text-decoration:underline","vertical-align:super", "vertical-align:sub" )

    def cleanStyle(v: String) = {
      v.split(";").filter(allowedStyles.contains).mkString(";")
    }

    val filterStyle: PartialFunction[(String, String), (String, String)] = (kv: (String, String)) ⇒ kv match {
      case ("style", v) ⇒ ("style", cleanStyle(v))
    }

    val cleanSpanAttrs: Elem ⇒ Elem = changeAttrs(_.collect(filterStyle).toMap)

    val filterRedundantTDAttrs = (m: Map[String, String]) ⇒ m.filter({
      case ("rowspan", "1") ⇒ false
      case ("colspan", "1") ⇒ false
      case _ ⇒ true
    })

    def keepOnly(keyNames: String*) = (m: Map[String, String]) ⇒ m.filter(x ⇒ keyNames.contains(x._1))

    val emptyAttributes = changeAttrs((_: Map[String, String]) ⇒ Map())

    def saveIndentation(e: Elem) = e match {
      case Elem(pref, name, attrs, scope, children @ _*) ⇒ {
        val styleMap = getStyleMap(attrs).withDefault(_ ⇒ "")
        def parseAndNormalize(s: String): Option[Double] = {
          val re = new Regex("(-?[0-9.]+)([a-z]+)")
          val ratios = Map[String, Double](
            "mm" -> 1, "cm" -> 10, "in" -> 25.4)
          val m = re.findAllIn(s)
          if (!m.isEmpty) {
            val num = m.group(1).toDouble
            val unit = m.group(2)
            val ratio = ratios.get(unit)
            ratio.map(_ * num)
          } else {
            None
          }
        }
        val textIndent = parseAndNormalize(styleMap("text-indent"))
        val marginLeft = parseAndNormalize(styleMap("margin-left"))
        val centered = styleMap("text-align") == "center"
        val indentation = List(textIndent, marginLeft).collect({ case Some(x) ⇒ x }).sum
        Elem(pref, name, new UnprefixedAttribute("indentation", Text(indentation.toString), new UnprefixedAttribute("centered", centered.toString, Null)), scope, true, children: _*)
      }
    }

    n match {
      case Elem(_, label, _, _, _*) ⇒ {
        var e = n.asInstanceOf[Elem]
        label match {
          case "span" ⇒ cleanSpanAttrs(e)
          case "table" ⇒ changeAttrs(keepOnly("rows", "cols"))(e)
          case "td" ⇒ changeAttrs(filterRedundantTDAttrs.andThen(keepOnly("colspan", "rowspan")))(e)
          case _ ⇒ emptyAttributes(e)
        }
      }
      case _ ⇒ n
    }

  })

  def fixSpans(nl: List[Node]): List[Node] = {
    nl.flatMap({  
	    case e @ (Elem(pref, label, attrs, scope, child @ _*)) ⇒ {
	      val child2 = fixSpans(child.toList)
	      e.label match {
	        case "span" ⇒ makeSpanOrIandB(pref, scope, attrs, child2)
	        case _ ⇒ List(Elem(pref, label, attrs, scope, true, child2: _*))
	      }
	    }
	    case n ⇒ List(n)
	  })
  }

  def makeSpanOrIandB(prefix: String, scope: NamespaceBinding, attrs: MetaData, child: Seq[Node]): Seq[Node] = {
    
    def makePair(s: String) = s.span(c ⇒ c != ':') match {
      case (k, "") ⇒ (k, "")
      case (k, v) ⇒ (k, v.substring(1))
    }
    val attrMap = attrs.asAttrMap
  
    val styleString = attrMap.get("style").getOrElse("")
    val otherAttrs: Map[String, String] = attrMap - "style"
    val styles = styleString.split(";").map(makePair).toMap

    val italicPresent = styles.get("font-style") match {
      case Some("italic") ⇒ true
      case Some("oblique") ⇒ true
      case _ ⇒ false
    }
    val boldPresent = styles.get("font-weight") match {
      case Some("bold") ⇒ true
      case Some("bolder") ⇒ true
      case _ ⇒ false
    }
    val isSuperScript = styles.get("vertical-align") == Some("super")    
    val isSubScript = styles.get("vertical-align") == Some("sub")
    val hasUnderline = styles.get("text-decoration") == Some("underline")
    
    val otherStyles = styles - "font-style" - "font-weight" - "text-decoration" - "vertical-align"
    val restMap: Map[String, String] = if (otherStyles.isEmpty) { otherAttrs } else {
      otherAttrs + (("style", otherStyles.toList.map(x ⇒ x._1 + ":" + x._2).mkString("", ";", "")))
    }
    
    var e = if (restMap.isEmpty) { child } else { Elem(prefix, "span", mapToAttributes(restMap), scope, true, child: _*) }
    e = if (italicPresent) { Elem(prefix, "i", Null, scope, true, e: _*) } else { e }
    e = if (boldPresent) { Elem(prefix, "b", Null, scope, true, e: _*) } else { e }
    e = if (isSuperScript) { Elem(prefix, "sup", Null, scope, true, e: _*) } else { e }
    e = if (isSubScript) { Elem(prefix, "sub", Null, scope, true, e: _*) } else { e }
    //e = if (hasUnderline)  { Elem(prefix, "u", Null, scope, true, e: _*) } else { e }
    if (hasUnderline)  { logger.warn("text has underline!") }
    //logger.info("makeSpanOrIandB: attrMap = " + attrMap + ", styleString = " + styleString + ", styles =  " + styles + ", italicPresent = " + italicPresent + ", boldPresent = " + boldPresent + ", restMap.isEmpty = " + restMap.isEmpty + ", res = " + child3)
    
    e
  }

  def mapElements[T](others: Node ⇒ T, elem: Elem ⇒ T) = (n: Node) ⇒
    n match {
      case e: Elem ⇒ elem(e)
      case _ ⇒ others(n)
    }

  def id[T]: T ⇒ T = (t: T) ⇒ t

  val validElements = Set("p", "span", "sup", "sub", "table", "tr", "td", "th", "b", "i", "ol", "li", "img", "blockquote", "u",
      "h1","h2","h3","h4")

  val cleanSeqNodes: List[Node] ⇒ List[Node] = bottomUp(mapElements(id,
    (e: Elem) ⇒ if (validElements.contains(e.label)) { e } else { e.child }))

  val headings = Set("h1", "h2", "h3", "h4")
    
  val renameHeadings: List[Node] ⇒ List[Node] = bottomUp(mapElements(id,
    (e: Elem) ⇒ if (headings.contains(e.label)) { e copy (label = "p") } else { e }))
    
  def bottomUp(f: Node ⇒ Seq[Node]): Seq[Node] ⇒ List[Node] = (ns: Seq[Node]) ⇒ {
    val chChildren = (n: Node) ⇒ (changeChildren(bottomUp(f))(n))

    var nl = ns.iterator.toList
    nl.flatMap(f.compose(chChildren))
  }

  def topDown(f: Node ⇒ Seq[Node]): Seq[Node] ⇒ List[Node] = (ns: Seq[Node]) ⇒ {
    val chChildren = (n: Node) ⇒ (changeChildren(bottomUp(f))(n))
    var nl = ns.iterator.toList
    nl.flatMap(f).map(chChildren)
  }

  def topDownUntil(f: PartialFunction[Node, Seq[Node]]): Seq[Node] ⇒ List[Node] = (ns: Seq[Node]) ⇒ {
    val rec = topDownUntil(f)
    ns.toList.flatMap((n: Node) ⇒
      (f.lift(n)) match {
        case None ⇒ changeChildren(rec)(n)
        case Some(ns2) ⇒ ns2
      })
  }

  def transformTextWith[A](f: (A, String) ⇒ (String, A))(a: A): Seq[Node] ⇒ List[Node] = (ns: Seq[Node]) ⇒ {
    def doit(bl: (A, List[Node]), n: Node): (A, List[Node]) = {
      val (b1, l) = bl
      n match {
        case Text(t1) ⇒ {
          val (t2, b2) = f(b1, t1)
          if (t2.isEmpty) { (b2, l) }
          else { (b2, Text(t2) :: l) }
        }
        case Elem(pref, name, attrs, scope, children @ _*) ⇒ {
          val (b2, rl) = children.foldLeft(b1, List[Node]())(doit)
          (b2, Elem(pref, name, attrs, scope, true, rl.reverse: _*) :: l)
        }
        case _ ⇒ (b1, n :: l)
      }
    }
    val (_, rl) = ns.foldLeft(a, List[Node]())(doit)
    rl.reverse
  }

  def transformTextBackwardsWith[A](f: (A, String) ⇒ Option[(String, A)])(a: A): Seq[Node] ⇒ List[Node] =
    (ns: Seq[Node]) ⇒ {
      def doit(bl: (A, List[Node], Boolean), n: Node): (A, List[Node], Boolean) = {
        val (b1, l, skip) = bl
        if (skip) { (b1, n :: l, skip) } else n match {
          case Text(t1) ⇒
            f(b1, t1) match {
              case None ⇒ (b1, n :: l, true)
              case Some((t2, b2)) ⇒ {
                if (t2.isEmpty) { (b2, l, false) }
                else { (b2, Text(t2) :: l, false) }
              }
            }
          case Elem(pref, name, attrs, scope, children @ _*) ⇒ {
            val (b2, rl, skip2) = children.reverse.foldLeft(b1, List[Node](), skip)(doit)
            (b2, Elem(pref, name, attrs, scope, true, rl: _*) :: l, skip2)
          }
          case _ ⇒ (b1, n :: l, skip)
        }
      }
      val (_, rl, _) = ns.reverse.foldLeft(a, List[Node](), false)(doit)
      rl
    }

  val re1 = new Regex("(\\s| )+")
  val re2 = new Regex("^ +")
  val re3 = new Regex("“ +")
  val re4 = new Regex(" +”")

  def cleanSpaces(trimLeft: Boolean, s: String) = {
    val s1 = re1.replaceAllIn(s, " ")
    val s2 = re3.replaceAllIn(s1, "“")
    val s3 = re4.replaceAllIn(s2, "”")
    val s4 = if (!trimLeft || s3.isEmpty) { s3 }
    else { re2.replaceFirstIn(s3, "") }
    val tl = if (s4.isEmpty) { trimLeft }
    else { s4.endsWith(" ") }
    (s4, tl)
  }

  val re5 = new Regex(" +$")

  val normalizeSpace: Seq[Node] ⇒ List[Node] = topDownUntil((n: Node) ⇒
    n match {
      case Elem(_, label, _, _, _*) if (label == "p" || label == "li" || label == "blockquote") ⇒ {
        val ns1 = transformTextWith(cleanSpaces)(true)(n)
        val ns2 = transformTextBackwardsWith(
          (skip: Boolean, t: String) ⇒
            if (skip) { None }
            else { Some(re5.replaceFirstIn(t, ""), true) })(false)(ns1)
        ns2
      }
    })

  def getAttr(md: MetaData, key: String): String = {
    md.get(key) match {
      case None ⇒ ""
      case Some(l) ⇒ l.map(_.text).mkString("", "", "")
    }
  }

  val cleanSpuriousSpans = topDown((n: Node) ⇒
    n match {
      case Elem(pref, "span", attrs, scope, children @ _*) if (getAttr(attrs, "style").isEmpty ||
        n.text.trim.isEmpty) ⇒ { children }
      case e: Elem if (e.label == "i" || e.label == "b") && e.text.trim.isEmpty ⇒ { e.child }
      case _ ⇒ n
    })

  val cleanNameSpaces = topDown((n: Node) ⇒
    n match {
      case Elem(pref, label, attrs, _, cl @ _*) ⇒
        Elem(pref, label, attrs, TopScope, true, cl: _*)
      case _ ⇒ n
    })

  def cleanRepeatedEmptyParagraphs(ns: Seq[Node]) = {
    def f(n: Node, ns: List[Node]) =
      n match {
        case Elem(_, name, _, _, _*) if name == "p" || name == "blockquote" ⇒ ns match {
          //case List() => n :: ns
          case ((n2 @ Elem(_, "p", _, _, _*)) :: _) ⇒
            if (n.text.isEmpty && n2.text.isEmpty) { ns }
            else { n :: ns }
          case _ ⇒ n :: ns
        }
        case _ ⇒ n :: ns
      }
    ns.foldRight(List[Node]())(f)
  }

  def getStyleMap(m: MetaData) = {
    m.get("style") match {
      case None ⇒ Map[String, String]()
      case Some(s) ⇒ (NodeSeq fromSeq s).text.split(";").
        map((x: String) ⇒ { val r = x.split(":"); (r(0), r(1)) }).
        toMap
    }
  }

  def styleIsTheSame(m1: MetaData, m2: MetaData) =
    getStyleMap(m1) == getStyleMap(m2)

  def collectWhiteSpace(ns: Seq[Node]): (Seq[Node], Seq[Node]) =
    ns.span((n: Node) ⇒ n match {
      case Text(t) ⇒ t.trim.isEmpty
      case _ ⇒ false
    })

  val mergeTextNodes = topDown(
    mapElements(id, changeChildren((ns: Seq[Node]) ⇒ {
      def mergeTexts(n: Node, ns: List[Node]): List[Node] = {
        (n, ns) match {
          case (Text(t1), Text(t2) :: ns2) ⇒ (Text(t1 + t2) :: ns2)
          case _ ⇒ n :: ns
        }
      }
      ns.foldRight(List[Node]())(mergeTexts)
    })))

  val mergeSpans = bottomUp(
    mapElements(id, (e: Elem) ⇒ {
      val Elem(pref, label, attrs, scope, cl @ _*) = e
      def mergeit(n: Node, ns: List[Node]) = {
        val (ws, nss) = collectWhiteSpace(ns)
        val res = n match {
          case Elem(_, "span", attrs1, _, cl1 @ _*) ⇒
            nss match {
              case Elem(pref, "span", attrs2, scope, cl2 @ _*) :: ns2 ⇒
                if (styleIsTheSame(attrs1, attrs2)) {
                  Elem(pref, "span", attrs2, scope, true,  (cl1 ++ ws ++ cl2): _*) :: ns2
                } else {
                  n :: ns
                }
              case _ ⇒ n :: ns
            }
          case _ ⇒ n :: ns
        }
        res
      }
      val cll = cl.foldRight(List[Node]())(mergeit)
      Elem(pref, label, attrs, scope, true, cll: _*)
    }))

  val cleanSpecialCharacters = {
    val cleanit = (x: Unit, s: String) ⇒ {
      val s1 = s.map((c: Char) ⇒ c match {
        case '\u0096' ⇒ '-'
        case _ ⇒ c
      })
      (s1, x)
    }
    transformTextWith(cleanit)(())
  }

  def applySeq[T](fs: Seq[T ⇒ T]) =
    (v0: T) ⇒ fs.foldLeft(v0)((v: T, f: T ⇒ T) ⇒ f(v))

  def applySeqTo[T](v0: T)(fs: Seq[T ⇒ T]) = applySeq(fs)(v0)

  def pipelineXHTML(xhtml: Elem): List[Node] = {
    
    def debug(where: String): List[Node] ⇒ List[Node] = (l: List[Node]) ⇒ {
      println("debug: " + where + ":")
      l.zipWithIndex foreach { 
        case (n,i) =>
          println("  [%20s][%06d]: %s ".format(where,i,n.toString) )
      }      
      l
    }
    
    val xhtml2 = renameHeadings(List(xhtml)).collect { case e : Elem => e }.head
    
    val baseElems = selectBaseElems(xhtml2)
    
    val divs = chooseDivs(baseElems)
       
    val validElems = explodeDivs(divs)

    val res = applySeqTo(validElems)(List[List[Node] ⇒ List[Node]](
      //debug("start"),
      cleanNameSpaces,
      //debug("after cleanNameSpaces"),
      cleanSeqNodes,
      //debug("after cleanSeqNodes"),
      _.flatMap(cleanAttributes),      
      normalizeSpace,
      cleanSpuriousSpans,      
      mergeTextNodes,
      mergeSpans,
      fixSpans, 
      cleanRepeatedEmptyParagraphs,
      cleanSpecialCharacters))
    res
  }
  
  def pipelineWithDefaultConverter(source: Array[Byte], mimeType: String) : Option[List[Node]] = 
    pipeline(source,mimeType,defaultConverter)
    
  def pipeline(source: Array[Byte], mimeType: String, converter : Converter): Option[List[Node]] = 
    convertSrcToXHTML(source, mimeType,converter).map(pipelineXHTML)
  

  def pipeline(rtfSource: InputStream,converter : Converter = defaultConverter): XHTMLProcessorResult =  
    pipeline(IOUtils.toByteArray(rtfSource), "text/rtf",converter) match {
      case None ⇒ Failure
      case Some(x) ⇒ Success(x)
    }
  
}


