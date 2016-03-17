package br.gov.lexml.parser.pl.converter

import java.io.File
import org.apache.commons.io.filefilter.PrefixFileFilter
import org.apache.commons.io.FileUtils
import java.io.FileFilter
import scala.collection.SortedMap
import br.gov.lexml.parser.pl.converter.graph.Graph
import scala.sys.process.ProcessIO
import scala.sys.process.BasicIO
import scala.sys.process.ProcessBuilder
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FilenameUtils
import java.io.File.TempDirectory
import com.typesafe.config.Config
import java.io.OutputStream
import java.io.InputStream
import java.io.ByteArrayOutputStream
import scala.concurrent.SyncVar
import org.apache.commons.io.FileSystemUtils
import java.nio.file.attribute.FileAttribute
import java.nio.file.Files
import br.gov.lexml.parser.pl.mime.MimeRegistry
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import br.gov.lexml.parser.pl.config.MainConfig
import scala.collection.mutable.ArrayBuffer

trait Converter {
  def convert(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String): Future[Array[Byte]]
  /*
  def deleteByPrefix(dir : File, prefix : String) =
        dir.listFiles(new PrefixFileFilter(prefix) : FileFilter).foreach(f => FileUtils.deleteQuietly(f))
        */
  def acceptMimeTypes : Map[String,Double]
  def provideMimeTypes : Map[String,Double]
  final def chain(intermediaryType : String, next : Converter)(implicit ec : ExecutionContext) = CompositeConverter(this,intermediaryType,next)
}

object Converter {
  def identity(mimeType : String) = new Converter {
    override def acceptMimeTypes = Map(mimeType -> 1.0)
    override def provideMimeTypes = Map(mimeType -> 1.0)
    override def convert(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String) = Future.successful(sourceData)
  }
}

final case class ConverterTask(fromMimeType : String, toMimeType : String, converter : Converter) {
  def apply(data : Array[Byte]) = converter.convert(fromMimeType,data,toMimeType)
}

class ConverterException(msg : String = "", cause : Option[Throwable] = None) extends Exception(msg,cause.orNull)

final case class CompositeConverter(first : Converter, intermediaryType : String, second : Converter)(implicit ec : ExecutionContext) extends Converter {
  if(!first.provideMimeTypes.contains(intermediaryType) ||
     !second.acceptMimeTypes.contains(intermediaryType)) {
    throw new ConverterException(s"CompositeConverter specified with invalid intermediary type: type=${intermediaryType}, converter=${this}, first.provide=${first.provideMimeTypes}, second.accept=${second.acceptMimeTypes}")
  }
  override def acceptMimeTypes = first.acceptMimeTypes
  override def provideMimeTypes = second.provideMimeTypes
  override def convert(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String) = {
    for {
      intermediaryData <- first.convert(sourceMimeType, sourceData, intermediaryType)
      finalData <- second.convert(intermediaryType, intermediaryData , targetMimeType) 
    } yield { finalData }
  }
}

trait ExternalProcessConverter extends Converter {
  type Ctx
  implicit val ec : ExecutionContext
  
  protected def prepareContext(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String) : Ctx
  protected def prepareBuilder(ctx : Ctx) : ProcessBuilder
  protected def prepareProcessIO(ctx : Ctx) : ProcessIO
  protected def extractResult(ctx : Ctx,exitValue : Int) : Array[Byte]
  protected def releaseResources(ctx : Ctx) : Unit
  
  def valid() : Boolean
    
  final override def convert(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String) = Future {
    val ctx = prepareContext(sourceMimeType,sourceData,targetMimeType)        
    try {
      val pb = prepareBuilder(ctx)          
      val pio = prepareProcessIO(ctx)
      //println(s"will run pb=${pb}, pio=${pio}")
      val process = pb.run(pio)
      //println(s"running: process=${process}")
      val exitValue = process.exitValue()
      //println(s"stopped: exitValue=${exitValue}")
      extractResult(ctx,exitValue)      
    } finally {
      releaseResources(ctx)
    }
  }
}

final class ConfigExternalProcessConverter(_cfg : Config)(implicit val ec : ExecutionContext) extends ExternalProcessConverter {

  import java.io.InputStreamReader
  import com.typesafe.config.ConfigFactory
  private val defaultConfig = 
    ConfigFactory.parseReader(
        new InputStreamReader(
            classOf[Converter].
            getResourceAsStream("default-external-process-converter-config.config")))
 
  val cfg = _cfg.withFallback(defaultConfig)
    
  def mimeToExtension(mime : String) : String = {
    //println(s"mimeToExtension: mime=${mime}")
    MimeRegistry(mime).extensions.head
  }
  
  val processTemplate = {
    import scala.collection.JavaConversions._
    val temp = cfg.getStringList("processTemplate").to[IndexedSeq].map(_.trim()).filterNot(_.isEmpty)  
    if(temp.isEmpty()) {
        sys.error(s"processTemplate cannot be empty at: ${cfg}")
    }
    temp
  }
  
  val removeAtEnd = cfg.getBoolean("removeAtEnd")
  
  final case class Ctx(      
    inputData : Array[Byte],
    tempDir : File,        
    inputFile : Option[File] = None,
    outputFile : Option[File] = None,
    outputBuffer : SyncVar[Array[Byte]] = new SyncVar()
    )     
  
  override def prepareContext(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String) = {
    val tempDirPrefix = cfg.getString("tempDirPrefix")
    var ctx  = {
      val f = Files.createTempDirectory(tempDirPrefix).toFile
      f.mkdirs()                  
      val ctx = Ctx(sourceData,f)
      ctx
    }    
    def fileOpt(opt : String,mimeType : String)(upd : (Ctx,File) => Ctx) = {
        val s = cfg.getString(opt)             
        val name = s.trim().replaceAll(File.separator,"_").replaceAll(":","_")
        if(!name.isEmpty) {
          ctx = upd(ctx,new File(ctx.tempDir,name + "." + mimeToExtension(mimeType)))
        }
    }
    fileOpt("inputFile",sourceMimeType) { 
      case (ctx,f) =>
        //println(s"preparing input file: ${f}")
        FileUtils.writeByteArrayToFile(f, sourceData) 
        ctx copy (inputFile = Some(f))  
    }
    fileOpt("outputFile",targetMimeType) { case (ctx,f) => ctx copy (outputFile = Some(f)) }    
    ctx
  }
  
  override def prepareBuilder(ctx : Ctx) : ProcessBuilder = {
    import scala.sys.process._
    var processStrings = processTemplate
    ctx.inputFile foreach { f =>
      processStrings = processStrings.map(_.replaceAllLiterally("{input}",f.getName))
    }
    ctx.outputFile foreach { f =>
      processStrings = processStrings.map(_.replaceAllLiterally("{output}",f.getName))
    }
    var pb : ProcessBuilder = Process(processStrings,Some(ctx.tempDir))
    pb
  }
  override def prepareProcessIO(ctx : Ctx) : ProcessIO = {
    val procIn : OutputStream => Unit = ctx.inputFile match {
      case None => os => { IOUtils.write(ctx.inputData,os) ; IOUtils.closeQuietly(os) }
      case _ => IOUtils.closeQuietly(_)
    }
    val procOut : InputStream => Unit = ctx.outputFile match {
      case None => is => { 
        val os = new ByteArrayOutputStream 
        BasicIO.transferFully(is,os) 
        BasicIO.close(os) 
        ctx.outputBuffer.put(os.toByteArray)
      }
      case _ => BasicIO.toStdOut
    }
    new ProcessIO(procIn,procOut,BasicIO.toStdErr)
  }
  val outputBufferTimeout = {
    import scala.concurrent.duration._
    1.second
  }
  override def extractResult(ctx : Ctx,exitValue : Int) : Array[Byte] =  {
    ctx.outputFile.map(FileUtils.readFileToByteArray).getOrElse {
      ctx.outputBuffer.get(outputBufferTimeout.toMillis).getOrElse(sys.error(s"timeout waiting for output buffer ctx=${ctx}"))
    }    
  }
  override def releaseResources(ctx : Ctx) {
    if(removeAtEnd) {
      ctx.inputFile foreach { _.delete() }
      ctx.outputFile foreach { _.delete() }
      FileUtils.deleteDirectory(ctx.tempDir)
    }
  }
  import scala.collection.JavaConversions._
  override def acceptMimeTypes = {
    cfg.getConfigList("accept").to[Seq].map(c => (c.getString("mime"),c.getDouble("cost"))).toMap
  }
  
  override def provideMimeTypes = { 
    cfg.getConfigList("provide").to[Seq].map(c => (c.getString("mime"),c.getDouble("value"))).toMap
  }
  
  override def valid() = Files.isExecutable(Paths.get(processTemplate.head))
}

object Converters { 
  
  def configConverters()(implicit ec : ExecutionContext) = {
    val convs = IndexedSeq.newBuilder[Converter] 
    import scala.collection.JavaConversions._
    val conf = MainConfig.config.getConfig("lexml-parser.converters")
    val externalConverters = conf.getConfigList("external-converters")
    import scala.concurrent.ExecutionContext.global
    externalConverters.map(new ConfigExternalProcessConverter(_)).filter(_.valid).foreach(convs += _)    
    convs.result()
  }
}
/*
trait ExternalProcessConverter[Res] extends Converter {
  implicit val ec : ExecutionContext
  
  final override def convert(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String) = Future {
        val res = prepareResources(sourceMimeType,targetMimeType,sourceData)
        try {
          val pb = makeBuilder(res)
          val pio = makeProcessIO(res)
          val process = pb.run(pio)
          val exitValue = process.exitValue()
          getResult(res,exitValue)
        } finally {
          releaseResources(res)
        }
    }
    
  
  def prepareResources(inMimeType : String, outMimeType : String, data : Array[Byte]) : Res
  def makeBuilder(res : Res) : ProcessBuilder  
  def makeProcessIO(res : Res) : ProcessIO = BasicIO.standard(false)
  def getResult(res : Res, exitValue : Int) : Array[Byte]
  def releaseResources(res : Res) : Unit = { }
}

final case class TempDirFileInputFileOutputResource(
    inMimeType : String,
    outMimeType : String,
    tempDir : File,
    inputFile : File,
    outputFile : File
    )
    
object MimeTypeExtensions {
  val extension : Map[String,String] = Map(
      )
}
    
trait TempDirFileInputFileOutputExternalProcessConverter extends ExternalProcessConverter[TempDirFileInputFileOutputResource] {
  def tempDirPreffix = "temp_dir_"
  def tempDirSuffix = ""
  
  def makeTempDir() : File = {
    val f = File.createTempFile(tempDirPreffix,tempDirSuffix)
    f.mkdirs()
    f
  }
  
  def extensionFromMimeType(mimeType : String) : Option[String] = MimeTypeExtensions.extension.get(mimeType)
  def inputFileBaseName(mimeType : String) : String = "input"
  def outputFileBaseName(mimeType : String) : String = "output"
  
  def makeInputFileName(mimeType : String) : String = {
    val bn = inputFileBaseName(mimeType)
    val ext = extensionFromMimeType(mimeType)
    bn + ext.map("." + _).getOrElse("")
  }
  def makeOutputFileName(mimeType : String) : String = {
    val bn = outputFileBaseName(mimeType)
    val ext = extensionFromMimeType(mimeType)
    bn + ext.map("." + _).getOrElse("")
  }
  final override def prepareResources(inMimeType : String, outMimeType : String, data : Array[Byte]) = {    
    val tempDir = makeTempDir()
    val tempInputFile = new File(tempDir,makeInputFileName(inMimeType))
    val tempOutputFile = new File(tempDir,makeOutputFileName(outMimeType))
    FileUtils.writeByteArrayToFile(tempInputFile,data)
    TempDirFileInputFileOutputResource(
        inMimeType,outMimeType,tempDir,tempInputFile,tempOutputFile)     
  }    
  final override def makeProcessIO(res : TempDirFileInputFileOutputResource) : ProcessIO = BasicIO.standard(false)
  final override def getResult(res : TempDirFileInputFileOutputResource, exitValue : Int) : Array[Byte] = 
    FileUtils.readFileToByteArray(res.outputFile)
  
  final override def releaseResources(res : TempDirFileInputFileOutputResource) : Unit =  
    FileUtils.deleteDirectory(res.tempDir)  
}

abstract sealed class FileExtSpec extends Product

case object DerivedFromMimeType extends FileExtSpec

case object NoExtension extends FileExtSpec

final case class DefinedExtension(ext : String) extends FileExtSpec 

abstract sealed class InputSpec extends Product

final case class FileNameSpec(baseName : String, ext : FileExtSpec = DerivedFromMimeType) {
  def makeFile(dir : File, mimeType : String, mimeTypeExtMap : Map[String,String]) : File = {
    val extension = ext match {
      case DerivedFromMimeType => mimeTypeExtMap.get(mimeType).map("." + _).getOrElse("")
      case NoExtension => ""
      case DefinedExtension(e) => "." + e
    }
    new File(dir,baseName + extension)
  }
}

final case class NamedInputFileSpec(fns : FileNameSpec) extends InputSpec

case object StdinInputSpec extends InputSpec

abstract sealed class OutputSpec extends Product

case object DerivedFromInputOutputSpec extends OutputSpec

case object StdoutOutputSpec extends OutputSpec

final case class NamedOutputFileSpec(fns : FileNameSpec) extends OutputSpec

final case class SimpleExternalProcessIOSpec(input : InputSpec = StdinInputSpec, output : OutputSpec = StdoutOutputSpec) {
  def requiresDir = input != StdinInputSpec || output != StdoutOutputSpec
  def makeResources(tempDirPrefix : String = "temp_dir_", tempDirSuffix : String = "", inputMimeType : String, outputMimeType : String, mimeTypeExtMap : Map[String,String]) : SimpleTemplateIOExternalProcessConverterRes = (input,output) match {
    case (StdinInputSpec,StdoutOutputSpec) => NoFileRes
    case _ =>
      val tempDir = {
        val f = File.createTempFile(tempDirPrefix, tempDirSuffix)
        f.mkdirs()
        f
      }
      val (inputFile,output2) = (input,output) match {                
        case (NamedInputFileSpec(fns),os) => 
          val inp = fns.makeFile(tempDir,inputMimeType,mimeTypeExtMap)
          val out = if (output == DerivedFromInputOutputSpec) { NamedOutputFileSpec(FileNameSpec(fns.baseName,DerivedFromMimeType)) } else { output }
          (Some(inp),out)          
        case (is,DerivedFromInputOutputSpec) => throw new ConverterException(s"Cannot derive output file name from input file spec: ${is}")
        case (_,os) => (None,os) 
      }
      val outputFile = output2 match {
        case StdoutOutputSpec => None
        case NamedOutputFileSpec(fns) => Some(fns.makeFile(tempDir,outputMimeType,mimeTypeExtMap))
        case _ => sys.error(s"Unexpected state: output2 should not be ${output2}")       
      }
      WithFileRes(tempDir,inputFile,outputFile)
  }
}

final case class SimpleTemplateExternalProcessConverterSpec(
    programPath : File,
    tempDirPrefix : String = "temp_dir_",
    tempDirSuffix : String = "",
    ioSpec : SimpleExternalProcessIOSpec = SimpleExternalProcessIOSpec(),
    argTemplates : IndexedSeq[String] = IndexedSeq(),
    acceptedMimeTypes : Map[String,Double] = Map(),
    providedMimeTypes : Map[String,Double] = Map(),
    mimeExtensionMap : Map[String,String] = Map()
    )

abstract sealed class SimpleTemplateIOExternalProcessConverterRes extends Product {
  def cleanup() : Unit
  def formatArg(argTemplate : String) : String
  def temporaryDir : Option[File]
  def inputFile : Option[File]
  def outputFile : Option[File]
}

case object NoFileRes extends SimpleTemplateIOExternalProcessConverterRes {
  override def cleanup() { }
  def formatArg(argTemplate : String) = argTemplate
  override def temporaryDir = None
  override def inputFile = None
  override def outputFile = None
}

final case class WithFileRes(tempDir : File, inputFile : Option[File] = None, outputFile : Option[File] = None) extends SimpleTemplateIOExternalProcessConverterRes {
  override def cleanup() {
    FileUtils.deleteDirectory(tempDir)
  }
  def formatArg(argTemplate : String) = {
    val arg1 = inputFile match {
      case None => argTemplate
      case Some(f) => argTemplate.replaceAll("{input}",f.getCanonicalPath)      
    }
    val arg2 = outputFile match {
      case None => argTemplate
      case Some(f) => argTemplate.replaceAll("{output}",f.getCanonicalPath)      
    }
    arg2
  }
  override def temporaryDir = Some(tempDir)
}
        
class SimpleTemplateIOExternalProcessConverter(spec : SimpleTemplateExternalProcessConverterSpec)(implicit ec : ExecutionContext) extends Converter {
  override val acceptMimeTypes = spec.acceptedMimeTypes
  override val provideMimeTypes = spec.providedMimeTypes
  override def convert(sourceMimeType: String, sourceData: Array[Byte], targetMimeType: String) = Future {
    val res = spec.ioSpec.makeResources(spec.tempDirPrefix,spec.tempDirSuffix,sourceMimeType,targetMimeType, spec.mimeExtensionMap)
    val args = spec.argTemplates.map(res.formatArg)
    import scala.sys.process._
    val cmd : Seq[String] = spec.programPath.getCanonicalPath +: args
    var pb : ProcessBuilder = Process(cmd, res.temporaryDir) 
    res.inputFile.foreach(f => pb = pb #< f)
    res.outputFile.foreach(f => pb = pb #> f)
    pb.run()
    val (tempDir,inputFile,outputFile) = spec.ioSpec.specs match  {
      (StdinInputSpec,StdoutOutputSpec) => (None,None,None)
      val f = File.createTempFile(spec.tempDirPrefix, spec.tempDirSuffix)
      f.mkdirs()
      Some(f)
    } else { None }
        /*val res = prepareResources(sourceMimeType,targetMimeType,sourceData)
        try {
          val pb = makeBuilder(res)
          val pio = makeProcessIO(res)
          val process = pb.run(pio)
          val exitValue = process.exitValue()
          getResult(res,exitValue)
        } finally {
          releaseResources(res)
        }*/
    ???
  }
  
}
    
*/    
