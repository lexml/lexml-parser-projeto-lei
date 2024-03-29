package br.gov.lexml.parser.pl.fe

import br.gov.lexml.parser.pl.metadado.Metadado

import java.io.File
import br.gov.lexml.parser.pl.profile.Lei
import br.gov.lexml.parser.pl.profile.OverridesData
import br.gov.lexml.parser.pl.metadado.Data
import br.gov.lexml.parser.pl.metadado.Timestamp
import br.gov.lexml.parser.pl.profile.DocumentProfileRegister
import br.gov.lexml.parser.pl.profile.DocumentProfile

import scala.reflect.ClassTag
import scala.util.matching.Regex
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils
import br.gov.lexml.parser.pl.xhtml.XHTMLProcessor
import br.gov.lexml.parser.pl.block.Block
import br.gov.lexml.parser.pl.ProjetoLeiParser
import br.gov.lexml.parser.pl.output.LexmlRenderer
import br.gov.lexml.parser.pl.validation.Validation
import br.gov.lexml.parser.pl.errors.ParseProblem
import br.gov.lexml.parser.pl.errors.PC_ErroGeralParser
import br.gov.lexml.parser.pl.errors.ProblemType
import br.gov.lexml.parser.pl.errors.ErroValidacaoSchema
import br.gov.lexml.parser.pl.ProjetoLei

import scala.xml.Elem
import br.gov.lexml.parser.pl.linker.Linker
import org.apache.logging.log4j.core.LoggerContext
import scopt.OptionDef


abstract sealed class SourceType {
  def toByteArray : Array[Byte]
}

case object ST_Stdin extends SourceType {
  lazy val toByteArray: Array[Byte] = IOUtils.toByteArray(System.in)
}


case class ST_File(f : File) extends SourceType {
  lazy val toByteArray: Array[Byte] = FileUtils.readFileToByteArray(f)
}

abstract sealed class SinkType {
  def write(data : Array[Byte]): Unit
}

case object SK_Stdout extends SinkType {
  def write(data : Array[Byte]): Unit = {
    System.out.write(data)
    System.out.flush()
    System.out.close()
  }
}

case class SK_File(f : File) extends SinkType {
  def write(data : Array[Byte]): Unit = {
    FileUtils.writeByteArrayToFile(f,data)
  }
}

abstract sealed class ErrorOutput {
  def writeErrors(errs : Seq[ParseProblem]): Unit
}

case object EO_Stderr extends ErrorOutput {
  def writeErrors(errs : Seq[ParseProblem]): Unit = {
    errs.foreach(e => System.err.println(e.desc + System.lineSeparator()))
    System.err.flush()
  }
}

final case class EO_File(f : File) extends ErrorOutput {
  import java.io._  
  def writeErrors(errs : Seq[ParseProblem]): Unit = {
    val w = new PrintWriter(
        new OutputStreamWriter(
            new FileOutputStream(f),"utf-8"))
    errs.foreach(e => w.println(e.desc + System.lineSeparator()))
    w.close()
  }
}

abstract sealed class Cmd

case object CmdHelp extends Cmd

case class CmdParse(    
    metadado : Metadado = Metadado(Lei,hashFonte = None),
    mimeType : String = "text/plain",
    input : SourceType = ST_Stdin,
    output : SinkType = SK_Stdout,
    linkerPath : Option[File] = None,
    overrides : Option[OverridesData] = None,    
    showInfo : Boolean = false,
    baseAutoridade : Option[String] = None,
    baseTipoNorma : Option[String] = None,
    errorOutput : ErrorOutput = EO_Stderr) extends Cmd {
  def changeOverrides(f : OverridesData => OverridesData): CmdParse = {
    val m = overrides.getOrElse(OverridesData())
    copy(overrides = Some(f(m)))
  }
  def changeMetadado(f : Metadado => Metadado): CmdParse =
    copy(metadado = f(metadado))
}

case class CmdParseArticulacao(
    input : SourceType = ST_Stdin,
    output : SinkType = SK_Stdout,
    linkerPath : Option[File] = None,
    contexto : String = "urn:lex:br:federal:lei:2000-01-01;1" 
    ) extends Cmd
    

case object CmdDumpProfiles extends Cmd    
    
case class CmdLineOpts(
    cmd : Cmd = CmdHelp,    
    verbose : Boolean = false,
    log4jConfigFile : Option[File] = None    
    ) {
  
  def command[C <: Cmd : ClassTag](dv : => C)(f : C => C) : CmdLineOpts = {    
    val ct = implicitly[ClassTag[C]]
    cmd match {
      case ct(c) => copy (cmd = f(c))
      case _ => copy (cmd = f(dv))
    }    
  }    
}

class FECmdLineOptionParser extends scopt.OptionParser[CmdLineOpts]("parser") {
    def cmdParse[T](f : (T,CmdParse) => CmdParse) : (T,CmdLineOpts) => CmdLineOpts = {
      case (v, opts) =>        
        opts.command(CmdParse())(c => f(v,c))        
    }
    
    def stringToRegexList(txt : String): Option[List[Regex]] =
      txt.split('%').to(List) match {
        case Nil => None
        case l => Some(l.map(_.r))      
      }
    def optOverrides[T](f : (T,OverridesData) => OverridesData) : (T,CmdLineOpts) => CmdLineOpts = 
      cmdParse {
        case (v,cmd) => 
          cmd changeOverrides { f(v,_) }                        
      } 
    
    
    def optOverridesRegex(suffix : String,desc : String)(f : (OverridesData,Option[List[Regex]]) => OverridesData): OptionDef[String, CmdLineOpts] = {
      opt[String](s"prof-regex-$suffix"). valueName("regex%regex%..."). action { optOverrides { 
          case (rl,o) => f(o,stringToRegexList(rl))                          
        }
      }.text("expressões regulares, separadas pelo caracter '%', que são usada para identificar " + desc)
    }
    
    def cmdParseArticulacao[T](f : (T,CmdParseArticulacao) => CmdParseArticulacao) : (T,CmdLineOpts) => CmdLineOpts = {
      case (v, opts) =>        
        opts.command(CmdParseArticulacao())(c => f(v,c))        
    }    
  
    head("LexML Parser Command-line tool")
    note("Opções Gerais:")
    opt[File]("log4j-configuration-xml").
      text("arquivo de configuração de log4j2. Será usado ao invés do padrão.").
      action { case (f,opts) => opts.copy(log4jConfigFile = Some(f)) }
    opt[Unit]('v',"verbose"). action {
      (_,opts) => opts copy (verbose = true)
    }.text("Mostra informações durante o parse.")
    help("help").text("Mostra esse texto")
    cmd("parse").action { (_,opts) => opts.copy(cmd = CmdParse()) }
      .text("Faz o parse de um documento completo")
      .children(
        opt[String]('m',"mime-type").
          text("tipo MIME do arquivo de entrada, ex.: text/plain (default)," +
               " application/vnd.openxmlformats-officedocument.wordprocessingml.document, " +
               "... ").
          action { cmdParse { case (mt,cmd) =>
            cmd copy (mimeType = mt)
          }
        },
        opt[File]('i',"input").
          text("arquivo de entrada. Se omitido, será usado a entrada padrão").
          action { cmdParse { case (f,cmd) =>
            cmd.copy(input = ST_File(f))
          }
        },
        opt[File]('o',"output").
          text("arquivo de saída. Se omitido, será usado a saída padrão").
          action { cmdParse { case (f,cmd) =>
            cmd.copy(output = SK_File(f))
          }
        },
        opt[File]("write-errors-to-file").
          text("gravar erros em arquivo de saída. Se omitido, será usado a saída de erro padrão").
          action { cmdParse { case (f,cmd) =>
            cmd.copy(errorOutput = EO_File(f))
          }
        },
        opt[Unit]("info"). action { cmdParse { case (_,cmd) =>
            cmd copy (showInfo = true)
          }
        }.text("Mostra os parâmetros usados no parse e sai sem executar o parse"),
        opt[String]('t',"tipo-norma").action { cmdParse { case (n,cmd) =>
            cmd changeMetadado {
              _.copy(tipoNorma = Some(n))
            }
            cmd.changeOverrides(_.copy(overrideUrnFragTipoNorma = Some(n)))
          }
        }.text("tipo de norma, na sintaxe usada na URN do LexML, eg. 'lei'"),
        opt[String]('a',"autoridade"). action { cmdParse { case (n,cmd) =>
            cmd changeMetadado {
              _.copy(autoridade = Some(n))
            }
            cmd.changeOverrides(_.copy(overrideUrnFragAutoridade = Some(n)))
          }
        }.text("autoridade, na sintaxe usada na URN do LexML, eg. 'federal'"),

        opt[String]('T',"base-tipo-norma").action { cmdParse { case (n,cmd) =>
          cmd changeMetadado {
            _.copy(tipoNorma = Some(n))
          }
        }
        }.text("tipo de norma usada como base para extensão"),
        opt[String]('A',"base-autoridade"). action { cmdParse { case (n,cmd) =>
          cmd changeMetadado {
            _.copy(autoridade = Some(n))
          }
        }
        }.text("autoridade usada como base para extensão"),

        opt[String]('l',"localidade"). action { cmdParse { case (n,cmd) =>
            cmd changeMetadado {
              _.copy(localidade = Some(n))
            }
            cmd.changeOverrides(_.copy(overrideUrnFragLocalidade = Some(Some(n))))
          }
        }.text("localidade, na sintaxe usada na URN do LexML, eg. 'br'."),

        opt[Int]('n',"numero").valueName("número da norma"). action { cmdParse { case (n,cmd) =>
            cmd changeMetadado {
              _ changeId {
                _.copy(num = n)
              }
            }
          }
        }.text("número da norma"),
        opt[Int]("complemento").valueName("complemento do número da norma"). action { cmdParse { case (n,cmd) =>
            cmd changeMetadado {
              _ changeId {
                _.copy(complemento = Some(n)) //(num, complemento, anoOuData, versao)
              }
            }
          }
        }.text("complemento do número da norma"),
        opt[Int]("ano"). action { cmdParse { case (n,cmd) =>
            cmd changeMetadado {
              _ changeId {
                _.copy(anoOuData = Left(n)) //(num, complemento, anoOuData, versao)
              }
            }
          }
        }.text("ano da publicação da norma (quando o mês e dia são desconhecidos)"),
        opt[String]("data"). action { cmdParse { case (n,cmd) =>
            Data.fromString(n) match {
              case None => cmd
              case Some(data) =>
                cmd changeMetadado {
                  _ changeId {
                    _.copy(anoOuData = Right(data)) //(num, complemento, anoOuData, versao)
                  }
                }
            }
          }
        }.text("data completa da publicação da norma (AAAA-MM-DD)"),
        opt[String]("data-evento"). action { cmdParse { case (n,cmd) =>
            Data.fromString(n) match {
              case None => cmd
              case Some(data) =>
                cmd changeMetadado {
                  _ changeId {
                    _ changeVersao {
                      _.map(_.copy(dataEvento = Some(data)))
                    }
                  }
                }
            }
          }
        }.text("data do evento que marca a versão da norma (AAAA-MM-DD)"),
        opt[String]("tipo-evento"). action { cmdParse { case (n,cmd) =>
          cmd changeMetadado {
              _ changeId {
                _ changeVersao {
                  _.map(_.copy(evento = n))
                }
              }
            }
          }
        }.text("tipo do evento que marca a versão da norma, eg. 'leitura'"),
        opt[String]("timestamp-evento"). action { cmdParse { case (n,cmd) =>
            cmd changeMetadado {
              _ changeId {
                _ changeVersao {
                  _.map(_.copy(timestamp = Timestamp.fromString(n)))
                }
              }
            }
          }
        }.text("data e horário do evento que marca a versão da norma (AAAA-MM-DDtHH:MM)"),

        optOverridesRegex("local-data","a expressão do local e data antes da assinatura") {
          case (o,rl) => o copy (overrideRegexLocalData = rl)
        },
        optOverridesRegex("justificativa","o início da seção de Justificativa") {
          case (o,rl) => o copy (overrideRegexJustificativa = rl)
        },
        optOverridesRegex("anexos","o início da seção de Anexos") {
          case (o,rl) => o copy (overrideRegexAnexos = rl)
        },
        optOverridesRegex("legislacao-citada","o início da seção de Legislação Citada") {
          case (o,rl) => o copy (overrideRegexLegislacaoCitada = rl)
        },
        optOverridesRegex("assinatura", "o iníco da assinatura") {
          case (o,rl) => o copy (overrideRegexAssinatura = rl)
        },
        optOverridesRegex("epigrafe","a epígrafe") {
          case (o,rl) => o copy (overrideRegexEpigrafe1 = rl)
        },
        optOverridesRegex("epigrafe-continuacao","a epígrafe") {
          case (o,rl) => o copy (overrideRegexEpigrafe = rl)
        },
        optOverridesRegex("pos-epigrafe","o texto entre a epígrafe e a ementa") {
          case (o,rl) => o copy (overrideRegexPosEpigrafe = rl)
        },
        optOverridesRegex("preambulo","o pré-âmbulo") {
          case (o,rl) => o copy (overrideRegexPreambulo = rl)
        },

        opt[Unit]("prof-epigrafe-obrigatoria"). action { optOverrides { case (_,o) =>
            o.copy(overrideEpigrafeObrigatoria = Some(true))
          }
        }.text("a epígrafe precisa estar presente"),
        opt[Unit]("prof-epigrafe-opcional").action { optOverrides { case (_,o) =>
            o.copy(overrideEpigrafeObrigatoria = Some(false))
          }
        }.text("a epígrafe não precisa estar presente"),
        opt[Unit]("prof-pre-epigrafe-permitida").action { optOverrides { case (_,o) =>
            o.copy(overridePreEpigrafePermitida = Some(true))
          }
        }.text("é permitido texto entre a epígrafe e a ementa"),
        opt[Unit]("prof-epigrafe-nao-permitida"). action { optOverrides { case (_,o) =>
            o.copy(overridePreEpigrafePermitida = Some(false))
          }
        }.text("não é permitido texto entre a epígrafe e a ementa"),
        opt[String]("prof-epigrafe-head"). action { optOverrides { case (x,o) =>
            o.copy(overrideEpigrafeHead = Some(x))
          }
        }.text("texto que precede o número na epígrafe"),
        opt[String]("prof-epigrafe-tail"). action { optOverrides { case (x,o) =>
            o.copy(overrideEpigrafeTail = Some(x))
          }
        }.text("texto que segue o número na epígrafe"),
        opt[File]("linker"). action { cmdParse { case (x,cmd) =>
            cmd copy (linkerPath = Some(x))
          }
        }.text("caminho para o executável do linker. Se omitido o linker não será usado")
      )
    cmd("dumpProfiles")
      .action { (_,opts) =>
        opts.copy(cmd = CmdDumpProfiles)
      }
      .text("mostra os perfis de normas presentes no parser")
    cmd("parseArticulacao")
      .text("faz o parse só da articulacao. A entrada deve ser um documento xml com apenas parágrafos XHTML abaixo do elemento raiz")
      .action { (_,opts) =>
        opts.copy(cmd = CmdParseArticulacao())
      }
      .children (
        opt[File]('i',"input").
          text("arquivo de entrada. Se omitido, será usado a entrada padrão").
          action { cmdParseArticulacao { case (f,cmd) =>
            cmd.copy(input = ST_File(f))
          }
        },
        opt[File]('o',"output").
          text("arquivo de saída. Se omitido, será usado a saída padrão").
          action { cmdParseArticulacao { case (f,cmd) =>
            cmd.copy(output = SK_File(f))
          }
        },
        opt[File]("linker"). action { cmdParseArticulacao { case (x,cmd) =>
            cmd copy (linkerPath = Some(x))
          }
        }.text("caminho para o executável do linker. Se omitido o linker não será usado")
      )
}

case object TFalhaNaRenderizacao extends ProblemType(3001,"Falha na renderização", PC_ErroGeralParser)

final case class ErroNaRenderizacao(ex: Exception) extends 
	ParseProblem(TFalhaNaRenderizacao,
	  Some("Erro durante a renderização do documento em XML: %s" format ErroValidacaoSchema.unchain(ex).mkString("", " => ", ""))
    ) 


object FECmdLine {
  def println(s : String): Unit = System.err.println(s)
  
  def setupLogging(log4jConfigFile : Option[File]): Unit = {
    import org.apache.logging.log4j.core.config._
    import org.apache.logging.log4j.core.config.xml._
    import java.io._

    val cfgStream : InputStream = log4jConfigFile match {
      case None => getClass.getClassLoader.getResourceAsStream("fecmdline.log4j.xml")
      case Some(is) => new BufferedInputStream(new FileInputStream(is))
    }
    val cfgSource = new ConfigurationSource(cfgStream)
    val loggerContext = new LoggerContext("")    
    val conf = new XmlConfiguration(loggerContext, cfgSource)
    println("Calling Configurator")
    Configurator.initialize(conf)
    println("Configurator.initialize finished")
  }
  def main(args : Array[String]): Unit = {
    println("Setting up logging")
    setupLogging(None)
    println("Logging set up")
    val parser = new FECmdLineOptionParser
    parser.parse(args,CmdLineOpts()) foreach { opts =>      
      val verbose = opts.verbose
      //setupLogging(opts.log4jConfigFile)
      
      opts.cmd match {
        case cmd : CmdParse =>      
          val md0 = cmd.metadado
          val urnFragAutoridade = cmd.baseAutoridade.getOrElse(md0.urnFragAutoridade)
          val urnFragTipoNorma = cmd.baseTipoNorma.getOrElse(md0.urnFragTipoNorma)
          val profile0 = DocumentProfileRegister.getProfile(urnFragAutoridade,urnFragTipoNorma).getOrElse(Lei)
          val profile = cmd.overrides match {
            case None => profile0
            case Some(o) => profile0 + o
          }         
          val md = md0.copy(profile = profile)
                
          if(verbose || cmd.showInfo) {
            println("Opções de I/O:")
            println(s"    --mime-type ${cmd.mimeType}")
            cmd.input match {
              case ST_File(f) => println(s"    --input $f")
              case _ => 
            }
            cmd.output match {
              case SK_File(f) => println(s"    --output $f")
              case _ => 
            }
            println("Metadados:")
            println(s"    --tipo-norma ${md.urnFragTipoNorma}")
            println(s"    --autoridade ${md.urnFragAutoridade}")
            println(s"    --localidade ${md.urnFragLocalidade}")
            md.id foreach { id =>
              println(s"    --numero ${id.num}")
              id.complemento.foreach { c =>
                println(s"    --complemento $c")
              }
              id.anoOuData match {
                case Left(ano) => println(s"    --ano $ano")
                case Right(data) => println(s"    --data ${data.urnRepr}")
              }
              id.versao.foreach(v => {
                v.dataEvento foreach { d => println(s"    --data-evento ${d.urnRepr}") }
                println(s"    --tipo-evento ${v.evento}")
                v.timestamp foreach { d =>  println(s"    --timestamp-evento ${d.txt}") }
              })
            }                                                                
            dumpProfile(profile)        
          }
          if(!cmd.showInfo) {
            cmd.linkerPath match {
              case None =>
                if(verbose) {
                  println("linker: skipping")
                }
                sys.props += ("lexml.skiplinker" -> "true")
              case Some(path) =>
                val path1 = path.getCanonicalPath
                if(verbose) {
                  println(s"linker: using linkertool executable at $path1")
                }
                sys.props += ("lexml.linkertool" -> path1)
            }
            process(profile,md,cmd.input,cmd.mimeType,cmd.output,cmd.linkerPath,verbose,cmd.errorOutput)
            Linker.system.terminate()
          }
        case CmdHelp => parser.showUsageAsError()
        case CmdDumpProfiles =>      
          DocumentProfileRegister.profiles.to(IndexedSeq).sortBy(_._1) foreach {
            case (_,p) => dumpProfile(p)
          }  
        case cmd : CmdParseArticulacao => processArticulacao(cmd,verbose)
      }      
    }
  }
     
  def dumpProfile(p : DocumentProfile): Unit = {
    import p._
    println(s"Profile localidade: ${urnFragLocalidade.getOrElse("-")}, autoridade: $urnFragAutoridade, tipoNorma: $urnFragTipoNorma")
    println(s"   --prof-regex-preambulo '${regexPosEpigrafe.mkString("%")}'")
    println(s"   --prof-regex-epigrafe '${regexEpigrafe1.mkString("%")}'")
    println(s"   --prof-regex-epigrafe-continuacao '${regexEpigrafe.mkString("%")}'")
    println(s"   --prof-regex-pos-epigrafe '${regexPosEpigrafe.mkString("%")}'")      
    println(s"   --prof-regex-assinatura '${regexAssinatura.mkString("%")}'")
    println(s"   --prof-regex-anexos '${regexAnexos.mkString("%")}'")
    println(s"   --prof-regex-legislacao-citada '${regexLegislacaoCitada.mkString("%")}'")
    println(s"   --prof-regex-local-data '${regexLocalData.mkString("%")}'")
    println(s"   --prof-regex-justificativa '${regexJustificativa.mkString("%")}'")
    if(epigrafeObrigatoria) {
      println(s"   --prof-epigrafe-obrigatoria")
    } else {
      println(s"   --prof-epigrafe-opcional")
    }
    if(preEpigrafePermitida) {
      println(s"   --prof-pre-epigrafe-permitida")
    } else {
      println(s"   --prof-pre-epigrafe-nao-permitida")
    }
    println(s"   --prof-epigrafe-head '$epigrafeHead'")
    println(s"   --prof-epigrafe-tail '$epigrafeTail'")
    //FIXME: adicionar suporte a template de epígrafe
    println("")
  }

  
  def renderAndValidaXML(pl : ProjetoLei) : (Elem,List[ParseProblem]) = {
    val pl2 = pl.remakeEpigrafe
    val res = LexmlRenderer.render(pl2)  
    val falhasXML = try { 
      new Validation().validaComSchema(res).toList
    } catch {
      case _ : Exception => List()
    }
    (res,falhasXML)
  }
  
  def process(profile : DocumentProfile, md : Metadado, input : SourceType, mimeType : String, output : SinkType,
              linkerPath : Option[File], verbose : Boolean, errorOutput : ErrorOutput): Unit = {
    XHTMLProcessor.pipelineWithDefaultConverter(input.toByteArray, mimeType) foreach { 
      xhtml =>
        val blocks = Block fromNodes xhtml        
        val (mpl1, falhasValidacao) = new ProjetoLeiParser(profile).fromBlocks(md, blocks)
        //Linker.system.terminate().wait() // Porque parar o linker aqui?
        val (res : Option[Elem],falhasXML : List[ParseProblem]) = mpl1 match {
          case None => (None,List())
          case Some(pl) =>
            try { val (xml,probs) = renderAndValidaXML(pl) ; (Some(xml),probs) } catch {
            case ex : Exception =>
              ex.printStackTrace()
              (None,List(ErroNaRenderizacao(ex)))
          }
        }
        val falhas = falhasValidacao ++ falhasXML
        
        res foreach {
          rootElem =>
            output.write(rootElem.toString.getBytes("utf-8"))
        }
        if (falhas.nonEmpty) {
          errorOutput.writeErrors(falhas)          
        }
    }
  }
  def processArticulacao(cmd : CmdParseArticulacao, verbose : Boolean): Unit = {
    import br.gov.lexml.parser.pl.block._
    import br.gov.lexml.parser.pl.profile._
    import java.io._
    import scala.xml._
    val sourceData = cmd.input.toByteArray
    val sourceElem = XML.load(new ByteArrayInputStream(sourceData))
    val pars = (sourceElem \ "p").to(List)
    val blocks = pars collect { case e : Elem => Paragraph(e.child) }    
    val parser = new ProjetoLeiParser(ProjetoDeLeiDoSenadoNoSenado)
    val contexto = cmd.contexto
    val useLinker = cmd.linkerPath match {
      case Some(f) if f.canExecute =>
        val path = f.getCanonicalPath
        if(verbose) {
          println(s"linker: using linkertool executable at $path")
        }
        sys.props += ("lexml.linkertool" -> path)
        true
      case _ =>        
        if(verbose) {
            println("linker: skipping linker")
        }
        sys.props += ("lexml.skiplinker" -> "true")        
        false
    }
    val articulacao = parser.parseArticulacao(blocks,useLinker,contexto)
    val res = LexmlRenderer.renderArticulacao(articulacao)
    cmd.output.write(res.toString.getBytes("utf-8"))
  }
}
