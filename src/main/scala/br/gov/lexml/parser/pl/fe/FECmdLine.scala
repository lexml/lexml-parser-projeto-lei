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

abstract sealed class SourceType

case object ST_Stdin extends SourceType

case class ST_File(f : File) extends SourceType

abstract sealed class SinkType

case object SK_Stdout extends SinkType

case class SK_File(f : File) extends SinkType

abstract sealed class Cmd

case object CmdHelp extends Cmd

case class CmdParse(    
    metadado : Metadado = Metadado(Lei,hashFonte = None),
    mimeType : String = "text/plain",
    input : SourceType = ST_Stdin,
    output : SinkType = SK_Stdout,
    linkerPath : Option[File] = None,
    overrides : Option[OverridesData] = None,
    showInfo : Boolean = false) extends Cmd {
  def changeOverrides(f : OverridesData => OverridesData) = {
    val m = overrides.getOrElse(OverridesData())
    copy(overrides = Some(f(m)))
  }
  def changeMetadado(f : Metadado => Metadado) = 
    copy(metadado = f(metadado))
}
    
case object CmdDumpProfiles extends Cmd    
    
case class CmdLineOpts(
    cmd : Cmd = CmdHelp,    
    verbose : Boolean = false    
    ) {
  
  def command[C <: Cmd : ClassTag](dv : => C)(f : C => C) : CmdLineOpts = {    
    val ct = implicitly[ClassTag[C]]
    cmd match {
      case ct(c) => copy (cmd = f(c))
      case _ => this
    }    
  }    
}

class FECmdLineOptionParser extends scopt.OptionParser[CmdLineOpts]("parser") {
    head("parser","1.9.11-SNAPSHOT")
    def cmdParse[T](f : (T,CmdParse) => CmdParse) : (T,CmdLineOpts) => CmdLineOpts = {
      case (v, opts) => opts.command(CmdParse())(c => f(v,c))        
    }
    opt[String]('m',"mime-type").
      text("tipo MIME do arquivo de entrada, ex.: text/plain (default)," + 
           " application/vnd.openxmlformats-officedocument.wordprocessingml.document, " +
           "... "). 
      action { cmdParse { case (mt,cmd) =>
        cmd copy (mimeType = mt)
      }
    }
    opt[File]('i',"input").
      text("arquivo de entrada. Se omitido, será usado a entrada padrão").
      action { cmdParse { case (f,cmd) =>
        cmd.copy(input = ST_File(f))
      }
    }
    opt[File]('o',"output").
      text("arquivo de saída. Se omitido, será usado a saída padrão").
      action { cmdParse { case (f,cmd) =>
        cmd.copy(output = SK_File(f))
      }
    }
    help("help") text("Mostra esse texto")
    def stringToRegexList(txt : String) =
      txt.split('%').to[List] match {
        case Nil => None
        case l => Some(l.map(_.r))      
      }
    
    opt[Unit]('v',"verbose"). action {
      (_,opts) => opts copy (verbose = true)
    }.text("Mostra informações durante o parse.")
    opt[Unit]("info"). action { cmdParse { case (_,cmd) =>
        cmd copy (showInfo = true)
      }
    }.text("Mostra os parâmetros usados no parse e sai sem executar o parse")
    opt[String]('t',"tipo-norma").action { cmdParse { case (n,cmd) =>
        cmd changeMetadado { 
          _.copy(tipoNorma = Some(n))
        }
      }
    }.text("tipo de norma, na sintaxe usada na URN do LexML, eg. 'lei'")    
    opt[String]('a',"autoridade"). action { cmdParse { case (n,cmd) =>
        cmd changeMetadado {
          _.copy(autoridade = Some(n))
        }
      }
    }.text("autoridade, na sintaxe usada na URN do LexML, eg. 'federal'")
    opt[String]('l',"localidade"). action { cmdParse { case (n,cmd) =>
        cmd changeMetadado {
          _.copy(localidade = Some(n))
        }
      }
    }.text("localidade, na sintaxe usada na URN do LexML, eg. 'br'")
    opt[Int]('n',"numero").valueName("número da norma"). action { cmdParse { case (n,cmd) =>
        cmd changeMetadado {
          _ changeId {
            _.copy(num = n)
          }
        }
      }
    }.text("número da norma")
    opt[Int]("complemento").valueName("complemento do número da norma"). action { cmdParse { case (n,cmd) =>
        cmd changeMetadado {
          _ changeId {          
            _.copy(complemento = Some(n)) //(num, complemento, anoOuData, versao)
          }
        }
      }
    }.text("complemento do número da norma")
    opt[Int]("ano"). action { cmdParse { case (n,cmd) =>
        cmd changeMetadado {
          _ changeId {          
            _.copy(anoOuData = Left(n)) //(num, complemento, anoOuData, versao)
          }
        }
      }
    }.text("ano da publicação da norma (quando o mês e dia são desconhecidos)")
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
    }.text("data completa da publicação da norma (AAAA-MM-DD)")
    opt[String]("data-evento"). action { cmdParse { case (n,cmd) =>      
        Data.fromString(n) match {
          case None => cmd
          case Some(data) =>
            cmd changeMetadado {
              _ changeId {                    
                _ changeVersao {
                  _.copy(dataEvento = Some(data)) 
                }
              }
            }
        }
      }
    }.text("data do evento que marca a versão da norma (AAAA-MM-DD)")
    opt[String]("tipo-evento"). action { cmdParse { case (n,cmd) =>
      cmd changeMetadado {
          _ changeId {                    
            _ changeVersao {
              _.copy(evento = n) 
            }
          }
        }
      }
    }.text("tipo do evento que marca a versão da norma, eg. 'leitura'")
    opt[String]("timestamp-evento"). action { cmdParse { case (n,cmd) =>      
        cmd changeMetadado {
          _ changeId {                    
            _ changeVersao {
              _.copy(timestamp = Timestamp.fromString(n)) 
            }
          }
        }
      }
    }.text("data e horário do evento que marca a versão da norma (AAAA-MM-DDtHH:MM)")
    
    def optOverrides[T](f : (T,OverridesData) => OverridesData) : (T,CmdLineOpts) => CmdLineOpts = 
      cmdParse {
        case (v,cmd) => 
          cmd changeOverrides { f(v,_) }                        
      } 
    
    
    def optOverridesRegex(suffix : String,desc : String)(f : (OverridesData,Option[List[Regex]]) => OverridesData) {      
      opt[String](s"prof-regex-$suffix"). valueName("regex%regex%..."). action { optOverrides { 
          case (rl,o) => f(o,stringToRegexList(rl))                          
        }
      }.text("expressões regulares, separadas pelo caracter '%', que são usada para identificar " + desc)
    } 
        
    optOverridesRegex("local-data","a expressão do local e data antes da assinatura") { 
      case (o,rl) => o copy (overrideRegexLocalData = rl)  
    }
    optOverridesRegex("justificativa","o início da seção de Justificativa") {
      case (o,rl) => o copy (overrideRegexJustificativa = rl)
    }
    optOverridesRegex("anexos","o início da seção de Anexos") {
      case (o,rl) => o copy (overrideRegexAnexos = rl)
    }
    optOverridesRegex("legislacao-citada","o início da seção de Legislação Citada") {
      case (o,rl) => o copy (overrideRegexLegislacaoCitada = rl)
    }
    optOverridesRegex("assinatura", "o iníco da assinatura") {
      case (o,rl) => o copy (overrideRegexAssinatura = rl)
    }
    optOverridesRegex("epigrafe","a epígrafe") {
      case (o,rl) => o copy (overrideRegexEpigrafe = rl)
    }
    optOverridesRegex("pos-epigrafe","o texto entre a epígrafe e a ementa") {
      case (o,rl) => o copy (overrideRegexPosEpigrafe = rl)
    }
    optOverridesRegex("preambulo","o pré-âmbulo") {
      case (o,rl) => o copy (overrideRegexPreambulo = rl)
    }

    opt[Unit]("prof-epigrafe-obrigatoria"). action { optOverrides { case (_,o) =>      
        o.copy(overrideEpigrafeObrigatoria = Some(true))
      }
    }.text("a epígrafe precisa estar presente")
    opt[Unit]("prof-epigrafe-opcional").action { optOverrides { case (_,o) =>      
        o.copy(overrideEpigrafeObrigatoria = Some(false))
      }      
    }.text("a epígrafe não precisa estar presente")
    opt[Unit]("prof-pre-epigrafe-permitida").action { optOverrides { case (_,o) =>
        o.copy(overridePreEpigrafePermitida = Some(true))
      }
    }.text("é permitido texto entre a epígrafe e a ementa")
    opt[Unit]("prof-epigrafe-nao-permitida"). action { optOverrides { case (_,o) =>
        o.copy(overridePreEpigrafePermitida = Some(false))
      }
    }.text("não é permitido texto entre a epígrafe e a ementa")
    opt[String]("prof-epigrafe-head"). action { optOverrides { case (x,o) =>
        o.copy(overrideEpigrafeHead = Some(x))
      }
    }.text("texto que precede o número na epígrafe")
    opt[String]("prof-epigrafe-tail"). action { optOverrides { case (x,o) =>
        o.copy(overrideEpigrafeTail = Some(x))
      }
    }.text("texto que segue o número na epígrafe")
    opt[File]("linker"). action { cmdParse { case (x,cmd) =>
        cmd copy (linkerPath = Some(x))
      }
    }.text("caminho para o executável do linker")
    opt[Unit]("profiles"). action { 
      case (_,opt) => opt copy (cmd = CmdDumpProfiles)
    }.text("mostra os perfis de normas presentes no parser")
}

object FECmdLine {
  def println(s : String) = System.err.println(s)
  def main(args : Array[String]) = {
    val parser = new FECmdLineOptionParser
    parser.parse(args,CmdLineOpts()) foreach { opts =>
      val verbose = opts.verbose
      opts.cmd match {
        case cmd : CmdParse =>      
          val md = cmd.metadado
          val profile0 = DocumentProfileRegister.getProfile(md.urnFragAutoridade,md.urnFragTipoNorma).getOrElse(Lei)
          val profile = cmd.overrides match {
            case None => profile0
            case Some(o) => profile0 + o
          }         
                
          if(verbose || cmd.showInfo) {
            println("Opções de I/O:")
            println(s"    --mime-type ${cmd.mimeType}")
            cmd.input match {
              case ST_File(f) => println(s"    --input ${f}")
              case _ => 
            }
            cmd.output match {
              case SK_File(f) => println(s"    --output ${f}")
              case _ => 
            }
            println("Metadados:")
            println(s"    --tipo-norma ${md.urnFragTipoNorma}")
            println(s"    --autoridade ${md.urnFragAutoridade}")
            println(s"    --localidade ${md.urnFragLocalidade}")
            md.id foreach { id =>
              println(s"    --numero ${id.num}")
              id.complemento.foreach { c =>
                println(s"    --complemento ${c}")
              }
              id.anoOuData match {
                case Left(ano) => println(s"    --ano ${ano}")
                case Right(data) => println(s"    --data ${data.urnRepr}")
              }
              id.versao.dataEvento foreach { d => println(s"    --data-evento ${d.urnRepr}") }
              println(s"    --tipo-evento ${id.versao.evento}")
              id.versao.timestamp foreach { d =>  println(s"    --timestamp-evento ${d.txt}") }
            }                                                                
            dumpProfile(profile)        
          }
          if(!cmd.showInfo) {
            process(profile,md,cmd.input,cmd.output,cmd.linkerPath,verbose)
          }        
        case CmdHelp => parser.showUsage()
        case CmdDumpProfiles =>      
          val l = DocumentProfileRegister.profiles.to[IndexedSeq].sortBy(_._1) foreach {
            case (_,p) => dumpProfile(p)
          }          
      }      
    }
  }
     
  def dumpProfile(p : DocumentProfile) = {    
    import p._
    println(s"Profile localidade: ${urnFragLocalidade.getOrElse("-")}, autoridade: ${urnFragAutoridade}, tipoNorma: ${urnFragTipoNorma}")
    println(s"   --prof-regex-preambulo '${regexPosEpigrafe.mkString("%")}'")
    println(s"   --prof-regex-epigrafe '${regexEpigrafe.mkString("%")}'")
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
    println(s"   --prof-epigrafe-head '${epigrafeHead}'")
    println(s"   --prof-epigrafe-tail '${epigrafeTail}'")
    println("")
  }
  
  def process(prof : DocumentProfile, md : Metadado, input : SourceType, output : SinkType, linkerPath : Option[File], verbose : Boolean) {
    
  }
}