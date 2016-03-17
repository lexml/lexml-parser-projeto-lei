package br.gov.lexml.parser.pl.mime

import com.typesafe.config.Config

import com.typesafe.config.ConfigFactory
import br.gov.lexml.parser.pl.config.MainConfig
import scala.collection.JavaConversions._

final case class MimeInfo (  
  extensions : Seq[String] = Seq()  
)

trait MimeRegistry {
  def mimeTypes : Set[String]
  def apply(mimeType : String) : MimeInfo 
}

object MimeRegistry extends MimeRegistry {
        
    var mimeTypeMap : Map[String,MimeInfo] = Map()
    override def mimeTypes = mimeTypeMap.keySet
    
    def apply(mimeType : String) : MimeInfo = {
      mimeTypeMap.getOrElse(mimeType,MimeInfo())
    }
    def addMimeExtensions(mimeType : String, exts : Seq[String]) = {
      val curMimeInfo = this(mimeType)         
      mimeTypeMap = mimeTypeMap + (mimeType -> curMimeInfo.copy(extensions = exts))
    }
    def addMimeInfo(conf : Config) {
      import scala.collection.JavaConversions._
      val mimeType = conf.getString("code")
      val extensions = conf.getStringList("extensions")
      addMimeExtensions(mimeType,extensions)
    }
    val conf = MainConfig.config.getConfig("mime-registry")
    //println(s"conf=${conf}")
            
    val mimeTypeConfs = conf.getConfigList("mime-types")
    //println(s"mimeTypeConfs=${mimeTypeConfs}")
    
    mimeTypeConfs.foreach(addMimeInfo)
    
    //println(s"mimeTypeMap: ${mimeTypeMap}") 
}
