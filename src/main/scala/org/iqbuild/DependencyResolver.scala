package org.iqbuild

import java.net.URL

class DependencyResolver {
    val mavenCentral = "http://central.maven.org/maven2/"
      
	def resolveDependencies(m:ModuleDescriptor) = {
	  val urls = m.deps.par.map{spec=>
        
        val artifactUrl = mavenCentral + spec.module.group.replaceAllLiterally(".", "/") + "/" + spec.module.name 
        
        def makeUrl(version:String) = artifactUrl + "/" + version + "/" + spec.module.name + "-" + version + ".jar"
        
        val resolution = spec.version match {
          case Some(version)=> spec -> Some(makeUrl(version))
          case None => try{
	          val metadataUrl = artifactUrl + "/maven-metadata.xml"
	          println("GET " + metadataUrl)
	          val xml = scala.xml.XML.load(new URL(metadataUrl).openStream())
	          val latestVersion = (xml \ "versioning" \ "latest").text 
	          spec->Some(makeUrl(latestVersion))
	        }catch{
	          case e:Exception => spec-> None
	        }
        }
        
        resolution
      }
	  urls
	}
}