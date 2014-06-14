package org.iqbuild

import java.net.URL
import scala.xml.XML
import scala.xml.Elem
import scala.collection.mutable.ListBuffer
import org.iqbuild.maven._

class MavenMetadata(mavenCentral:String, group:String, artifactId:String, cache:HttpFetcher) {
  val metadataUrl = Util.makeArtifactUrl(mavenCentral, group , artifactId ) + "/maven-metadata.xml"
  val file = cache.get(new URL(metadataUrl))
  val xml = scala.xml.XML.load(file.toURL())
  val latestVersion = (xml \ "versioning" \ "latest").text 
}

class DependencyResolver(val cache:HttpFetcher, val mavenCentral:String = "http://central.maven.org/maven2/") {
    
    private def resolveDepsFor(spec:DependencySpec):ResolvedDependency = {
      
        val artifactUrl = Util.makeArtifactUrl(mavenCentral, spec.module .group , spec.module .name )
        def makeUrl(version:String) = artifactUrl + "/" + version + "/" + spec.module.name + "-" + version + ".jar"

        val maybeVersion = spec.version match {
          case Some(version)=> Some(version)
          case None => try{
	          val metadata = new MavenMetadata(mavenCentral, spec.module.group , spec.module.name, cache)
	          Some(metadata.latestVersion)
	        }catch{
	          case e:Exception => None
	        }
        }
        
        val resolution = maybeVersion match {
          case Some(version)=>Some(spec.copy(version=maybeVersion)-> makeUrl(version))
          case None => None
        }
        
        val transitives:Seq[ResolvedDependency] = maybeVersion match {
          case Some(version)=> {
        	  try{
        	      val heirarchy = new PomStuff(ModuleIdAndVersion(spec.module, version), mavenCentral, cache)
        	      
        		  heirarchy.dependencies.map{moduleAndVersion=>
        		    val ModuleIdAndVersion(moduleId, version) = moduleAndVersion
        		    val spec = new DependencySpec(moduleId, Some(version))
        		  	resolveDepsFor(spec)
        	      }
        	  }catch{
        	    case e:Throwable => throw new RuntimeException("Error fetching dependencies for " + spec.module + " " + version, e)
        	  }
          }
          case None => List()
        }
        
        resolution match {
          case Some(r)=>{
            val (spec, url) = r
            ResolvedDependency(url, spec, transitives)
          }
          case None=> throw new Exception("Unable to determine version for " + spec)
        }
    }
      
	def resolveDependencies(m:ModuleDescriptor):DependencyResolutionResult = {
	  DependencyResolutionResult(m.deps.map{spec=>resolveDepsFor(spec)})
	}
}