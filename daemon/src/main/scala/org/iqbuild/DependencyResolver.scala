package org.iqbuild

import java.net.URL
import scala.xml.XML
import scala.xml.Elem
import scala.collection.mutable.ListBuffer
import org.iqbuild.maven._
import org.iqbuild.util.Util

class MavenMetadata(mavenCentral:String, group:String, artifactId:String, cache:HttpFetcher) {
  val metadataUrl = Util.makeArtifactUrl(mavenCentral, group , artifactId ) + "/maven-metadata.xml"
  val file = cache.get(new URL(metadataUrl))
  val xml = scala.xml.XML.load(file.toURL())
  val latestVersion = (xml \ "versioning" \ "latest").text 
}

trait DependencyResolver {
  def resolveDepsFor(spec:DependencySpec):PartiallyResolvedDependency
}

//class InternalDependencyResolver(currentModules:  => Seq[ModuleDescriptor]) extends DependencyResolver {
//	def resolveDependencies(m:ModuleDescriptor):DependencyResolutionResult = {
//	  m.deps
//	}
//}

class MavenDependencyResolver(val cache:HttpFetcher, val mavenCentral:String = "http://central.maven.org/maven2/") extends DependencyResolver {
    
    override def resolveDepsFor(spec:DependencySpec):PartiallyResolvedDependency = {
      
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
        
        val transitives:Seq[DependencySpec] = maybeVersion match {
          case Some(version)=> {
        	  try{
      	      val heirarchy = new PomStuff(ModuleIdAndVersion(spec.module, version), mavenCentral, cache)
        	      
        		  heirarchy.dependencies().map{moduleAndVersion=>
        		    val ModuleIdAndVersion(moduleId, version) = moduleAndVersion
        		    new DependencySpec(moduleId, Some(version))
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
            PartiallyResolvedDependency(url, spec, transitives)
          }
          case None=> throw new Exception("Unable to determine version for " + spec)
        }
    }
      
}