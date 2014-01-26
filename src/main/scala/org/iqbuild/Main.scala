package org.iqbuild

import scala.io.Source
import java.net.URI
import java.net.URL
import java.io.File

object Main {
  /*
   * 1 - what am I building?
   *    - jar
   *    - war
   * 2 - what are my dependencies?
   * 3 - how do I consume each dependency?
   */
  
    trait BuildMechanism {
      
      def build(m:ModuleDescriptor)
    }
    
    object JarBuild extends BuildMechanism {
      override def build(m:ModuleDescriptor) {
        println("foo");
      }
    }
    
    case class Timestamped[T](lastModified:Long, item:T)
    
    case class ModuleWorkingInfo (descriptor:Timestamped[ModuleDescriptor])
    
    def main(args: Array[String]) {
      
      def findDir(f:File):File = {
        val candidate = new File(f, ".iq")
        if(candidate.exists()){
          candidate
        }else if(f.getParentFile()==null){
          new File(System.getProperty("user.home"), ".iq")
        }else {
          findDir(f.getParentFile())
        }
      }
      
      val iqDir = findDir(new File(System.getProperty("user.dir")))
      println("iqd - starting-up (" + iqDir.getAbsolutePath() + ")")
      
      val buildMechanisms:Map[String, BuildMechanism] = Map("jar"-> JarBuild )  
      
      // read the cache from ~/.iq/data
      // setup command socket <- 'iq' client instances communicate with this
      // enter watch/build loop
      
      val moduleDescriptorFile = new File(args(0))
      val text = Source.fromFile(moduleDescriptorFile).getLines.mkString("\n")
      val m = ModuleDescriptor.parse(text)
      println("reading " + m.id)
      
      val mavenCentral = "http://repo1.maven.org/maven2/"
      val urls = m.deps.par.map{spec=>
        
        val artifactUrl = mavenCentral + spec.module.group.replaceAllLiterally(".", "/") + "/" + spec.module.name 
        
        def makeUrl(version:String) = artifactUrl + "/" + spec.module.name + "-" + version + ".jar"
        
        val resolution = spec.version match {
          case Some(version)=> spec -> Some(makeUrl(version))
          case None => try{
	          val metadataUrl = artifactUrl + "/maven-metadata.xml"
	          val xml = scala.xml.XML.load(new URL(metadataUrl).openStream())
	          val latestVersion = (xml \ "versioning" \ "latest").text 
	          spec->Some(makeUrl(latestVersion))
	        }catch{
	          case e:Exception => spec-> None
	        }
        }
        
        resolution
      }
      
      println(urls.mkString("\n"))
      
      val unresolvableDependencies = urls.filter(!_._2.isDefined)
      
      if(!unresolvableDependencies.isEmpty){
        throw new Exception("Unable to resolve dependencies: " + unresolvableDependencies.map(_._1).mkString("\n"))
      }
      
      
      
      
      buildMechanisms(m.build).build(m)
      
    }
    
}