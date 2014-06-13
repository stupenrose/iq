package org.iqbuild

import scala.io.Source
import java.net.URI
import java.net.URL
import java.io.File
import java.io.InputStream
import java.io.OutputStream
import org.apache.commons.io.IOUtils
import scala.collection.JavaConversions._
import org.apache.commons.httpclient.HttpClient
import org.apache.commons.httpclient.methods.GetMethod
import java.io.FileOutputStream
import org.apache.commons.io.FileUtils
import org.joda.time.Instant

object Main {
  /*
   * 1 - what am I building?
   *    - jar
   *    - war
   * 2 - what are my dependencies?
   * 3 - how do I consume each dependency?
   */
  
    case class Timestamped[T](lastModified:Long, item:T)
    
    case class ModuleWorkingInfo (descriptor:Timestamped[ModuleDescriptor])
    
    
    var resolver = new DependencyResolver()
    var cache = new URLCache
    
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
      val dir = moduleDescriptorFile.getParentFile() 
      val text = Source.fromFile(moduleDescriptorFile).getLines.mkString("\n")
      val m = ModuleDescriptor.parse(text)
      println("reading " + m.id)
      
      val urls = resolver.resolveDependencies(m)
      
      println(urls.mkString("\n"))
      
      val unresolvableDependencies = urls.filter(!_._2.isDefined)
      
      if(!unresolvableDependencies.isEmpty){
        throw new Exception("Unable to resolve dependencies: " + unresolvableDependencies.map(_._1).mkString("\n"))
      }
      
      
      val deps = urls.toList.map{t=>
        val (spec, maybeUrl) = t
        val file = cache.get(new URL(maybeUrl.get))
        ResolvedDependency(file, spec)
      }

      var prev:FSNode = null
	  while(true){
		  val targetDir = new File(dir, "target")
		  val fs = FSNode.forPath(dir, {f => f!=targetDir && !f.getName().startsWith(".")})
		  if(prev==null || fs!=prev){
			  println("Something changed")
			  try{
				buildMechanisms(m.build).build(fs, targetDir, deps, m)
			  }catch{
			  	case e:Throwable=>e.printStackTrace()
			  }
		  }
		  prev = fs
		  Thread.sleep(500)
	  }
      
    }
}