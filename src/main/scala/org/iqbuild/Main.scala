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
    
    
    var cache = new URLCache
    var resolver = new DependencyResolver(cache)
    
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
	  val targetDir = new File(dir, "target")
     
      
      def doBuild(prev:FSNode, fs:FSNode){
          val start = System.currentTimeMillis()
          println("Working on " + fs.path)
          
          val text = Source.fromFile(moduleDescriptorFile).getLines.mkString("\n")
		  val m = ModuleDescriptor.parse(text)
		  println("reading " + m.id)

		  val urls = resolver.resolveDependencies(m)

		  val unresolvableDependencies = urls.filter(_._2.isEmpty)

		  if(!unresolvableDependencies.isEmpty){
			throw new Exception("Unable to resolve dependencies: " + unresolvableDependencies.map(_._1).mkString("\n"))
		  }

		  val deps = urls.toList.flatMap(_._2).map{t=>
			val (spec, url) = t
			val file = cache.get(new URL(url))
			ResolvedDependency(file, spec)
		  }

          buildMechanisms(m.build).build(fs, targetDir, deps, m)
          
          val end = System.currentTimeMillis()
          val seconds = (end - start) /1000.0
          println("Finished - " + seconds + " seconds")
      }
      

      var prev:FSNode = null
	  while(true){
	    
		  val fs = FSNode.forPath(dir, {f => f!=targetDir && !f.getName().startsWith(".")})
		  val deltas = fs.deltas(prev)
		  if(prev==null || !deltas.isEmpty){
			  println(s"Something changed: ${deltas}")
			  println(deltas.map{beforeAndAfter=>
			  	val (before, after) = beforeAndAfter
			  	
			  	val timeDiff = if(!before.lastModified.isEqual(after.lastModified )){
			  	  before.lastModified + " vs " + after.lastModified
			  	}else ""
			  	
			  	val fileDiff = if(before.isFile!=after.isFile ){
			  	  before.isFile  + " vs " + after.isFile
			  	}else ""
			  	
			  	before.path + " (" + timeDiff + fileDiff + ")"
			  }.mkString("    ", "\n    ", "\n"))

			  try{
				  doBuild(prev, fs)
			  }catch{
			  	case e:Throwable=>e.printStackTrace()
			  }

		  }
		  prev = fs
		  Thread.sleep(500)
	  }
      
    }
}