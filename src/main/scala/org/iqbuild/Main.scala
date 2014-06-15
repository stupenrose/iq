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
    
    
    val cache = new URLCache
    val resolver = new DependencyResolver(cache)
    
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
     
      
      def doBuild(maybePrev:Option[FSNode], fs:FSNode){
    
        val text = Source.fromFile(moduleDescriptorFile).getLines.mkString("\n")
        val m = ModuleDescriptor.parse(text)
        val label = m.id
        
        val dependencyTree = time("Resolving dependencies for " + label){
		  
		  
		  resolver.resolveDependencies(m)
        }
        
        val dependencies = time("processing dependency tree"){
          dependencyTree.flatten
        }
        
        time("building " + label){
        	val buildMechanism = buildMechanisms(m.build)
        			
        	
			buildMechanism.build(fs, targetDir, dependencies, m)
        }
          
      }
      
      val pollingCache = new File(targetDir, "fs.json")

	  while(true){
	    
		  val maybePrev = if (pollingCache.exists()) Some(Jackson.jackson.readValue(pollingCache, classOf[FSNode])) else None 
		  val fs = FSNode.forPath(dir, {f => f!=targetDir && !f.getName().startsWith(".")})
		  val deltas = maybePrev match {
		    case Some(prev) => fs.deltas(prev)
		    case None => Seq()
		  }
		  
		  val needsBuild = (maybePrev, deltas) match {
		    case (None, _) => true
		    case (_, deltas) if !deltas.isEmpty => {
		      println(s"Something changed:")
			  println(deltas.map{beforeAndAfter=>
			  	val (before, after) = beforeAndAfter
			  	
			  	val timeDiff = if(before.lastModified != after.lastModified ){
			  	  before.lastModified + " vs " + after.lastModified
			  	}else ""
			  	
			  	val fileDiff = if(before.isFile!=after.isFile ){
			  	  before.isFile  + " vs " + after.isFile
			  	}else ""
			  	
			  	before.path + " (" + timeDiff + fileDiff + ")"
			  }.mkString("    ", "\n    ", "\n"))
		      true
		    }
		    case _ => false
		  }
		  if(needsBuild){
			  try{
				  doBuild(maybePrev, fs)
			  }catch{
			  	case e:Throwable=>e.printStackTrace()
			  }

		  }

		  Jackson.jackson.writerWithDefaultPrettyPrinter().writeValue(pollingCache, fs);		  
		  Thread.sleep(500)
	  }
      
    }
    
    private def time[T](name:String)(fn: =>T):T = {
        val start = System.currentTimeMillis()
        println(name)
        val t = fn
        val end = System.currentTimeMillis()
        val seconds = (end - start) /1000.0
        println("Finished - " + seconds + " seconds")
        t
    }
      
}