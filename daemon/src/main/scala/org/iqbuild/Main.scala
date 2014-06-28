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
import org.httpobjects.jetty.HttpObjectsJettyHandler
import org.httpobjects.HttpObject
import org.httpobjects.Request
import org.httpobjects.DSL._
import org.httpobjects.util.HttpObjectUtil
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.io.FileInputStream
import org.httpobjects.Representation
import scala.collection.generic.MutableMapFactory
import scala.collection.mutable.ListBuffer

object Main {
    val cache = new URLCache
    val resolver = new DependencyResolver(cache)
    
    case class ModuleListItem (path:String, id:String)
    
    def main(whatever: Array[String]) {
      
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
      
      val dataFilePath = new File(iqDir, "data.json")
      var data = if(dataFilePath.exists()){
        Jackson.parseJson[Data](dataFilePath)
      }else{
        Data()
      }
      
      val logPath = new File(iqDir, "log")
      val bytesOut = new FileOutputStream(logPath)
      val out = new PrintStream(bytesOut)
      
      val buildMechanisms:Map[String, BuildMechanism] = Map("jar"-> JarBuild )  
      
      def newBuildHandler(moduleDescriptorPath:String) = new BuildHandler(moduleDescriptorPath, buildMechanisms, out)
      
      var handlers = (data.moduleDescriptors.map{moduleDescriptorPath=>        
        val text = Source.fromFile(moduleDescriptorPath).getLines.mkString("\n")
        val m = ModuleDescriptor.parse(text)
        val handler = newBuildHandler(moduleDescriptorPath)
        
        handler
      })
      
      def handlersByPath(path:String) = handlers.find(_.moduleDescriptorPath == path)
      
      handlers.foreach(_.start())
      
      HttpObjectsJettyHandler.launchServer(33421, 
          new HttpObject("/"){
    	  	override def get(req:Request) = OK(Html(
    	  	    <html>
    	  		  <body>
    	  				<h1>iqbuild [{iqDir.getAbsolutePath}]</h1>
    	  				{data.moduleDescriptors.map{f=>
    	  				  	<div>{f}</div>
    	  				}}
    	  		  </body>
    	  	    </html>.toString
    	  	))
          },
          new HttpObject("/modules"){
    	  	override def get(req:Request) = {
    	  	  val matchingHandlers = req.query().valueFor("path") match {
    	  	    case null=>handlers
    	  	    case p:String=>handlers.filter(_.moduleDescriptorPath ==p)
    	  	  }
    	  	  
    	  	  val results = matchingHandlers.map{handler=>
    	  	    val id = handler.maybeDescriptor match {
    	  	      case None=>None
    	  	      case Some(descriptor)=>Some(descriptor.id.toString) 
    	  	    }
    	  	    ModuleListItem(
    	  	        handler.moduleDescriptorPath, id.getOrElse(null))
    	  	  }
    	  	  
    	  	  
    	  	  OK(Json(Jackson.jackson.writerWithDefaultPrettyPrinter().writeValueAsString(results)))
    	  	}
    	  	override def post(req:Request) = {
    	  	  val path = HttpObjectUtil.toAscii(req.representation())
    	  	  val p = new File(path)
    	  	  
    	  	  val maybeDescriptor = try {
	  	        val text = Source.fromFile(p).getLines.mkString("\n")
	  	        Some(ModuleDescriptor.parse(text))
	  	      }catch{
	  	        case e:Throwable => None
	  	      }
    	  	  
	  	      maybeDescriptor match {
	  	        case None => BAD_REQUEST(Text("Unable to read/parse descriptor at" + p.getAbsolutePath()))
	  	        case Some(d) => {
	    	  	  data = data.copy(moduleDescriptors = data.moduleDescriptors.toList :+ path)
	    	  	  Jackson.jackson .writeValue(dataFilePath, data)
	    	  	  val handler = newBuildHandler(path)
	    	  	  handlers = handlers :+ handler
	    	  	  handler.start()
	    	  	  OK(Text("Added " + d.id))
	  	        }
	  	      }
    	  	}
          },
          new HttpObject("/modules/{moduleId}"){
    	  	override def get(req:Request) = {
    	  	  val id = req.path().valueFor("moduleId")
    	  	  val maybeDescriptor = handlers.find(_.maybeDescriptor.get.id.toString == id)
    	  	  
    	  	  maybeDescriptor match {
    	  	    case None=>NOT_FOUND
    	  	    case Some(descriptor)=>OK(Json(Jackson.jackson .writerWithDefaultPrettyPrinter().writeValueAsString(descriptor)))
    	  	  }
    	  	}
    	  	override def delete(req:Request) = {
    	  	  val idString = req.path().valueFor("moduleId")
    	  	  val id = {
    	  	    val parts = idString.split(":")
    	  	    ModuleId(group = parts(0), name=parts(1))
    	  	  }
    	  	  
    	  	  handlers.filter(_.maybeDescriptor .isDefined).find(_.maybeDescriptor.get.id == id) match {
    	  	    case None=>NOT_FOUND
    	  	    case Some(handler)=>{
    	  	      handlers = handlers.filter(_!=handler)
    	  	      data = data.copy(moduleDescriptors = data.moduleDescriptors.filter(_!=handler.moduleDescriptorPath ))
	    	  	  Jackson.jackson .writeValue(dataFilePath, data)
    	  	      
    	  	      OK(Text("Deleted"))
    	  	    }
    	  	  }
    	  	}
    	  	
    	  },
          new HttpObject("/nextBuild"){
            override def get(req:Request) = {
            	Main.synchronized(Main.wait())
            	OK(Text("done"))
            }
          },
          new HttpObject("/log"){
            override def get(req:Request) = {
              OK(new Representation(){
                override def contentType = "text/plain"
                override def write(out:OutputStream) {
                  val in = new FileInputStream(logPath)
                  
                  while(true){
                    val x = in.read()
                    x match {
                      case -1 => Thread.sleep(100)
                      case _=> {
                        out.write(x)
                        out.flush()
                      }
                    }
                  }
                }
              })
            }
          }
      )
    }
    
    
    
  class BuildHandler(val moduleDescriptorPath:String, buildMechanisms:Map[String, BuildMechanism], out:PrintStream) extends Thread {
    
    var maybeDescriptor :Option[ModuleDescriptor] = None
    
    override def run = {
      println("Found module: " + moduleDescriptorPath)
      val moduleDescriptorFile = new File(moduleDescriptorPath)
      val dir = moduleDescriptorFile.getParentFile() 
	  val targetDir = new File(dir, "target")
     
      
      
      def doBuild(maybePrev:Option[FSNode], fs:FSNode){
    
        val text = Source.fromFile(moduleDescriptorFile).getLines.mkString("\n")
        val m = ModuleDescriptor.parse(text)
        val label = m.id
        
        maybeDescriptor = Some(m)
        
        val dependencyTree = time("Resolving dependencies for " + label, out){
		  resolver.resolveDependencies(m)
        }
        
        val dependencies = time("processing dependency tree", out){
          dependencyTree.flatten
        }
        
        time("building " + label, out){
        	val buildMechanism = buildMechanisms(m.build)
        			
        	
			buildMechanism.build(fs, targetDir, dependencies, m, out)
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
		      out.println(s"Something changed:")
			  out.println(deltas.map{beforeAndAfter=>
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
			  Main.synchronized(Main.notifyAll)
		  }

		  Jackson.jackson.writerWithDefaultPrettyPrinter().writeValue(pollingCache, fs);	
		  
		  Thread.sleep(500)
	  }
      
    }
  }
    
    private def time[T](name:String, out:PrintStream)(fn: =>T):T = {
        val start = System.currentTimeMillis()
        out.println(name)
        val t = fn
        val end = System.currentTimeMillis()
        val seconds = (end - start) /1000.0
        out.println("Finished - " + seconds + " seconds")
        t
    }
      
}