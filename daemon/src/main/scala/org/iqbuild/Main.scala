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
      
      case class ModuleStatus(descriptorPath:String, maybeDescriptor:Option[ModuleDescriptor])
      
      case class BuildResult(somethingChanged:Boolean, modulesStatus:Seq[ModuleStatus])
      
      case class FilesystemChanges(descriptorPath:String, maybePrev:Option[FSNode], currentState:FSNode, deltas:Seq[(FSNode, FSNode)]) {
        def needsBuild = (maybePrev, deltas) match {
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
      }
                
      case class Paths(val descriptorPath:String){
        val moduleDescriptorFile = new File(descriptorPath)
	      val dir = moduleDescriptorFile.getParentFile() 
		    val targetDir = new File(dir, "target")
        val pollingCache = new File(targetDir, "fs.json")
        pollingCache.getParentFile.mkdirs()
      }
      
      def buildAsNeeded(moduleDescriptors:Seq[String], prevModulesState:Seq[ModuleStatus]):BuildResult = {
        val fsChanges = moduleDescriptors.map{descriptorPath=>
          val paths = Paths(descriptorPath)
          val maybePrev = if (paths.pollingCache.exists()) Some(Jackson.jackson.readValue(paths.pollingCache, classOf[FSNode])) else None 
		      val fs = FSNode.forPath(paths.dir, {f => f!=paths.targetDir && !f.getName().startsWith(".")})
  			  val deltas = maybePrev match {
  			    case Some(prev) => fs.deltas(prev)
  			    case None => Seq()
  			  }
		      Jackson.jackson.writerWithDefaultPrettyPrinter().writeValue(paths.pollingCache, fs);
		      FilesystemChanges(descriptorPath, maybePrev, fs, deltas)
        }
        
        def doBuild(descriptorPath:String, maybePrev:Option[FSNode], fs:FSNode):ModuleStatus = {
          val paths = Paths(descriptorPath)
    
	        val text = Source.fromFile(paths.moduleDescriptorFile).getLines.mkString("\n")
	        val m = ModuleDescriptor.parse(text)
	        val label = m.id
	        
	        val dependencyTree = time("Resolving dependencies for " + label, out){
			      resolver.resolveDependencies(m)
	        }
	        
	        val dependencies = time("processing dependency tree", out){
	          dependencyTree.flatten
	        }
	        
	        time("building " + label, out){
	        	val buildMechanism = buildMechanisms(m.build)
    				buildMechanism.build(fs, paths.targetDir, dependencies, m, out)
	        }
			
          ModuleStatus(descriptorPath, maybeDescriptor=Some(m))
	       
	      }
          
		    val results = fsChanges.map{changes=>
          val prevState = prevModulesState.find(_.descriptorPath ==changes.descriptorPath)
		          
				  val nextState = if(prevState.isDefined && !changes.needsBuild) {
				    (false, prevState.get)
				  }else{
					  (true, doBuild(changes.descriptorPath, changes.maybePrev, changes.currentState))
				  }
				  
				  nextState
				  
				  
              // STEP 1: parse all the module descriptors, if able
//		      case class PathState(val path:String, val maybeDescriptor:Option[ModuleDescriptor])
//		      
//		      val paths = data.moduleDescriptors.map{descriptorPath=>
//		        val maybeDescriptor = try{
//		          val descriptorFile = new File(descriptorPath)
//		          val text = Source.fromFile(descriptorFile).getLines.mkString("\n")
//		          Some(ModuleDescriptor.parse(text))
//		        }catch{
//		          case e:Exception => {
//		            System.err.println("Error reading " + descriptorPath)
//		            e.printStackTrace(System.err)
//		            None
//		          }
//		        }
//		        PathState(descriptorPath, maybeDescriptor)
//		      }
		      
		      
		      // STEP 2: resolve all the module dependencies
		         // if
		      
		      // STEP 3: verify no 'bad' cycles (dependency cycles that will result in build cycles)
		      
		      // STEP 4: determine module build order
		      
		      // STEP 5: for each moduleInBuildOrder, build
		//          val result = build(path, moduleDescriptor, prevModuleBuildState, filesystemChanges, dependencyChanges)
		//          val newModuleState = result.buildState
		//          val errors = result.errors
		      
		      // don't add new paths that are currently invalid?
		      
		      // for each existing path
		      }
          BuildResult(
              somethingChanged=results.exists(_._1 ), 
              modulesStatus = results.map(_._2 ))
      }
      
      var modulesStatus = Seq[ModuleStatus]()
      
      new Thread(){
        override def run = while(true){
          val oldModulesStatus = modulesStatus
          val result = buildAsNeeded(data.moduleDescriptors, modulesStatus)
          modulesStatus = result.modulesStatus 
          if(result.somethingChanged){
              println("notifying of changes")
		        Main.synchronized(Main.notifyAll)
          }
	        Thread.sleep(200)
        }
      }.start();
      
      
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
      	  	  val matchingModules = req.query().valueFor("path") match {
      	  	    case null=>modulesStatus
      	  	    case p:String=>modulesStatus.filter(_.descriptorPath  ==p)
      	  	  }
      	  	  
      	  	  val results = matchingModules.map{handler=>
      	  	    val id = handler.maybeDescriptor match {
      	  	      case None=>None
      	  	      case Some(descriptor)=>Some(descriptor.id.toString) 
      	  	    }
      	  	    ModuleListItem(handler.descriptorPath, id.getOrElse(null))
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
    	    	  	  OK(Text("Added " + d.id))
  	  	        }
  	  	      }
      	  	}
          },
          new HttpObject("/modules/{moduleId}"){
      	  	override def get(req:Request) = {
      	  	  val id = req.path().valueFor("moduleId")
      	  	  val maybeDescriptor = modulesStatus.find(_.maybeDescriptor.get.id.toString == id)
      	  	  
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
      	  	  
      	  	  modulesStatus.filter(_.maybeDescriptor .isDefined).find(_.maybeDescriptor.get.id == id) match {
      	  	    case None=>NOT_FOUND
      	  	    case Some(handler)=>{
      	  	      data = data.copy(moduleDescriptors = data.moduleDescriptors.filter(_!=handler.descriptorPath))
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