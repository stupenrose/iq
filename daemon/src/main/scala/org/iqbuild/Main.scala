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
import scala.util.Try
import java.io.PrintWriter
import org.httpobjects.Response
import org.iqbuild.maven.PomGenerator


case class FilesystemChanges(descriptorPath:String, maybePrev:Option[FSNode], currentState:FSNode, deltas:Seq[(FSNode, FSNode)]) {
  def needsBuild = (maybePrev, deltas) match {
    case (None, _) => true
    case (_, deltas) if !deltas.isEmpty => {
//      out.println(s"Something changed:")
//		  out.println(deltas.map{beforeAndAfter=>
//		  	val (before, after) = beforeAndAfter
//		  	
//		  	val timeDiff = if(before.lastModified != after.lastModified ){
//		  	  before.lastModified + " vs " + after.lastModified
//		  	}else ""
//		  	
//		  	val fileDiff = if(before.isFile!=after.isFile ){
//		  	  before.isFile  + " vs " + after.isFile
//		  	}else ""
//		  	
//		  	before.path + " (" + timeDiff + fileDiff + ")"
//		  }.mkString("    ", "\n    ", "\n"))
      true
    }
    case _ => false
  }
}

case class ModuleListItem (path:String, id:String)
case class ModuleBuildError(path:String, where:String, description:String)
case class ModuleStatus(
      descriptorPath:String, 
      maybeDescriptor:Option[ModuleDescriptor],
      errors:Seq[ModuleBuildError])
      
case class BuildResult( modulesStatus:Seq[ModuleStatus])
case class Paths(val descriptorPath:String){
      val moduleDescriptorFile = new File(descriptorPath)
      val dir = moduleDescriptorFile.getParentFile() 
	    val targetDir = new File(dir, "target")
      val pollingCache = new File(targetDir, "fs.json")
      val result = new File(targetDir, "result")
      pollingCache.getParentFile.mkdirs()
    }

object Main {
    val buildMechanisms:Map[String, BuildMechanism] = Map(
        "jar" -> JarBuild,
        "node" -> new ExternalBuildMechanism(
                        name="node", 
                        port=8080,
                        cmd = "python -m SimpleHTTPServer 9000"))
    
    val cache = new URLCache
    val mavenResolver = new MavenDependencyResolver(cache)
    
    
    
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
      
    def save(d:Data){
      Jackson.jackson.writer.withDefaultPrettyPrinter().writeValue(dataFilePath, d)
      this.data = d
    }
    
    def parseDescriptor(descriptorPath:String):ModuleDescriptor = {
      val paths = Paths(descriptorPath)
      val text = Source.fromFile(paths.moduleDescriptorFile).getLines.mkString("\n")
      ModuleDescriptor.parse(text)
    }
    
    var modulesStatus:Seq[ModuleStatus] = data.moduleDescriptors.map{path=> 
      ModuleStatus(
          descriptorPath = path,
          maybeDescriptor = Try(parseDescriptor(path)).toOption,
          errors = Seq())
    }
    
    /// DEP RESOLUTION
    def fullyResolve(p:PartiallyResolvedDependency):ResolvedDependency = {
      val transitives = fullyResolveAll(p.transitives)
      
      ResolvedDependency(
          url = p.url, 
          spec = p.spec, 
          transitives)
          
    }
    
    def partiallyResolve(spec:DependencySpec):PartiallyResolvedDependency = {
      
      val maybeInternalMatch = modulesStatus.find{s=>
        s.maybeDescriptor.exists{d=>
          d.id == spec.module
        }
      }.map{s=>
        val paths = Paths(s.descriptorPath)
        PartiallyResolvedDependency(
            url = "file://" + paths.result,
            spec = spec.copy(version=Some(Constants.INTERNAL_VERSION)), 
            transitives = s.maybeDescriptor.get.deps)
      }
      
      maybeInternalMatch match {
        case Some(internalMatch) => {
          println("Internal Match!")
          internalMatch
        }
        case None => {
          println("Using maven for " + spec)
          mavenResolver.resolveDepsFor(spec)
        }
      }
      
    }
    

    def fullyResolveDependencies(m:ModuleDescriptor):DependencyResolutionResult = {
      println("Resolving..")
      val resolutions = fullyResolveAll(m.deps)
  	  println("Done") 
  	  DependencyResolutionResult(resolutions)
  	}
    
    def fullyResolveAll(specs:Seq[DependencySpec]) = {
      specs
          .map(partiallyResolve)
          .map(fullyResolve)
    }
    
    
    
    
    def main(whatever: Array[String]) {
      
      val logPath = new File(iqDir, "log")
      val bytesOut = new FileOutputStream(logPath)
      val out = new PrintStream(bytesOut)
      
      
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
    	    	  	  save(data.copy(moduleDescriptors = data.moduleDescriptors.toList :+ path))
    	    	  	  OK(Text("Added " + d.id))
  	  	        }
  	  	      }
      	  	}
          },
          new HttpObject("/modules/{moduleId}/dependencies"){
      	  	override def get(req:Request) = {
      	  	  val id = req.path().valueFor("moduleId")
      	  	  val maybeDescriptor = modulesStatus.find(_.maybeDescriptor.get.id.toString == id)
      	  	  
      	  	  maybeDescriptor match {
      	  	    case None=>NOT_FOUND
      	  	    case Some(descriptor)=>{
      	  	      
      	  	      val result = DependencyResolutionResult(fullyResolveAll(descriptor.maybeDescriptor.get.deps))
  	              val resolutions = result.flatten().map{dep=>
      	  	        dep.spec.module -> dep.spec.version
      	  	      }
      	  	      
      	  	      OK(Json(Jackson.jackson .writerWithDefaultPrettyPrinter().writeValueAsString(resolutions)))
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
      	  	      save(data.copy(moduleDescriptors = data.moduleDescriptors.filter(_!=handler.descriptorPath)))
      	  	      
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
          new HttpObject("/modules/{moduleId}/pom"){
            override def get(req:Request) = {
      	  	  val id = req.path().valueFor("moduleId")
      	  	  val maybeDescriptor = modulesStatus.find(_.maybeDescriptor.get.id.toString == id)

      	  	  maybeDescriptor
      	  	    .flatMap(_.maybeDescriptor)
      	  	    .map{d=>PomGenerator.generatePOM(d, fullyResolveDependencies(d))}
      	  	    .map{text=>OK(Text(text))}
      	  	    .getOrElse(INTERNAL_SERVER_ERROR(Text("Invalid descriptor")))
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
      
      
      
      
      
      val filesystemChangesOverTime:Stream[Seq[FilesystemChanges]] = {
        def scanForChanges():Stream[Seq[FilesystemChanges]]= {
          println("Scanning...")
          val moduleDescriptors = data.moduleDescriptors
          val fsChanges = moduleDescriptors.map{descriptorPath=>
            val paths = Paths(descriptorPath)
            val maybePrev = if (paths.pollingCache.exists()) Some(Jackson.jackson.readValue(paths.pollingCache, classOf[FSNode])) else None 
  		      val fs = FSNode.forPath(paths.dir, {f => 
  		        f!=paths.targetDir && 
  		        !f.getName.equals("pom.xml") && // hack!
  		        !f.getName().startsWith(".")})
    			  val deltas = maybePrev match {
    			    case Some(prev) => fs.deltas(prev)
    			    case None => Seq()
    			  }
  		      Jackson.jackson.writerWithDefaultPrettyPrinter().writeValue(paths.pollingCache, fs);
  		      
  		      if(!deltas.isEmpty){
  		        println("Something changed")
  		      }
		        Thread.sleep(200)
		         
  		      FilesystemChanges(
  		          descriptorPath = descriptorPath, 
  		          maybePrev = maybePrev, 
  		          currentState = fs, 
  		          deltas = deltas)
          }
          Stream.cons(fsChanges, scanForChanges())
        }
        
        scanForChanges()
      }
      
      val reactor = new BuildReactor(
          buildMechanisms = buildMechanisms, 
          parseDescriptor = parseDescriptor,
          fullyResolveDependencies = fullyResolveDependencies,
          out = out)
      
      val reactorInputEvents = filesystemChangesOverTime.map{fsChanges => 
        ReactorState(fsChanges, data)
      }
      
      reactor.blockUntilAllInputHasBeenProcessed(reactorInputEvents);
    }
    
    
      
}