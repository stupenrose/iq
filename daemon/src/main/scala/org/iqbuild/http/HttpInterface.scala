package org.iqbuild.http

import org.httpobjects.jetty.HttpObjectsJettyHandler
import org.httpobjects.HttpObject
import org.httpobjects.Request
import org.httpobjects.DSL._
import org.httpobjects.util.HttpObjectUtil
import org.iqbuild.ModuleStatus
import java.io.File
import org.iqbuild.Guts
import org.iqbuild.ModuleListItem
import org.iqbuild.Jackson
import org.iqbuild.ModuleDescriptor
import scala.io.Source
import org.iqbuild.Data
import org.iqbuild.DependencyResolutionResult
import org.iqbuild.maven.PomGenerator
import org.httpobjects.Representation
import java.io.OutputStream
import java.io.FileInputStream

class HttpInterface (guts:Guts){
      def data = guts.data
      def save(d:Data) = guts.save(d)
      
      HttpObjectsJettyHandler.launchServer(33421, 
          new HttpObject("/"){
          	  	override def get(req:Request) = OK(Html(
          	  	       <html>
                      <body>
                       	<h1>iqbuild [{guts.iqDir.getAbsolutePath}]</h1>
                       	{guts.data.moduleDescriptors.map{f=>
                       				  	<div>{f}</div>
                       				}}
                      </body>
                   </html>.toString
          	  	))
          },
          new HttpObject("/modules"){
          	  	override def get(req:Request) = {
          	  	  val matchingModules = req.query().valueFor("path") match {
          	  	    case null=>guts.modulesStatus
          	  	    case p:String=>guts.modulesStatus.filter(_.descriptorPath  ==p)
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
          	  	  val maybeDescriptor = guts.modulesStatus.find(_.maybeDescriptor.get.id.toString == id)
          	  	  
          	  	  maybeDescriptor match {
          	  	    case None=>NOT_FOUND
          	  	    case Some(descriptor)=>{
          	  	      
          	  	      val result = DependencyResolutionResult(guts.fullyResolveAll(descriptor.maybeDescriptor.get.deps))
      	              val resolutions = result.flatten().map{dep=>
          	  	        dep.spec.module -> dep.spec.version
          	  	      }
          	  	      
          	  	      OK(Json(Jackson.jackson .writerWithDefaultPrettyPrinter().writeValueAsString(resolutions)))
          	  	    }
          	  	  }
          	  	}
          },
          new HttpObject("/modules/{moduleId*}"){
          	  	override def get(req:Request) = {
          	  	  val id = req.path().valueFor("moduleId")
          	  	  val maybeDescriptor = guts.modulesStatus.find(_.maybeDescriptor.get.id.toString == id)
          	  	  
          	  	  maybeDescriptor match {
          	  	    case None=>NOT_FOUND
          	  	    case Some(descriptor)=>OK(Json(Jackson.jackson .writerWithDefaultPrettyPrinter().writeValueAsString(descriptor)))
          	  	  }
          	  	}
          	  	override def delete(req:Request) = {
          	  	  val descriptorPath = req.path().valueFor("moduleId")
          	  	  val f = new File(descriptorPath).getAbsolutePath
          	  	  
          	  	  data.moduleDescriptors.find(_ == f) match {
          	  	    case None => NOT_FOUND(Text("No such descriptor: " + f))
          	  	    case Some(descriptor) => {
          	  	      guts.remove(descriptor)
          	  	      OK(Text("Deleted"))
          	  	    }
          	  	  }
          	  	}
      	  	
      	    },
          new HttpObject("/nextBuild"){
            override def get(req:Request) = {
              	HttpInterface.this.synchronized(HttpInterface.this.wait())
              	OK(Text("done"))
            }
          },
          new HttpObject("/modules/{moduleId}/pom"){
            override def get(req:Request) = {
          	  	  val id = req.path().valueFor("moduleId")
          	  	  val maybeDescriptor = guts.modulesStatus.find(_.maybeDescriptor.get.id.toString == id)
    
          	  	  maybeDescriptor
          	  	    .flatMap(_.maybeDescriptor)
          	  	    .map{d=>PomGenerator.generatePOM(d, guts.fullyResolveDependencies(d))}
          	  	    .map{text=>OK(Text(text))}
          	  	    .getOrElse(INTERNAL_SERVER_ERROR(Text("Invalid descriptor")))
            }
          },
          new HttpObject("/log"){
            override def get(req:Request) = {
              OK(new Representation(){
                override def contentType = "text/plain"
                override def write(out:OutputStream) {
                  val in = new FileInputStream(guts.logPath)
                  
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