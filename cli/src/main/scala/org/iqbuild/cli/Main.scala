package org.iqbuild.cli

import org.apache.commons.httpclient.HttpClient
import org.apache.commons.httpclient.methods.GetMethod
import org.apache.commons.io.IOUtils
import java.io.File
import java.io.PrintStream
import org.iqbuild.util.Util
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.ByteArrayOutputStream
import scala.collection.JavaConversions._
import org.apache.commons.httpclient.methods.PostMethod
import org.apache.commons.httpclient.methods.DeleteMethod
import java.io.OutputStream
import hudson.util.ProcessTree
import org.iqbuild.Jackson
import scala.io.Source
import org.iqbuild.ModuleDescriptor
import java.io.PrintWriter
import com.fasterxml.jackson.databind.JsonNode

case class ModuleListItem (path:String, id:String)
    
object Main {
  val BASE_URL = "http://localhost:33421"
    
  val commands = Map(
    "tail" -> tail _,
    "watchrun" -> watchRun _,
    "web" -> openWebUI _,
    "include" -> include _,
    "remove" -> remove _,
    "list" -> list _,
    "dependencies" -> dependencies _, 
    "run" -> run _)
    
  val usage = "Commands I know are:\n    " + commands.keys.mkString(",\n    ")
  
	def main(args: Array[String]) {
        
        if(args.isEmpty) {
          println(usage)
        }else{
          val command = args(0)
        
          commands.get(command) match {
            case Some(fn) => fn(args.tail)
            case None => println(s"""Unknown command: "$command".  """ + usage)
          }
        }
        
	}
  
  def run(args:Seq[String]){
    org.iqbuild.Main.main(Array())
  }
  
  def dependencies(args:Seq[String]){
    

	  val items = Jackson.jackson.readTree(getText(BASE_URL + "/modules")).map{node=>
  	  ModuleListItem(path=node.at("/path").asText(), id=node.at("/id").asText())
    }
    
    val maybeModule = items.find{module=>
      val a = new File(module.path).getCanonicalFile.getParentFile.getCanonicalPath
      val b = new File(System.getProperty("user.dir")).getCanonicalPath
      a == b
    }
    
    maybeModule match {
      case Some(module) => {
    	  val items = Jackson.jackson.readTree(getText(BASE_URL + "/modules/" + module.id + "/dependencies")).map{node=>
    	    val List(id, version) = node.elements().toList.asInstanceOf[List[JsonNode]]
    	    println("    " + id.at("/group").asText() + ":" + id.at("/name").asText() + ":" + version.asText)
        }
      }
        
    }
  }
  
  private def getText(url:String):String = {
    val client = new HttpClient
      
    val request = new GetMethod(url)
    val statusCode = client.executeMethod(request)
  
    val text = request.getResponseBodyAsString()
    request.releaseConnection()
  
    if(statusCode!=200) throw new RuntimeException("error " + statusCode + ":\n" + text)
    
    text
  }
  
  def list(args:Seq[String]){
    val client = new HttpClient
	    
    val request = new GetMethod(BASE_URL + "/modules")
    val statusCode = client.executeMethod(request)

    val text = request.getResponseBodyAsString()
    request.releaseConnection()

    if(statusCode!=200) throw new RuntimeException("error " + statusCode + ":\n" + text)

	  val items = Jackson.jackson.readTree(text).map{node=>
  	  ModuleListItem(path=node.at("/path").asText(), id=node.at("/id").asText())
    }
    
    println("REGISTERED MODULES:")
    items.foreach{module=>
      println(s"""    ${module.id} | ${module.path}""")
    }
  }
  
    def remove(args:Seq[String]) {
      val arg = new File(args(0)).getCanonicalFile
      val client = new HttpClient
	    
      
	    val paths = if(arg.isDirectory){
	      scan(arg)
	    }else{
	      Seq(arg)
	    }
      
      paths.foreach{path=>
        println("Removing " + path.getAbsolutePath)
//        val request = new GetMethod(BASE_URL + "/modules?path=" + path.getAbsolutePath())
//        val statusCode = client.executeMethod(request)
//  
//        val text = request.getResponseBodyAsString()
//        request.releaseConnection()
//  
//        if(statusCode!=200) throw new RuntimeException("error " + statusCode + ":\n" + text)
//  
//    	  val items = Jackson.jackson.readTree(text).map{node=>
//      	  ModuleListItem(path=node.at("/path").asText(), id=node.at("/id").asText())
//        }
  
//        items.find(_.path  == path.getAbsolutePath()) match {
//          case None=> println("no module for " + path.getAbsolutePath())
//          case Some(item)=> {
        	  val request = new DeleteMethod(BASE_URL + "/modules/" + path.getAbsolutePath())
    			  execute(client, request) {statusCode=>
    			  if(statusCode!=200)
    				  throw new Exception("Error " + statusCode + ": (" + request.getURI() + ")\n" + request.getResponseBodyAsString())
        	  }
//          }
//        }
      }
      
	}
    
    def execute(client:HttpClient, request:org.apache.commons.httpclient.HttpMethod)(fn:Int=>Unit){
	  val statusCode = client.executeMethod(request)
      try{
        fn(statusCode)
      }finally{
    	  request.releaseConnection()
      }
    }
    
    
  private def scan(path:File):Seq[File] = {
    if(path.isDirectory()){
      path.listFiles().flatMap(scan)
    }else if(path.isFile() && path.getName == "module.iq"){
      Seq(path)
    }else{
      Seq()
    }
  }
  
	def include(args:Seq[String]) {
	    val arg = new File(args(0)).getCanonicalFile
	    
	    val pathsToAdd = if(arg.isDirectory){
	      scan(arg)
	    }else{
	      Seq(arg)
	    }
	    
	    pathsToAdd.foreach{path=> 
        println("Including " + path.getAbsolutePath)
	      val client = new HttpClient
  	    val request = new PostMethod(BASE_URL + "/modules")
        request.setRequestBody(path.getAbsolutePath())
  	    val statusCode = client.executeMethod(request)
  	    
  	    val text = request.getResponseBodyAsString()
  	    request.releaseConnection()
  	    
  	    if(statusCode!=200) 
  	      throw new RuntimeException("error " + statusCode + ":\n" + text)
	    }
      
	    
	}
	def openWebUI(args:Seq[String]) {
	  Runtime.getRuntime().exec(Array("gnome-open", BASE_URL));
	}
	
	def watchRun(cmd:Seq[String]){
	  while(true){
		  println("starting")
		  
		  val p = new ProcessBuilder().command(cmd :_*).redirectErrorStream(false).start();
		  
		  val out = new ByteArrayOutputStream()
		  def copyAndClose( name:String, in:InputStream, out:OutputStream){
		      new Thread(){
		        
		        override def run = try {
		          def copyByte() = in.read() match {
	                case -1 => false
	                case byte:Int => {
	                  out.write(byte)
	                  true
	                }
	              }
		          
		          while(copyByte){} // 
			    }catch {
			      case e:Exception => e.printStackTrace()
			    }
		        
		      }.start();
		      
		    }
		    
		    copyAndClose("stderr", p.getErrorStream(), System.err)
		    copyAndClose("stdout", p.getInputStream(), System.out)
	        
		    println("Waiting")
	        
	      val client = new HttpClient
		    val request = new GetMethod(BASE_URL + "/nextBuild")
		    client.executeMethod(request)
		    request.releaseConnection()
		    
		    if(p!=null){
		      println("stopping")
		      
		      
		      killRecursivelyAndBlockTillCompletelyDead(p)
		 	 
		      println("stopped " + p.exitValue())
		      
		    }
	  }
	  
	}
	
	
	private def killRecursivelyAndBlockTillCompletelyDead(p:Process) {
	  val op = ProcessTree.get().get(p);
	  if(op!=null){
	      op.killRecursively();
		  p.waitFor()
		
		  val pids = List(op.getPid) ::: op.getChildren.map(_.getPid).toList
		
		  var isRunning = true;
		  while(isRunning){
	  	    val procs = pids.map(ProcessTree.get.get(_))
		    isRunning = procs.exists(_!=null)
		    println("Sleeping (" + procs.mkString("\n") + ")")
		    Thread.sleep(200)
		  }
	  }
	  
	}
	
	def tail(args:Seq[String]){
	  val client = new HttpClient
	  val request = new GetMethod("http://localhost:33421/log")
	  client.executeMethod(request) match {
	    case 200 => IOUtils.copy(request.getResponseBodyAsStream(), System.out)
	    case code:Int => throw new Exception("Returned " + code)
	  }
	  System.out.flush()
	}
	
}