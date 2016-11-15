package org.iqbuild

import java.io.PrintStream
import org.apache.commons.httpclient.HttpClient
import org.apache.commons.httpclient.methods.PostMethod
import java.io.InputStream
import java.io.OutputStream

case class GenericBuildJson(paths:Paths, dependencies:Seq[ResolvedDependency], descriptor:ModuleDescriptor)

class ExternalBuildMechanism(val name:String, val port:Int, val cmd:String) extends BuildMechanism {
  val process = Runtime.getRuntime.exec(cmd)
  
  new Thread(){
    override def run(){
      pipeOneByetAtATime(process.getInputStream, System.out)
    }
  }.start
  new Thread(){
    override def run(){
      pipeOneByetAtATime(process.getErrorStream, System.err)
    }
  }.start
  
  Runtime.getRuntime.addShutdownHook(new Thread(){
    override def run(){
      System.out.println("Shutting down " + name + " daemon")
      process.destroyForcibly()
    }
  })
  
	def build(paths:Paths, tree:DependencyResolutionResult, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor,
	          maybePreviousState:Option[ModuleStatus], out:PrintStream) = {
    val data = GenericBuildJson(
        paths = paths,
        dependencies = dependencies,
        descriptor = m)
        
	  post(s"http://localhost:$port/build", Jackson.jackson.writeValueAsString(data))(pipeOneByetAtATime(_, out))
	  
	  Seq()
	}
  
  private def pipeOneByetAtATime(input:InputStream, out:OutputStream){
    var keepReading = true
	    while(keepReading){
  	    val n = input.read()
	      if(n == -1){
	        keepReading = false
	      }else{
	        out.write(n)
	      }
	    }
  }
  
  private def post(url:String, body:String)(fn:(InputStream)=>Unit) = {
    
    val client = new HttpClient
    val request = new PostMethod(url)
    request.setRequestBody(body)
    val statusCode = client.executeMethod(request)
    
    if(statusCode!=200) 
      throw new RuntimeException("error " + statusCode + ":\n" + request.getResponseBodyAsString)
    
    try{
      fn(request.getResponseBodyAsStream)
    }finally{
      request.releaseConnection()
    }
    
  }
}