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
import hudson.util.ProcessTree
import scala.collection.JavaConversions._

object Main {
	def main(args: Array[String]) {
        val command = args(0)
        
        command match {
          case "tail" => tail()
          case "watchrun" => watchRun(args.tail)
        }
	}
	
	def watchRun(cmd:Seq[String]){
	  var p:Process = null
	  var t:Thread = null
	  while(true){
		  println("starting")
		  
		  p = new ProcessBuilder().command(cmd :_*).redirectErrorStream(false).start();
		  
		  var out = new ByteArrayOutputStream()
		  def copyAndClose( name:String, o:InputStream){
		      new Thread(){
		        
		        override def run = try {
			          val bytes = new ByteArrayOutputStream
			    	  IOUtils.copy(o, bytes)
			    	  o.close()
			    	  val text = new String(bytes.toByteArray())
			          println(name + ": " + text)
			    }catch {
			      case e:Exception => e.printStackTrace()
			    }
		        
		      }.start();
		      
		    }
		    
		    copyAndClose("stderr", p.getErrorStream())
		    copyAndClose("stdout", p.getInputStream())
	        
		    println("Waiting")
	        
	        val client = new HttpClient
		    val request = new GetMethod("http://localhost:33421/nextBuild")
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
	  op.killRecursively();
	  p.waitFor()
	
	  val pids = List(op.getPid) ::: op.getChildren.map(_.getPid).toList
	
	  var isRunning = true;
	  while(isRunning){
  	    val procs = pids.map(ProcessTree.get.get(_))
	    isRunning = procs.forall(_==null)
	    println("Sleeping (" + procs.mkString("\n") + ")")
	    Thread.sleep(200)
	  }
	}
	
	def tail(){
	  val client = new HttpClient
	  val request = new GetMethod("http://localhost:33421/log")
	  client.executeMethod(request) match {
	    case 200 => IOUtils.copy(request.getResponseBodyAsStream(), System.out)
	    case code:Int => throw new Exception("Returned " + code)
	  }
	  System.out.flush()
	}
	
}