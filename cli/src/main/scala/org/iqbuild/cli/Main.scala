package org.iqbuild.cli

import org.apache.commons.httpclient.HttpClient
import org.apache.commons.httpclient.methods.GetMethod
import org.apache.commons.io.IOUtils

object Main {
	def main(args: Array[String]) {
        val command = args(0)
        
        command match {
          case "tail" => tail()
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