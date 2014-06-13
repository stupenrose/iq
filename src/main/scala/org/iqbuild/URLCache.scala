package org.iqbuild

import java.net.URL
import java.io.File
import org.apache.commons.httpclient.HttpClient
import org.apache.commons.httpclient.methods.GetMethod
import java.io.FileOutputStream
import org.apache.commons.io.IOUtils

class URLCache(val cache:File = new File(System.getProperty("user.home"), ".iq/cache")) {
  
    cache.mkdirs()
      
	def get(u:URL):File = {
	  val x = u.getProtocol() + "_" + u.getHost() + "_" + (if(u.getPort() == -1) "" else u.getPort())
        val q = List(x) ::: u.getPath().split("/").toList
        val f = Util.path(cache, q)
        
        if(!f.exists()){
        	f.getParentFile().mkdirs()
        	println("GET " + u)
        	val client = new HttpClient()
        	val request = new GetMethod(u.toExternalForm())
        	val statusCode = client.executeMethod(request)
        	if(statusCode == 200){
        		val fout = new FileOutputStream(f)
        		IOUtils.copy(request.getResponseBodyAsStream(), fout)
        		fout.close()
        	}else throw new RuntimeException("Error " + statusCode + ": " + u)
        	request.releaseConnection()
        }
	  f
	}
}