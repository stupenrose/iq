package org.iqbuild

import java.io.File
import java.net.URL
import org.apache.commons.httpclient.HttpClient
import org.apache.commons.httpclient.methods.GetMethod
import java.io.FileOutputStream
import org.apache.commons.io.IOUtils
import org.iqbuild.util.Util

class URLCache(val cache:File = new File(System.getProperty("user.home"), ".iq/cache")) extends HttpFetcher {
  
    cache.mkdirs()
      
  override def get(u:URL):File = {
    if(u.getProtocol == "file"){
      new File(u.getFile).getAbsoluteFile.getCanonicalFile
    }else{
      val x = u.getProtocol() + "_" + u.getHost() + "_" + (if(u.getPort() == -1) "" else u.getPort())
      val q = List(x) ::: u.getPath().split("/").toList
      val f = Util.path(cache, q)
      
      if(!f.exists()){
      	f.getParentFile().mkdirs()
      	print("GET " + u + " ...")
      	val client = new HttpClient()
      	val request = new GetMethod(u.toExternalForm())
      	val statusCode = client.executeMethod(request)
      	if(statusCode == 200){
      		val fout = new FileOutputStream(f)
      		IOUtils.copy(request.getResponseBodyAsStream(), fout)
      		fout.close()
      		println("OK")
      	}else {
      	  println(" " + statusCode)
      	  System.out.flush()
      	  throw new RuntimeException("Error " + statusCode + ": " + u)
      	}
      	request.releaseConnection()
      }
      f
    }
  }
}