package org.iqbuild

import java.io.InputStream
import java.io.OutputStream
import org.apache.commons.io.IOUtils
import java.io.File

object Util {
    def copy(i:InputStream, o:OutputStream) {
      new Thread(){
        override def run(){
          IOUtils.copy(i, o)
        }
      }.start()
    }
    
    def find(dir:File)(filter:File=>Boolean):Seq[File] = {
      
      if(dir.isDirectory()){
    	dir.listFiles().flatMap{child=>
    	  find(child)(filter)
    	}
      }else{
        Seq(dir)
      }
    }
    
    
    def path(d:File, segments:Seq[String]):File = {
        if(segments.isEmpty){
          d
        }else{
          path(new File(d, segments.head), segments.tail)
        }
      }
}