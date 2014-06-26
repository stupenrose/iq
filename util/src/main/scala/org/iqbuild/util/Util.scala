package org.iqbuild.util

import java.io.InputStream
import java.io.OutputStream
import org.apache.commons.io.IOUtils
import java.io.File
import java.io.PrintStream

object Util {
    def makeArtifactUrl(mavenCentral:String, groupId:String, artifactId:String) = mavenCentral + groupId.replaceAllLiterally(".", "/") + "/" + artifactId
    def makePomUrl(mavenCentral:String, groupId:String, artifactId:String, version:String) = makeArtifactUrl(mavenCentral, groupId , artifactId) + "/" + version + "/" + artifactId + "-" + version + ".pom"
        
    def copy(i:InputStream, o:OutputStream) {
      new Thread(){
        override def run(){
          IOUtils.copy(i, o)
        }
      }.start()
    }
    
    def find(dir:File)(filter:File=>Boolean):Seq[File] = {
      
      if(dir.isDirectory()){
    	dir.listFiles().filter(filter).flatMap{child=>
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
    
    def exec(cmd: Seq[String], dir:File, out:PrintStream = System.out, err:PrintStream = System.err): Unit = {
        out.println(cmd.mkString(" "))
        
        val p = new ProcessBuilder().directory(dir).command(cmd :_*).start()
        
        watchProc(p, out, err)
    }
    
    def watchProc(p:Process, out:PrintStream = System.out, err:PrintStream = System.err): Unit = {
        Util.copy(p.getInputStream(), out)
        Util.copy(p.getErrorStream(), err)
        
        val result = p.waitFor()
        if(result!=0)
        	throw new RuntimeException("Exit " + result)
    }
}