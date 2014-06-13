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

object Main {
  /*
   * 1 - what am I building?
   *    - jar
   *    - war
   * 2 - what are my dependencies?
   * 3 - how do I consume each dependency?
   */
  
    trait BuildMechanism {
      
      def build(path:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor)
    }
    
    object JarBuild extends BuildMechanism {
      override def build(path:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor) {
        val sourceDir = new File(path, "src")
        val targetDir = new File(path, "target")
        targetDir.mkdirs()
        
        val files = find(sourceDir)(_.getName().endsWith(".java")).toList
        
        files.foreach(println)
        println("foo " + path);
        
        
        val classpath = dependencies.map(_.path .getAbsolutePath()).mkString(":")
        
        
        exec(List("javac", "-d", targetDir.getAbsolutePath(), "-cp", classpath) ::: files.map(_.getAbsolutePath()), sourceDir)
      }

      private def exec(cmd: Seq[String], dir:File): Unit = {
        println(cmd.mkString(" "))
        
        
        
        val p = new ProcessBuilder().directory(dir).command(cmd.toList).start()
        
        copy(p.getInputStream(), System.out)
        copy(p.getErrorStream(), System.err)
        
        val result = p.waitFor()
        System.out.println("Exit " + result)
      }
    }
    
    
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
    
    
    case class Timestamped[T](lastModified:Long, item:T)
    
    case class ModuleWorkingInfo (descriptor:Timestamped[ModuleDescriptor])
    
    case class ResolvedDependency(path:File, spec:DependencySpec)
    
    def main(args: Array[String]) {
      
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
      
      val buildMechanisms:Map[String, BuildMechanism] = Map("jar"-> JarBuild )  
      
      // read the cache from ~/.iq/data
      // setup command socket <- 'iq' client instances communicate with this
      // enter watch/build loop
      
      val moduleDescriptorFile = new File(args(0))
      val dir = moduleDescriptorFile.getParentFile() 
      val text = Source.fromFile(moduleDescriptorFile).getLines.mkString("\n")
      val m = ModuleDescriptor.parse(text)
      println("reading " + m.id)
      
      val mavenCentral = "http://central.maven.org/maven2/"
      val urls = m.deps.par.map{spec=>
        
        val artifactUrl = mavenCentral + spec.module.group.replaceAllLiterally(".", "/") + "/" + spec.module.name 
        
        def makeUrl(version:String) = artifactUrl + "/" + version + "/" + spec.module.name + "-" + version + ".jar"
        
        val resolution = spec.version match {
          case Some(version)=> spec -> Some(makeUrl(version))
          case None => try{
	          val metadataUrl = artifactUrl + "/maven-metadata.xml"
	          val xml = scala.xml.XML.load(new URL(metadataUrl).openStream())
	          val latestVersion = (xml \ "versioning" \ "latest").text 
	          spec->Some(makeUrl(latestVersion))
	        }catch{
	          case e:Exception => spec-> None
	        }
        }
        
        resolution
      }
      
      println(urls.mkString("\n"))
      
      val unresolvableDependencies = urls.filter(!_._2.isDefined)
      
      if(!unresolvableDependencies.isEmpty){
        throw new Exception("Unable to resolve dependencies: " + unresolvableDependencies.map(_._1).mkString("\n"))
      }
      
      val cache = new File(System.getProperty("user.home"), ".iq/cache")
      cache.mkdirs()

      
      val deps = urls.toList.map{t=>
        val (spec, url) = t
        val u = new URL(url.get)
        val x = u.getProtocol() + "_" + u.getHost() + "_" + (if(u.getPort() == -1) "" else u.getPort())
        val q = List(x) ::: u.getPath().split("/").toList
        val f = path(cache, q)
        
        if(!f.exists()){
        	f.getParentFile().mkdirs()
        	println("GET " + url.get)
        	val client = new HttpClient()
        	val request = new GetMethod(url.get)
        	val statusCode = client.executeMethod(request)
        	if(statusCode == 200){
        		val fout = new FileOutputStream(f)
        		IOUtils.copy(request.getResponseBodyAsStream(), fout)
        		fout.close()
        	}else throw new RuntimeException("Error " + statusCode + ": " + url.get)
        	request.releaseConnection()
        }
        
        ResolvedDependency(path=f, spec=t._1 )
      }
      
      buildMechanisms(m.build).build(dir, deps, m)
      
    }
    def path(d:File, segments:Seq[String]):File = {
        if(segments.isEmpty){
          d
        }else{
          path(new File(d, segments.head), segments.tail)
        }
      }
}