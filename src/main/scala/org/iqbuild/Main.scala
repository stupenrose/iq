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
import org.apache.commons.io.FileUtils
import org.joda.time.Instant

object Main {
  /*
   * 1 - what am I building?
   *    - jar
   *    - war
   * 2 - what are my dependencies?
   * 3 - how do I consume each dependency?
   */
  
    trait BuildMechanism {
      
      def build(state:FSNode, targetDir:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor)
    }
    
    object FSNode {
      def forPath(p:File, filter:File=>Boolean = {f=>true}):FSNode = {
        
        val children = if(p.isDirectory()) p.listFiles().filter(filter).map(forPath(_)).toSeq else Seq[FSNode]()
        FSNode(p.getAbsolutePath(), new Instant(p.lastModified()), p.isFile(), children)
      }
    }
    case class FSNode(path:String, lastModified:Instant, isFile:Boolean, children:Seq[FSNode]) {
      override def equals(node:Any) = {
    	  node match {
    	    case n:FSNode => {
    	      
    	      val childrenMatch = children.zip(n.children ).forall{t=>
    	        val (a, b) = t
    	        val m = a == b
    	        if(!m) System.out.println(s"$a $b $m")
    	        m
    	       }
    	      
    	      path == n.path  && lastModified == n.lastModified  && isFile == n.isFile && childrenMatch//  && children == n.children 
    	    } 
    	    case _ => false
    	  }
      }
    }
    
    
    
    object JarBuild extends BuildMechanism {
      override def build(state:FSNode, targetDir:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor) {
        val path = new File(state.path) 
        val start = System.currentTimeMillis()
        println("Working on " + path.getAbsolutePath())
        val sourceDir = new File(path, "src")
        val javaFiles = new File(sourceDir, "java")
        val stagingDir = new File(targetDir, "jar")
        targetDir.mkdirs()
        stagingDir.mkdirs()
        
        val files = find(sourceDir)(_.getName().endsWith(".java")).toList
        
        files.foreach(println)
        println("foo " + path);
        
        
        val classpath = dependencies.map(_.path .getAbsolutePath()).mkString(":")
        
        val productFile = new File(targetDir, "product.jar")
        
        println("Deleting " + stagingDir.getAbsolutePath())
        FileUtils.deleteDirectory(stagingDir)
        stagingDir.mkdirs()
        
        exec(List("javac", "-d", stagingDir.getAbsolutePath(), "-cp", classpath) ::: files.map(_.getAbsolutePath()), sourceDir)
        val contents = find(stagingDir){i=>true}.toList
        exec(List("jar", "-cvf", productFile.getAbsolutePath()) ::: contents.map(relativePath(_, stagingDir)), stagingDir)
        val end = System.currentTimeMillis()
        val seconds = (end - start) /1000.0
        println("Finished - " + seconds + " seconds")
      }

      private def relativePath(child:File, parent:File) = {
        val parentPath = parent.getAbsolutePath()
        val childPath = child.getAbsolutePath()
        if(childPath.startsWith(parentPath))
          childPath.substring(parentPath.length()+1)
        else
          throw new RuntimeException()
      }
      
      private def exec(cmd: Seq[String], dir:File): Unit = {
        println(cmd.mkString(" "))
        
        
        
        val p = new ProcessBuilder().directory(dir).command(cmd.toList).start()
        
        copy(p.getInputStream(), System.out)
        copy(p.getErrorStream(), System.err)
        
        val result = p.waitFor()
        if(result!=0)
        	throw new RuntimeException("Exit " + result)
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

      var prev:FSNode = null
	  while(true){
		  val targetDir = new File(dir, "target")
		  val fs = FSNode.forPath(dir, {f => f!=targetDir && !f.getName().startsWith(".")})
		  if(prev==null || fs!=prev){
			  println("Something changed")
			  try{
				buildMechanisms(m.build).build(fs, targetDir, deps, m)
			  }catch{
			  	case e:Throwable=>e.printStackTrace()
			  }
		  }
		  prev = fs
		  Thread.sleep(500)
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