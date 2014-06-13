package org.iqbuild

import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._

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
        
        val files = Util.find(sourceDir)(_.getName().endsWith(".java")).toList
        
        files.foreach(println)
        println("foo " + path);
        
        
        val classpath = dependencies.map(_.path .getAbsolutePath()).mkString(":")
        
        val productFile = new File(targetDir, "product.jar")
        
        println("Deleting " + stagingDir.getAbsolutePath())
        FileUtils.deleteDirectory(stagingDir)
        stagingDir.mkdirs()
        
        exec(List("javac", "-d", stagingDir.getAbsolutePath(), "-cp", classpath) ::: files.map(_.getAbsolutePath()), sourceDir)
        val contents = Util.find(stagingDir){i=>true}.toList
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
        
        Util.copy(p.getInputStream(), System.out)
        Util.copy(p.getErrorStream(), System.err)
        
        val result = p.waitFor()
        if(result!=0)
        	throw new RuntimeException("Exit " + result)
      }
    }
    