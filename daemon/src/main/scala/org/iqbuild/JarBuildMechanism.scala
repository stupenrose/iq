package org.iqbuild

import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._
import java.net.URL
import java.io.PrintStream
import org.iqbuild.util.Util

object JarBuild extends BuildMechanism {
      override def build(moduleDirectory:String, targetDir:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor, out:PrintStream) {
        val path = new File(moduleDirectory) 
        val sourceDir = new File(path, "src")
        val javaFiles = new File(sourceDir, "java")
        val stagingDir = new File(targetDir, "jar")
        targetDir.mkdirs()
        stagingDir.mkdirs()
        
        out.println("scanning")
        
        val files = Util.find(sourceDir){path=>
          val name = path.getName()
          path.isDirectory() || name.endsWith(".java")
        }.toList
        
        out.println("done scanning")
        
        files.foreach(println)
        out.println("foo " + files);
        
        
        val cache = new URLCache()
        
        val dependenciesOnDisk = dependencies.map{d=>cache.get(new URL(d.url))}
        val classpath = dependenciesOnDisk.map(_.getAbsolutePath()).mkString(":")
        
        val productFile = new File(targetDir, "result")
        
        out.println("Deleting " + stagingDir.getAbsolutePath())
        FileUtils.deleteDirectory(stagingDir)
        stagingDir.mkdirs()
        FileUtils.write(new File(targetDir, "classpath"), classpath + ":" + productFile.getAbsolutePath())
        
        exec(List("javac", "-d", stagingDir.getAbsolutePath(), "-cp", classpath) ::: files.map(_.getAbsolutePath()), sourceDir, out = out, err = out)
        val contents = Util.find(stagingDir){i=>true}.toList
        exec(List("jar", "-cvf", productFile.getAbsolutePath()) ::: contents.map(relativePath(_, stagingDir)), stagingDir, out = out, err = out)

      }

      private def relativePath(child:File, parent:File) = {
        val parentPath = parent.getAbsolutePath()
        val childPath = child.getAbsolutePath()
        if(childPath.startsWith(parentPath))
          childPath.substring(parentPath.length()+1)
        else
          throw new RuntimeException()
      }
      
      private def exec(cmd: Seq[String], dir:File, out:PrintStream = System.out, err:PrintStream = System.err): Unit = {
        out.println(cmd.mkString(" "))
        
        
        
        val p = new ProcessBuilder().directory(dir).command(cmd.toList).start()
        
        Util.copy(p.getInputStream(), out)
        Util.copy(p.getErrorStream(), err)
        
        val result = p.waitFor()
        if(result!=0)
        	throw new RuntimeException("Exit " + result)
      }
    }
    