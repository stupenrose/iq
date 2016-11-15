package org.iqbuild

import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._
import java.net.URL
import java.io.PrintStream
import org.iqbuild.util.Util
import org.iqbuild.maven.PomGenerator
import java.io.PrintWriter
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import scala.util.Try

object JarBuild extends BuildMechanism {
      private def toString(t:Throwable):String = {
        val out = new StringWriter
        t.printStackTrace(new PrintWriter(out))
        out.getBuffer.toString()
      }
      
      
      override def build(paths:Paths, tree:DependencyResolutionResult, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor, 
	          maybePreviousState:Option[ModuleStatus], out:PrintStream) = {
        val path = paths.dir
        val javaFiles = new File(path, "src")
        val stagingDir = new File(paths.targetDir, "jar")
        paths.targetDir.mkdirs()
        stagingDir.mkdirs()
        
        
        def generateCourtesyPom():Seq[ModuleBuildError] = {
          try{
            val pomFile = new File(path, "pom.xml")
            out.println("generating courtesy pom: " + pomFile.getAbsolutePath)
            new PrintWriter(pomFile) {
              write(PomGenerator.generatePOM(m, tree)); 
              close 
            } 
            Seq()
          }catch{
            case t:Throwable => Seq(ModuleBuildError(path=paths.descriptorPath, "courtesy pom generation", toString(t)))
          }
          
        }
        
        
        def compileJava() = {
          out.println("scanning")
          
          val files = Util.find(javaFiles){path=>
            val name = path.getName()
            path.isDirectory() || name.endsWith(".java")
          }.toList
          
          out.println("done scanning")
          
          files.foreach(println)
          out.println("foo " + files);
          
          val cache = new URLCache()
          
          val dependenciesOnDisk = dependencies.map{d=>cache.get(new URL(d.url))}
          val classpath = dependenciesOnDisk.map(_.getAbsolutePath()).mkString(":")
          
          
          out.println("Deleting " + stagingDir.getAbsolutePath())
          FileUtils.deleteDirectory(stagingDir)
          stagingDir.mkdirs()
          FileUtils.write(new File(paths.targetDir, "classpath"), classpath + ":" + paths.result.getAbsolutePath())
          
          val maybeJavac = Try(exec(List("javac", "-d", stagingDir.getAbsolutePath(), "-cp", classpath) ::: files.map(_.getAbsolutePath()), javaFiles, out = out, err = out))
          
          maybeJavac.failed.toOption.toSeq.map{t=>
            ModuleBuildError(path=paths.result.getAbsolutePath(), "creating jar", toString(t))  
          }
        }
        
        def createJar() = {
          val contents = Util.find(stagingDir){i=>true}.toList
          val maybeJar = Try(exec(List("jar", "-cvf", paths.result.getAbsolutePath()) ::: contents.map(relativePath(_, stagingDir)), stagingDir, out = out, err = out))
          
          maybeJar.failed.toOption.toSeq.map{t=>
            ModuleBuildError(path=paths.result.getAbsolutePath(), "creating jar", toString(t))  
          }
        }
        
        generateCourtesyPom ++ compileJava ++ createJar
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
    