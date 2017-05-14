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
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import java.io.PipedInputStream
import java.io.PipedOutputStream
import java.io.FileInputStream
import org.httpobjects.Representation
import scala.collection.generic.MutableMapFactory
import scala.collection.mutable.ListBuffer
import scala.util.Try
import java.io.PrintWriter
import org.httpobjects.Response
import org.iqbuild.maven.PomGenerator
import org.iqbuild.http.HttpInterface

trait Guts {
  def logPath:File
  def save(d:Data):Unit
  def data:Data
  def modulesStatus:Seq[ModuleStatus]
  def iqDir:File
  def fullyResolveAll(specs:Seq[DependencySpec]):Seq[ResolvedDependency]
  def remove(descriptorPath:String)
  def fullyResolveDependencies(m:ModuleDescriptor):DependencyResolutionResult
}

class Daemon extends Guts {
    val buildMechanisms:Map[String, BuildMechanism] = Map(
        "jar" -> JarBuild,
        "node" -> new ExternalBuildMechanism(
                        name="node", 
                        port=8080,
                        cmd = "python -m SimpleHTTPServer 9000"))
    
    val cache = new URLCache
    val mavenResolver = new MavenDependencyResolver(cache)
    
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
    
    val dataFilePath = new File(iqDir, "data.json")
    
    var data = if(dataFilePath.exists()){
      Jackson.parseJson[Data](dataFilePath)
    }else{
      Data()
    }
      
    def save(d:Data){
      Jackson.jackson.writer.withDefaultPrettyPrinter().writeValue(dataFilePath, d)
      this.data = d
    }
    
    def parseDescriptor(descriptorPath:String):ModuleDescriptor = {
      val paths = Paths(descriptorPath)
      val text = Source.fromFile(paths.moduleDescriptorFile).getLines.mkString("\n")
      ModuleDescriptor.parse(text)
    }
    
    var modulesStatus:Seq[ModuleStatus] = data.moduleDescriptors.map{path=> 
      ModuleStatus(
          descriptorPath = path,
          maybeDescriptor = Try(parseDescriptor(path)).toOption,
          errors = Seq())
    }
    
    /// DEP RESOLUTION
    def fullyResolve(p:PartiallyResolvedDependency):ResolvedDependency = {
      val transitives = fullyResolveAll(p.transitives)
      
      ResolvedDependency(
          url = p.url, 
          spec = p.spec, 
          transitives)
          
    }
    
    def partiallyResolve(spec:DependencySpec):PartiallyResolvedDependency = {
      
      val maybeInternalMatch = modulesStatus.find{s=>
        s.maybeDescriptor.exists{d=>
          d.id == spec.module
        }
      }.map{s=>
        val paths = Paths(s.descriptorPath)
        PartiallyResolvedDependency(
            url = "file://" + paths.result,
            spec = spec.copy(version=Some(Constants.INTERNAL_VERSION)), 
            transitives = s.maybeDescriptor.get.deps)
      }
      
      maybeInternalMatch match {
        case Some(internalMatch) => {
          println("Internal Match!")
          internalMatch
        }
        case None => {
          println("Using maven for " + spec)
          mavenResolver.resolveDepsFor(spec)
        }
      }
      
    }
    
    def fullyResolveDependencies(m:ModuleDescriptor):DependencyResolutionResult = {
      println("Resolving..")
      val resolutions = fullyResolveAll(m.deps)
    	  println("Done") 
    	  DependencyResolutionResult(resolutions)
  	  }
    
    def fullyResolveAll(specs:Seq[DependencySpec]) = {
      specs
          .map(partiallyResolve)
          .map(fullyResolve)
    }
    
    def remove(descriptorPath:String) = {
      save(data.copy(moduleDescriptors = data.moduleDescriptors.filterNot(_ == descriptorPath)))
      modulesStatus = modulesStatus.filterNot(_.descriptorPath == descriptorPath)
    }
      
    val logPath = new File(iqDir, "log")
    val bytesOut = new FileOutputStream(logPath)
    val out = new PrintStream(bytesOut)
    
    val http = new HttpInterface(this)
    
    val fsScanner = new FSScanner(this)
    
    val reactor = new BuildReactor(
        buildMechanisms = buildMechanisms, 
        parseDescriptor = parseDescriptor,
        fullyResolveDependencies = fullyResolveDependencies,
        out = out)
    
    def reactorInputEvents = fsScanner.filesystemChangesOverTime.map{fsChanges => 
      ReactorState(fsChanges, data)
    }
    
    reactor.allBuildResults(reactorInputEvents).foreach { x => null /* this just blocks & realizes the stream*/}
    
    println("Twilight zone! .... the world has stopped changing!!")
      
}