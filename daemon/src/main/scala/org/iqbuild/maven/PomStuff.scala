package org.iqbuild.maven

import java.net.URL
import scala.xml.NodeSeq
import org.iqbuild.util.Util
import org.iqbuild.ModuleId
import org.iqbuild.HttpFetcher
import scala.xml.parsing.XhtmlParser
import scala.xml.Elem
import scala.xml.parsing.ConstructingParser
import javax.xml.parsers.DocumentBuilderFactory
import java.io.FileReader
import org.apache.maven.model.Dependency
import scala.collection.JavaConversions._
import java.util.Properties
import org.apache.maven.model.DependencyManagement

case class ModuleIdAndVersion(id:ModuleId, version:String)

class PomStuff(val coords:ModuleIdAndVersion, val mavenCentral:String, val cache:HttpFetcher){
  val pomUrl = Util.makePomUrl(mavenCentral, coords.id .group , coords.id .name , coords.version)
  val file = cache.get(new URL(pomUrl))
  val xml = try{
	val reader = new FileReader(file);
	
	try {
	    val xpp3Reader = new org.apache.maven.model.io.xpp3.MavenXpp3Reader();
	    xpp3Reader.read(reader);
	} finally {
	    reader.close();
	}
	
  }catch {
    case e:Exception=> throw new Exception("Error parsing " + file, e)
  }
  
  val parentTag = xml.getParent()
  
  val maybeParent = if(parentTag==null){
      None
  }else{
	  val groupId = parentTag.getGroupId()
	  val artifactId = parentTag.getArtifactId()
	  val version = parentTag.getVersion()
	  
	  Some(new PomStuff(ModuleIdAndVersion(ModuleId(groupId, artifactId), version), mavenCentral, cache))
  }
  
  def resolveExpressions(text:String, props:Map[String, String] = this.props):String = {
    val expressions = props.map{s=>
      val (k, v) = s
      "${" + k + "}" -> v
    };
    expressions.foldLeft(text){(text, change)=>
      val (expression, value) = change
      text.replaceAllLiterally(expression, value)
    }
  } 		  
  
  def getVersionFor(module:ModuleId):String = {
    val deps = xml.getDependencyManagement() match {
      case null => Seq()
      case dm:DependencyManagement=> {
        val depsNodes = dm.getDependencies().asInstanceOf[java.util.List[Dependency]]
		depsNodes.map{node=>
			  val groupId = resolveExpressions(node.getGroupId())
			  val artifactId = resolveExpressions(node.getArtifactId())
			  val version = resolveExpressions(node.getVersion())
			  val moduleId = ModuleId(groupId, artifactId)
			  ModuleIdAndVersion(moduleId, version)
	    }
      }
    }
    
    val maybeMatch = deps.find(_.id == module)
    
    (maybeMatch, maybeParent) match {
      case (Some(dep), _)=>dep.version
      case (None, Some(parent)) => parent.getVersionFor(module)
      case (None, None) => println(xml); throw new Exception("No version for " + module)
    }
  }
  
  case class Coords(group:String, artifact:String, version:Option[String], scope:Option[String], isOptional:Boolean)
  
  def dependencies(scope:String = "compile"):Seq[ModuleIdAndVersion] = {
    val depsNodes = xml.getDependencies().asInstanceOf[java.util.List[Dependency]]
	  val deps = depsNodes.map{node=>
		  val groupId = resolveExpressions(node.getGroupId())
		  val artifactId = resolveExpressions(node.getArtifactId())
	    
	    def resolveIfPresent(e:String):Option[String] = {
	      if(e == null) None
	      else Some(resolveExpressions(e))
	    }
	    
	    val isOptional = node.isOptional()
		  
	    Coords(
	      group=groupId, 
	      artifact=artifactId, 
	      version=resolveIfPresent(node.getVersion()),
	      scope=resolveIfPresent(node.getScope()),
	      isOptional=isOptional)
		  
	  }
    
    val requiredDeps = deps
                        .filter(!_.isOptional)
                        .filter(_.scope.getOrElse("compile") != "provided")
                        .filter(_.scope.getOrElse("compile") == scope)
                        
    requiredDeps.map{coords=>
		  val moduleId = ModuleId(coords.group , coords.artifact)
		  val version = coords.version  match {
		    case None=> getVersionFor(moduleId)
		    case Some(value)=>value
		  }
		  
		  ModuleIdAndVersion(moduleId, version)
    }
  }
  
  def props():Map[String, String] = {
    
    val rawProps = xml.getProperties() match {
      case null=>Map[String, String]()
      case p:Properties=>{
        p.toMap
      }
    } 
   
    def r(key:String, props:Map[String, String]):Map[String, String] = {
      val value = props(key)
      val newVal = resolveExpressions(value, props)
      (props - key) + (key -> newVal)
    }
    
   val props = rawProps.foldLeft(rawProps){case (props:Map[String, String], (key, value))=>
     val newProps = r(key, props)
     newProps
   }
    
    val parentProps = maybeParent match {
      case Some(parent) => parent.props
      case None => Map[String, String]()
    }
    
    props ++ parentProps ++ Map(
    			  "project.groupId" -> coords.id.group, 
    		      "project.version" -> coords.version ,
    		      "pom.version" -> coords.version )
    
  }
  
}