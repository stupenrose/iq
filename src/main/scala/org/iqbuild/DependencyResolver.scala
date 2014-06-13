package org.iqbuild

import java.net.URL
import scala.xml.XML
import scala.xml.Elem
import scala.collection.mutable.ListBuffer

class DependencyResolver(val cache:URLCache) {
    val mavenCentral = "http://central.maven.org/maven2/"
    
      
    def resolveDepsFor(spec:DependencySpec):List[(DependencySpec, String)] = {
      
        def makeArtifactUrl(groupId:String, artifactId:String) = mavenCentral + groupId.replaceAllLiterally(".", "/") + "/" + artifactId
        val artifactUrl = makeArtifactUrl(spec.module .group , spec.module .name )
        def makeUrl(version:String) = artifactUrl + "/" + version + "/" + spec.module.name + "-" + version + ".jar"
        def makePomUrl(groupId:String, artifactId:String, version:String) = makeArtifactUrl(groupId , artifactId) + "/" + version + "/" + artifactId + "-" + version + ".pom"
        
        val maybeVersion = spec.version match {
          case Some(version)=> Some(version)
          case None => try{
	          val metadataUrl = artifactUrl + "/maven-metadata.xml"
//	          println("GET " + metadataUrl)
	          val file = cache.get(new URL(metadataUrl))
	          val xml = scala.xml.XML.load(file.toURL())
	          val latestVersion = (xml \ "versioning" \ "latest").text 
	          Some(latestVersion)
	        }catch{
	          case e:Exception => None
	        }
        }
        
        val resolution = maybeVersion match {
          case Some(version)=>Some(spec-> makeUrl(version))
          case None => None
        }
        
        case class ModuleIdAndVersion(id:ModuleId, version:String)
        
        class PomStuff(val coords:ModuleIdAndVersion){
          val pomUrl = makePomUrl(coords.id .group , coords.id .name , coords.version)
		  val file = cache.get(new URL(pomUrl))
		  val xml = scala.xml.XML.load(file.toURL())
		  
		  val parentTag = (xml \ "parent")
		  
		  val maybeParent = if(parentTag.isEmpty){
		    	None
		    }else{
				val groupId = (parentTag \ "groupId").text
				val artifactId = (parentTag \ "artifactId").text
				val version = (parentTag \ "version").text
				
				Some(new PomStuff(ModuleIdAndVersion(ModuleId(groupId, artifactId), version)))
		  }
          
          def props():Map[String, String] = {
            
            val props = (xml \ "properties").headOption match {
              case None=>Map[String, String]()
              case Some(propsNode)=>{
                propsNode.child.map{f=>
//                  println("PROP:" + f)
			  	"${" + f.label + "}" -> f.text
    		   }.toMap
              }
            } 
            
            
            val parentProps = maybeParent match {
              case Some(parent) => parent.props
              case None => Map[String, String]()
            }
            val f = props.toMap
            //
            f ++ parentProps
          }
          
        }
        
//        def getHeirarchy(id:ModuleId, version:String):Seq[PomStuff] = {
//          
//          val results = new ListBuffer[PomStuff]
//          var maybeNext:Option[ModuleIdAndVersion] = Some(ModuleIdAndVersion(id, version))
//          while(maybeNext.isDefined){
//        	  val p = new PomStuff(maybeNext.get)
//        	  results += p
//        	  maybeNext = p.maybeParentCoords 
//          }
//          results.toSeq
//        }
        
        
        // transitives
        val transitives = maybeVersion match {
          case Some(version)=> {
        	  try{
//        	      System.out.println("START:" + spec + " version " + version)
        	      val heirarchy = new PomStuff(ModuleIdAndVersion(spec.module, version))
        	      
        	      val props = heirarchy.props()
        		  val expressions = props ++ Map(
        		      "${project.version}" -> version,
        		      "${pom.version}" -> version)
        		  
//        		  println("PROPERTIES: " + expressions)
        		  
				  def resolveExpressions(text:String):String = {
        		    expressions.foldLeft(text){(text, change)=>
        		      val (expression, value) = change
        		      
        		      text.replaceAllLiterally(expression, value)
        		    }
        		  }
				  
				  val depsNodes = (heirarchy.xml \ "dependencies" \ "dependency") 
				  val deps = depsNodes.flatMap{node=>
					  val groupId = resolveExpressions((node \ "groupId").text)
					  val artifactId = resolveExpressions((node \ "artifactId").text)
					  val version = resolveExpressions((node \ "version").text)
					  
					  val moduleId = ModuleId(groupId, artifactId)
					  val maybeVersion = if(version==""){
						  None
					  }else {
						  Some(version)
					  }
					  val spec = DependencySpec(moduleId, maybeVersion)
					  
					  resolveDepsFor(spec)
        		  }
//        		  if(!deps.isEmpty) println("DEPS for " + spec.module  + ":" + deps)
        		  deps
        	  }catch{
        	    case e:Throwable => throw new RuntimeException("Error fetching dependencies for " + spec.module + " " + version, e)
        	  }
          }
          case None => List()
        }
        
        resolution.toList ::: transitives.toList
    }
      
	def resolveDependencies(m:ModuleDescriptor):Seq[(DependencySpec, Seq[(DependencySpec, String)])] = {
	  val urls = m.deps.map{spec=>
        (spec, resolveDepsFor(spec))
      }
	  urls.toList
	}
}