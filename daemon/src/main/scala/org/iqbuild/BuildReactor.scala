package org.iqbuild

import java.io.PrintStream
import scala.util.Try

case class ReactorState(fsChanges:Seq[FilesystemChanges], data:Data)

class BuildReactor(
    val buildMechanisms:Map[String, BuildMechanism], 
    val fullyResolveDependencies:ModuleDescriptor=>DependencyResolutionResult,
    val parseDescriptor:String=>ModuleDescriptor) {
  
  def blockUntilAllInputHasBeenProcessed(externalChanges:Stream[ReactorState], data:Data, out:PrintStream):BuildResult = {
    
      def initialState(data:Data):BuildResult = {
        BuildResult(
              modulesStatus = data.moduleDescriptors.map{path=> 
                ModuleStatus(
                    descriptorPath = path,
                    maybeDescriptor = Try(parseDescriptor(path)).toOption,
                    errors = Seq())})
      }
      // should never finish ... this is our "loop"
      externalChanges.foldLeft(initialState(data))(respondToFilesystemChanges(out, _, _))
  }
  
  def respondToFilesystemChanges(out:PrintStream, previousBuild:BuildResult, state:ReactorState):BuildResult = {
        val data = state.data
        val fsChanges = state.fsChanges
        val filesChanged = fsChanges.flatMap(_.deltas).map(_._1.path)
        println(filesChanged.size + " files changed: " + filesChanged.mkString("\n    ", ",\n    ", "\n"))
        val moduleDescriptors = data.moduleDescriptors
        
        case class AffectedModule(descriptorPath:String, reasonsForBuild:Seq[String])
        
        val changesRequiringRebuild = fsChanges.flatMap{changes=>
          val prevState = previousBuild.modulesStatus.find(_.descriptorPath ==changes.descriptorPath)
          
          val maybeReasonToBuild = if(changes.needsBuild){
            Some("files changed")
//          }else if(!prevState.isDefined){
//            Some("hasn't been built yet")
          }else{
            None
          }
          
          maybeReasonToBuild.map{reason=> AffectedModule(changes.descriptorPath, Seq(reason))}
        }
        
        
        case class BuildPass(affectedModules:Seq[AffectedModule], moduleStates:Seq[ModuleStatus])
        
        def buildAffected(input:BuildPass):Stream[BuildPass] = {
          
          
          val deDupedReasoning = input.affectedModules.foldLeft(Map[String, Seq[String]]()){(reasonsByDescriptorPath, nextReason) => 
            val otherReasons = reasonsByDescriptorPath.getOrElse(nextReason.descriptorPath, Seq())
            
            val allReasons = otherReasons ++ nextReason.reasonsForBuild
            
            
            reasonsByDescriptorPath + (nextReason.descriptorPath -> allReasons)
          }.map{case (path, reasons) => AffectedModule(path, reasons)}
          
          val affectedModules = deDupedReasoning
          
          println("Building " + affectedModules.size + " affected modules:")
          affectedModules.foreach{m=>
            println("    " + m.descriptorPath)
            m.reasonsForBuild.foreach { reason => 
              println("        " + reason)  
            }
          }
          val results = input.affectedModules.map{affMod=>
            val prevState = input.moduleStates.find(_.descriptorPath == affMod.descriptorPath)
  			    doBuild(affMod.descriptorPath, data, prevState, out)
          } 
          val affectedByThisBuildPass = results.flatMap{result=> 
            result.depsPathsToBuild.map{pathToBuild=> 
              AffectedModule(pathToBuild, Seq("Dependency " + result.status.descriptorPath + " was built"))}
          }
          
          val nextPass = BuildPass(affectedByThisBuildPass, results.map(_.status))
          
          if(nextPass.affectedModules.isEmpty){
            Stream.cons(nextPass, Stream.Empty)
          }else{
            println("Still more deps to build")
            Stream.cons(nextPass, buildAffected(nextPass))
          }
        }
        
        
        
        val buildPasses = buildAffected(BuildPass(
          affectedModules = changesRequiringRebuild,
          moduleStates = previousBuild.modulesStatus
        ))
        
        val finalPass = buildPasses.last
        
        val result = BuildResult(
            modulesStatus = finalPass.moduleStates)
        
            
        // TODO: need to fold over the full set of module states, but we're not for some reason
        result
      }
  
  
    case class DoBuildResult(status:ModuleStatus, depsPathsToBuild:Seq[String])
    /**
     * fold(moduleToBuild, previousBuildState, previousFsState, listOfDependenciesToBuild) => newBuildState, newListOfDependenciesToBuild
     */
    def doBuild(descriptorPath:String, data:Data, maybePrevState:Option[ModuleStatus], out:PrintStream):DoBuildResult = {
      println(s"""#####################################
                 |## Building $descriptorPath """.stripMargin)
      val paths = Paths(descriptorPath)

      val m = parseDescriptor(paths.moduleDescriptorFile.getAbsolutePath)
      val label = m.id
      
      val dependencyTree = time("Resolving dependencies for " + label, out){
	      fullyResolveDependencies(m)
      }
      
      val dependencies = time("processing dependency tree", out){
        dependencyTree.flatten
      }
      
      val errors = time("building " + label, out){
      	val buildMechanism = buildMechanisms(m.build)
				buildMechanism.build(paths, dependencyTree, dependencies, m, maybePrevState, out)
      }
	    
      DoBuildResult(
          ModuleStatus(descriptorPath, maybeDescriptor=Some(m), errors=errors),
    	    buildDownstreamDependencies(m.id, data, out))
          
     
    }
    
    def buildDownstreamDependencies(updatedModuleId:ModuleId, data:Data, out:PrintStream):Seq[String] = {
      data.moduleDescriptors.filter{descriptorPath=>
        val m = parseDescriptor(descriptorPath)
  	    val dependencyTree = fullyResolveDependencies(m)
  	    val dependencies = dependencyTree.flatten
  	    
  	    dependencies.exists { d => 
  	      if(updatedModuleId == d.spec.module){
  	        out.println(s""" I've just built a module (${updatedModuleId}) that is a dependency of another module (${m.id}).  Rebuilding the latter...""")
  	        true
  	      }else false
  	    }
      }
    }
    
    
    def time[T](name:String, out:PrintStream)(fn: =>T):T = {
        val start = System.currentTimeMillis()
        out.println(name)
        val t = fn
        val end = System.currentTimeMillis()
        val seconds = (end - start) /1000.0
        out.println("Finished - " + seconds + " seconds")
        t
    }
}