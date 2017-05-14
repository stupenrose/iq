package org.iqbuild

import java.io.PrintStream
import scala.util.Try

case class ReactorState(fsChanges:Seq[FilesystemChanges], data:Data)

class BuildReactor(
    val buildMechanisms:Map[String, BuildMechanism], 
    val fullyResolveDependencies:ModuleDescriptor=>DependencyResolutionResult,
    val parseDescriptor:String=>ModuleDescriptor, 
    val out:PrintStream) {
  
  
  
  def allBuildResults(externalChanges:Stream[ReactorState]):Stream[BuildResult] = {
    
    def nextBuild(externalChanges:Stream[ReactorState],  previousBuild:BuildResult):Stream[BuildResult] = {
      val remainder = externalChanges.headOption match {
        case Some(changes) => {
          val result = respondToFilesystemChanges(out, previousBuild, changes)
          val t = externalChanges.tail
          Stream.cons(result, nextBuild(t, result))
        }

        case None => {
          Stream.Empty
        }
      }
      remainder
    }
    
    val initialState = BuildResult(modulesStatus = Seq())
    
    nextBuild(externalChanges, initialState)
  }
  
  def respondToFilesystemChanges(out:PrintStream, previousBuild:BuildResult, state:ReactorState):BuildResult = {
        val data = state.data
        val fsChanges = state.fsChanges
        val filesChanged = fsChanges.flatMap(_.deltas).map(_._1.path)
        println(filesChanged.size + " files changed: " + filesChanged.mkString("\n    ", ",\n    ", "\n"))
        val moduleDescriptors = data.moduleDescriptors
        
        case class AffectedModule(descriptorPath:String, reasonsForBuild:Seq[String])
        
        val changesRequiringRebuild = fsChanges.flatMap{changes=>
          val maybeReasonToBuild = if(changes.needsBuild){
            Some("whatever")//TODO: Should be "files changed"
          }else{
            None
          }
          
          maybeReasonToBuild.map{reason=> AffectedModule(changes.descriptorPath, Seq(reason))}
        }
        
        
        case class BuildPass(affectedModules:Seq[AffectedModule], moduleStates:Seq[ModuleStatus], maybePreviousPass:Option[BuildPass])
        
        def buildAffected(input:BuildPass):Stream[BuildPass] = {
          
          val affectedModules = input.affectedModules//deDupedReasoning
          
          def hasBuilt(descriptorPath:String, pass:BuildPass):Boolean = {
            val x = pass.affectedModules.exists { x => x.descriptorPath == descriptorPath }
            val y = pass.maybePreviousPass.map(hasBuilt(descriptorPath, _)).getOrElse(false)
            x || y
          }
          
          val results = input.affectedModules.map{affMod=>
            
            val hasAlreadyBeenBuilt = input.maybePreviousPass.map(hasBuilt(affMod.descriptorPath, _)).getOrElse(false)
            if(hasAlreadyBeenBuilt){
              println("Already built!")
              DoBuildResult(status = ModuleStatus(
                    descriptorPath = affMod.descriptorPath, 
                    maybeDescriptor = None,
                    errors = Seq(ModuleBuildError(
                          path = affMod.descriptorPath, 
                          where = "Dependencies", 
                          description = "Circular dependency"))), 
                    depsPathsToBuild = Seq())
            }else{
              val prevState = input.moduleStates.find(_.descriptorPath == affMod.descriptorPath)
              doBuild(affMod.reasonsForBuild, affMod.descriptorPath, data, prevState, out)
            }
  			    
          } 
          val affectedByThisBuildPass = results.flatMap{result=> 
            result.depsPathsToBuild.map{pathToBuild=> 
              AffectedModule(pathToBuild, Seq())}
          }
          
          val nextPass = BuildPass(affectedByThisBuildPass, results.map(_.status), Some(input))
          
          if(nextPass.affectedModules.isEmpty){
            Stream.cons(nextPass, Stream.Empty)
          }else{
            println("Still more deps to build")
            Stream.cons(nextPass, buildAffected(nextPass))
          }
        }
        
        val buildPasses = buildAffected(BuildPass(
          affectedModules = changesRequiringRebuild,
          moduleStates = Seq(),
          None
        ))
        
        val finalPass = buildPasses.last
        
        val result = BuildResult(
            modulesStatus = finalPass.moduleStates)
        
        // TODO: need to fold over the full set of module states, but we're not for some reason
        result
      }
  
  
    case class DoBuildResult(status:ModuleStatus, depsPathsToBuild:Seq[String])
    
    def doBuild(buildReasons:Seq[String], descriptorPath:String, data:Data, maybePrevState:Option[ModuleStatus], out:PrintStream):DoBuildResult = {
      println(s"""#####################################
                 |## Building $descriptorPath """.stripMargin)
      val paths = Paths(descriptorPath)
      
      case class Foo(m:ModuleDescriptor, dependencyTree:DependencyResolutionResult, dependencies:Seq[ResolvedDependency])
      val fooByPath = data.moduleDescriptors.map{path=> 
        val descriptor = parseDescriptor(path)
        val dependencyTree = time("Resolving dependencies for " + descriptor.id, out){
  	      fullyResolveDependencies(descriptor)
        }
        val dependencies = time("processing dependency tree", out){
          dependencyTree.flatten
        }
        path->Foo(descriptor, dependencyTree, dependencies)}.toMap
      
      val Foo(m, dependencyTree, dependencies) = fooByPath(paths.moduleDescriptorFile.getAbsolutePath)
      val label = m.id
      
      
      
      val errors = time("building " + label, out){
        val otherModules = fooByPath.filterNot{case (path, _) => path == paths.moduleDescriptorFile.getAbsolutePath}
        println(otherModules.size + " vs " + fooByPath.size)
        
        val prettyString = Jackson.jackson.writer.withDefaultPrettyPrinter().writeValueAsString _
        
        	val buildMechanism = buildMechanisms(m.build)
          println("No Error!!!")
  				buildMechanism.build(buildReasons, paths, dependencyTree, dependencies, m, maybePrevState, out)
      }
	    
      DoBuildResult(
          ModuleStatus(descriptorPath, maybeDescriptor=Some(m), errors=errors),
    	    findDownstreamDependencies(m.id, data, out))
          
     
    }
    
    def findDownstreamDependencies(updatedModuleId:ModuleId, data:Data, out:PrintStream):Seq[String] = {
      
      data.moduleDescriptors.filter{descriptorPath=>
        val m = parseDescriptor(descriptorPath)
  	    val dependencyTree = fullyResolveDependencies(m)
  	    val dependencies = dependencyTree.flatten
  	    println(s"BUILDING ${dependencies.size} DEPENDENCIES")
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