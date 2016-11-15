package org.iqbuild

import java.io.File
import java.io.PrintStream
import org.iqbuild.Main.Paths
import org.iqbuild.Main.ModuleBuildError
import org.iqbuild.Main.ModuleStatus

trait BuildMechanism {
  
      /**
       * This is essentially a fold{(previousState, changes)=>newState}
       */
	def build(
	          /*
	           * Input
	           */
	          paths:Paths, 
	          tree:DependencyResolutionResult, 
	          dependencies:Seq[ResolvedDependency], 
	          m:ModuleDescriptor, 
	          maybePreviousState:Option[ModuleStatus],
	          
	          /*
	           * Output
	           */
	          
	          out:PrintStream
	          ):Seq[ModuleBuildError]
}