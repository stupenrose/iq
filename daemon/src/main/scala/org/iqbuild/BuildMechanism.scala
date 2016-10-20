package org.iqbuild

import java.io.File
import java.io.PrintStream
import org.iqbuild.Main.Paths

trait BuildMechanism {
	def build(paths:Paths, tree:DependencyResolutionResult, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor, out:PrintStream)
}