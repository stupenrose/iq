package org.iqbuild

import java.io.File
import java.io.PrintStream

trait BuildMechanism {
	def build(moduleDirectory:String, targetDir:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor, out:PrintStream)
}