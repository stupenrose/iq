package org.iqbuild

import java.io.File
import java.io.PrintStream

trait BuildMechanism {

	def build(state:FSNode, targetDir:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor, out:PrintStream)
}