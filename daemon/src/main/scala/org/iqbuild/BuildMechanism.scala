package org.iqbuild

import java.io.File

trait BuildMechanism {

	def build(state:FSNode, targetDir:File, dependencies:Seq[ResolvedDependency], m:ModuleDescriptor)
}