package org.iqbuild

import java.io.File

object Constants {
  val INTERNAL_VERSION = "LATEST_INTERNAL_VERSION"
}

case class PartiallyResolvedDependency(url:String, spec:DependencySpec, transitives:Seq[DependencySpec])

case class ResolvedDependency(url:String, spec:DependencySpec, transitives:Seq[ResolvedDependency])