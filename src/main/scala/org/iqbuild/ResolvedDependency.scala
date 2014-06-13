package org.iqbuild

import java.io.File

case class ResolvedDependency(path:File, spec:DependencySpec)