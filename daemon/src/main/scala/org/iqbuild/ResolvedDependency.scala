package org.iqbuild

import java.io.File

case class PartiallyResolvedDependency(url:String, spec:DependencySpec, transitives:Seq[DependencySpec])

case class ResolvedDependency(url:String, spec:DependencySpec, transitives:Seq[ResolvedDependency]){
  def flatten():Seq[ResolvedDependency] = {
    transitives.flatMap {d=>
        List(d) ::: transitives.flatMap(_.flatten).toList
    }
  }
  
}