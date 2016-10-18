package org.iqbuild

case class DependencyResolutionResult (dependencies:Seq[ResolvedDependency]) {
  
    def flatten(i:ResolvedDependency):Seq[ResolvedDependency] = {
      i.transitives.flatMap {d=>
          List(d) ::: i.transitives.flatMap(flatten).toList
      }
    }
  
	def flatten():Seq[ResolvedDependency] = {
	  println("Making list")
	  val list = dependencies.flatMap{d=>
	    List(d) ::: flatten(d).toList
	  }
	  
	  println("Making histogram")
	  
	  val versionsByModuleId = list.foldLeft(Map[ModuleId, Seq[String]]()){(accum, f)=>
	    val moduleId = f.spec.module
	    val t = accum.getOrElse(moduleId, Seq())
	    val x = f.spec.version.get +: t.toList
	    
	  	accum + (f.spec.module -> x)
	  }
	  
	  
	  println("removing dups")
	  val minusDups = versionsByModuleId.keys.toList.foldLeft(list.toList){(accum, moduleId)=>
	    val nearest = nearestVersion(moduleId, dependencies, 0)
	    
	    val versionsToCull = versionsByModuleId.get(moduleId) match {
	      case None=>Seq()
	      case Some(versions) => versions.filter(_!=nearest)
	    }
	    
	    accum.filter{a=>
	      a.spec .module != moduleId || !versionsToCull.contains(a.spec.version.get)}
	  }
	  
	  val result = minusDups
	  
	  println("cleanup")
	  val too = result.map{d=>
	  	d.copy(transitives=Seq())
	  }
	  
	  println("distinct")
	  too.distinct
	}
	
	def nearestVersion(moduleId:ModuleId, transitives:Seq[ResolvedDependency], level:Int):String = {
//	  println(s"level $level : " + transitives.map(_.spec.module).mkString(" "))
	  val matchesAtThisLevel = transitives.filter(_.spec.module == moduleId) 
	  
	  if(matchesAtThisLevel.isEmpty){
	    val nextLowerLevel = transitives.flatMap(_.transitives )
	    nearestVersion(moduleId, nextLowerLevel, level + 1)
	  }else{
	    matchesAtThisLevel.head.spec.version.get
	  }
  }
}