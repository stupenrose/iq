package org.iqbuild

case class FilesystemChanges(
    descriptorPath:String, 
    maybePrev:Option[FSNode], 
    currentState:FSNode, 
    deltas:Seq[(FSNode, FSNode)]) {
  
  def needsBuild = (maybePrev, deltas) match {
    case (None, _) => true
    case (_, deltas) if !deltas.isEmpty => {
//      out.println(s"Something changed:")
//		  out.println(deltas.map{beforeAndAfter=>
//		  	val (before, after) = beforeAndAfter
//		  	
//		  	val timeDiff = if(before.lastModified != after.lastModified ){
//		  	  before.lastModified + " vs " + after.lastModified
//		  	}else ""
//		  	
//		  	val fileDiff = if(before.isFile!=after.isFile ){
//		  	  before.isFile  + " vs " + after.isFile
//		  	}else ""
//		  	
//		  	before.path + " (" + timeDiff + fileDiff + ")"
//		  }.mkString("    ", "\n    ", "\n"))
      true
    }
    case _ => false
  }
}
