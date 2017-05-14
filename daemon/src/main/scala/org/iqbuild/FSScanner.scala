package org.iqbuild

class FSScanner(guts:Guts) {
  
    def filesystemChangesOverTime:Stream[Seq[FilesystemChanges]] = {
      def scanForChanges():Stream[Seq[FilesystemChanges]]= {
        println("Scanning...")
        val moduleDescriptors = guts.data.moduleDescriptors
        val fsChanges = moduleDescriptors.map{descriptorPath=>
          val paths = Paths(descriptorPath)
          val maybePrev = if (paths.pollingCache.exists()) Some(Jackson.jackson.readValue(paths.pollingCache, classOf[FSNode])) else None 
		      val fs = FSNode.forPath(paths.dir, {f => 
		        f!=paths.targetDir && 
		        !f.getName.equals("pom.xml") && // hack!
		        !f.getName().startsWith(".")})
    			  val deltas = maybePrev match {
    			    case Some(prev) => fs.deltas(prev)
    			    case None => Seq()
    			  }
		      Jackson.jackson.writerWithDefaultPrettyPrinter().writeValue(paths.pollingCache, fs);
		      
		      if(!deltas.isEmpty){
		        println("Something changed")
		      }
	        Thread.sleep(200)
	         
		      FilesystemChanges(
		          descriptorPath = descriptorPath, 
		          maybePrev = maybePrev, 
		          currentState = fs, 
		          deltas = deltas)
        }
        Stream.cons(fsChanges, scanForChanges)
      }
      
      scanForChanges
    }
    
}