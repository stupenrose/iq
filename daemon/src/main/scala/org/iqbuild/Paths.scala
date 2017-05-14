package org.iqbuild

import java.io.File

case class Paths(val descriptorPath:String){
  val moduleDescriptorFile = new File(descriptorPath)
  val dir = moduleDescriptorFile.getParentFile() 
  val targetDir = new File(dir, "target")
  val pollingCache = new File(targetDir, "fs.json")
  val result = new File(targetDir, "result")
  pollingCache.getParentFile.mkdirs()
}