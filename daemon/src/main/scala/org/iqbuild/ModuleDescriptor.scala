package org.iqbuild

object ModuleId {
  def parse(t:String) = {
     val idParts = t.substring("id ".length()).split(":").toSeq
    
     ModuleId(idParts(0), idParts(1))
    
  }
}
case class ModuleId (group:String, name:String)

case class DependencySpec(module:ModuleId, version:Option[String] = None, inclusion:Option[String] = None)

object ModuleDescriptor{
  def parse(text:String): ModuleDescriptor = {
    val lines = text.trim.split("""\r\n|\r|\n""")
    				.filter(!_.trim.startsWith("//")) // minus comments
    				.toSeq
    
    val firstLine = lines.head
    if(!firstLine.startsWith("id ")){
      throw new Exception("First line should be an id, but was: " + firstLine)
    }
    
    val idParts = lines.head.substring("id ".length()).split(":").toSeq
    
    val id = ModuleId(idParts(0), idParts(1))
    
    val depsLines = lines.indexWhere(_.trim().startsWith("deps")) match {
      case -1 => Seq()
      case n => lines.slice(n+1, lines.size).filter(!_.trim().isEmpty())
    }
    
    val deps = depsLines.map{line=>
      val i = line.indexOf("(")
      
      val (ids, inclusionSpec) = i match {
        case -1=> (line, None)
        case _=> (line.substring(0, i), Some(line.substring(i+1, line.length()-1)))
      }
      
      val parts = ids.split(":").map(_.trim)
      
      val moduleAndVersion = parts match {
        case Array("~", a) => (ModuleId(id.group, parts(1)), None)
        case Array(label, "", version) => (ModuleId(label, label), Some(version))
        case Array(string1, string2) => (ModuleId(group=string1, name=string2), None)
        case _ => throw new Exception("Unable to parse dependency: " + line) 
      }
      DependencySpec(module=moduleAndVersion._1, version=moduleAndVersion._2, inclusion=inclusionSpec)
    }
    
    val build = lines.find(_.startsWith("build ")).get.substring("build ".length)
    
    ModuleDescriptor(id=id, build = build, deps=deps)
  }
}

case class ModuleDescriptor (
    id:ModuleId, build:String, deps:Seq[DependencySpec])
