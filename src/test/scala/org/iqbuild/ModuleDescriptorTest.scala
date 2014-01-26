package org.iqbuild

import org.junit.Test
import org.junit.Assert._

class ModuleDescriptorTest {

  private def prettyText(t:String) = t.stripMargin('|').trim
  
  @Test
  def everyVariant(){
    // given
    val text = prettyText("""
         |id us.penrose:myapp
         |build jar
         |
         |deps (classpath jars)
         |
         | ~:mylibA
         | commons-lang::1.2.2
         | ~:htmldocs (unzipped)
         | org.codehaus.mojo:animal-sniffer-maven-plugin""")
     
     // when
     val m = ModuleDescriptor.parse(text)
     
     // then
     assertEquals(ModuleId(group="us.penrose", name="myapp"), m.id)
     assertEquals("jar", m.build)
     assertEquals(4, m.deps.size)
     assertEquals(DependencySpec(module=ModuleId(group="us.penrose", name="mylibA")), m.deps(0))
     assertEquals(DependencySpec(module=ModuleId(group="commons-lang", name="commons-lang")), m.deps(1))
     assertEquals(DependencySpec(module=ModuleId(group="us.penrose", name="htmldocs"), inclusion=Some("unzipped")), m.deps(2))
     assertEquals(DependencySpec(module=ModuleId(group="org.codehaus.mojo", name="animal-sniffer-maven-plugin")), m.deps(3))
  }
  
  @Test
  def minimalDescriptor(){
    // given
    val text = prettyText("""
         |id foo:bar
         |build jar""")
     
     // when
     val m = ModuleDescriptor.parse(text)
     
     // then
     assertEquals(ModuleId(group="foo", name="bar"), m.id)
     assertEquals("jar", m.build)
     assertEquals(0, m.deps.size)
  }
  
  @Test
  def ignoresComments(){
    // given
    val text = prettyText("""
         | // this is a comment
         | // this is a comment
         | // this isn't code, this is a comment
         |id foo:bar
         | // this is a comment.  ignore me
         |build jar
         | // this one too""")
     
     // when
     val m = ModuleDescriptor.parse(text)
     
     // then
     assertEquals(ModuleId(group="foo", name="bar"), m.id)
     assertEquals("jar", m.build)
     assertEquals(0, m.deps.size)
  }
}