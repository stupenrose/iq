package org.iqbuild

import org.scalatest.FunSuite

class ModuleDescriptorTest extends FunSuite {

  private def prettyText(t:String) = t.stripMargin('|').trim

  test("every variant"){
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
     assert(ModuleId(group="us.penrose", name="myapp") === m.id)
     assert("jar" === m.build)
     assert(4 === m.deps.size)
     assert(DependencySpec(module=ModuleId(group="us.penrose", name="mylibA")) === m.deps(0))
     assert(DependencySpec(module=ModuleId(group="commons-lang", name="commons-lang"), version=Some("1.2.2")) === m.deps(1))
     assert(DependencySpec(module=ModuleId(group="us.penrose", name="htmldocs"), inclusion=Some("unzipped")) === m.deps(2))
     assert(DependencySpec(module=ModuleId(group="org.codehaus.mojo", name="animal-sniffer-maven-plugin")) === m.deps(3))
  }

  test("minimal descriptor"){
    // given
    val text = prettyText("""
         |id foo:bar
         |build jar""")

     // when
     val m = ModuleDescriptor.parse(text)

     // then
     assert(ModuleId(group="foo", name="bar") === m.id)
     assert("jar" === m.build)
     assert(0 === m.deps.size)
  }

  test("ignores comments"){
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
     assert(ModuleId(group="foo", name="bar") === m.id)
     assert("jar" === m.build)
     assert(0 === m.deps.size)
  }
}
