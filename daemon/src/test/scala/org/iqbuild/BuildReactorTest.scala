package org.iqbuild

import org.scalatest.FunSuite
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.collection.mutable.ListBuffer

class BuildReactorTest extends FunSuite {
  
  test("When there is nothing to build, nothing is built"){
    
    // input
    val bytesOut = new ByteArrayOutputStream
    val out = new PrintStream(bytesOut)
    
    val reactor = new BuildReactor(
          buildMechanisms = Map(), 
          parseDescriptor = {i=> ???},
          fullyResolveDependencies = {i=> ???},
          out = out)
    
    val data = Data()
    
    val externalChanges = Stream(ReactorState(fsChanges = Seq(), data = data))
    
    // when
    val finalState = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(finalState == BuildResult(Seq()))
  }
  
  test("Invokes the build when files change"){
    
    // input
    val bytesOut = new ByteArrayOutputStream
    val out = new PrintStream(bytesOut)
    
    val build = new FakeBuildMechanism(errors=Seq())
    val descriptor = ModuleDescriptor(
                                id = ModuleId(group = "mygroup", name="mymodule"), 
                                build = "funky", 
                                deps = Seq())
    val reactor = new BuildReactor(
          buildMechanisms = Map("funky"->build), 
          parseDescriptor = {path=> descriptor},
          fullyResolveDependencies = {m:ModuleDescriptor=>
            DependencyResolutionResult(Seq())},
          out = out)
    val descriptorPath = "/path/to/project/module.iq"
    val data = Data(Seq(descriptorPath))
    
    val fsChanges = FilesystemChanges(descriptorPath = descriptorPath, 
        maybePrev = None, 
        currentState = FSNode(
                  path = descriptorPath, 
                  lastModified = 0, 
                  isFile = true, 
                  children = Seq()),
        deltas = Seq()) 
    val externalChanges = Stream(ReactorState(fsChanges = Seq(fsChanges), data = data))
    
    // when
    val finalState = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(build.invocations.size == 1)
    assert(finalState == BuildResult(List(ModuleStatus(
          descriptorPath = descriptorPath,
          maybeDescriptor = Some(descriptor),
          errors = List()))))
  }
  
  test("Doesn't invoke the build when files stay the same"){
    
    // input
    val bytesOut = new ByteArrayOutputStream
    val out = new PrintStream(bytesOut)
    
    val build = new FakeBuildMechanism(errors=Seq())
    val descriptor = ModuleDescriptor(
                                id = ModuleId(group = "mygroup", name="mymodule"), 
                                build = "funky", 
                                deps = Seq())
    val reactor = new BuildReactor(
          buildMechanisms = Map("funky"->build), 
          parseDescriptor = {path=> descriptor},
          fullyResolveDependencies = {m:ModuleDescriptor=>
            DependencyResolutionResult(Seq())},
          out = out)
    val descriptorPath = "/path/to/project/module.iq"
    val data = Data(Seq(descriptorPath))
    
    val externalChanges = Stream(ReactorState(fsChanges = Seq(), data = data))
    
    // when
    val finalState = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(build.invocations.size == 0)
    assert(finalState == BuildResult(Seq()))
  }
  
  class FakeBuildMechanism(val errors:Seq[ModuleBuildError]) extends BuildMechanism {
    val invocations = ListBuffer[String]()
  	def build(
  	          paths:Paths, 
  	          tree:DependencyResolutionResult, 
  	          dependencies:Seq[ResolvedDependency], 
  	          m:ModuleDescriptor, 
  	          maybePreviousState:Option[ModuleStatus],
  	          out:PrintStream
  	          ):Seq[ModuleBuildError] = {
  	  
      println("Fake Build Invoked")
  	  invocations.append("invoked")
  	  
  	  errors
  	}
  }
  
}