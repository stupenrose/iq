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
    val result = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(result.last == BuildResult(Seq()))
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
    val result = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(build.invocations.size == 1)
    assert(result.last == BuildResult(List(ModuleStatus(
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
    val result = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(build.invocations.size == 0)
    assert(result.last == BuildResult(Seq()))
  }
  
  test("Builds dependencies"){
    
    // input
    val bytesOut = new ByteArrayOutputStream
    val out = new PrintStream(bytesOut)
    
    val build = new FakeBuildMechanism(errors=Seq())
    
    val dependency = "/fake/path/dependency/module.iq" -> ModuleDescriptor(
                                id = ModuleId(group = "mygroup", name="a"), 
                                build = "funky", 
                                deps = Seq())
    val dependent = "/fake/path/dependent/module.iq" -> ModuleDescriptor(
                                id = ModuleId(group = "mygroup", name="b"), 
                                build = "funky", 
                                deps = Seq(DependencySpec(module=dependency._2.id)))
    
    val paths = Map(dependency, dependent)
                                
    val reactor = new BuildReactor(
          buildMechanisms = Map("funky"->build), 
          parseDescriptor = {path=> paths.get(path).getOrElse(throw new Exception("No Such File: " + path))},
          fullyResolveDependencies = {m:ModuleDescriptor=>
            
            val Some((path, _)) = paths.find{case (path, d) => d == m}
            
            DependencyResolutionResult(
                m.deps.map{d=>ResolvedDependency(url=path, d.copy(version = Some(Constants.INTERNAL_VERSION)), transitives=Seq())}
                )},
          out = out)
    val data = Data(paths.keys.toSeq)
    
    val changedPath = dependency._1
    val fsChanges = FilesystemChanges(descriptorPath = changedPath, 
        maybePrev = None, 
        currentState = FSNode(
                  path = changedPath, 
                  lastModified = 0, 
                  isFile = true, 
                  children = Seq()),
        deltas = Seq()) 
    val externalChanges = Stream(ReactorState(fsChanges = Seq(fsChanges), data = data))
    
    // when
    val result = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(build.invocations.size == 2)
    assert(build.invocations.map{bi=> bi.m} == ListBuffer(dependency._2, dependent._2))
    assert(result.last == BuildResult(List(
      ModuleStatus(
          descriptorPath = dependent._1,
          maybeDescriptor = Some(dependent._2),
          errors = List()))))
  }
  test("Builds transitive dependencies"){
    
    // input
    val bytesOut = new ByteArrayOutputStream
    val out = new PrintStream(bytesOut)
    
    val build = new FakeBuildMechanism(errors=Seq())
    
    val deepDependency = "/fake/path/deepDependency/module.iq" -> ModuleDescriptor(
                                id = ModuleId(group = "mygroup", name="a"), 
                                build = "funky", 
                                deps = Seq())
    val dependency = "/fake/path/dependency/module.iq" -> ModuleDescriptor(
                                id = ModuleId(group = "mygroup", name="b"), 
                                build = "funky", 
                                deps = Seq(DependencySpec(module=deepDependency._2.id)))
    val dependent = "/fake/path/dependent/module.iq" -> ModuleDescriptor(
                                id = ModuleId(group = "mygroup", name="c"), 
                                build = "funky", 
                                deps = Seq(DependencySpec(module=dependency._2.id)))
    
    val paths = Map(dependency, dependent, deepDependency)
                                
    val reactor = new BuildReactor(
          buildMechanisms = Map("funky"->build), 
          parseDescriptor = {path=> paths.get(path).getOrElse(throw new Exception("No Such File: " + path))},
          fullyResolveDependencies = {m:ModuleDescriptor=>
            
            val Some((path, _)) = paths.find{case (path, d) => d == m}
            
            DependencyResolutionResult(
                m.deps.map{d=>ResolvedDependency(url=path, d.copy(version = Some(Constants.INTERNAL_VERSION)), transitives=Seq())}
                )},
          out = out)
    val data = Data(paths.keys.toSeq)
    
    val changedPath = deepDependency._1
    val fsChanges = FilesystemChanges(descriptorPath = changedPath, 
        maybePrev = None, 
        currentState = FSNode(
                  path = changedPath, 
                  lastModified = 0, 
                  isFile = true, 
                  children = Seq()),
        deltas = Seq()) 
    val externalChanges = Stream(ReactorState(fsChanges = Seq(fsChanges), data = data))
    
    // when
    val result = reactor.blockUntilAllInputHasBeenProcessed(externalChanges)
    
    // then
    assert(build.invocations.size == 3)
    assert(build.invocations.map{bi=> bi.m} == ListBuffer(deepDependency._2, dependency._2, dependent._2))
    assert(result.last == BuildResult(List(
      ModuleStatus(
          descriptorPath = dependent._1,
          maybeDescriptor = Some(dependent._2),
          errors = List()))))
  }
  class FakeBuildMechanism(val errors:Seq[ModuleBuildError]) extends BuildMechanism {
    case class BuildInvocation(paths:Paths, 
  	          tree:DependencyResolutionResult, 
  	          dependencies:Seq[ResolvedDependency], 
  	          m:ModuleDescriptor, 
  	          maybePreviousState:Option[ModuleStatus],
  	          out:PrintStream)
    val invocations = ListBuffer[BuildInvocation]()
  	def build(
  	          paths:Paths, 
  	          tree:DependencyResolutionResult, 
  	          dependencies:Seq[ResolvedDependency], 
  	          m:ModuleDescriptor, 
  	          maybePreviousState:Option[ModuleStatus],
  	          out:PrintStream
  	          ):Seq[ModuleBuildError] = {
  	  
      println("Fake Build Invoked")
  	  invocations.append(BuildInvocation(paths=paths, tree=tree, dependencies=dependencies,
  	      m = m,
  	      maybePreviousState = maybePreviousState,
  	      out = out))
  	  
  	  errors
  	}
  }
  
}