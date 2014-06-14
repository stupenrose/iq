package org.iqbuild
import org.scalatest.FunSuite
import java.net.URL
import java.io.File
import org.apache.commons.io.IOUtils
import org.apache.commons.io.FileUtils

class DependencyResolverTest extends FunSuite {
  
  test("fetches a fully specified dependency with no transitive dependencies"){
    // given
    
    val http = new MockFetcher(Map(
    	"http://host.com/foo/bar/1.0/bar-1.0.pom" -> 
    	      <project>
    			<groupId>foo</groupId>
    			<artifactId>bar</artifactId>
    			<version>1.0</version>
    	      </project>
    
    ))
    
    val testSubject = new DependencyResolver(cache=http, mavenCentral="http://host.com/")
    val moduleDescriptor = ModuleDescriptor(
    							id=ModuleId("group", "name"), 
    							build="jar", 
    							deps=Seq(DependencySpec(ModuleId("foo", "bar"), Some("1.0"))))		
    // when
    val result = testSubject.resolveDependencies(moduleDescriptor)
    
    // then
    assert(result == DependencyResolutionResult(Seq(ResolvedDependency(url="http://host.com/foo/bar/1.0/bar-1.0.jar", spec=DependencySpec( ModuleId("foo", "bar"), Some("1.0")), transitives=Seq()))))
  }
  
  
  test("fetches the latest version when no version is specified"){
    // given
    
    val http = new MockFetcher(Map(
    	"http://host.com/foo/bar/maven-metadata.xml" -> 
    		<metadata>
                <versioning>
                    <latest>2.0</latest>
                </versioning>
            </metadata>,
        "http://host.com/foo/bar/2.0/bar-2.0.pom" -> 
    	      <project>
    			<groupId>foo</groupId>
    			<artifactId>bar</artifactId>
    			<version>2.0</version>
    	      </project>
    ))
    
    val testSubject = new DependencyResolver(cache=http, mavenCentral="http://host.com/")
    val moduleDescriptor = ModuleDescriptor(
    							id=ModuleId("group", "name"), 
    							build="jar", 
    							deps=Seq(DependencySpec(ModuleId("foo", "bar"),version = None)))		
    // when
    val result = testSubject.resolveDependencies(moduleDescriptor)
    
    // then
    assert(result == DependencyResolutionResult(Seq(ResolvedDependency(url="http://host.com/foo/bar/2.0/bar-2.0.jar", spec=DependencySpec(ModuleId("foo", "bar"), Some("2.0")), transitives=Seq()))))
  }
  
  
  test("fetches transitive dependencies"){
    // given
    val http = new MockFetcher(Map(
        "http://host.com/alpha/alpha/1.0/alpha-1.0.jar" -> "fake jar contents", 
        "http://host.com/alpha/alpha/1.0/alpha-1.0.pom" -> 
    	      <project>
    			<groupId>alpha</groupId>
    			<artifactId>alpha</artifactId>
    			<version>1.0</version>
        		<dependencies>
        			<dependency>
        			    <groupId>beta</groupId>
        			    <artifactId>beta</artifactId>
        			    <version>2.0</version>
        			</dependency>
        		</dependencies>
    	      </project>,
        "http://host.com/beta/beta/2.0/beta-2.0.jar" -> "fake jar contents", 
        "http://host.com/beta/beta/2.0/beta-2.0.pom" -> 
    	      <project>
    			<groupId>alpha</groupId>
    			<artifactId>alpha</artifactId>
    			<version>1.0</version>
        		<dependencies>
        			<dependency>
        			    <groupId>gamma</groupId>
        			    <artifactId>gamma</artifactId>
        			    <version>3.0</version>
        			</dependency>
        		</dependencies>
    	      </project>,
        "http://host.com/gamma/gamma/3.0/gamma-3.0.jar" -> "fake jar contents", 
        "http://host.com/gamma/gamma/3.0/gamma-3.0.pom" -> 
    	      <project>
    			<groupId>gamma</groupId>
    			<artifactId>gamma</artifactId>
    			<version>3.0</version>
    	      </project>
    ))
    
    val testSubject = new DependencyResolver(cache=http, mavenCentral="http://host.com/")
    val moduleDescriptor = ModuleDescriptor(
    							id=ModuleId("group", "name"), 
    							build="jar", 
    							deps=Seq(DependencySpec(ModuleId("alpha", "alpha"),version = Some("1.0"))))
    
    // when
    val result = testSubject.resolveDependencies(moduleDescriptor)
    
    // then
    assert(result  == 
      DependencyResolutionResult(Seq(
        ResolvedDependency(
            spec=DependencySpec(ModuleId("alpha", "alpha"), Some("1.0")), 
            url="http://host.com/alpha/alpha/1.0/alpha-1.0.jar",
        	transitives=Seq(
        		ResolvedDependency(
        		    url="http://host.com/beta/beta/2.0/beta-2.0.jar",
        		    spec=DependencySpec(ModuleId("beta", "beta"), Some("2.0")),
        		    transitives=Seq(
        		        ResolvedDependency(
        		            url="http://host.com/gamma/gamma/3.0/gamma-3.0.jar",
        		            spec=DependencySpec(ModuleId("gamma", "gamma"), Some("3.0")),
        		            transitives=Seq()
        		        )
        		    )
        		)
        	)
        ))))
    
  }
  
  
  test("understands maven's <dependencyManagement> & pom inheritance mechanisms"){
    // given
    val http = new MockFetcher(Map(
        "http://host.com/parent/parent/1.0/parent-1.0.jar" -> "fake jar contents", 
        "http://host.com/parent/parent/1.0/parent-1.0.pom" -> 
    	      <project>
    			<groupId>parent</groupId>
    			<artifactId>parent</artifactId>
    			<version>1.0</version>
        		<dependencyManagement>
        			<dependencies>
        				<dependency>
        				    <groupId>lib</groupId>
        				    <artifactId>lib</artifactId>
        				    <version>2.0</version>
        				</dependency>
        			</dependencies>
        		</dependencyManagement>
    	      </project>,
        "http://host.com/child/child/1.0/child-1.0.jar" -> "fake jar contents", 
        "http://host.com/child/child/1.0/child-1.0.pom" -> 
    	      <project>
        		<parent>
    				<groupId>parent</groupId>
    				<artifactId>parent</artifactId>
    				<version>1.0</version>
        		</parent>
    			<groupId>child</groupId>
    			<artifactId>child</artifactId>
    			<version>1.0</version>
        		<dependencies>
        			<dependency>
        			    <groupId>lib</groupId>
        			    <artifactId>lib</artifactId>
        			</dependency>
        		</dependencies>
    	      </project>,
    	      
        "http://host.com/lib/lib/2.0/lib-2.0.pom" -> 
    	      <project>
    			<groupId>lib</groupId>
    			<artifactId>lib</artifactId>
    			<version>2.0</version>
    	      </project>
    ))
    
    val testSubject = new DependencyResolver(cache=http, mavenCentral="http://host.com/")
    val moduleDescriptor = ModuleDescriptor(
    							id=ModuleId("group", "name"), 
    							build="jar", 
    							deps=Seq(DependencySpec(ModuleId("child", "child"),version = Some("1.0"))))
    
    // when
    val result = testSubject.resolveDependencies(moduleDescriptor)
    
    // then
    assert(result  == 
      DependencyResolutionResult(Seq(
        ResolvedDependency(
            spec=DependencySpec(ModuleId("child", "child"), Some("1.0")), 
            url="http://host.com/child/child/1.0/child-1.0.jar",
        	transitives=Seq(
        		ResolvedDependency(
        		    url="http://host.com/lib/lib/2.0/lib-2.0.jar",
        		    spec=DependencySpec(ModuleId("lib", "lib"), Some("2.0")),
        		    transitives=Seq()
        		)
        	)
        ))))
    
  }
  
  
} 


