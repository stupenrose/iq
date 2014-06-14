package org.iqbuild.maven

import org.scalatest.FunSuite
import org.iqbuild.ModuleId
import org.iqbuild.MockFetcher

class PomStuffTest extends FunSuite { 
    test("supports the typical maven properties"){
      // given
      val coords = ModuleIdAndVersion(ModuleId("foo", "bar"), "1.0")
      val http = new MockFetcher(Map(
        "http://host.com/foo/bar/1.0/bar-1.0.pom" -> 
        <project>
            <groupId>foo</groupId>
            <artifactId>bar</artifactId>
            <version>1.0</version>
            <dependencies>
                <dependency>
                   <groupId>${{project.groupId}}</groupId>
                   <artifactId>a</artifactId>
                   <version>${{project.version}}</version>
                </dependency>
                <dependency>
                   <groupId>b</groupId>
                   <artifactId>b</artifactId>
                   <version>${{pom.version}}</version>
                </dependency>
            </dependencies>
        </project>))
      
      // when
      val result = new PomStuff(
                          coords=coords, 
                          mavenCentral="http://host.com/", 
                          cache=http)
      // then
      assert(result.dependencies == Seq(
                                  ModuleIdAndVersion(ModuleId("foo","a"),"1.0"), 
                                  ModuleIdAndVersion(ModuleId("b","b"),"1.0")))
    }
}