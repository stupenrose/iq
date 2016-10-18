package org.iqbuild

import org.scalatest.FunSuite

class DependencyResolutionResultTest extends FunSuite {

  test("flattensTheTree"){
	    // given
	    val testSubject = DependencyResolutionResult(Seq(
	    			dep("a", "a", "1.0",
					  dep("aa", "aa", "2.0",
					      dep("aaa", "aaa", "3.0"))),
	    			dep("b", "b", "1.0",
					  dep("bb", "bb", "2.0",
					      dep("bbb", "bbb", "3.0")))))
					
	    // when
	    val result = testSubject.flatten()
	    
	    // then
	    assert(result  == Seq(
	        dep("a", "a", "1.0"),
		    dep("aa", "aa", "2.0"),
			dep("aaa", "aaa", "3.0"),
	        dep("b", "b", "1.0"),
		    dep("bb", "bb", "2.0"),
			dep("bbb", "bbb", "3.0")
	    ))
  }
  
  test("duplicatesAreRemoved"){
	    // given
	    val testSubject = DependencyResolutionResult(Seq(
	        dep("a", "a", "1.0",
	            dep("z", "z", "100")),
	        dep("b", "b", "1.0"),
	        dep("z", "z", "100"),
	        dep("c", "c", "1.0")
	    ))
					
	    // when
	    val result = testSubject.flatten()
	    
	    // then
	    assert(result  == Seq(
	        dep("a", "a", "1.0"),
          dep("z", "z", "100"),
	        dep("b", "b", "1.0"),
	        dep("c", "c", "1.0")
	    ))
  }
  
  
//  test("handles internal resolutions"){
//	    // given
//	    val testSubject = DependencyResolutionResult(Seq(
//	        dep("a", "a", "1.0"),
//	        dep("b", "b",Constants.INTERNAL_VERSION)
//	    ))
//					
//	    // when
//	    val result = testSubject.flatten()
//	    
//	    // then
//	    assert(result  == Seq(
//	        dep("a", "a", "1.0"),
//	        dep("b", "b", Constants.INTERNAL_VERSION)
//	    ))
//  }
  
  
  test("when transitive dependencies conflict, 'closest' dependency in the tree wins"){
    // given
    val testSubject = DependencyResolutionResult(Seq(
    			dep("foo", "lib", "1.0",
				  dep("util", "util", "2.0")
				),
				dep("util", "util", "1.0")))
				
    // when
    val result = testSubject.flatten()
    
    // then
    assert(result  == Seq(
    			dep("foo", "lib", "1.0"),
				dep("util", "util", "1.0")))
				
  }
  private def dep(group:String, name:String, version:String, dependencies:ResolvedDependency*) = {
    ResolvedDependency(
    					url=s"http://host.com/$group/$name/$version/$name-$version.jar", 
    					spec=DependencySpec(ModuleId(group, name), Some(version)),
    					transitives=dependencies)
  }
}