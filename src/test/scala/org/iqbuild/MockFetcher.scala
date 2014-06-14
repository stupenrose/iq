package org.iqbuild

import java.net.URL
import java.io.File
import org.apache.commons.io.FileUtils

class MockFetcher(resources: Map[String, AnyRef]) extends HttpFetcher {
  override def get(u: URL) = {
    println("GET " + u)
    resources.get(u.toExternalForm()) match {
      case None => throw new Exception("Not mocked: " + u)
      case Some(content) => {
        val f = File.createTempFile("foo", ".pom")
        FileUtils.write(f, content.toString, "UTF8")
        f
      }
    }
  }
}