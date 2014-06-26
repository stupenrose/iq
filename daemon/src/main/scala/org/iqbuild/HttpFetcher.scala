package org.iqbuild

import java.io.File
import java.net.URL

trait HttpFetcher {
  def get(u:URL):File
}
