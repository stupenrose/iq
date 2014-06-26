package org.iqbuild

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import java.io.InputStream
import java.io.OutputStream
import java.io.File
import java.io.FileInputStream

object Jackson {

  val jackson:ObjectMapper = {
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    mapper
  }
  
  def parseJson[T](is:File)(implicit manifest:Manifest[T]):T = parseJson(new FileInputStream(is))
  
  def parseJson[T](is:InputStream)(implicit manifest:Manifest[T]):T = {
    jackson.readValue[T](is, manifest.erasure.asInstanceOf[Class[T]])
  }
  
}