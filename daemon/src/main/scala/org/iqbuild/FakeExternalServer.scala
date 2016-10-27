package org.iqbuild

import org.httpobjects.jetty.HttpObjectsJettyHandler
import org.httpobjects.HttpObject
import org.httpobjects.DSL._
import org.httpobjects.Request
import org.httpobjects.Response
import org.httpobjects.util.HttpObjectUtil

object FakeExternalServer extends App {
  HttpObjectsJettyHandler.launchServer(8080, new HttpObject("/build"){
      override def post(r:Request):Response = {
        val bytes = HttpObjectUtil.toByteArray(r.representation())
        
        val data = Jackson.jackson.readValue(bytes, classOf[GenericBuildJson])
        
        System.out.println(data);
        
        OK(Text("You betcha!"))
      }
  })
}