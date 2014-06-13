package org.iqbuild

import org.joda.time.Instant
import java.io.File

object FSNode {
	def forPath(p:File, filter:File=>Boolean = {f=>true}):FSNode = {
		val children = if(p.isDirectory()) p.listFiles().filter(filter).map(forPath(_)).toSeq else Seq[FSNode]()
		FSNode(p.getAbsolutePath(), new Instant(p.lastModified()), p.isFile(), children)
	}
}
case class FSNode(path:String, lastModified:Instant, isFile:Boolean, children:Seq[FSNode]) {
	def deltas(node:Any):List[(FSNode, FSNode)] = {
		node match {
		case n:FSNode => {

			val childDeltas = children.zip(n.children ).flatMap{t=>
				val (a, b) = t
				a.deltas(b)
			}.toList

			val imUnchanged = path == n.path  && lastModified == n.lastModified  && isFile == n.isFile
			
			val changes = if(!imUnchanged){
			  println("I changed: " + this)
			  List((this, n))
			}else {
			  List()
			}
			
			changes ::: childDeltas
		} 
		case _ => List()
		}
	}
}