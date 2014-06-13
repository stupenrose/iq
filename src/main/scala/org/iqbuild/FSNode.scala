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
	override def equals(node:Any) = {
		node match {
		case n:FSNode => {

			val childrenMatch = children.zip(n.children ).forall{t=>
			val (a, b) = t
			val m = a == b
			if(!m) System.out.println(s"$a $b $m")
			m
			}

			path == n.path  && lastModified == n.lastModified  && isFile == n.isFile && childrenMatch//  && children == n.children 
		} 
		case _ => false
		}
	}
}