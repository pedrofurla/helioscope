package com.nowhere.helioscope

object commons {	
	import annotation.elidable
	import annotation.elidable._
	
	type elide = elidable
	
	@elide(INFO)
	def info(s:String*) = println("[INFO]" + s)
	@elide(FINER)
	def trace(s:String*) = println("[INFO]" + s)
	
}