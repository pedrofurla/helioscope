package com.nowhere.helioscope

import scala.xml._

object Solr {
	val schemaUrlSuffix = "admin/file/?file=schema.xml"
	
	case class SolrField(val name:String, val `type`:String, 
			val indexed:Boolean, val stored:Boolean, val multiValued:Boolean)
	case class SolrSchema(val key:SolrField , val fields:List[SolrField]) {
		def names = fields.map { _.name }
		def storeds = fields.filter( _.stored )
		def indexeds = fields.filter( _.indexed )
		def editables = fields.filter( f => f.stored || f.indexed)
		def multiValueds = fields.filter( _.indexed )
		def byName(name:String) = fields find { _.name == name } 
	}
	
	object SolrField {
		def apply(fieldXml : Node):SolrField = {
			val f = fieldXml
			SolrField((f \ "@name").text.trim, 
					  (f \ "@type").text.trim, 
					  (f \ "@indexed").text.trim.toLowerCase == "true", 
					  (f \ "@stored").text.trim.toLowerCase  == "true", 
					  (f \ "@multiValue").text.trim.toLowerCase  == "true")
		}
	}
}

class Solr(val url:String) {
	
	import org.apache.solr.client.solrj._
	import org.apache.solr.client.solrj.impl._
	import Solr._
	
	lazy val server = new CommonsHttpSolrServer( url );
	lazy val uniqueKey:String = schema.key.name
	
	lazy val schema = {
		import scala.io._
		val s = Source.fromURL( new java.net.URL(url + Solr.schemaUrlSuffix) )(Codec(Codec.ISO8859))
		val x = XML.loadString(s.getLines().mkString)
				
		val uk = (x \\ "uniqueKey").text
		val fields = for(f <- x \\ "field" ; if (f \ "@name").text != "uk") yield {			
			SolrField(f)
		}
		val ukNode = (x \\ "field" ) find { x =>  (x \ "@name").text == uk } get
		val ukField = SolrField(ukNode)
		
		SolrSchema(ukField,fields.toList)
	}
	
	def query(query:String) = {
		val q = new SolrQuery(query)
		server.query(q);
	}
	
	import java.text._
	val sdf:SimpleDateFormat  = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'");        
	private def solrValue(o:AnyRef) = o match {
		case d : java.util.Date => sdf.format( d );
		case _ => o.toString
	}
	
	def queryXml(q:String) = {
		import scala.collection.JavaConversions._
				
		val r = query(q);
		val l = for(x <- r.getResults()) yield {			
			val doc:NodeSeq = for(entry <- x.entrySet.toSeq) yield {
				entry.getValue match {
					case values:java.util.ArrayList[AnyRef] => {
						<xml:group>
							{ for(v <- values) yield <f name={entry.getKey}>{solrValue(v)}</f> }
						</xml:group>
					}
					case value @ _ => <f name={entry.getKey}>{solrValue(entry.getValue)}</f>
				}
			}			
			new SolrResultVO(<doc>{doc}</doc>,x.getFieldValue(uniqueKey).toString)
		}		
		l
	}
	
	def updateXml(x:String, key:String) = {		
		import org.apache.solr.common._
		val doc = new SolrInputDocument();
		val xml = XML.loadString(x);
		for(f <- xml \\ "f" ) {
			val fName = (f \ "@name").text
			doc.addField(fName,f.text)
		}
		server.add(doc);
		server.commit;
	}
	
	case class SolrResultVO(val doc:NodeSeq, val key:String, val missing:List[SolrField]=List.empty) {
		override def toString = uniqueKey + ": " + key
		private val pp = new PrettyPrinter(80,5)
		lazy val prettyPrint = pp.formatNodes(doc)		
	}
}