package com.nowhere.helioscope

object Solr {
	val schemaUrlSuffix = "admin/file/?file=schema.xml"
}

class Solr(val url:String) {
	import org.apache.solr.client.solrj.impl._
	import org.apache.solr.client.solrj._
	import scala.xml._
	
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
	private def solrValue(o:Object) = o match {
		case d : java.util.Date => sdf.format( d );
		case _ => o
	}
	
	def queryXml(q:String) = {
		import scala.collection.JavaConversions._
				
		val r = query(q);
		val l = for(x <- r.getResults()) yield {			
			val doc:NodeSeq = for(entry <- x.entrySet.toSeq) yield {
				entry.getValue match {
					case values:java.util.ArrayList[String] => {
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
			val value:Object = if(doc.containsKey(fName))
			 	  doc.get(fName).getValue match {
			 	  	case v : java.util.ArrayList[String] =>  {
			 	  		f.text
			 	  	}
			 	  	case v @ _ => {
			 	  		f.text
			 	  	}
			      }
			  else f.text
			//println("Adding field: "+fName);
			doc.addField(fName,value)
		}
		server.add(doc);
		server.commit;
	}

	case class SolrField(val name:String, val `type`:String, 
			val indexed:Boolean, val stored:Boolean, val multiValued:Boolean)
	case class SolrSchema(val key:SolrField , val fields:List[SolrField]) {
		val names = fields.map { _.name }
		lazy val stored = fields.filter( _.stored )
		lazy val indexed = fields.filter( _.indexed )
		lazy val editable = fields.filter( f => f.stored || f.indexed)
		def byName(name:String) = fields find { _.name == name } 
	}
	
	object SolrField {
		import scala.xml._
		def apply(fieldXml : Node):SolrField = {
			val f = fieldXml
			SolrField((f \ "@name").text.trim, 
					  (f \ "@type").text.trim, 
					  (f \ "@indexed").text.trim.toLowerCase == "true", 
					  (f \ "@stored").text.trim.toLowerCase  == "true", 
					  (f \ "@multiValue").text.trim.toLowerCase  == "true")
		}
	}
	
	case class SolrResultVO(val doc:NodeSeq, val key:String, val missing:List[SolrField]=List.empty) {
		override def toString = uniqueKey + ": " + key
		private val pp = new PrettyPrinter(80,5)
		lazy val prettyPrint = pp.formatNodes(doc)
		
	}
}