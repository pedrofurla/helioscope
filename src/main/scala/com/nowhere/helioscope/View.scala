package com.nowhere.helioscope

import scala.swing._
import scala.swing.event._
import java.awt.Dimension

import javax.swing.tree.{ DefaultMutableTreeNode => DMTN }
import scala.collection.mutable._

abstract class View extends SimpleSwingApplication { view =>
  val solr:Solr ;
  val editor = new EditorPane("text/plain","")
  val input = new TextField("*:*")
  lazy val fieldsDialog = new FieldsDialog(solr.schema.editable)
    
  val queries = new DMTN("Queries") 
  val tree = new TreeComponent(queries) { 
	  showsRootHandles = true
	  selectionMode = TreeSelectionMode.Single
  }
  
  val updateBtn = new Button("Upd")
  val runBtn = new Button("Run") 
  
  def top = new MainFrame {
	title = "Helioscope - The Solr query and editor"
    
	contents = new SplitPane(Orientation.Vertical, new ScrollPane(tree),
	new BorderPanel { 
      val box=new BoxPanel(Orientation.Vertical) {		
		contents+= new ScrollPane(editor)
		contents+= new BoxPanel(Orientation.Horizontal) {
			maximumSize = new Dimension(300,100)
			contents+=updateBtn
			contents+=input
			contents+=runBtn
		}		
      }
      layout(box) = BorderPanel.Position.Center      
    } ) { resizeWeight=0.5 }
  } 

  val runClicked: (() => Unit) 
  val treeClicked: (() => Unit) 
  val updClicked: (() => Unit) 
  val editorEdited: (() => Unit)  
  
  reactions += {
	  case ButtonClicked(`runBtn`) => println("Run")
	  case ButtonClicked(`updateBtn`) => println("Upd")
	  case SelectionChanged(`tree`) => println("Selected")
	  case ValueChanged(`editor`) => println("Edited")
  }
  
  listenTo(runBtn,updateBtn,editor,tree)
  
}

object View extends View {
	import scala.xml._
	//import solr._
	import javax.swing.tree._
	
	val solr = choosenServer	
	
	def choosenServer = {
		val choice = Config.showServerDialog
		// Must exist a better way to do that
		new Solr(choice.getOrElse({ System.exit(1); ""}))		
	}
	
	val runClicked = () => { 
		val res = solr.queryXml(input.text)
		println("executed: "+input.text+" returned "+res.size)
		val queryNode = new DMTN(input.text)
		for(x <- res ) {
			queryNode.add(new DMTN(x))
		}
		tree.model.asInstanceOf[DefaultTreeModel].insertNodeInto(queryNode, queries,  queries.getChildCount);
		tree.expandNode(queryNode)
		tree.scrollPathToVisible(queryNode)
	}
	
	val treeClicked = () => {
		println(tree.lastSelectedPathComponent)
		// TODO this can be easily improved
		tree.lastSelectedPathComponent match {
			case node:DMTN => 
				node.getUserObject match { 
					case res:solr.SolrResultVO => editor.text = res.prettyPrint
					case _ => 
				}
			case _ => 
		}
	}
	
	val updClicked = () => {
		tree.lastSelectedPathComponent match {
			case node:DMTN => 
				node.getUserObject match { 
					case res:solr.SolrResultVO => {
						println(res.key); 
						solr.updateXml(editor.text,res.key); 
						()
					}
					case _ => 
				}
			case _ => 
		} 
	}
	
	val editorEdited = () => {
		
	}
	
	object Config {
		import scala.io._
		import java.io._
		import java.util.Properties
		import scala.collection.immutable._
		
		val configFile = new File("servers.properties")
		lazy val properties = load
				
		def load:Properties = {		
			val stringReader = new java.io.StringReader(if(!configFile.exists) "servers=" else Source.fromFile(configFile).getLines().mkString("\n"));		
			val properties = new Properties()
			properties.load(stringReader)
			stringReader.close();
			properties
		}
		def save = {
			properties.store(new FileOutputStream(configFile,false), null)
		}
		
		def servers:Set[String] = {
			val prop = properties.getProperty("servers")			
			(if(prop!=null && prop.length>0) prop.split(",") else Array[String]()).toSet
		}
		def servers_=(set:Set[String]):Unit = {
			val serversStr = set.mkString(",")
			properties.setProperty("servers",  serversStr)
			println(servers)
		}		
		
		def showServerDialog = {			
			val choosen:Option[String] = new ServersDialog(servers.toList :+ "").prompt
			println("Choosen:" + choosen)
			for(server <- choosen) { 
				servers += server
				save
			}
			choosen
		}
		
		
	}
	
	override def main(args: Array[String]):Unit = {
		super.main(args)
	}
}

object Run {
	def main(args: Array[String]):Unit = {
		View.main(args)
	}
}

