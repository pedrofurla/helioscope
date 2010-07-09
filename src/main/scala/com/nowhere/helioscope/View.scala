package com.nowhere.helioscope

import scala.swing._
import scala.swing.event._
import java.awt.Dimension

//import javax.swing.tree.{ DefaultMutableTreeNode => DMTN }
import scala.collection.mutable._

import com.nowhere.helioscope.{ ScalaDefaultTreeNode => SDTN }

abstract class View extends SimpleSwingApplication { view =>
  val solr:Solr ;
  
  lazy val fieldsDialog = new FieldsDialog(solr.schema.editable)

  // Editor needs to be encapsulated to properly deals with ValueChangedEvent  
  //private[this] // too lazy to do now 
  val editor = new EditorPane("text/plain","") // TODO Make private[this]  
  val input = new TextField("*:*")    
  val queries = new SDTN(new Nodes.Root)
  val tree = new DefaultTreeComponent[Nodes.Node](queries) { 
	  showsRootHandles = true
	  selectionMode = TreeSelectionMode.Single
  }  
  val updateBtn = new Button("Upd")
  val runBtn = new Button("Run") 
  
  // editor 
  private var ignoreEditor = false
  
  def editorText_=(s:String) = {
	  ignoreEditor=true
	  editor.text = s
	  ignoreEditor=false
  }
  def editorText = editor.text
  
  
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
  
  def busyCursor = {
	  import java.awt.Cursor
	  import java.awt.Cursor._
	  top.contents(0).cursor= new Cursor(WAIT_CURSOR)
	  //runBtn.cursor= new Cursor(WAIT_CURSOR)
  }
  def defaultCursor = {
	  import java.awt.Cursor
	  import java.awt.Cursor._
	  //top.contents(0).cursor= new Cursor(DEFAULT_CURSOR)
	  runBtn.cursor= new Cursor(DEFAULT_CURSOR)
  }
  
  val runClicked: (() => Unit) 
  val treeClicked: (() => Unit) 
  val updClicked: (() => Unit) 
  val editorEdited: (() => Unit)  
  
  reactions += {
	  case ButtonClicked(`runBtn`) => {
	 	  //Swing.onEDT(busyCursor)
	 	  runClicked() 
	 	  //Swing.onEDT(defaultCursor)
	  }
	  case ButtonClicked(`updateBtn`) => {
	 	  //busyCursor; 
	 	  updClicked(); 
	 	  //defaultCursor;
	  }
	  case SelectionChanged(`tree`) => treeClicked()
	  case v@ValueChanged(`editor`) => if(!ignoreEditor) editorEdited() 
  }
  
  editor.enabled = false
  updateBtn.enabled = false
  
  listenTo(runBtn,updateBtn,editor,tree)
  
}

object Nodes {
	sealed abstract class Node {
		val isRoot = false
		val isQuery = false
		val isResult = false
	}
	
	case class Root extends Node {
		override val isRoot = true
		override def toString = "Queries" 
	}
	
	case class Query(query:String) extends Node {
		override val isQuery=true
	}
	
	case class Result(id:String,result:Solr#SolrResultVO) extends Node {
		override val isResult=true
		override def toString = id + (if(changed) "*" else "")
		var changed = false		
	}
}

object View extends View {
	import scala.xml._
	//import solr._
	import javax.swing.tree._
	
	val solr = choosenServer	
	
	def node = tree.lastSelectedPathComponent
	def nodeVal = node.value
	def nodeVal_=(n:Nodes.Node) = node.value = n 
	
	def updatable(b:Boolean) = updateBtn.enabled = b
	def editable(b:Boolean) = editor.enabled = b
	
	def choosenServer = {
		val choice = Config.showServerDialog
		// Must exist a better way to do that
		new Solr(choice.getOrElse({ System.exit(1); ""}))		
	}
	
	val runClicked = () => { 
		val res = solr.queryXml(input.text)
		println("executed: "+input.text+" returned "+res.size)
		val queryNode = new SDTN(Nodes.Query(input.text))
		for(x <- res ) {
			queryNode.add(new SDTN(Nodes.Result(x.key,x)))
		}
		tree.model.insertNodeInto(queryNode, queries,  queries.getChildCount);
		tree.expandNode(queryNode)
		tree.scrollPathToVisible(queryNode)
	}
	
	val treeClicked = () => {
		println(tree.lastSelectedPathComponent)
		editable(false)
		updatable(false)
		nodeVal match {
			case nodeRes @ Nodes.Result(_, res @ _) => {
				updatable(nodeRes.changed)
				editable(true)
				//editor.text = res.prettyPrint
				editorText = res.prettyPrint
			}
			case _ => 
		}
	}
	
	val updClicked = () => {		
		nodeVal match {
			case Nodes.Result( key @ _, res @ _) => {
				val xml = XML.loadString(editor.text)
				println(res.key); 
				solr.updateXml(editor.text,res.key); 
				nodeVal = Nodes.Result(key,solr.SolrResultVO(xml,key))
				tree.model.nodeChanged(node)
				updatable(false)
			}
			case _ => 
		}
		{}
	}
	
	val editorEdited = () => {		
		nodeVal match {
			case resNode @ Nodes.Result(_,_) => {				
				resNode.changed = true
				updatable(true)
				tree.model.nodeChanged(node)
			}
			case _ => 
		}
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

