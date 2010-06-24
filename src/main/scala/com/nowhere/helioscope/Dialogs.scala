package com.nowhere.helioscope

import scala.swing._
import scala.swing.event._

trait UIElementUtils { self:Window =>
	import java.awt.Dimension;
	import java.awt.Toolkit;
	def centerOnScreen = {
		val screenSize = Toolkit.getDefaultToolkit().getScreenSize()
		val windowSize = this.size
		implicit def double2Int(x:Double) = x.toInt
		val pos = new Dimension((screenSize.getWidth - windowSize.getWidth)/2,(screenSize.getHeight - windowSize.getHeight)/2)
	}
}

class ModalDialog extends Dialog /*with UIElementUtils*/ { dialog => 
	modal = true
		
	val borderPanel = new BorderPanel {		 
		//layout(grid) = BorderPanel.Position.Center
		val box = new BoxPanel(Orientation.Horizontal) {
			val buttons = List(new Button("Ok"), new Button("Cancel"))
			contents ++ buttons
			dialog.listenTo(buttons:_*)
		}
		layout(box) = BorderPanel.Position.South  
		
	}
	contents = borderPanel
	
	var buttonClicked:Option[String] = None
	
	reactions += {
		case ButtonClicked(b @ _) => {
			println("Clicked:"+b.text)
			buttonClicked = Option(b.text)
			dialog.visible = false;
		}
	}
	
	def setCenterContents(c:Component) = { 
		borderPanel.layout(c) = BorderPanel.Position.Center 
		pack
	}
		
	def show = {
		this.visible=true;
		buttonClicked
	}	
}

class FieldsDialog(fields:List[Solr#SolrField]) extends ModalDialog {
	import scala.collection.mutable._
	
	abstract class MyCheckBox(text:String) extends CheckBox(text) { val field:Solr#SolrField }
	val checkBoxes = new ArrayBuffer[MyCheckBox] 
	val grid = new GridPanel(0,4) {
		for(f <- fields) {
			 checkBoxes += new MyCheckBox(f.name) { 
				 val field = f
			 }
			 contents += checkBoxes.last
		}
	}
	setCenterContents(grid)
	
	def disable(fields:List[Solr#SolrField])={
		checkBoxes.filter{ x=> fields.contains( x.field) }.foreach { _.enabled=false } 
	}
	
	def promptFields:List[Solr#SolrField] = {
		show match {
			case Some("Ok") =>
				checkBoxes.filter { _.selected } map { _.field } toList
			case _ =>
				List.empty
		}
	}
}

class ServersDialog(servers:List[String]) extends ModalDialog  {
	val combo = new ComboBox(servers) { makeEditable }
	minimumSize = new Dimension(300,50)
	setCenterContents(new BoxPanel(Orientation.Horizontal) { contents += combo })
		
	implicit def comboBoxModel2List(listModel:javax.swing.ComboBoxModel) = new Object {
		def toList[A]:List[A] = {
			(for(i <- 0 until listModel.getSize) yield { listModel.getElementAt(i).asInstanceOf[A] }).toList
		}
	}
	
	def prompt:Option[String] = {
		centerOnScreen()
		show match {
			case Some("Ok") =>
				if(combo.selection.item.trim.length>0) Some(combo.selection.item) else None
			case _ =>
				None
		}
		
	}
}