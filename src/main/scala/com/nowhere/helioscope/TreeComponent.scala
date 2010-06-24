package com.nowhere.helioscope

import javax.swing.tree._
import javax.swing._
import javax.swing.event._
import scala.swing._
import scala.swing.event._

class TreeComponent extends Component with Publisher {
	override lazy val peer: JTree = new JTree() with SuperMixin {}
	
	//var model:DefaultTreeModel = _
	
	def this(root: TreeNode) = {
		this()		
		peer.setModel(new DefaultTreeModel(root))
	}
	
	def expandPath(path:TreePath):Unit = peer.expandPath(path)
	def expandPath(path:Array[TreeNode]):Unit = expandPath(new TreePath(path.asInstanceOf[Array[Object]]))
	
	// TODO put into DefaultTreeComponent
	def expandNode(node:TreeNode):Unit = expandPath(model.asInstanceOf[DefaultTreeModel].getPathToRoot(node))
	
	def selectionMode = peer.getSelectionModel().getSelectionMode
	def selectionMode_=(mode:TreeSelectionMode.Value) = peer.getSelectionModel().setSelectionMode(mode.id)
	def lastSelectedPathComponent = peer.getLastSelectedPathComponent
	
	def showsRootHandles = peer.getShowsRootHandles
	def showsRootHandles_=(shows:Boolean) = peer.setShowsRootHandles(shows)
	
	def model = peer.getModel
	def model_=(model:TreeModel) = peer.setModel(model)
	
	def scrollPathToVisible(node:TreeNode) = peer.scrollPathToVisible(new TreePath(model.asInstanceOf[DefaultTreeModel].getPathToRoot(node)));
	
	peer.addTreeSelectionListener(new TreeSelectionListener {
		def	valueChanged(e: TreeSelectionEvent) = {
			publish(new SelectionChanged(TreeComponent.this))
		}
	})
}
object TreeSelectionMode extends Enumeration {
  import javax.swing.tree.TreeSelectionModel._
  val Single = Value(SINGLE_TREE_SELECTION)
  val Contiguous = Value(CONTIGUOUS_TREE_SELECTION)
  val Discontiguous = Value(DISCONTIGUOUS_TREE_SELECTION)
}