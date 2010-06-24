package com.nowhere.helioscope

import javax.swing.tree._
import javax.swing._
import javax.swing.event._
import scala.swing._
import scala.swing.event._

trait GenericTreeNode[T] {
	//val peer = DefaultMutableTreeNode
	def insert(node : GenericTreeNode[T], index:Int)
	def remove(node : GenericTreeNode[T])
	def setUserObject(obj:T)
	def setParent(node : GenericTreeNode[T])
}

class GenericTreeComponent[N <: TreeNode : Manifest, M <: TreeModel] extends Component with Publisher {
	override lazy val peer: JTree = new JTree() with SuperMixin {}
	
	//var model:DefaultTreeModel = _
	
	//type MODEL
	//type NODE = N
	
	def this(root: N) = {
		this()		
		peer.setModel(new DefaultTreeModel(root))
	}
	
	object TreePath {
		def apply(node : N):TreePath = new TreePath(model.asInstanceOf[DefaultTreeModel].getPathToRoot(node))
	}
	
	def expandPath(path:TreePath):Unit = peer.expandPath(path)
	def expandPath(path:List[N]):Unit = expandPath(new TreePath(path.toArray))
		
	def selectionMode = peer.getSelectionModel().getSelectionMode
	def selectionMode_=(mode:TreeSelectionMode.Value) = peer.getSelectionModel().setSelectionMode(mode.id)
	def lastSelectedPathComponent = peer.getLastSelectedPathComponent
	
	def showsRootHandles = peer.getShowsRootHandles
	def showsRootHandles_=(shows:Boolean) = peer.setShowsRootHandles(shows)
	
	def model = peer.getModel
	def model_=(model:M) = peer.setModel(model)
	
	// TODO put into DefaultTreeComponent
	def expandNode(node:N):Unit = expandPath(TreePath(node))
	def scrollPathToVisible(node:N) = 
		peer.scrollPathToVisible(TreePath(node));
	
	peer.addTreeSelectionListener(new TreeSelectionListener {
		def	valueChanged(e: TreeSelectionEvent) = {
			publish(new SelectionChanged(GenericTreeComponent.this))
		}
	})
}
