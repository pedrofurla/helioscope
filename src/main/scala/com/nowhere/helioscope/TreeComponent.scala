package com.nowhere.helioscope

import javax.swing.tree._
import javax.swing._
import javax.swing.event._
import scala.swing._
import scala.swing.event._

trait ScalaTreeNode[+T] { self : DefaultMutableTreeNode =>
	def value_=[A >: T](a:A):Unit = self.setUserObject(a)
	def value:T = self.getUserObject.asInstanceOf[T]
}

class ScalaDefaultTreeNode[+T](t:T) extends DefaultMutableTreeNode(t) with ScalaTreeNode[T]
/**
 * A quick implementation of a JTtree, using DefaultTreeModel and ScalaDefaultTreeNode. 
 * 
 * @tparam T the type the nodes of this tree will be holding
 */
class DefaultTreeComponent[T] extends Component with Publisher { self =>
	override lazy val peer: JTree = new JTree() with SuperMixin {}
	
	type MODEL = DefaultTreeModel
	type NODE = ScalaDefaultTreeNode[T]
	
	def this(root: TreeNode) = {
		this()		
		peer.setModel(new MODEL(root))
	}
	
	def expandPath(path:TreePath):Unit = peer.expandPath(path)
	def expandPath(nodes:Array[TreeNode]):Unit = 
		expandPath(new TreePath(nodes.asInstanceOf[Array[Object]])) // Not using the .asInstanceOf[Array[Object]] here
																	// causes a java.lang.ClassCastException (why!?)
		
	def selectionMode = TreeSelectionMode(peer.getSelectionModel().getSelectionMode)
	def selectionMode_=(mode:TreeSelectionMode.Value) = peer.getSelectionModel().setSelectionMode(mode.id)	
	def showsRootHandles = peer.getShowsRootHandles
	def showsRootHandles_=(shows:Boolean) = peer.setShowsRootHandles(shows)	
	def model = peer.getModel.asInstanceOf[MODEL]
	def model_=(model:MODEL) = peer.setModel(model)
	
	def lastSelectedPathComponent = peer.getLastSelectedPathComponent.asInstanceOf[NODE]
	
	def expandNode(node:NODE):Unit = expandPath(model.getPathToRoot(node))
	def scrollPathToVisible(node:NODE) = peer.scrollPathToVisible(new TreePath(model.getPathToRoot(node)));
		
	peer.addTreeSelectionListener(new TreeSelectionListener {
		def	valueChanged(e: TreeSelectionEvent) = {
			publish(new SelectionChanged(self))
		}
	})
}
object TreeSelectionMode extends Enumeration {
  import javax.swing.tree.TreeSelectionModel._
  val Single = Value(SINGLE_TREE_SELECTION)
  val Contiguous = Value(CONTIGUOUS_TREE_SELECTION)
  val Discontiguous = Value(DISCONTIGUOUS_TREE_SELECTION)
}