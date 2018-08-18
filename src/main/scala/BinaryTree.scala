

/**
  * Created by Ken.J.Zheng on 8/17/2018.
  */
object BinaryTreeTest extends App {
  val bt = new BinaryTree()
  bt.rootNode = bt.insert(41,bt.rootNode)
  bt.rootNode = bt.insert(20,bt.rootNode)
  bt.rootNode = bt.insert(11,bt.rootNode)
  bt.rootNode = bt.insert(29,bt.rootNode)
  bt.rootNode = bt.insert(26,bt.rootNode)
  bt.rootNode = bt.insert(65,bt.rootNode)
  bt.rootNode = bt.insert(50,bt.rootNode)

  bt.printTree(bt.rootNode, name = "root")
}

case class TreeNode(keyValue:Int, var left:TreeNode,var right:TreeNode){
  var height:Int=0
}

//AVL implementation
class BinaryTree{
  var rootNode : TreeNode = null

  def max(x:Int,y:Int):Int = {
    if(x > y) x
    else y
  }

  def getTreeNodeHeight(node: TreeNode):Int = {
    if(node!=null) node.height
    else -1
  }

  def getHeightDifference(node: TreeNode):Int = {
    if(node == null) -1
    else getTreeNodeHeight(node.left) - getTreeNodeHeight(node.right)
  }

  def insert(keyValue: Int, node: TreeNode): TreeNode ={
    if(node == null)
      return TreeNode(keyValue,null,null) //insert new node here

    if(keyValue < node.keyValue){
      node.left = insert(keyValue,node.left)
    }
    else if(keyValue > node.keyValue) {
      node.right = insert(keyValue,node.right)
    }
    else
      return node //duplicated keyValue is not allowed.

    //reset parent node height
    node.height = 1 + max(getTreeNodeHeight(node.left),getTreeNodeHeight(node.right))

    //get current node balance
    val diff = getHeightDifference(node)

    //four rotation methods
    if(diff > 1 && keyValue < node.left.keyValue) //5,3,1
      return rightRotate(node)
    if(diff < -1 && keyValue > node.right.keyValue) //5,7,9
      return leftRotate(node)
    if(diff > 1 && keyValue > node.left.keyValue){ //5,2,3
      node.left = leftRotate(node.left)
      return rightRotate(node)
    }
    if(diff < -1 && keyValue < node.right.keyValue){ //5,8,7
      node.right = rightRotate(node.right)
      return leftRotate(node)
    }

    node
  }

  def printTree(node: TreeNode, name:String): Unit={
    println(name + "->" + node.keyValue + " height:"+node.height)
    if(node.left != null){
      printTree(node.left,"left")
    }
    if(node.right != null){
      printTree(node.right,"right")
    }
  }

  def rightRotate(parent: TreeNode): TreeNode = {
    val child = parent.left
    val childRight = child.right

    child.right = parent
    parent.left = childRight

    parent.height = max(getTreeNodeHeight(parent.left),getTreeNodeHeight(parent.right)) + 1
    child.height = max(getTreeNodeHeight(child.left),getTreeNodeHeight(child.right)) + 1

    child //become parent
  }

  def leftRotate(parent: TreeNode): TreeNode = {
    val child = parent.right
    val childLeft = child.left

    child.left = parent
    parent.right = childLeft

    parent.height = max(getTreeNodeHeight(parent.left),getTreeNodeHeight(parent.right)) + 1
    child.height = max(getTreeNodeHeight(child.left),getTreeNodeHeight(child.right)) + 1

    child //becomre parent
  }
}
