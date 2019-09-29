/**
  * @ author    ynats
  * @ date      2019/9/26 - 18:59
  * @ encoding  utf-8
  * @ title     AVLTree
  */

package Tree

object AVLTree {
  def main(args: Array[String]): Unit = {

    val s1 = new Student("natsu1", 91)
    val s2 = new Student("natsu2", 92)
    val s3 = new Student("natsu3", 93)
    val s4 = new Student("natsu4", 94)

    val tree = new AVLTree[Student]()

    tree.add(s1)
    tree.add(s2)
    tree.add(s3)
    tree.add(s4)


    println(tree.isBalance)

    tree.remove(s1)

    println(tree.isBalance)


  }

}

class AVLTree[T <: Comparable[T]] {


  private class Node(var value: T) {

    var left: Node = null
    var right: Node = null

    override def toString = s"Node(value=$value)"
  }


  private var root: Node = null

  private var size: Int = 0

  def getSize: Int = size


  private def getHeight(node: Node): Int = {

    if (node == null)
      return 0

    return getHeight(node.left).max(getHeight(node.right)) + 1
  }

  private def getBalanceFactor(node: Node): Int = {
    if (node == null) return 0
    return getHeight(node.left) - getHeight(node.right)
  }

  private def isBalanced(node: Node): Boolean = {

    if (node == null) return true

    val balanceFactor = getBalanceFactor(node)

    if (balanceFactor.abs > 1)
      return false

    return isBalanced(node.left) && isBalanced(node.right)
  }

  def isBalance: Boolean = isBalanced(root)



  private def setBalanced(nodeTemp: Node) : Node ={

    var node = nodeTemp

    val balanceFactor: Int = getBalanceFactor(node)

    if (balanceFactor > 1 && getBalanceFactor(node.left) >= 0) {
      node = rightRotate(node)
    }
    else if (balanceFactor > 1 && getBalanceFactor(node.left) < 0) {
      node.left = leftRotate(node.left)
      node = rightRotate(node)
    }

    else if (balanceFactor < -1 && getBalanceFactor(node.right) < 0) {
      node = leftRotate(node)
    }

    else if (balanceFactor < -1 && getBalanceFactor(node.right) > 0) {
      node.right = rightRotate(node.right)
      node = leftRotate(node)
    }

    node
  }



  private def add(nodeTemp: Node, value: T): Node = {

    var node = nodeTemp

    if (node == null) {
      size += 1
      return new Node(value)
    }

    if (value.compareTo(node.value) == 1)
      node.left = add(node.left, value)

    else if (value.compareTo(node.value) == -1)
      node.right = add(node.right, value)

    else node.value = value


    node = setBalanced(node)

    return node
  }

  def add(value: T): Unit = root = add(root, value)


  //   node                     x
  //  /   \     左旋转         /  \
  // T1   x   --------->   node   T3
  //     / \              /   \
  //    T2 T3            T1   T2
  private def leftRotate(node: Node): Node = {

    val x: Node = node.right

    node.right = x.left
    x.left = node

    return x
  }

  //     node                   x
  //    /   \     右旋转       /  \
  //   x    T2   ------->   y   node
  //  / \                       /  \
  // y  T1                     T1  T2
  private def rightRotate(node: Node): Node = {
    val x: Node = node.left

    node.left = x.right
    x.right = node
    return x
  }



  private def getMininum(node: Node): Node = {

    if (node.left == null)
      return node

    return getMininum(node.left)
  }

  private def removeMin(node: Node): Node = {

    if (node.left == null) {
      val rightNode = node.right
      node.right = null
      size -= 1
      return rightNode
    }

    node.left = removeMin(node.left)
    return node
  }

  private def remove(node: Node, value: T): Node = {
    if (node == null)
      return null

    var returnNode: Node = null

    value.compareTo(node.value) match {

      case -1 => {
        node.left = remove(node.left, value)
        returnNode = node
      }

      case 1 => {
        node.right = remove(node.right, value)
        returnNode = node
      }

      case 0 => {

        if (node.left == null) {
          val rightNode = node.right
          node.right = null
          size -= 1
          returnNode = rightNode
        }

        if(node.right == null){
          val leftNode = node.left
          node.left = null
          size-=1
          returnNode = leftNode
        }

        val successor = getMininum(node)
        successor.right = removeMin(node.right)
        successor.left = node.left
        returnNode = successor
      }
    }

    returnNode = setBalanced(returnNode)

    return returnNode
  }

  def remove(value : T) : Unit = remove(root, value)




  private def preOrder(node: Node): Unit = {

    if (node == null)
      return

    println(node.value)
    preOrder(node.left)
    preOrder(node.right)
  }

  def preOrder: Unit = preOrder(root)

  private def inOrder(node: Node): Unit = {

    if (node == null) return

    inOrder(node.right)
    println(node.value)
    inOrder(node.left)
  }

  def inOrder: Unit = inOrder(root)

  private def afterOrder(node: Node): Unit = {

    if (node == null) return

    inOrder(node.right)

    inOrder(node.left)
    println(node.value)
  }

  def afteOrder: Unit = afterOrder(root)

}