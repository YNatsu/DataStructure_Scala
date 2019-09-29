/**
  * @ author    ynats
  * @ date      2019/9/26 - 19:48
  * @ encoding  utf-8
  * @ title
  */

package LinearList

object LinkedList {
  def main(args: Array[String]): Unit = {
    val list = new LinkedList[Student]
  }
}

class LinkedList[T: Manifest] {

  private class Node(var value: T, var next: Node = null) {
    override def toString = s"Node(value=$value)"
  }

  private var head: Node = null
  private var size: Int = 0

  def getSize: Int = size
  def isEmpty: Boolean = size == 0

  def addFirst(value: T): Unit = head = new Node(value = value, next = head)


}
