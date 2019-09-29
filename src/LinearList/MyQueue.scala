package LinearList

trait MyQueue[T] {

  def enqueue(value : T*) : Unit

  def dequeue() : T

  def getFront() : T

  def getSize() : Int

  def isEmpty() : Boolean

}
