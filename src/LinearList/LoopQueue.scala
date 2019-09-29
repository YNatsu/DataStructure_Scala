/**
  * @ author    ynats
  * @ date      2019/9/26 - 19:10
  * @ encoding  utf-8
  * @ title
  */

package LinearList

object LoopQueue {
  def main(args: Array[String]): Unit = {

    val s1 = new Student("s1")
    val s2 = new Student("s2")
    val s3 = new Student("s3")
    val s4 = new Student("s4")

    println("<<<  enqueue >>>")

    val queue = new LoopQueue[Student]()

    queue.enqueue(s1,s2,s3,s4)

    queue.show

    println("\n<<<  dequeue >>>")

    println(queue.dequeue())
    println(queue.dequeue())

    queue.show

  }
}

class LoopQueue[T: Manifest](capacity: Int = 2) extends MyQueue[T] {

  private var data = new Array[T](capacity + 1)

  private var front, tail, size = 0

  def getCapacity: Int = data.length - 1

  override def enqueue(values: T*): Unit = {

    for (value <- values) {
      if ((tail + 1) % data.length == front)
        resize(getCapacity * 2)
      data(tail) = value
      tail = (tail + 1) % data.length
      size += 1
    }


  }

  private def resize(capacity: Int): Unit = {

    val newData = new Array[T](capacity + 1)

    for (i <- 0 until size) {
      newData(i) = data((i + front) % data.length)
    }

    data = newData

    front = 0

    tail = size

  }

  override def dequeue(): T = {
    if (isEmpty())
      throw new Exception("Can't dequeue : empty queue !")

    val temp = data(front)

    front = (front + 1) % data.length

    size -= 1

    if(size == getCapacity /4 && getCapacity /2 != 0)
      resize(getCapacity / 2)
    return temp

  }

  override def getFront(): T = {
    if (isEmpty())
      throw new Exception("Can't get front : empty queue !")

    return data(front)
  }

  override def getSize(): Int = size

  override def isEmpty(): Boolean = front == tail

  def show : Unit = {
    println(s"Capacity : $getCapacity , size : $size")
    for (i <- 0 until size){
      println(data((i + front) % data.length))
    }
  }
}
