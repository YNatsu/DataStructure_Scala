package LinearList



object ArrayQueue{
  def main(args: Array[String]): Unit = {

  }
}

class ArrayQueue[T : Manifest] (capacity : Int) extends MyQueue[T] {

  val array = new MyArrayBuffer[T](capacity=capacity)

  override def enqueue(values: T*): Unit = {

    for (value <- values)
      array.append(value)

  }

  override def dequeue(): T = array.removeFirst()

  override def getFront(): T = array.getFirst()

  override def getSize(): Int = array.getSize()

  def getCapacity() : Int = array.getCapacity()

  override def isEmpty(): Boolean = array.isEmpty()

  def show = {
    printf("Queue : [")
    for (i <- 0 until array.getSize()){
      printf(" %s ", array.get(i).toString)

      if(i != array.getSize() - 1)
        print(",")
    }


    print("]\n")
  }
}

