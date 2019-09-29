package LinearList

object  MyArrayBuffer{
  def main(args: Array[String]): Unit = {

  }
}

class MyArrayBuffer[T : Manifest](private val capacity: Int = 2) {

  private var data: Array[T] =  new Array[T](capacity)
  private var size: Int = 0


  def getCapacity(): Int = data.length

  def getSize(): Int = size


  def get(index: Int): T = {
    checkIndex(index)
    data(index)
  }

  def getFirst() : T = get(0)


  def getLast() : T = get(size -1)


  def set(index: Int, value: T): Unit = {
    checkIndex(index)
    data(index) = value
  }

  def find(value: T): Int = {

    for (i <- 0 until size) {
      if (data(i).equals(value))
        return i
    }

    -1
  }



  def isEmpty(): Boolean = size == 0

  def isContains(value: T): Boolean = {
    for (i <- 0 until size) {
      if (data(i).equals(value))
        return true
    }
    false
  }


  def insert(index: Int, value: T): Unit = {

    checkIndex(index)

    if (size == data.length)
      resize(data.length * 2)

    for (i <- (index + 1) to size reverse)
      data(i + 1) = data(i)

    data(index) = value
    size += 1
  }

  def append(value: T): Unit = {
    insert(size, value)
  }


  def remove(index : Int) : T ={

    checkIndex(index)

    val removeValue = data(index)


    for (i <- index until size - 1 )
      data(i) = data(i+1)


    size -= 1

    if (size <= data.length / 2 && data.length/2 != 0)
      resize(data.length/2)

    removeValue
  }

  def removeLast() : T = remove(size-1)

  def removeFirst() : T = remove(0)

  def removeValue(value : T) : Int = {
    val index = find(value)
    if(index != -1 )
      remove(index)

    return index
  }


  def toArray : Array[T] = {

    if (isEmpty())
      throw new Exception("Array is empty !")

    val array = new Array[T](size)
    for (i <- 0 until size)
      array(i) = data(i)

    array
  }



  private def resize(newCapacity: Int): Unit = {

    var newData = new Array[T](newCapacity)

    for (i <- 0 until size)
      newData(i) = data(i)

    data = newData
    newData = null

  }

  private def checkIndex(index: Int): Unit = {
    if (index < 0 || index > size)
      throw new Exception("Insert failed : require i >= 0 and i <= size")
  }



  def show : Unit = {

    printf("Array : size = %d , capacity = %d data = [", size, data.length)

    for (i <- 0 until size) {
      print(data(i))
      if (i != size - 1)
        print(",")

    }

    print("]\n")
  }

}

