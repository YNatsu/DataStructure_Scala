package Tree

class Student(val name: String, val credit: Double) extends Comparable[Student] {

  override def compareTo(s: Student): Int = {
    if (credit > s.credit) return 1
    else if (credit == s.credit) return 0
    else return -1
  }


  override def toString = s"Student(name=$name, credit=$credit)"
}
