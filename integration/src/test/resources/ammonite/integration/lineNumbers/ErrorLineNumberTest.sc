object Qs{
  def main(a: Int){
    println("OK")
    var unsorted: List[Int] = List(5,4,2,3,1)
    printlnqs(unsorted))
  }

  def qs(unsorted: List[Int]): List[Int] = {
    if(unsorted.length <= 1) unsorted
    else{
      val pivot = unsorted.head
      return qs(unsorted.filter(_ < pivot)) ::: List(pivot) ::: qs(unsorted.filter(_ > pivot))
    }
  }
}
