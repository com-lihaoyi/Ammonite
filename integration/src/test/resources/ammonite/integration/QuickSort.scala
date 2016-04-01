def fib(n : Int) : Int = {
  if (n == 0) 0
  else if (n == 1) 1
  else fib(n-1) + fib(n-2)
}

def quicksort(unsorted : List[Int]) : List[Int] = {
  if(unsorted.length <= 1) unsorted
  else{
    val pivot = unsorted.head
    quicksort(unsorted.filter(_ < pivot)):::List(pivot):::quicksort(unsorted.filter(_ > pivot))
  }
}
