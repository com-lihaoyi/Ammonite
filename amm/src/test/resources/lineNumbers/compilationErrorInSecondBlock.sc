//Another Comment

def quicksort(unsorted: List[Int]): List[Int] = {
  if (unsorted.length <= 1) unsorted
  else {
    val pivot = unsorted.head
    quicksort(unsorted.filter(_ < pivot)) ::: List(pivot) ::: quicksort(unsorted.filter(_ > pivot))
  }
}

@
printnl("OK")

//One more comment
//This one is long one!
//check empty lines

val x = 1
println(x)
//print the value of x

@
//lots of comments!!

prinntl("Ammonite")
