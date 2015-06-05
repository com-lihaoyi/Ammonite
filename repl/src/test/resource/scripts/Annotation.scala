import annotation.tailrec
val list= List(1,2,3,4)
@tailrec def product(acc: Int, l: List[Int]): Int = {
  if(l.isEmpty) acc
  else product(acc*l.head,l.tail)
}
val res = product(1,list)
