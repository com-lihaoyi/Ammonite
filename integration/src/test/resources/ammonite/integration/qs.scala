def fib(n:Int):Int = {
		if (n == 0) return 0
		if (n == 1) return 1
		return fib(n-1) + fib(n-2)
	}

def qs(unsorted:List[Int]):List[Int]={
		if(unsorted.length<=1) unsorted
		else{
		val pivot=unsorted.head
		return qs(unsorted.filter(_ < pivot)):::List(pivot):::qs(unsorted.filter(_ > pivot))
		}
	}
