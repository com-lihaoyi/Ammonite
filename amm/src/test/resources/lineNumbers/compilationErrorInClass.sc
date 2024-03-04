//adsfasdfasd
println("Hello")

//Comment before an empty line

@
class numPair(a: Int, b: Int) {

  val num1 = a
  val num2 = b
}

def functionSwap(nums: numPair) = {

  val temp = nums.a ^ nums.b
  nums.a = nums.a ^ nums.b
  nums.b = nums.a ^ nums.b
  nums.a = nums.a ^ nums.b

}
