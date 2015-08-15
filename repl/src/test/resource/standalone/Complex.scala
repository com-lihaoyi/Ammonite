load.ivy("org.spire-math" %% "spire" % "0.10.1")

@

import spire.implicits._
import spire.math._

def euclidGcd[A: Integral](x: A, y: A): A = {
  if (y == 0) x
  else euclidGcd(y, x % y)
}

println("Int GCD " + euclidGcd(42, 96))
println("Long GCD " + euclidGcd(42L, 96L))
