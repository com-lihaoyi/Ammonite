package ammonite.testcode

package paulp1 {
  class Paulp {
    override def toString = "paulp1.Paulp1"
  }
}

package paulp2 {
  object Paulp {
    override def toString = "paulp2.Paulp2"
  }
}

package paulp3 {
  object Paulp {
    override def toString = "paulp3.Paulp-object"
  }
  class Paulp {
    override def toString = "paulp3.Paulp-class"
  }
}
