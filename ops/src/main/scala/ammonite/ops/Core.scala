package ammonite.ops


trait Op1[T1, R] extends (T1 => R){
  def apply(arg: T1): R
  def !(arg: T1): R = apply(arg)
}


trait Op2[T1, T2, R] extends ((T1, T2) => R){
  def apply(arg1: T1, arg2: T2): R
  case class !(arg1: T1) extends (T2 => R){
    def apply(arg2: T2) = Op2.this.apply(arg1, arg2)
    def !(arg2: T2): R = Op2.this.apply(arg1, arg2)
  }
}