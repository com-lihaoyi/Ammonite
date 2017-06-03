package ammonite.main
import fastparse.utils.Compat.Context
object Compat{
  def copyAnnotatedType(c: Context)
                       (tpe: c.universe.AnnotatedType,
                        newAnnots: List[c.universe.Annotation]) = {
    c.universe.AnnotatedType(newAnnots, tpe.underlying, tpe.selfsym)
  }
}
