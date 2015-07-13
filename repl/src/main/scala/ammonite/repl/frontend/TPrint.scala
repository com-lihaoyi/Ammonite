package ammonite.repl.frontend

import pprint._
/**
 * Summoning an implicit `TPrint[T]` provides a pretty-printed
 * string representation of the type `T`, much better than is
 * provided by the default `Type#toString`. In particular
 *
 * - More forms are properly supported and printed
 * - Prefixed Types are printed un-qualified, according to
 *   what's currently in scope
 */
trait TPrint[T]{
  def render(implicit cfg: Config): String
}

object TPrint extends TPrintGen[TPrint, Config] with TPrintLowPri{
  def literal[T](s: String) = new TPrint[T]{
    def render(implicit cfg: Config) = cfg.colors.literalColor + s + cfg.colors.endColor
  }
  def lambda[T](f: Config => String) = new TPrint[T]{
    def render(implicit cfg: Config) = f(cfg)
  }
  def make[T](f: Config => String) = TPrint.lambda[T](f)
  def get[T](cfg: Config)(implicit t: TPrint[T]) = t.render(cfg)
  def implicitly[T](implicit t: TPrint[T]): TPrint[T] = t
  implicit val NothingTPrint: TPrint[Nothing] = TPrint.literal("Nothing")
}
