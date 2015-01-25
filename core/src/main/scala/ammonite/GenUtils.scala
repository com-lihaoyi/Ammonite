/**
 * Helpers to help inject behavior into the generated code
 * without having any circular dependencies
 */
package ammonite
import acyclic.file

trait GenConfig[T <: GenConfig[T]]{
  def deeper: T
  def rename(s: String): String
}

trait GenUtils{
  type UP[T]
  type PP[T]
  type Config <: GenConfig[Config]
  def render[T: PP](t: T, c: Config): String
  def make[T](r: (T, Config) => (Seq[String], () => Seq[String])): UP[T]
}