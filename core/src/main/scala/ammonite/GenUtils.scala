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
  type PP[T]
  type PPer[T]
  type Config <: GenConfig[Config]
  def render[T: PP](t: T, c: Config): String
  def apply[T](r: (T, Config) => String): PPer[T]
  def handleChunks(name0: String,
                   chunks: Traversable[String],
                   chunks2: Traversable[String],
                   c: Config): String
}