package test.ammonite.ops

/**
  * Created by haoyi on 2/17/16.
  */
object Unix {
  def apply() =
    java.nio.file.Paths.get("").toAbsolutePath.getRoot.toString == "/"
}

/**
  * Dummy class just used to test classloader relative/absolute resource logic
  */
class Testing
