package ammonite.repl.frontend

import java.io.File

import ammonite.ops._
import ammonite.repl.Colors
import pprint.{PPrinter, PPrint, Config}
import ammonite.repl.{Colors, Ref, History}

import scala.reflect.runtime.universe._
import acyclic.file

import scala.util.control.ControlThrowable


class ReplAPIHolder {
  var repl0: FullReplAPI = null
  implicit lazy val repl = repl0
}

/**
 * Thrown to exit the REPL cleanly
 */
case class ReplExit(value: Any) extends ControlThrowable

trait ReplAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit = throw ReplExit(())

  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit(value: Any) = throw ReplExit(value)

  /**
   * Read/writable prompt for the shell. Use this to change the
   * REPL prompt at any time!
   */
  val prompt: Ref[String]
  /**
   * The front-end REPL used to take user input. Modifiable!
   */
  val frontEnd: Ref[FrontEnd]

  /**
   * Display help text if you don't know how to use the REPL
   */
  def help: String

  /**
   * History of commands that have been entered into the shell, including
   * previous sessions
   */
  def fullHistory: History

  /**
   * History of commands that have been entered into the shell during the
   * current session
   */
  def history: History

  /**
   * Get the `Type` object of [[T]]. Useful for finding
   * what its methods are and what you can do with it
   */
  def typeOf[T: WeakTypeTag]: Type

  /**
   * Get the `Type` object representing the type of `t`. Useful
   * for finding what its methods are and what you can do with it
   *
   */
  def typeOf[T: WeakTypeTag](t: => T): Type
  
  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  /**
   * The colors that will be used to render the Ammonite REPL in the terminal
   */
  val colors: Ref[Colors]

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  /**
   * Access the compiler to do crazy things if you really want to!
   */
  def compiler: scala.tools.nsc.Global

  /**
   * Show all the imports that are used to execute commands going forward
   */
  def imports: String
  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit val pprintConfig: Ref[pprint.Config]

  implicit def derefPPrint(implicit t: Ref[pprint.Config]): pprint.Config = t()

  /**
   * Current width of the terminal
   */
  def width: Int
  /**
   * Current height of the terminal
   */
  def height: Int

  def replArgs: Vector[ammonite.repl.Bind[_]]

  /**
   * Lets you configure the pretty-printing of a value. By default, it simply
   * disables truncation and prints the entire thing, but you can set other
   * parameters as well if you want.
   */
  def show[T: PPrint](implicit cfg: Config): T => Unit
  def show[T: PPrint](t: T,
                      width: Integer = 0,
                      height: Integer = null,
                      indent: Integer = null,
                      colors: pprint.Colors = null)
                     (implicit cfg: Config = Config.Defaults.PPrintConfig): Unit
}

// End of OpsAPI
trait LoadJar {
  /**
   * Load a `.jar` file
   */
  def jar(jar: Path): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit
}
trait Load extends ((String, Boolean) => Unit) with LoadJar{
  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String, visible: Boolean = false): Unit

  /**
   * Loads and executes the scriptfile on the specified path.
   * Compilation units separated by `@\n` are evaluated sequentially.
   * If an error happens it prints an error message to the console.
   */ 
  def exec(path: Path): Unit

  def module(path: Path): Unit

  def plugin: LoadJar

}

// End of ReplAPI
/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
abstract class FullReplAPI extends ReplAPI{
  val Internal: Internal
  trait Internal{
    def combinePrints(iters: Iterator[String]*): Iterator[String]

    /**
     * Kind of an odd signature, splitting out [[T]] and [[V]]. This is
     * seemingly useless but necessary because when you add both [[TPrint]]
     * and [[PPrint]] context bounds to the same type, Scala's type inference
     * gets confused and does the wrong thing
     */
    def print[T: TPrint: WeakTypeTag, V: PPrint]
             (value: => T, value2: => V, ident: String, custom: Option[String])
             (implicit cfg: Config): Iterator[String]

    def printDef(definitionLabel: String, ident: String): Iterator[String]
    def printImport(imported: String): Iterator[String]
  }
  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]
}

object ReplAPI{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI) = {
    val method = holder
      .getDeclaredMethods
      .find(_.getName == "repl0_$eq")
      .get
    method.invoke(null, api)
  }
}


trait DefaultReplAPI extends FullReplAPI {

  def help =
    """Welcome to the Ammonite Scala REPL! Enter a Scala expression and it will be evaluated.
      |All your standard Bash hotkeys should work for navigating around or editing the line
      |being entered, as well as some GUI hotkeys like alt-shift-left/right to select words
      |to replace. Hit <tab> to autocomplete possible names.
      |
      |For a list of REPL built-ins and configuration, use `repl.<tab>`. For a more detailed
      |description of how to use the REPL, check out http://lihaoyi.github.io/Ammonite
    """.stripMargin.trim
  object Internal extends Internal{
    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator
           .filter(!_.isEmpty)
           .flatMap(Iterator("\n") ++ _)
           .drop(1)
    }

    def print[T: TPrint: WeakTypeTag, V: PPrint](value: => T,
                                                 value2: => V,
                                                 ident: String,
                                                 custom: Option[String])
                                                (implicit cfg: pprint.Config) = {
      if (typeOf[T] =:= typeOf[Unit]) Iterator()
      else {
        val pprint = implicitly[PPrint[V]]
        val rhs = custom match {
          case None => pprint.render(value2, cfg)
          case Some(s) => Iterator(cfg.colors.literalColor, s, cfg.colors.endColor)
        }
        Iterator(
          colors().ident(), ident, colors().reset(), ": ",
          implicitly[TPrint[T]].render(cfg), " = "
        ) ++ rhs
      }
    }
    def printDef(definitionLabel: String, ident: String) = {
      Iterator(
        "defined ", colors().`type`(), definitionLabel, " ",
        colors().ident(), ident, colors().reset()
      )
    }
    def printImport(imported: String) = {
      Iterator(colors().`type`(), "import ", colors().ident(), imported, colors().reset())
    }
  }
}
object ReplBridge extends ammonite.repl.frontend.ReplAPIHolder{}