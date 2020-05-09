package ammonite.terminal


/**
 * The core logic around a terminal; it defines the base `filters` API
 * through which anything (including basic cursor-navigation and typing)
 * interacts with the terminal.
 *
 * Maintains basic invariants, such as "cursor should always be within
 * the buffer", and "ansi terminal should reflect most up to date TermState"
 */
object Terminal {



  type Action = (Vector[Char], Int) => (Vector[Char], Int)
  type MsgAction = (Vector[Char], Int) => (Vector[Char], Int, String)



  /**
   * Blockingly reads a line from the given input stream and returns it.
   *
   * @param prompt The prompt to display when requesting input
   * @param reader The input-stream where characters come in, e.g. System.in
   * @param writer The output-stream where print-outs go, e.g. System.out
   * @param filters A set of actions that can be taken depending on the input,
   *                to manipulate the internal state of the terminal.
   * @param displayTransform code to manipulate the display of the buffer and
   *                         cursor, without actually changing the logical
   *                         values inside them.
   */
  def readLine(prompt: Prompt,
               reader: java.io.Reader,
               writer: java.io.Writer,
               filters: Filter,
               displayTransform: (Vector[Char], Int) => (fansi.Str, Int) = LineReader.noTransform)
               : Option[String] = {
    TTY.withSttyOverride(TTY.readLineStty()) {
      new LineReader(ConsoleDim.width(), prompt, reader, writer, filters, displayTransform)
        .readChar(TermState(LazyList.continually(reader.read()), Vector.empty, 0, ""), 0)
    }
  }
}
