package com.skraba.docoptcli

import scala.io.AnsiColor

/** A configurable, colourful mechanism for interacting with the user via the terminal. */
trait AnsiConsole {

  /** True if the script should print extra verbose information. By default, be concise. */
  protected val _verbose: Boolean

  /** True if the script should avoid ANSI colour codes. By default, be colourful. */
  protected val _plain: Boolean

  /** True if the script should assume the user would reply yes to prompts. By default, ask whether to proceed. */
  protected val _yes: Boolean

  /** Optionally provide a printer to capture output. */
  protected val _print: Option[Any => Any]

  lazy val Black: String = ifAnsi(AnsiColor.BLACK)
  lazy val Red: String = ifAnsi(AnsiColor.RED)
  lazy val Green: String = ifAnsi(AnsiColor.GREEN)
  lazy val Yellow: String = ifAnsi(AnsiColor.YELLOW)
  lazy val Blue: String = ifAnsi(AnsiColor.BLUE)
  lazy val Magenta: String = ifAnsi(AnsiColor.MAGENTA)
  lazy val Cyan: String = ifAnsi(AnsiColor.CYAN)
  lazy val White: String = ifAnsi(AnsiColor.WHITE)

  lazy val BlackBg: String = ifAnsi(AnsiColor.BLACK_B)
  lazy val RedBg: String = ifAnsi(AnsiColor.RED_B)
  lazy val GreenBg: String = ifAnsi(AnsiColor.GREEN_B)
  lazy val YellowBg: String = ifAnsi(AnsiColor.YELLOW_B)
  lazy val BlueBg: String = ifAnsi(AnsiColor.BLUE_B)
  lazy val MagentaBg: String = ifAnsi(AnsiColor.MAGENTA_B)
  lazy val CyanBg: String = ifAnsi(AnsiColor.CYAN_B)
  lazy val WhiteBg: String = ifAnsi(AnsiColor.WHITE_B)

  lazy val Bold: String = ifAnsi(AnsiColor.BOLD)
  lazy val Reset: String = ifAnsi(AnsiColor.RESET)

  private[this] def ifAnsi(ansi: String, notAnsi: String = ""): String =
    if (this._plain) notAnsi else ansi;

  def style(
      in: Any,
      in2: Any = "",
      clr: String = White,
      reset: Boolean = true,
      bold: Boolean = false
  ): String = if (in2.toString.isEmpty && _plain)
    in.toString
  else if (in2.toString.isEmpty)
    s"${if (bold) Bold else ""}$clr$in${if (reset) Reset else ""}"
  else if (_plain)
    s"$in $in2"
  else
    s"$Bold$clr$in${if (bold) "" else s"$Reset$clr"} $in2${if (reset) Reset
      else ""}"

  def black(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, Black, reset, bold)
  def red(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, Red, reset, bold)
  def green(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, Green, reset, bold)
  def yellow(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, Yellow, reset, bold)
  def blue(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, Blue, reset, bold)
  def magenta(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, Magenta, reset, bold)
  def cyan(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, Cyan, reset, bold)
  def white(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, White, reset, bold)

  def blackBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, BlackBg, reset, bold)
  def redBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, RedBg, reset, bold)
  def greenBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, GreenBg, reset, bold)
  def yellowBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, YellowBg, reset, bold)
  def blueBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, BlueBg, reset, bold)
  def magentaBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, MagentaBg, reset, bold)
  def cyanBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, CyanBg, reset, bold)
  def whiteBg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    style(in, in2, WhiteBg, reset, bold)

  def bold(in: Any, in2: Any = "", reset: Boolean = true): String =
    style(in, in2, "", reset, bold = true)

  def msg(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    cyan(in, in2, reset, bold)
  def msg1(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    msg(in, in2, reset, bold)
  def msg2(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    blue(in, in2, reset, bold)
  def msg3(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    magenta(in, in2, reset, bold)
  def ok(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    green(in, in2, reset, bold)
  def warn(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    yellow(in, in2, reset, bold)
  def error(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    red(in, in2, reset, bold)
  def left(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    cyan(in, in2, reset, bold)
  def right(
      in: Any,
      in2: Any = "",
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    magenta(in, in2, reset, bold)
  def kv(
      key: Any,
      value: Any,
      reset: Boolean = true,
      bold: Boolean = false
  ): String =
    magenta(key, "", reset, bold) + ": " + value

  /** Print a standardized help for a script with subcommands
    * @param cli
    *   The script name
    * @param description
    *   A oneliner description of the script
    * @param subcommands
    *   Pairs of subcommands and their oneliner description
    * @return
    *   The string to print for that script.
    */
  def helpHeader(
      cli: String,
      description: String,
      subcommands: (String, String)*
  ): String = {

    val tmpl = s" $Cyan%1$$${subcommands.map(_._1.length).max}s$Reset : %2$$s"

    s"""$Bold$Green$cli$Reset - $description
       |
       |${subcommands.map(p => tmpl.format(p._1, p._2)).mkString("\n")}
       |
       |${Bold}Usage:$Reset
       |""".stripMargin
  }

  /** Print a usage line for a script.
    * @param cli
    *   The script name
    * @param subcommand
    *   The subcommand to run
    * @param args
    *   The arguments to demonstrate the script
    * @return
    *   The string to print for that script line
    */
  def helpUse(cli: String, subcommand: String, args: String*): String =
    s"""$Green$cli $Cyan$subcommand$Reset """ + args
      .map {
        case arg if arg.startsWith("[") => arg
        case arg                        => bold(arg)
      }
      .mkString(" ")

  /** Only if verbose is turned on, calls [[Console.print]] on the input. */
  def vPrint(in: => Any): Unit = if (_verbose) _print.getOrElse(Console.print _)(in)

  /** Only if verbose is turned on, calls [[Console.println]] on the input. */
  def vPrintln(in: => Any): Unit = if (_verbose) _print.map(_(in.toString + "\n")).getOrElse(Console.println(in))

  /** Only if verbose is turned on, calls [[Console.println]] on the input. */
  def vPrintln(): Unit = if (_verbose) _print.map(_("\n")).getOrElse(Console.println())

  /** Prompt the user and execute a function based on the response.
    *
    * If the --yes flag was set, the {{yFn}} is always used without prompting.
    *
    * @param prompt
    *   The query to use for the user.
    * @param yFn
    *   A function to execute if the prompt is positive (Y)
    * @param nFn
    *   A function to execute if the prompt is negative (n), by default, nothing
    * @param qFn
    *   A function to execute if the prompt is to abort (q), by default this is sys.exit(1)
    * @param otherFn
    *   A function to execute on other lines. This returns [[None]] if the prompt should be retried, {{Some(None)}} if
    *   [[None]] is the final answer, and {{Some(Some(x))}} if {{x}} should be returned.
    * @tparam T
    *   The return value of the function
    * @return
    *   The value returned by the function that was executed, or None if no function was called.
    */
  def ask[T](prompt: String)(
      yFn: => T,
      nFn: => Option[T] = None,
      qFn: => Option[T] = sys.exit(1),
      otherFn: Function[String, Option[Option[T]]] = (_: String) => None
  ): Option[T] = {
    if (!_yes) {
      LazyList
        .continually {
          print(s"$prompt (Y/n/q): ")
          val line = Console.in.readLine()
          val ask = line.toLowerCase().headOption
          if (ask.isEmpty || ask.contains('y')) Some(Some(yFn))
          else if (ask.isEmpty || ask.contains('n')) Some(nFn)
          else if (ask.isEmpty || ask.contains('q')) Some(qFn)
          else otherFn(line)
        }
        .filter(_.nonEmpty)
        .head
        .get
    } else {
      vPrintln(s"$prompt (Y/n/q): " + bold("Y"))
      Some(yFn)
    }
  }
}

object AnsiConsole {
  def apply(
      verbose: Boolean = false,
      plain: Boolean = false,
      yes: Boolean = false,
      print: Option[Any => Any] = None
  ): AnsiConsole = new AnsiConsole() {
    override protected val _verbose: Boolean = verbose
    override protected val _plain: Boolean = plain
    override protected val _yes: Boolean = yes
    override protected val _print: Option[Any => Any] = print
  }
}
