package com.skraba.docoptcli

import com.skraba.docoptcli.DocoptCliGo.Task
import org.docopt.{Docopt, DocoptExitException}

import scala.jdk.CollectionConverters._

/** A base class for a Docopt oriented command line script that can take multiple subcommands. */
trait DocoptCliGo {

  /** The script name. */
  val Cli: String

  /** The version to print when the --version flag is set. */
  val Version: String

  /** A list of subcommands that can be executed. */
  val Tasks: Seq[Task]

  /** The full, main docopt that can be used for the script. */
  lazy val Doc: String = SimpleDoc

  /** The simple docopt without any description or additional text. */
   val SimpleDoc: String = UsageDoc + "\n\n" + OptionsDoc + "\n\n" + CommandsDoc

  /** A suggested usage section for the basic docopt. */
  lazy val UsageDoc: String =
    s"""Usage:
       |  $Cli [--debug] <command> [args...]""".stripMargin

  /** A suggested options section for the docopt. */
  lazy val OptionsDoc: String =
    s"""Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  --debug    Log extra information to the console while executing.""".stripMargin

  /** A suggested commands section for the docopt. */
  lazy val CommandsDoc: String =
    """Commands:
      |%s""".stripMargin.format {
      // Align the task subcommands and descriptions to the longest subcommand.
      val col = (0 +: Tasks.map(_.Cmd.length)).max
      Tasks
        .map(task => s"%${col + 2}s  %s".format(task.Cmd, task.Description))
        .mkString("\n")
    }.trim

  /** [[DocoptExitException]] constructors are protected. */
  class InternalDocoptException(
      msg: String,
      ex: Throwable = None.orNull,
      val docopt: String = Doc
  ) extends RuntimeException(msg, ex)

  /** Runs the tool. This does not handle any docopt exception automatically while parsing the command line.
    *
    * @param args
    *   command-line arguments as described in [[Doc]]
    */
  @throws[DocoptExitException]
  @throws[InternalDocoptException]
  def go(args: String*): Unit = {
    // Java docopts doesn't support ignoring options after the command, so strip them first.
    val mainArgs: Seq[String] = if (args.nonEmpty) {
      val (options, cmd) = args.span(_.startsWith("-"))
      if (cmd.isEmpty) options :+ "???" else options :+ cmd.head
    } else Seq("--help")

    // Get the command, but throw exceptions for --help and --version
    val cmd = new Docopt(Doc)
      .withVersion(Version)
      .withOptionsFirst(true)
      .withExit(false)
      .parse(mainArgs.asJava)
      .get("<command>")
      .asInstanceOf[String]

    // This is only here to rewrap any internal docopt exception with the current docopt
    if (cmd == "???")
      throw new InternalDocoptException("Missing command", docopt = Doc)

    // Reparse with the specific command.
    val task = Tasks
      .find(_.Cmd == cmd)
      .getOrElse(throw new InternalDocoptException(s"Unknown command: $cmd"))

    try {
      val opts = new Docopt(task.Doc).withVersion(Version).withExit(false).parse(args.asJava)
      task.go(new task.TaskOptions(opts))
    } catch {
      // This is only here to rewrap any internal docopt exception with the current docopt
      case ex: InternalDocoptException =>
        throw new InternalDocoptException(ex.getMessage, ex, task.Doc)
      case ex: DocoptExitException if ex.getMessage == null =>
        throw new InternalDocoptException(null, ex, task.Doc)
    }
  }

  def main(args: Array[String]): Unit = {
    // All of the command is executed in the go method, and this wraps DocOpt and exceptions for
    // console feedback.
    try {
      go(args.toIndexedSeq: _*)
    } catch {
      case ex: DocoptExitException =>
        Option(if (ex.getExitCode == 0) System.out else System.err)
          .foreach(ps => {
            if (ex.getMessage != null) ps.println(ex.getMessage)
            else ps.println(Doc)
          })
        System.exit(ex.getExitCode)
      case ex: InternalDocoptException =>
        println(ex.docopt)
        if (ex.getMessage != null) {
          println()
          println(ex.getMessage)
        }
        System.exit(1)
      case ex: Exception =>
        println(Doc)
        println()
        ex.printStackTrace()
        System.exit(1)
    }
  }
}

object DocoptCliGo {

  /** A subcommand or task supported by this driver */
  abstract class Task() {

    /** A helper class to extract options from the docopts.
      * @param opts
      *   The raw map exposed by docopts.
      */
    class TaskOptions(private val opts: java.util.Map[String, AnyRef]) {
      def getStringsOption(key: String): Option[Iterable[String]] =
        Option(opts.get(key)).map(_.asInstanceOf[java.lang.Iterable[String]].asScala)
      def getStrings(key: String, default: Iterable[String]): Iterable[String] =
        getStringsOption(key).getOrElse(default)
      def getStrings(key: String): Iterable[String] = getStringsOption(key).get

      def getStringOption(key: String): Option[String] = Option(opts.get(key)).map(_.toString)
      def getString(key: String, default: String): String = getStringOption(key).getOrElse(default)
      def getString(key: String): String = getStringOption(key).get

      def getIntOption(key: String): Option[Int] = Option(opts.get(key)).map(_.toString.toInt)
      def getInt(key: String, default: Int): Int = getIntOption(key).getOrElse(default)
      def getInt(key: String): Int = getIntOption(key).get

      def getBooleanOption(key: String): Option[Boolean] = Option(opts.get(key)).map(_.toString.toBoolean)
      def getBoolean(key: String, default: Boolean): Boolean = getBooleanOption(key).getOrElse(default)
      def getBoolean(key: String): Boolean = getBooleanOption(key).get
    }

    /** The [[Docopt]] for the subcommand */
    val Doc: String

    /** The subcommand token, used to pick the task from the driver */
    val Cmd: String

    /** A short description for the subcommand */
    val Description: String

    /** Executes this subcommand
      * @param opts
      *   the docopts already parsed
      */
    def go(opts: TaskOptions): Unit
  }

}
