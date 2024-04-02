package com.skraba.docoptcli

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
  val Doc: String = TaskDoc

  /** A minimal docopt constructed from the subtasks. */
  lazy val TaskDoc: String =
    s"""Usage:
       |  $Cli [--debug] <command> [args...]
       |
       |Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  --debug    Log extra information to the console while executing.
       |
       |Commands:
       |%s
       |
       |""".stripMargin.format {
      // Align the task subcommands and descriptions to the longest subcommand.
      val col = (0 +: Tasks.map(_.cmd.length)).max
      Tasks
        .map(task => s"%${col + 2}s  %s".format(task.cmd, task.description))
        .mkString("\n")
    }.trim

  /** The subcommands that this driver supports.
    * @param doc
    *   The [[Docopt]] for the subcommand.
    * @param cmd
    *   The subcommand token.
    * @param description
    *   The short description for the subcommand.
    * @param go
    *   The method to call with the argument map from the subcommand docopts.
    */
  case class Task(
      doc: String,
      cmd: String,
      description: String,
      go: java.util.Map[String, AnyRef] => Unit
  )

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
      .find(_.cmd == cmd)
      .getOrElse(throw new InternalDocoptException(s"Unknown command: $cmd"))

    try {
      val opts = new Docopt(task.doc)
        .withVersion(Version)
        .withExit(false)
        .parse(args.toList.asJava)
      task.go(opts)
    } catch {
      // This is only here to rewrap any internal docopt exception with the current docopt
      case ex: InternalDocoptException =>
        throw new InternalDocoptException(ex.getMessage, ex, task.doc)
      case ex: DocoptExitException if ex.getMessage == null =>
        throw new InternalDocoptException(
          null,
          ex,
          task.doc
        )
    }
  }

  def main(args: Array[String]): Unit = {
    // All of the command is executed in the go method, and this wraps DocOpt and exceptions for
    // console feedback.
    try {
      go(args: _*)
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
