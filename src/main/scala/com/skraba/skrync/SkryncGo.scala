package com.skraba.skrync

import org.docopt.{Docopt, DocoptExitException}

import scala.collection.JavaConverters._
import scala.reflect.io.Directory

/** My synchronization tool.
  */
object SkryncGo {

  val Version: String = "0.0.1-SNAPSHOT"

  /** The subcommands that this driver supports.
    * @param doc The [[Docopt]] for the subcommand.
    * @param cmd The subcommand token.
    * @param description The short description for the subcommand.
    * @param go The method to call with the argument map from the subcommand docopts.
    */
  case class Task(
      doc: String,
      cmd: String,
      description: String,
      go: java.util.Map[String, AnyRef] => Unit
  )

  /** An analysis contains all of the information discovered while reading a directory.
    *
    * This can be persisted to disk and used to communicate between the tasks.
    *
    * @param src The root of the directory that was digested/analysed.
    * @param created The time that the the analysis was performed.
    * @param info All of the information information discovered at that location.
    */
  case class Analysis(src: Directory, created: Long, info: SkryncDir)

  /** [[DocoptExitException]] constructors are protected. */
  class InternalDocoptException(
      msg: String,
      ex: Throwable = None.orNull,
      val docopt: String = Doc
  ) extends RuntimeException(msg, ex)

  val Tasks: Seq[Task] =
    Seq(DigestTask.Task, ReportTask.Task, CompareTask.Task, ExecuteTask.Task)

  val Doc: String =
    """My file synchronization and backup tool.
      |
      |Usage:
      |  SkryncGo [--debug] <command> [args...]
      |
      |Options:
      |  -h --help  Show this screen.
      |  --version  Show version.
      |  --debug    Log extra information to the console while executing.
      |
      |Commands:
      |%s
      |
      |Analyzes and synchronizes changes between two directories.
      |""".stripMargin.format {
      // Align the task subcommands and descriptions to the longest subcommand.
      val col = (0 +: Tasks.map(_.cmd.length)).max
      Tasks
        .map(task => s"%${col + 2}s  %s".format(task.cmd, task.description))
        .mkString("\n")
    }.trim

  /** Runs the tool.  This does not handle any docopt exception automatically while parsing the
    * command line.
    *
    * @param args command-line arguments as described in [[Doc]]
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
      throw new InternalDocoptException("Missing command")

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
      // Rewrap any internal exception with the current docopt
      case ex @ (_: DocoptExitException | _: InternalDocoptException) =>
        throw new InternalDocoptException(ex.getMessage, ex, task.doc)
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
