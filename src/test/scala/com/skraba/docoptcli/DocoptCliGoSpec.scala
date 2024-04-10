package com.skraba.docoptcli

import org.docopt.DocoptExitException
import org.scalactic.source

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.reflect.ClassTag
import scala.reflect.io.Streamable

/** Unit test specification base for an [[DocoptCliGo]] */
abstract class DocoptCliGoSpec(protected val Cli: DocoptCliGo, protected val Task: Option[DocoptCliGo.Task] = None)
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterAll {

  /** The Docopt for the Task (if present) but defaulting to the Cli if not. */
  lazy val Doc: String = Task.map(_.Doc).getOrElse(Cli.Doc)

  /** The command used to specify the task (if present) or the empty string if not. */
  lazy val TaskCmd: String = Task.map(_.Cmd).getOrElse("")

  /** A flag that doesn't exist in the Docopt. */
  lazy val UnknownFlag: String = "--unknownDoesNotExistGarbage"

  /** A helper method used to capture the console and apply it to a partial function.
    * @param thunk
    *   code to execute that may use Console.out and Console.err print streams
    * @param pf
    *   A partial function to apply matchers
    * @tparam T
    *   The return value type of the thunk code to execute
    * @tparam U
    *   The return value type of the partial function to return.
    * @return
    *   The return value of the partial function.
    */
  def withConsoleMatch[T, U](
      thunk: => T
  )(pf: scala.PartialFunction[(T, String, String), U]): U = {
    Streamable.closing(new ByteArrayOutputStream()) { out =>
      Streamable.closing(new ByteArrayOutputStream()) { err =>
        Console.withOut(out) {
          Console.withErr(err) {
            val t = thunk
            Console.out.flush()
            Console.err.flush()
            // The return value
            pf(
              t,
              new String(out.toByteArray, StandardCharsets.UTF_8),
              new String(err.toByteArray, StandardCharsets.UTF_8)
            )
          }
        }
      }
    }
  }

  /** A helper method used to capture the console of a Cli execution and apply it to a partial function.
    * @param args
    *   String arguments to pass to the [[DocoptCliGo.go()]] method
    * @param pf
    *   A partial function to apply matchers
    * @tparam T
    *   The return value type of the thunk code to execute
    * @tparam U
    *   The return value type of the partial function to return.
    * @return
    *   The return value of the partial function.
    */
  def withGoMatching[T, U](args: Any*)(pf: scala.PartialFunction[(String, String), U]): U =
    withConsoleMatch(Cli.go(args.map(_.toString): _*)) { case (_, stdout, stderr) =>
      pf(stdout, stderr)
    }

  /** A helper method used to capture the console of a Cli execution and return the output.
    *
    * @param args
    *   String arguments to pass to the [[DocoptCliGo.go()]] method
    * @return
    *   A tuple of the stdout and stderr
    */
  def withGo(args: Any*): (String, String) = withGoMatching(args: _*) { case any => any }

  /** A helper method used to capture an exception thrown by [[withGo]]
    *
    * @param args
    *   String arguments to pass to the [[DocoptCliGo.go()]] method
    * @return
    *   The exception thrown when the arguments are run
    */
  def interceptGo[EX <: AnyRef](args: Any*)(implicit classTag: ClassTag[EX], pos: source.Position): EX =
    intercept[EX] { withGo(args: _*) }(classTag, pos)

  /** A helper method used to capture an [[DocoptExitException]] thrown by [[withGo]]
    *
    * @param args
    *   String arguments to pass to the [[DocoptCliGo.go()]] method
    * @return
    *   The exception thrown when the arguments are run
    */
  def interceptGoDocoptExitEx(args: Any*): DocoptExitException = interceptGo[DocoptExitException](args: _*)

  /** A helper method used to capture an [[Cli.InternalDocoptException]] thrown by [[withGo]]
    *
    * @param args
    *   String arguments to pass to the [[DocoptCliGo.go()]] method
    * @return
    *   The exception thrown when the arguments are run
    */
  def interceptGoDocoptEx(args: Any*): Cli.InternalDocoptException = interceptGo[Cli.InternalDocoptException](args: _*)

  /** A helper method used to capture an [[IllegalArgumentException]] thrown by [[withGo]]
    *
    * @param args
    *   String arguments to pass to the [[DocoptCliGo.go()]] method
    * @return
    *   The exception thrown when the arguments are run
    */
  def interceptGoIAEx(args: Any*): IllegalArgumentException = interceptGo[IllegalArgumentException](args: _*)

  /** Run tests on the --help and --version flags that cause a system exit. */
  val itShouldThrowOnHelpAndVersionFlags: () => Unit = () => {

    val prefixArgs = Task.map(_.Cmd).toSeq

    it(s"throws an exception with ${prefixArgs.mkString(" ")} --help") {
      val t = interceptGoDocoptExitEx(prefixArgs :+ "--help": _*)
      t.getMessage shouldBe Doc
      t.getExitCode shouldBe 0
    }

    it(s"throws an exception with a bare ${prefixArgs.mkString(" ")}") {
      val t = interceptGoDocoptExitEx(prefixArgs :+ "--help": _*)
      t.getMessage shouldBe Doc
      t.getExitCode shouldBe 0
    }

    it(s"throws an exception with ${prefixArgs.mkString(" ")} --version") {
      val t = interceptGoDocoptExitEx(prefixArgs :+ "--version": _*)
      t.getMessage shouldBe Cli.Version
      t.getExitCode shouldBe 0
    }
  }

  /** Run tests on an unrecognized flag. */
  val itShouldThrowOnUnknownFlag: () => Unit = () => {

    val prefixArgs = Task.map(_.Cmd).toSeq

    it("throws an exception with unknown option") {
      val t = interceptGoDocoptEx(prefixArgs :+ UnknownFlag: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe null
    }
  }

  /** Run tests on a command line that is missing necessary information for the Cli to proceed. */
  val itShouldThrowOnMissingOpt: Seq[String] => Unit = args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on missing options: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(allArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe null
    }
  }

  /** Run tests on a command line where the last argument is an option missing its value. */
  val itShouldThrowOnMissingOptValue: Seq[String] => Unit = args => {
    val allArgs = Task.map(_.Cmd).toSeq ++ args
    it("throws an exception on missing option parameters: " + allArgs.mkString(" ")) {
      val t = interceptGoDocoptExitEx(allArgs: _*)
      t.getExitCode shouldBe 1
      t.getMessage shouldBe s"${args.last} requires argument"
    }
  }
}
