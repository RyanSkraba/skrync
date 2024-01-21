package com.skraba.skrync

import com.skraba.skrync.SkryncGo.InternalDocoptException
import com.skraba.skrync.SkryncGoSpec.withSkryncGo
import org.docopt.DocoptExitException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[SkryncGo]] */
class SkryncGoSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {

  describe("SkryncGo docopt check") {
    it("should have less than 80 characters per string for readability.") {
      for (line <- SkryncGo.Doc.split("\n")) {
        withClue("main" -> line) {
          line.length should be < 80
        }
      }
      for (
        task <- SkryncGo.Tasks;
        line <- task.doc.split("\n")
      ) {
        withClue(task.cmd -> line) {
          line.length should be < 80
        }
      }
    }
  }

  describe("SkryncGo valid commands") {
    it("throw an exception with --version") {
      val t = intercept[DocoptExitException] {
        withSkryncGo("--version")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe SkryncGo.Version
    }

    it("throw an exception with --help") {
      val t = intercept[DocoptExitException] {
        withSkryncGo("--help")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe SkryncGo.Doc
    }

    it("throw an exception like --help when run bare") {
      val t = intercept[DocoptExitException] {
        withSkryncGo()
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe SkryncGo.Doc
    }
  }

  describe("SkryncGo command line options") {
    it("throw an exception like --help when run without a command") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("--debug")
      }
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe SkryncGo.Doc
    }

    for (
      args <- Seq(
        Seq("--garbage"),
        Seq("--debug", "--garbage"),
        Seq("--garbage", "--debug"),
        Seq("--garbage", "garbage")
      )
    ) it(s"throw an exception with unknown option $args") {
      val t = intercept[DocoptExitException] {
        withSkryncGo(args: _*)
      }
      t.getExitCode shouldBe 1
      t.getMessage shouldBe null
    }

    for (
      args <- Seq(
        Seq("garbage"),
        Seq("--debug", "garbage")
      )
    ) it(s"throw an exception when an unknown command is sent $args") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe SkryncGo.Doc
    }
  }
}

object SkryncGoSpec {

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

  /** A helper method used to capture the console of a SkryncGo execution and apply it to a partial function.
    * @param args
    *   String arguments to pass to the SkryncGo.go method
    * @param pf
    *   A partial function to apply matchers
    * @tparam T
    *   The return value type of the thunk code to execute
    * @tparam U
    *   The return value type of the partial function to return.
    * @return
    *   The return value of the partial function.
    */
  def withSkryncGoMatch[T, U](
      args: String*
  )(pf: scala.PartialFunction[(String, String), U]): U = {
    withConsoleMatch(SkryncGo.go(args: _*)) { case (_, stdout, stderr) =>
      pf(stdout, stderr)
    }
  }

  /** A helper method used to capture the console of a SkryncGo execution and return the output.
    *
    * @param args
    *   String arguments to pass to the SkryncGo.go method
    * @return
    *   A tuple of the stdout and stderr
    */
  def withSkryncGo(args: String*): (String, String) = {
    withSkryncGoMatch(args: _*) { case any => any }
  }

  /** A helper method used to create an analysis file and read it into memory.
    *
    * @param srcDir
    *   The source directory to read files from.
    * @param dstDigest
    *   Either the exact file to write the analysis to, or a directory to create with a default filename. If the file
    *   exists, it is read without re-scanning.
    * @param mustExist
    *   If true, scanning is never performed and the file is read as it is.
    * @return
    *   The destination digest and the analysis in memory.
    */
  def withSkryncGoAnalysis(
      srcDir: Directory,
      dstDigest: Path,
      mustExist: Boolean = false
  ): (File, SkryncGo.Analysis) = {
    dstDigest match {
      case f: File if f.exists || mustExist => (f, Json.read(f))
      case f: File =>
        f.parent.createDirectory(force = true, failIfExists = false);
        withSkryncGo(
          "digest",
          "--srcDir",
          srcDir.toString,
          "--dstDigest",
          dstDigest.toString
        )
        (f, Json.read(f))
      case d: Directory =>
        if (!d.exists)
          d.createDirectory(force = true, failIfExists = false)
        if (!mustExist)
          withSkryncGo(
            "digest",
            "--srcDir",
            srcDir.toString,
            "--dstDigest",
            dstDigest.toString
          )
        val dstDigestDefault: File = d.list.maxBy(_.lastModified).toFile
        (dstDigestDefault, Json.read(dstDigestDefault))
    }
  }
}
