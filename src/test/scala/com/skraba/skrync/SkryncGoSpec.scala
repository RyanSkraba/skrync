package com.skraba.skrync

import com.skraba.skrync.SkryncGo.{InternalDocoptException, go}
import org.docopt.DocoptExitException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[SkryncGo]]
  */
class SkryncGoSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  describe("SkryncGo docopt check") {
    it("should have less than 80 characters per string for readability.") {
      for (line <- SkryncGo.Doc.split("\n")) {
        withClue("main" -> line) {
          line.size should be < 80
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
        go("--version")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe SkryncGo.Version
    }

    it("throw an exception with --help") {
      val t = intercept[DocoptExitException] {
        go("--help")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe SkryncGo.Doc
    }

    it("throw an exception like --help when run bare") {
      val t = intercept[DocoptExitException] {
        go()
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe SkryncGo.Doc
    }
  }

  describe("SkryncGo command line options") {
    it("throw an exception like --help when run without a command") {
      val t = intercept[InternalDocoptException] {
        go("--debug")
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
        go(args: _*)
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
        go("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe SkryncGo.Doc
    }
  }
}
