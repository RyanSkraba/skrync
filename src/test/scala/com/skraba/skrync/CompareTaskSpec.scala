package com.skraba.skrync

import com.skraba.skrync.SkryncGo.{InternalDocoptException, go}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[CompareTask]]
  */
class CompareTaskSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val TempFolder: Path = Directory.makeTemp("skryncGo")

  /** Create a pre-existing file. */
  val ExistingFile = TempFolder / File("exists")
  Streamable.closing(ExistingFile.outputStream())(_.write(1))

  override protected def afterAll(): Unit =
    try {
      TempFolder.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  describe("SkryncGo compare command line") {
    it("throws an exception with --help") {
      val t = intercept[InternalDocoptException] {
        go("compare", "--help")
      }
      t.getMessage shouldBe CompareTask.Doc
      t.docopt shouldBe CompareTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("compare"),
        List("compare", "--dstDigest"),
        List("compare", "--dstDigest", "x"),
        List("compare", "--dstDigest", "x", "--srcDigest"),
        List("compare", "--srcDigest"),
        List("compare", "--srcDigest", "x", "--dstDigest")
      )
      for (args <- invalid) {
        val t = intercept[InternalDocoptException] {
          go(args: _*)
        }
        t.docopt shouldBe CompareTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = intercept[InternalDocoptException] {
        go("compare", "--garbage")
      }
      t.docopt shouldBe CompareTask.Doc
    }

    it("throws an exception when the source or destination doesn't exist") {
      val tSrc = intercept[IllegalArgumentException] {
        go(
          "compare",
          "--srcDigest",
          "/doesnt-exist",
          "--dstDigest",
          ExistingFile.toString
        )
      }
      tSrc.getMessage shouldBe "Source doesn't exist: /doesnt-exist"

      val tDst = intercept[IllegalArgumentException] {
        go(
          "compare",
          "--srcDigest",
          ExistingFile.toString,
          "--dstDigest",
          "/doesnt-exist"
        )
      }
      tDst.getMessage shouldBe "Destination doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source or destination is a directory") {
      val tSrc = intercept[IllegalArgumentException] {
        go(
          "compare",
          "--srcDigest",
          TempFolder.toString(),
          "--dstDigest",
          ExistingFile.toString
        )
      }
      tSrc.getMessage shouldBe s"Source is not a file: $TempFolder"

      val tDst = intercept[IllegalArgumentException] {
        go(
          "compare",
          "--srcDigest",
          ExistingFile.toString,
          "--dstDigest",
          TempFolder.toString()
        )
      }
      tDst.getMessage shouldBe s"Destination is not a file: $TempFolder"
    }
  }
}
