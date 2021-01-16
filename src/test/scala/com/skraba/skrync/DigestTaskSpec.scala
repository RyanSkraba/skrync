package com.skraba.skrync

import com.skraba.skrync.SkryncGo.{InternalDocoptException, go}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[DigestTask]]
  */
class DigestTaskSpec
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

  describe("SkryncGo digest command line") {

    it("throws an exception with --help") {
      val t = intercept[InternalDocoptException] {
        go("digest", "--help")
      }
      t.getMessage shouldBe DigestTask.Doc
      t.docopt shouldBe DigestTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("digest"),
        List("digest", "--srcDir"),
        List("digest", "--srcDir", "x", "--dstDir")
      )
      for (args <- invalid) {
        val t = intercept[InternalDocoptException] {
          go(args: _*)
        }
        t.docopt shouldBe DigestTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = intercept[InternalDocoptException] {
        go("digest", "--srcDigest")
      }
      t.docopt shouldBe DigestTask.Doc
    }

    it("throws an exception when the source doesn't exist") {
      val t = intercept[IllegalArgumentException] {
        go("digest", "--srcDir", "/doesnt-exist")
      }
      t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source is a file") {
      val t = intercept[IllegalArgumentException] {
        go("digest", "--srcDir", ExistingFile.toString)
      }
      t.getMessage shouldBe s"Source is not a directory: $ExistingFile"
    }
  }
}
