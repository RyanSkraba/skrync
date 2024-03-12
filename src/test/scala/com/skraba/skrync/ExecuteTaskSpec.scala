package com.skraba.skrync

import com.skraba.skrync.SkryncGo.InternalDocoptException
import com.skraba.skrync.SkryncGoSpec.{interceptSkryncGoDocoptEx, interceptSkryncGoIAEx, withSkryncGo}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[ExecuteTask]] */
class ExecuteTaskSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val TempFolder: Path = Directory.makeTemp("skryncGo")

  /** Create a pre-existing file. */
  val ExistingFile: File = TempFolder / File("exists")
  Streamable.closing(ExistingFile.outputStream())(_.write(1))

  override protected def afterAll(): Unit =
    try {
      TempFolder.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  describe("SkryncGo execute command line") {
    it("throws an exception with --help") {
      val t = interceptSkryncGoDocoptEx("execute", "--help")
      t.getMessage shouldBe ExecuteTask.Doc
      t.docopt shouldBe ExecuteTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("execute"),
        List("execute", "--dstDigest"),
        List("execute", "--dstDigest", "x"),
        List("execute", "--dstDigest", "x", "--srcDigest"),
        List("execute", "--srcDigest"),
        List("execute", "--srcDigest", "x", "--dstDigest"),
        List("execute", "--srcDigest", "x", "--dstDigest", "y", "--backup"),
        List("execute", "--plan"),
        List("execute", "--plan", "--backup", "x"),
        List("execute", "--plan", "x", "--backup")
      )
      for (args <- invalid) {
        val t = interceptSkryncGoDocoptEx(args: _*)
        t.docopt shouldBe ExecuteTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = interceptSkryncGoDocoptEx("execute", "--garbage")
      t.docopt shouldBe ExecuteTask.Doc
    }

    it("throws an exception when the source or destination doesn't exist") {
      val tSrc = interceptSkryncGoIAEx("execute", "--srcDigest", "/doesnt-exist", "--dstDigest", ExistingFile)
      tSrc.getMessage shouldBe "Source doesn't exist: /doesnt-exist"

      val tDst = interceptSkryncGoIAEx("execute", "--srcDigest", ExistingFile, "--dstDigest", "/doesnt-exist")
      tDst.getMessage shouldBe "Destination doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source or destination is a directory") {
      val tSrc = interceptSkryncGoIAEx("execute", "--srcDigest", TempFolder, "--dstDigest", ExistingFile)
      tSrc.getMessage shouldBe s"Source is not a file: $TempFolder"

      val tDst = interceptSkryncGoIAEx("execute", "--srcDigest", ExistingFile, "--dstDigest", TempFolder)
      tDst.getMessage shouldBe s"Destination is not a file: $TempFolder"
    }
  }
}
