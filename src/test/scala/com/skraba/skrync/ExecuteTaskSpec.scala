package com.skraba.skrync

import com.skraba.docoptcli.DocoptCliGoSpec

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[ExecuteTask]] */
class ExecuteTaskSpec extends DocoptCliGoSpec(SkryncGo, Some(ExecuteTask)) {

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
      val t = interceptGoDocoptExitEx("execute", "--help")
      t.getMessage shouldBe ExecuteTask.Doc
      // t.docopt shouldBe ExecuteTask.Doc
    }

    describe("when missing information") {
      for (
        args <- List(
          List("execute"),
          List("execute", "--srcDigest", "x"),
          List("execute", "--dstDigest", "x"),
          List("execute", "--backup", "x")
        )
      ) {
        it("throws an exception on missing options: " + args.mkString(" ")) {
          val t = interceptGoDocoptEx(args: _*)
          t.docopt shouldBe ExecuteTask.Doc
        }
      }

      for (
        (opt, args) <- List(
          "--dstDigest" -> List("execute", "--dstDigest"),
          "--srcDigest" -> List("execute", "--dstDigest", "x", "--srcDigest"),
          "--srcDigest" -> List("execute", "--srcDigest"),
          "--dstDigest" -> List("execute", "--srcDigest", "x", "--dstDigest"),
          "--backup" -> List("execute", "--srcDigest", "x", "--dstDigest", "x", "--backup"),
          "--plan" -> List("execute", "--plan"),
          "--backup" -> List("execute", "--plan", "x", "--backup"),
          "--plan" -> List("execute", "--backup", "x", "--plan")
        )
      ) {
        it("throws an exception on missing option parameters: " + args.mkString(" ")) {
          val t = interceptGoDocoptExitEx(args: _*)
          t.getExitCode shouldBe 1
          t.getMessage shouldBe s"$opt requires argument"
        }
      }
    }

    it("throws an exception with unknown option") {
      val t = interceptGoDocoptEx("execute", "--garbage")
      t.docopt shouldBe ExecuteTask.Doc
    }

    it("throws an exception when the source or destination doesn't exist") {
      val tSrc = interceptGoIAEx("execute", "--srcDigest", "/doesnt-exist", "--dstDigest", ExistingFile)
      tSrc.getMessage shouldBe "Source doesn't exist: /doesnt-exist"

      val tDst = interceptGoIAEx("execute", "--srcDigest", ExistingFile, "--dstDigest", "/doesnt-exist")
      tDst.getMessage shouldBe "Destination doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source or destination is a directory") {
      val tSrc = interceptGoIAEx("execute", "--srcDigest", TempFolder, "--dstDigest", ExistingFile)
      tSrc.getMessage shouldBe s"Source is not a file: $TempFolder"

      val tDst = interceptGoIAEx("execute", "--srcDigest", ExistingFile, "--dstDigest", TempFolder)
      tDst.getMessage shouldBe s"Destination is not a file: $TempFolder"
    }
  }
}
