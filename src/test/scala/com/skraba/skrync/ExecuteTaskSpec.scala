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

  describe(s"${Cli.Cli} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnMissingOpt(Seq.empty)
    itShouldThrowOnMissingOpt(Seq("--dstDigest", "x"))
    itShouldThrowOnMissingOpt(Seq("--srcDigest", "x"))
    itShouldThrowOnMissingOpt(Seq("--backup", "x"))

    itShouldThrowOnMissingOptValue(Seq("--dstDigest"))
    itShouldThrowOnMissingOptValue(Seq("--dstDigest", "x", "--srcDigest"))
    itShouldThrowOnMissingOptValue(Seq("--srcDigest"))
    itShouldThrowOnMissingOptValue(Seq("--srcDigest", "x", "--dstDigest"))
    itShouldThrowOnMissingOptValue(Seq("--srcDigest", "x", "--dstDigest", "x", "--backup"))
    itShouldThrowOnMissingOptValue(Seq("--plan"))
    itShouldThrowOnMissingOptValue(Seq("--plan", "x", "--backup"))
    itShouldThrowOnMissingOptValue(Seq("--backup", "x", "--plan"))

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
