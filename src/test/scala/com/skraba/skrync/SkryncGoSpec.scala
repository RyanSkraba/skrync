package com.skraba.skrync

import com.skraba.docoptcli.DocoptCliGoSpec
import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[SkryncGo]] */
class SkryncGoSpec extends DocoptCliGoSpec(SkryncGo) {

  describe(s"${Cli.Cli} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Cli.Doc
    }

    for (
      args <- Seq(
        Seq("--garbage"),
        Seq("--debug", "--garbage"),
        Seq("--garbage", "--debug"),
        Seq("--garbage", "garbage")
      )
    ) it(s"throw an exception with unknown option $args") {
      val t = interceptGoDocoptExitEx(args: _*)
      t.getExitCode shouldBe 1
      t.getMessage shouldBe null
    }

    for (
      args <- Seq(
        Seq("garbage"),
        Seq("--debug", "garbage")
      )
    ) it(s"throw an exception when an unknown command is sent $args") {
      val t = interceptGoDocoptEx("garbage")
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe SkryncGo.Doc
    }
  }
}

object SkryncGoSpec extends DocoptCliGoSpec(SkryncGo) {

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
        withGo("digest", "--srcDir", srcDir, "--dstDigest", dstDigest)
        (f, Json.read(f))
      case d: Directory =>
        if (!d.exists)
          d.createDirectory(force = true, failIfExists = false)
        if (!mustExist)
          withGo("digest", "--srcDir", srcDir, "--dstDigest", dstDigest)
        val dstDigestDefault: File = d.list.maxBy(_.lastModified).toFile
        (dstDigestDefault, Json.read(dstDigestDefault))
    }
  }
}
