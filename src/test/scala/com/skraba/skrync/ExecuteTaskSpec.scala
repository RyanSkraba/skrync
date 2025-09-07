package com.skraba.skrync

import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[ExecuteTask]] */
class ExecuteTaskSpec extends MultiTaskMainSpec(SkryncGo, Some(ExecuteTask)) with TmpDir {

  describe(s"${Main.Name} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq.empty)
    itShouldThrowOnIncompleteArgs(Seq("--dstDigest", "x"))
    itShouldThrowOnIncompleteArgs(Seq("--srcDigest", "x"))
    itShouldThrowOnIncompleteArgs(Seq("--backup", "x"))

    itShouldThrowOnMissingFlagValue(Seq("--dstDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--dstDigest", "x", "--srcDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dstDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dstDigest", "x", "--backup"))
    itShouldThrowOnMissingFlagValue(Seq("--plan"))
    itShouldThrowOnMissingFlagValue(Seq("--plan", "x", "--backup"))
    itShouldThrowOnMissingFlagValue(Seq("--backup", "x", "--plan"))

    it("throws an exception when the source or destination doesn't exist") {
      val tSrc =
        interceptGo[IllegalArgumentException]("execute", "--srcDigest", "/doesnt-exist", "--dstDigest", ExistingFile)
      tSrc.getMessage shouldBe "Source doesn't exist: /doesnt-exist"

      val tDst =
        interceptGo[IllegalArgumentException]("execute", "--srcDigest", ExistingFile, "--dstDigest", "/doesnt-exist")
      tDst.getMessage shouldBe "Destination doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source or destination is a directory") {
      val tSrc = interceptGo[IllegalArgumentException]("execute", "--srcDigest", Tmp, "--dstDigest", ExistingFile)
      tSrc.getMessage shouldBe s"Source is not a file: $Tmp"

      val tDst = interceptGo[IllegalArgumentException]("execute", "--srcDigest", ExistingFile, "--dstDigest", Tmp)
      tDst.getMessage shouldBe s"Destination is not a file: $Tmp"
    }
  }
}
