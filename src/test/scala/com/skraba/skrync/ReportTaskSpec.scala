package com.skraba.skrync

import com.skraba.skrync.SkryncGoSpec._
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

import scala.reflect.io.File

/** Unit tests for [[ReportTask]] */
class ReportTaskSpec extends MultiTaskMainSpec(SkryncGo, Some(ReportTask)) with TmpDir {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(Tmp)

  describe(s"${Main.Name} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq.empty)

    itShouldThrowOnMissingFlagValue(Seq("--srcDigest"))

    it("throws an exception when the source digest doesn't exist") {
      val tSrc = interceptGo[IllegalArgumentException](TaskCmd, "--srcDigest", NonExistingPath)
      tSrc.getMessage shouldBe s"Source doesn't exist: $NonExistingPath"
    }

    it("throws an exception when the source digest is a directory") {
      val tSrc = interceptGo[IllegalArgumentException](TaskCmd, "--srcDigest", Small.src)
      tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"
    }

    ignore("throws an exception when the source digest is not a JSON file") {
      // TODO
      val tSrc = interceptGo[IllegalArgumentException](TaskCmd, "--srcDigest", Small.src / "ids.txt")
      tSrc.getMessage shouldBe s"Source is not a digest file: ${Small.src / "ids.txt"}"
    }
  }

  describe("SkryncGo report on small scenario") {

    describe("in the original") {
      it(s"it doesn't have any duplicates") {
        val cmp = ReportTask.report(SkryncGo.Analysis(Small.src, 0, SkryncDir.scan(Small.src, digest = true)))
        cmp.duplicateFiles shouldBe List()
      }
    }

    describe("in scenario4") {

      // Generate an analysis of the scenario4 directory
      val (dstFile, analysis) =
        withSkryncGoAnalysis(Small.srcWithDuplicateFile, Small.root / "dst" / File("scenario4.gz"))

      it("via the CLI") {

        // No exception should occur, and output is dumped to the console.
        val (stdout, stderr) = withGo(TaskCmd, "--srcDigest", dstFile)

        stdout should not have size(0)
        stdout should include(s"\nfrom: $dstFile\n")
        stdout should include("\ntotal files: 3\n")
        stdout should include(s"\n   ${Small.srcWithDuplicateFile}/ids.txt\n")
        stdout should include(s"\n   ${Small.srcWithDuplicateFile}/sub/ids.txt\n")
        stderr shouldBe ""
      }

      it("via the Report has one duplicate file") {
        val report: ReportTask.Report = ReportTask.report(analysis)
        report.duplicateFiles should have size 1
        report.duplicateFiles.head should have size 2
      }
    }
  }
}
