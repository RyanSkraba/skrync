package com.skraba.skrync

import com.skraba.skrync.SkryncGo.InternalDocoptException
import com.skraba.skrync.SkryncGoSpec.{withSkryncGo, withSkryncGoAnalysis}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[ReportTask]] */
class ReportTaskSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  describe("SkryncGo report command line") {
    it("throws an exception with --help") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("report", "--help")
      }
      t.getMessage shouldBe ReportTask.Doc
      t.docopt shouldBe ReportTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("report"),
        List("report", "--srcDigest")
      )
      for (args <- invalid) {
        val t = intercept[InternalDocoptException] {
          withSkryncGo(args: _*)
        }
        t.docopt shouldBe ReportTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("report", "--garbage")
      }
      t.docopt shouldBe ReportTask.Doc
    }

    it("throws an exception when the source or destination doesn't exist") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "report",
          "--srcDigest",
          "/doesnt-exist"
        )
      }
      tSrc.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source or destination is a directory") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "report",
          "--srcDigest",
          Small.src.toString()
        )
      }
      tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"
    }
  }

  describe("SkryncGo report on small scenario") {

    describe("in the original") {
      it(s"it doesn't have any duplicates") {
        val cmp = ReportTask.report(
          SkryncGo.Analysis(
            Small.src,
            0,
            SkryncDir.scan(Small.src, digest = true)
          )
        )
        cmp.duplicateFiles shouldBe List()
      }
    }

    describe("in scenario4") {

      // Generate an analysis of the scenario4 directory
      val (dstFile, analysis) = withSkryncGoAnalysis(
        Small.srcWithDuplicateFile,
        Small.root / "dst" / File("scenario4.gz")
      )

      it("via the CLI") {

        // No exception should occur, and output is dumped to the console.
        val (stdout, stderr) = withSkryncGo(
          "report",
          "--srcDigest",
          dstFile.toString()
        )

        stdout should not have size(0)
        stdout should include(s"\nfrom: $dstFile\n")
        stdout should include("\ntotal files: 3\n")
        stdout should include(s"\n   ${Small.srcWithDuplicateFile}/ids.txt\n")
        stdout should include(
          s"\n   ${Small.srcWithDuplicateFile}/sub/ids.txt\n"
        )
        stderr shouldBe ""
      }

      it("via the Report has one duplicate file") {
        val report: ReportTask.Report = ReportTask.report(analysis)
        report.duplicateFiles should have size 1
        report.duplicateFiles.head should have size 2
      }
    }
  }

  /** Extracts paths relative to src/ for testing as strings. */
  def extractNameForTests(src: Path)(f: (Path, SkryncPath)): String =
    src.relativize(f._1).toString()

  describe(s" Using ${ReportTask.DedupPathReport.getClass.getSimpleName}") {

    // Generate an analysis of the scenario6 directory
    val (_, analysis) = withSkryncGoAnalysis(
      Small.srcWithDuplicates,
      Small.root / Directory("dst")
    )

    val extract = extractNameForTests(Small.srcWithDuplicates) _

    it("has one duplicate in dup1/") {
      val dupReport = ReportTask.DedupPathReport(
        analysis,
        Small.srcWithDuplicates / Directory("dup1")
      )

      dupReport.uniques should have size 1
      dupReport.uniques.map(extract) shouldBe List("dup1/ids3.txt")
      dupReport.duplicates should have size 1
      dupReport.duplicates.map(extract) shouldBe List("dup1/ids.txt")
    }

    it("has one duplicate in dup2/") {
      val dupReport = ReportTask.DedupPathReport(
        analysis,
        Small.srcWithDuplicates / Directory("dup2")
      )

      // TODO: This is the expected behaviour
      //      dupReport.uniques should have size 1
      //      dupReport.uniques.map(named) shouldBe List(
      //        "dup2/ids4.txt"
      //      )
      //      dupReport.duplicates should have size 1
      //      dupReport.duplicates.map(named) shouldBe List("dup2/ids4a.txt")
    }

    it("has two duplicates in dup3/") {
      val dupReport = ReportTask.DedupPathReport(
        analysis,
        Small.srcWithDuplicates / Directory("dup3")
      )

      analysis.src
      // TODO: This is the expected behaviour
      //      dupReport.uniques should have size 1
      //      dupReport.uniques.map(named) shouldBe List("dup3/ids5.txt")
      //      dupReport.duplicates should have size 2
      //      dupReport.duplicates.map(named) shouldBe List("dup3/ids2a.txt", "dup3/sub/ids5a.txt")
    }
  }
}
