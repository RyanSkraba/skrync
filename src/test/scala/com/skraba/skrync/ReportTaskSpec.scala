package com.skraba.skrync

import com.skraba.skrync.SkryncGoSpec.{
  interceptSkryncGoDocoptEx,
  interceptSkryncGoDocoptExitEx,
  interceptSkryncGoIAEx,
  withSkryncGo,
  withSkryncGoAnalysis
}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[ReportTask]] */
class ReportTaskSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  val DoesntExist: String = (Small.root / "doesnt-exist").toString()

  describe("SkryncGo report command line") {
    it("throws an exception with --help") {
      val t = interceptSkryncGoDocoptExitEx("report", "--help")
      t.getMessage shouldBe ReportTask.Doc
      t.getExitCode shouldBe 0
    }

    describe("when missing information") {
      for (
        args <- List(
          List("report")
        )
      ) {
        it("throws an exception on missing options: " + args.mkString(" ")) {
          val t = interceptSkryncGoDocoptEx(args: _*)
          t.docopt shouldBe ReportTask.Doc
        }
      }

      for (
        (opt, args) <- List(
          "--srcDigest" -> List("report", "--srcDigest")
        )
      ) {
        it("throws an exception on missing option parameters: " + args.mkString(" ")) {
          val t = interceptSkryncGoDocoptExitEx(args: _*)
          t.getExitCode shouldBe 1
          t.getMessage shouldBe s"$opt requires argument"
        }
      }
    }

    it("throws an exception with unknown option") {
      val t = interceptSkryncGoDocoptEx("report", "--garbage")
      t.docopt shouldBe ReportTask.Doc
    }

    it("throws an exception when the source digest doesn't exist") {
      val tSrc = interceptSkryncGoIAEx("report", "--srcDigest", DoesntExist)
      tSrc.getMessage shouldBe s"Source doesn't exist: $DoesntExist"
    }

    it("throws an exception when the source digest is a directory") {
      val tSrc = interceptSkryncGoIAEx("report", "--srcDigest", Small.src)
      tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"
    }

    ignore("throws an exception when the source digest is not a JSON file") {
      // TODO
      val tSrc = interceptSkryncGoIAEx("report", "--srcDigest", Small.src / "ids.txt")
      tSrc.getMessage shouldBe s"Source is not a digest file: ${Small.src / "ids.txt"}"
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
        val (stdout, stderr) = withSkryncGo("report", "--srcDigest", dstFile)

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
}
