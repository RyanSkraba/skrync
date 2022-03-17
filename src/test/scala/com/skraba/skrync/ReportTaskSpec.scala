package com.skraba.skrync

import com.skraba.skrync.SkryncGo.InternalDocoptException
import com.skraba.skrync.SkryncGoSpec.withSkryncGo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[ReportTask]]
  */
class ReportTaskSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit =
    Small.cleanup()

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

  describe("SkryncGo report on the small scenario") {

    // Create a analysis file from the same scenario.
    val dstDir: Directory = Small.root.resolve("dst").toDirectory
    dstDir.createDirectory()
    withSkryncGo(
      "digest",
      "--srcDir",
      Small.srcWithDuplicateFile.toString,
      "--dstDigest",
      (dstDir / File("compare.gz")).toString
    )

    it(s"doesn't have any duplicates in ${Small.src}") {
      val cmp = ReportTask.report(
        SkryncGo.Analysis(
          Small.src,
          0,
          SkryncDir.scan(Small.src, digest = true)
        )
      )
      cmp.duplicateFiles shouldBe List()
    }

    it(s"via the CLI in ${Small.srcWithDuplicateFile}") {
      // No exception should occur, and output is dumped to the console.
      val (stdout, stderr) = withSkryncGo(
        "report",
        "--srcDigest",
        (dstDir / File("compare.gz")).toString()
      )

      stdout should not have size(0)
      stdout should include(s"\nfrom: $dstDir/compare.gz\n")
      stdout should include("\ntotal files: 3\n")
      stdout should include(s"\n   ${Small.srcWithDuplicateFile}/ids.txt\n")
      stdout should include(s"\n   ${Small.srcWithDuplicateFile}/sub/ids.txt\n")
      stderr shouldBe ""
    }

    it("scenario4 has one duplicate file") {
      val analysis: SkryncGo.Analysis = Json.read(dstDir / File("compare.gz"))
      val report: ReportTask.Report = ReportTask.report(analysis);

      report.duplicateFiles should have size 1
      report.duplicateFiles.head should have size 2
    }
  }
}
