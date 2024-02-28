package com.skraba.skrync

import com.skraba.skrync.DeduplicateTask.DedupPathReport
import com.skraba.skrync.SkryncGo.InternalDocoptException
import com.skraba.skrync.SkryncGoSpec.{withSkryncGo, withSkryncGoAnalysis}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.List
import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[DeduplicateTask]] */
class DeduplicateTaskSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  val DoesntExist: String = (Small.root / "doesnt-exist").toString()

  describe("SkryncGo report command line") {
    it("throws an exception with --help") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("dedup", "--help")
      }
      t.getMessage shouldBe DeduplicateTask.Doc
      t.docopt shouldBe DeduplicateTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("dedup"),
        List("dedup", "--srcDigest"),
        List("dedup", "--dedupDir")
      )
      for (args <- invalid) {
        val t = intercept[InternalDocoptException] {
          withSkryncGo(args: _*)
        }
        t.docopt shouldBe DeduplicateTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("dedup", "--garbage")
      }
      t.docopt shouldBe DeduplicateTask.Doc
    }

    it("throws an exception when the source digest doesn't exist") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "dedup",
          "--srcDigest",
          DoesntExist,
          "--dedupDir",
          Small.src.toString
        )
      }
      tSrc.getMessage shouldBe s"Source doesn't exist: $DoesntExist"
    }

    it("throws an exception when the source digest is a directory") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "dedup",
          "--srcDigest",
          Small.src.toString,
          "--dedupDir",
          Small.src.toString
        )
      }
      tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"
    }

    ignore("throws an exception when the source digest is not a JSON file") {
      // TODO
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "dedup",
          "--srcDigest",
          (Small.src / "ids.txt").toString(),
          "--dedupDir",
          Small.src.toString
        )
      }
      tSrc.getMessage shouldBe s"Source is not a digest file: ${Small.src / "ids.txt"}"
    }

    it("throws an exception when the dedup directory doesn't exist") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "dedup",
          "--srcDigest",
          (Small.src / "ids.txt").toString(),
          "--dedupDir",
          DoesntExist
        )
      }
      tSrc.getMessage shouldBe s"Deduplication directory doesn't exist: $DoesntExist"
    }

    it("throws an exception when the dedup directory is not a directory") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "dedup",
          "--srcDigest",
          (Small.src / "ids.txt").toString(),
          "--dedupDir",
          (Small.src / "ids.txt").toString()
        )
      }
      tSrc.getMessage shouldBe s"Deduplication directory is not a directory: ${Small.src / "ids.txt"}"
    }

    it("throws an exception when the dedup move directory doesn't exist") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "dedup",
          "--srcDigest",
          (Small.src / "ids.txt").toString(),
          "--dedupDir",
          Small.src.toString,
          "--mvDir",
          DoesntExist
        )
      }
      tSrc.getMessage shouldBe s"Duplicate destination directory doesn't exist: $DoesntExist"
    }

    it("throws an exception when the dedup move directory is not a directory") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "dedup",
          "--srcDigest",
          (Small.src / "ids.txt").toString(),
          "--dedupDir",
          Small.src.toString,
          "--mvDir",
          (Small.src / "ids.txt").toString()
        )
      }
      tSrc.getMessage shouldBe s"Duplicate destination directory is not a directory: ${Small.src / "ids.txt"}"
    }
  }

  /** Extracts paths relative to src/ for testing as strings. */
  def extractNameForTests(src: Path)(f: (Path, SkryncPath)): String =
    src.relativize(f._1).toString()

  describe(s"Using ${DedupPathReport.getClass.getSimpleName}") {

    // Generate an analysis of the scenario6 directory
    val (_, analysis) = withSkryncGoAnalysis(
      Small.srcWithDuplicates,
      Small.root / Directory("dst")
    )

    val extract = extractNameForTests(Small.srcWithDuplicates) _

    it("has one duplicate in dup1/") {
      val dupReport = DedupPathReport(
        analysis,
        Small.srcWithDuplicates / Directory("dup1")
      )

      dupReport.unknown should have size 1
      dupReport.unknown.map(extract) shouldBe List("dup1/ids3.txt")
      dupReport.known should have size 1
      dupReport.known.map(extract) shouldBe List("dup1/ids.txt")
    }

    it("has one duplicate in dup2/") {
      val dupReport = DedupPathReport(
        analysis,
        Small.srcWithDuplicates / Directory("dup2")
      )

      dupReport.unknown should have size 1
      dupReport.unknown.map(extract) shouldBe List(
        "dup2/ids4.txt"
      )
      dupReport.known should have size 1
      dupReport.known.map(extract) shouldBe List("dup2/ids4a.txt")
    }

    it("has two duplicates in dup3/") {
      val dupReport = DedupPathReport(
        analysis,
        Small.srcWithDuplicates / Directory("dup3")
      )

      dupReport.unknown should have size 1
      dupReport.unknown.map(extract) shouldBe List("dup3/ids5.txt")
      dupReport.known should have size 2
      dupReport.known.map(extract) shouldBe List(
        "dup3/ids2a.txt",
        "dup3/sub/ids5.txt"
      )
    }

    it("outside original scenario is entirely duplicated in this scenario") {
      val dupReport = DedupPathReport(
        analysis,
        Small.src
      )

      dupReport.unknown shouldBe empty
      dupReport.known should have size 2
      dupReport.known.map(extract) shouldBe List(
        "../../original/small/ids.txt",
        "../../original/small/sub/ids2.txt"
      )
    }

    it("outside scenario2/ has some duplicated files in this scenario") {
      val dupReport = DedupPathReport(
        analysis,
        Small.srcModifiedFile
      )

      dupReport.unknown should have size 1
      dupReport.unknown.map(extract) shouldBe List(
        "../../srcModifiedFile/small/sub/ids2.txt"
      )
      dupReport.known should have size 1
      dupReport.known.map(extract) shouldBe List(
        "../../srcModifiedFile/small/ids.txt"
      )
    }
  }
}