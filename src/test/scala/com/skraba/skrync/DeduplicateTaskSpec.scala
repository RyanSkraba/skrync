package com.skraba.skrync

import com.skraba.skrync.DeduplicateTask.DedupPathReport
import com.skraba.skrync.SkryncGoSpec._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[DeduplicateTask]] */
class DeduplicateTaskSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  // Generate an analysis of the scenario6 directory
  val (srcDigest, analysis) = withSkryncGoAnalysis(
    Small.srcWithDuplicates,
    Small.dst
  )

  describe("SkryncGo dedup command line") {

    it("throws an exception with --help") {
      val t = interceptSkryncGoDocoptEx("dedup", "--help")
      t.getMessage shouldBe DeduplicateTask.Doc
      t.docopt shouldBe DeduplicateTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("dedup"),
        List("dedup", "--srcDigest"),
        List("dedup", "--srcDigest", "x", "--dedupDir"),
        List("dedup", "--srcDigest", "x", "--dedupDir", "x", "--root"),
        List("dedup", "--srcDigest", "x", "--dedupDir", "x", "--mvDir"),
        List("dedup", "--srcDigest", "x", "--dedupDir", "x", "--knownExt"),
        List("dedup", "--srcDigest", "x", "--dedupDir", "x", "--unknownExt")
      )
      for (args <- invalid) {
        val t = interceptSkryncGoDocoptEx(args: _*)
        t.docopt shouldBe DeduplicateTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = interceptSkryncGoDocoptEx("dedup", "--garbage")
      t.docopt shouldBe DeduplicateTask.Doc
    }

    describe("without --root") {

      it("throws an exception when the source digest doesn't exist") {
        val tSrc = interceptSkryncGoIAEx("dedup", "--srcDigest", Small.DoesntExist, "--dedupDir", Small.src)
        tSrc.getMessage shouldBe s"Source doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when the source digest is a directory") {
        val tSrc = interceptSkryncGoIAEx("dedup", "--srcDigest", Small.src, "--dedupDir", Small.src)
        tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"
      }

      ignore("throws an exception when the source digest is not a JSON file") {
        // TODO
        val tSrc = interceptSkryncGoIAEx("dedup", "--srcDigest", Small.src / "ids.txt", "--dedupDir", Small.src)
        tSrc.getMessage shouldBe s"Source is not a digest file: ${Small.src / "ids.txt"}"
      }

      it("throws an exception when the dedup directory doesn't exist") {
        val tSrc = interceptSkryncGoIAEx("dedup", "--srcDigest", Small.src / "ids.txt", "--dedupDir", Small.DoesntExist)
        tSrc.getMessage shouldBe s"Deduplication directory doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when the dedup directory is not a directory") {
        val tSrc =
          interceptSkryncGoIAEx("dedup", "--srcDigest", Small.src / "ids.txt", "--dedupDir", Small.src / "ids.txt")
        tSrc.getMessage shouldBe s"Deduplication directory is not a directory: ${Small.src / "ids.txt"}"
      }

      it("throws an exception when the dedup move directory doesn't exist") {
        val tSrc = interceptSkryncGoIAEx(
          "dedup",
          "--srcDigest",
          Small.src / "ids.txt",
          "--dedupDir",
          Small.src,
          "--mvDir",
          Small.DoesntExist
        )
        tSrc.getMessage shouldBe s"Duplicate destination directory doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when the dedup move directory is not a directory") {
        val tSrc = interceptSkryncGoIAEx(
          "dedup",
          "--srcDigest",
          Small.src / "ids.txt",
          "--dedupDir",
          Small.src.toString,
          "--mvDir",
          Small.src / "ids.txt"
        )
        tSrc.getMessage shouldBe s"Duplicate destination directory is not a directory: ${Small.src / "ids.txt"}"
      }
    }

    describe("with --root") {

      /** We'll test that the root is taken into account by using a nonsense command line. We want to provoke a "doesn't
        * exist: FILENAME" CLI error. Outside of the root, all of the file resources exist, so we'll replace them one by
        * one with resources that don't exist, and examine the errors that result.
        */
      val dedupExistingArgs = Seq(
        "dedup",
        "--root",
        Small.DoesntExist,
        "--srcDigest",
        Small.src / "ids.txt",
        "--dedupDir",
        Small.src,
        "--mvDir",
        Small.src
      )

      it("throws an exception when a relative --srcDigest doesn't exist") {
        val tSrc = interceptSkryncGoIAEx(dedupExistingArgs.updated(4, "nox"): _*)
        tSrc.getMessage shouldBe s"Source doesn't exist: ${Small.DoesntExist}/nox"
      }

      it("throws an exception when an absolute --srcDigest doesn't exist") {
        val tSrc = interceptSkryncGoIAEx(dedupExistingArgs.updated(4, Small.DoesntExist): _*)
        tSrc.getMessage shouldBe s"Source doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when a relative --dedupDir doesn't exist") {
        val tSrc = interceptSkryncGoIAEx(dedupExistingArgs.updated(6, "nox"): _*)
        tSrc.getMessage shouldBe s"Deduplication directory doesn't exist: ${Small.DoesntExist}/nox"
      }

      it("throws an exception when an absolute --dedupDir doesn't exist") {
        val tSrc = interceptSkryncGoIAEx(dedupExistingArgs.updated(6, Small.DoesntExist): _*)
        tSrc.getMessage shouldBe s"Deduplication directory doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when a relative --mvDir doesn't exist") {
        val tSrc = interceptSkryncGoIAEx(dedupExistingArgs.updated(8, "nox"): _*)
        tSrc.getMessage shouldBe s"Duplicate destination directory doesn't exist: ${Small.DoesntExist}/nox"
      }

      it("throws an exception when an absolute --mvDir doesn't exist") {
        val tSrc = interceptSkryncGoIAEx(dedupExistingArgs.updated(8, Small.DoesntExist): _*)
        tSrc.getMessage shouldBe s"Duplicate destination directory doesn't exist: ${Small.DoesntExist}"
      }
    }
  }

  /** Extracts paths relative to src/ for testing as strings. */
  def extractNameForTests(src: Path)(f: (Path, SkryncPath)): String =
    src.relativize(f._1).toString()

  describe(s"Using ${DedupPathReport.getClass.getSimpleName}") {

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

  def withDedup1Go(
      src: Directory = Small.srcWithDuplicates,
      srcDigest: File = srcDigest,
      dst: Directory = Small.dst,
      baseArgs: Seq[Any]
  )(testArgs: Seq[Any]): String = {
    val (stdout, stderr) = withSkryncGo(baseArgs ++ testArgs: _*)
    stderr shouldBe empty
    stdout
      .replace(src.toString, "<SRC>")
      .replace(srcDigest.toString, "<SRCDIGEST>")
      .replace(dst.toString, "<DST>")
  }

  def withDedup1DryRunGo(testArgs: Any*): String =
    withDedup1Go(
      baseArgs = Seq("dedup", "--srcDigest", srcDigest, "--dedupDir", Small.srcWithDuplicates / "dup1")
    )(testArgs)

  def withDedup1DryRunVerboseGo(testArgs: Any*): String =
    withDedup1Go(baseArgs =
      Seq("dedup", "--srcDigest", srcDigest, "--dedupDir", Small.srcWithDuplicates / "dup1", "--verbose")
    )(testArgs)

  def withDedup1NewScenarioGo(testArgs: Any*): (ScenarioSmallFiles, String) = {
    val small = new ScenarioSmallFiles(
      Directory.makeTemp(getClass.getSimpleName),
      deleteRootOnCleanup = true
    )
    val (newSrcDigest, _) = withSkryncGoAnalysis(
      small.srcWithDuplicates,
      small.dst
    )
    val stdout =
      withDedup1Go(
        small.srcWithDuplicates,
        newSrcDigest,
        small.dst,
        baseArgs = Seq(
          "dedup",
          "--srcDigest",
          newSrcDigest,
          "--root",
          small.root,
          "--dedupDir",
          small.srcWithDuplicates / "dup1"
        )
      )(testArgs)
    (small, stdout)
  }

  val dedup1Report: String = """DEDUPLICATION REPORT
      |===========
      |from: <SRCDIGEST>
      |src: <SRC>
      |dedup: <SRC>/dup1
      |new files: 1
      |known files: 1""".stripMargin

  describe("Executes dry run actions on known and unknown files in dup1/") {

    it("getting information only (without dry run)") {
      val stdout = withDedup1DryRunGo()
      stdout shouldBe
        s"""$dedup1Report
           |
           |Use --verbose to list the files.
           |""".stripMargin
    }

    it("renaming the extensions of both known and unknown") {
      val stdout = withDedup1DryRunGo("--dryRun", "--knownExt", "known", "--unknownExt", "unknown")
      stdout shouldBe
        s"""$dedup1Report
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<SRC>/dup1/ids.known.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |mv "<SRC>/dup1/ids3.txt" "<SRC>/dup1/ids3.unknown.txt"
           |""".stripMargin
    }

    it("renaming the extensions of only unknown") {
      val stdout = withDedup1DryRunGo("--dryRun", "--unknownExt", "unknown")
      stdout shouldBe
        s"""$dedup1Report
           |dryRun: true (No files will be changed)
           |
           |Unknown files (unique)
           |==================================================
           |
           |mv "<SRC>/dup1/ids3.txt" "<SRC>/dup1/ids3.unknown.txt"
           |""".stripMargin
    }

    it("moving known files to a new directory") {
      val stdout = withDedup1DryRunGo("--dryRun", "--mvDir", Small.dst)
      stdout shouldBe
        s"""$dedup1Report
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<DST>/ids.txt"
           |""".stripMargin
    }

    it("moving known files to a new directory and renaming") {
      val stdout = withDedup1DryRunGo("--dryRun", "--knownExt", "known", "--mvDir", Small.dst)
      stdout shouldBe
        s"""$dedup1Report
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<DST>/ids.known.txt"
           |""".stripMargin
    }

    it("deleting known files") {
      val stdout = withDedup1DryRunGo("--dryRun", "--rmKnown")
      stdout shouldBe
        s"""$dedup1Report
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |rm "<SRC>/dup1/ids.txt"
           |""".stripMargin
    }
  }

  describe("Executes verbose and dry run actions on known and unknown files in dup1/") {

    it("getting information only (without dry run)") {
      val stdout = withDedup1DryRunVerboseGo()
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |
           |Known files (duplicates)
           |==================================================
           |
           |# <SRC>/dup1/ids.txt
           |
           |Unknown files (unique)
           |==================================================
           |
           |# <SRC>/dup1/ids3.txt
           |""".stripMargin
    }

    it("renaming the extensions of both known and unknown") {
      val stdout = withDedup1DryRunVerboseGo("--dryRun", "--knownExt", "known", "--unknownExt", "unknown")
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<SRC>/dup1/ids.known.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |mv "<SRC>/dup1/ids3.txt" "<SRC>/dup1/ids3.unknown.txt"
           |""".stripMargin
    }

    it("renaming the extensions of only unknown") {
      val stdout = withDedup1DryRunVerboseGo("--dryRun", "--unknownExt", "unknown")
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |# <SRC>/dup1/ids.txt
           |
           |Unknown files (unique)
           |==================================================
           |
           |mv "<SRC>/dup1/ids3.txt" "<SRC>/dup1/ids3.unknown.txt"
           |""".stripMargin
    }

    it("moving known files to a new directory") {
      val stdout = withDedup1DryRunVerboseGo("--dryRun", "--mvDir", Small.dst)
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<DST>/ids.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |# <SRC>/dup1/ids3.txt
           |""".stripMargin
    }

    it("moving known files to a new directory and renaming") {
      val stdout = withDedup1DryRunVerboseGo("--dryRun", "--knownExt", "known", "--mvDir", Small.dst)
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<DST>/ids.known.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |# <SRC>/dup1/ids3.txt
           |""".stripMargin
    }

    it("deleting known files") {
      val stdout = withDedup1DryRunVerboseGo("--dryRun", "--rmKnown")
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |dryRun: true (No files will be changed)
           |
           |Known files (duplicates)
           |==================================================
           |
           |rm "<SRC>/dup1/ids.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |# <SRC>/dup1/ids3.txt
           |""".stripMargin
    }
  }

  describe("Executes verbose actions on known and unknown files in dup1/") {

//    it("renaming the extensions of both known and unknown") {
//      val (small, stdout) = withDedup1NewScenarioGo("--verbose", "--knownExt", "known", "--unknownExt", "unknown")
//      stdout shouldBe
//        s"""Verbose is ON
//           |Dry run. No commands will be executed.
//           |
//           |$dedup1Report
//           |Known files (duplicates)
//           |==================================================
//           |
//           |mv "<SRC>/dup1/ids.txt" "<SRC>/dup1/ids.known.txt"
//           |
//           |Unknown files (unique)
//           |==================================================
//           |
//           |mv "<SRC>/dup1/ids3.txt" "<SRC>/dup1/ids3.unknown.txt"
//           |""".stripMargin
//      small.cleanup()
//    }
//
//    it("renaming the extensions of only unknown") {
//      val (small, stdout) = withDedup1NewScenarioGo("--verbose", "--unknownExt", "unknown")
//      stdout shouldBe
//        s"""Verbose is ON
//           |Dry run. No commands will be executed.
//           |
//           |$dedup1Report
//           |Known files (duplicates)
//           |==================================================
//           |
//           |# <SRC>/dup1/ids.txt
//           |
//           |Unknown files (unique)
//           |==================================================
//           |
//           |mv "<SRC>/dup1/ids3.txt" "<SRC>/dup1/ids3.unknown.txt"
//           |""".stripMargin
//      small.cleanup()
//    }

    it("moving known files to a new directory") {
      val (small, stdout) = withDedup1NewScenarioGo("--verbose", "--mvDir", "dst")
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<DST>/ids.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |# <SRC>/dup1/ids3.txt
           |""".stripMargin
      (small.srcWithDuplicates / "dup1" / "ids.txt").exists shouldBe false
      (small.srcWithDuplicates / "dup1" / "ids3.txt").exists shouldBe true
      (small.dst / "ids.txt").toFile.slurp() shouldBe Small.File1Contents
      small.cleanup()
    }

    it("moving known files to a new directory and renaming") {
      val (small, stdout) = withDedup1NewScenarioGo("--verbose", "--knownExt", "known", "--mvDir", "dst")
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |
           |Known files (duplicates)
           |==================================================
           |
           |mv "<SRC>/dup1/ids.txt" "<DST>/ids.known.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |# <SRC>/dup1/ids3.txt
           |""".stripMargin
      (small.srcWithDuplicates / "dup1" / "ids.txt").exists shouldBe false
      (small.srcWithDuplicates / "dup1" / "ids3.txt").exists shouldBe true
      (small.dst / "ids.known.txt").toFile.slurp() shouldBe Small.File1Contents
      small.cleanup()
    }

    it("deleting known files") {
      val (small, stdout) = withDedup1NewScenarioGo("--verbose", "--rmKnown")
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
           |
           |Known files (duplicates)
           |==================================================
           |
           |rm "<SRC>/dup1/ids.txt"
           |
           |Unknown files (unique)
           |==================================================
           |
           |# <SRC>/dup1/ids3.txt
           |""".stripMargin
      (small.srcWithDuplicates / "dup1" / "ids.txt").exists shouldBe false
      (small.srcWithDuplicates / "dup1" / "ids3.txt").exists shouldBe true
      small.cleanup()
    }
  }
}
