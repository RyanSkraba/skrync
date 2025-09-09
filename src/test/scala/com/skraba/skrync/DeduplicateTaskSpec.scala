package com.skraba.skrync

import com.skraba.skrync.DeduplicateTask.DedupPathReport
import com.skraba.skrync.SkryncGoSpec._
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[DeduplicateTask]] */
class DeduplicateTaskSpec extends MultiTaskMainSpec(SkryncGo, Some(DeduplicateTask)) with TmpDir {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(Tmp)

  // Generate an analysis of the scenario6 directory
  val (srcDigest, analysis) = withSkryncGoAnalysis(
    Small.srcWithDuplicates,
    Small.dst
  )

  describe(s"${Main.Name} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq.empty)
    itShouldThrowOnIncompleteArgs(Seq("--srcDigest", "x"))
    itShouldThrowOnIncompleteArgs(Seq("--dedupDir", "x"))

    itShouldThrowOnMissingFlagValue(Seq("--srcDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dedupDir"))
    itShouldThrowOnMissingFlagValue(Seq("--dedupDir"))
    itShouldThrowOnMissingFlagValue(Seq("--dedupDir", "x", "--srcDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dedupDir", "x", "--root"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dedupDir", "x", "--mvDir"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dedupDir", "x", "--knownExt"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dedupDir", "x", "--unknownExt"))

    describe("without --root") {

      it("throws an exception when the source digest doesn't exist") {
        val tSrc =
          interceptGo[IllegalArgumentException]("dedup", "--srcDigest", Small.DoesntExist, "--dedupDir", Small.src)
        tSrc.getMessage shouldBe s"Source doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when the source digest is a directory") {
        val tSrc = interceptGo[IllegalArgumentException]("dedup", "--srcDigest", Small.src, "--dedupDir", Small.src)
        tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"
      }

      ignore("throws an exception when the source digest is not a JSON file") {
        // TODO
        val tSrc =
          interceptGo[IllegalArgumentException]("dedup", "--srcDigest", Small.src / "ids.txt", "--dedupDir", Small.src)
        tSrc.getMessage shouldBe s"Source is not a digest file: ${Small.src / "ids.txt"}"
      }

      it("throws an exception when the dedup directory doesn't exist") {
        val tSrc = interceptGo[IllegalArgumentException](
          "dedup",
          "--srcDigest",
          Small.src / "ids.txt",
          "--dedupDir",
          Small.DoesntExist
        )
        tSrc.getMessage shouldBe s"Deduplication directory doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when the dedup directory is not a directory") {
        val tSrc =
          interceptGo[IllegalArgumentException](
            "dedup",
            "--srcDigest",
            Small.src / "ids.txt",
            "--dedupDir",
            Small.src / "ids.txt"
          )
        tSrc.getMessage shouldBe s"Deduplication directory is not a directory: ${Small.src / "ids.txt"}"
      }

      it("throws an exception when the dedup move directory doesn't exist") {
        val tSrc = interceptGo[IllegalArgumentException](
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
        val tSrc = interceptGo[IllegalArgumentException](
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
        * exist: FILENAME" CLI error. Outside of the root, all the file resources exist, so we'll replace them one by
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
        val tSrc = interceptGo[IllegalArgumentException](dedupExistingArgs.updated(4, "nox"): _*)
        tSrc.getMessage shouldBe s"Source doesn't exist: ${Small.DoesntExist}/nox"
      }

      it("throws an exception when an absolute --srcDigest doesn't exist") {
        val tSrc = interceptGo[IllegalArgumentException](dedupExistingArgs.updated(4, Small.DoesntExist): _*)
        tSrc.getMessage shouldBe s"Source doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when a relative --dedupDir doesn't exist") {
        val tSrc = interceptGo[IllegalArgumentException](dedupExistingArgs.updated(6, "nox"): _*)
        tSrc.getMessage shouldBe s"Deduplication directory doesn't exist: ${Small.DoesntExist}/nox"
      }

      it("throws an exception when an absolute --dedupDir doesn't exist") {
        val tSrc = interceptGo[IllegalArgumentException](dedupExistingArgs.updated(6, Small.DoesntExist): _*)
        tSrc.getMessage shouldBe s"Deduplication directory doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when a relative --mvDir doesn't exist") {
        val tSrc = interceptGo[IllegalArgumentException](dedupExistingArgs.updated(8, "nox"): _*)
        tSrc.getMessage shouldBe s"Duplicate destination directory doesn't exist: ${Small.DoesntExist}/nox"
      }

      it("throws an exception when an absolute --mvDir doesn't exist") {
        val tSrc = interceptGo[IllegalArgumentException](dedupExistingArgs.updated(8, Small.DoesntExist): _*)
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

  /** Helper utility to run the dedup command on the small scenario with the given digest. By default, it uses a single
    * Small scenario and should leave the contents unmodified.
    *
    * @param src
    *   The directory that the analysis was created on
    * @param srcDigest
    *   The digest file containing the analysis
    * @param dst
    *   A destination directory
    * @param baseArgs
    *   A list of base arguments that can be reused
    * @param testArgs
    *   The test arguments to add
    * @return
    *   The standard output from running the command
    */
  def withDedup1Go(
      src: Directory = Small.srcWithDuplicates,
      srcDigest: File = srcDigest,
      dst: Directory = Small.dst,
      baseArgs: Seq[Any]
  )(testArgs: Seq[Any]): String = {
    val (stdout, stderr) = withGo(baseArgs ++ testArgs: _*)
    stderr shouldBe empty
    stdout
      .replace(src.toString, "<SRC>")
      .replace(srcDigest.toString, "<SRCDIGEST>")
      .replace(dst.toString, "<DST>")
      .replaceFirst(raw"(read analysis: )\d+(ms\ndedup analysis: )\d+(ms\n)", "$1<###>$2<###>$3")
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
    val small = new ScenarioSmallFiles(nonExisting(Tmp).createDirectory(failIfExists = true))
    val (newSrcDigest, _) = withSkryncGoAnalysis(small.srcWithDuplicates, small.dst)
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
      |==================================================
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

    it("getting information only (with timing)") {
      val stdout = withDedup1DryRunGo("--timing")
      stdout shouldBe
        s"""$dedup1Report
           |read analysis: <###>ms
           |dedup analysis: <###>ms
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

    it("getting information only (with timing)") {
      val stdout = withDedup1DryRunVerboseGo("--timing")
      stdout shouldBe
        s"""$dedup1Report
           |read analysis: <###>ms
           |dedup analysis: <###>ms
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

    it("renaming the extensions of both known and unknown") {
      val (small, stdout) = withDedup1NewScenarioGo("--verbose", "--knownExt", "known", "--unknownExt", "unknown")
      stdout shouldBe
        s"""$dedup1Report
           |verbose: true
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
      (small.srcWithDuplicates / "dup1").toDirectory.files should have size 2
      (small.srcWithDuplicates / "dup1" / "ids.txt").exists shouldBe false
      (small.srcWithDuplicates / "dup1" / "ids.known.txt").toFile.slurp() shouldBe Small.File1Contents
      (small.srcWithDuplicates / "dup1" / "ids3.txt").exists shouldBe false
      (small.srcWithDuplicates / "dup1" / "ids3.unknown.txt").toFile.slurp() shouldBe Small.File3Contents
      small.dst.toDirectory.files should have size 1
    }

    it("renaming the extensions of only unknown") {
      val (small, stdout) = withDedup1NewScenarioGo("--verbose", "--unknownExt", "unknown")
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
           |mv "<SRC>/dup1/ids3.txt" "<SRC>/dup1/ids3.unknown.txt"
           |""".stripMargin
      (small.srcWithDuplicates / "dup1").toDirectory.files should have size 2
      (small.srcWithDuplicates / "dup1" / "ids.txt").toFile.slurp() shouldBe Small.File1Contents
      (small.srcWithDuplicates / "dup1" / "ids3.unknown.txt").toFile.slurp() shouldBe Small.File3Contents
      small.dst.toDirectory.files should have size 1
    }

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
      (small.srcWithDuplicates / "dup1").toDirectory.files should have size 1
      (small.srcWithDuplicates / "dup1" / "ids3.txt").toFile.slurp() shouldBe Small.File3Contents
      small.dst.toDirectory.files should have size 2
      (small.dst / "ids.txt").toFile.slurp() shouldBe Small.File1Contents
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
      (small.srcWithDuplicates / "dup1").toDirectory.files should have size 1
      small.dst.toDirectory.files should have size 2
      (small.dst / "ids.known.txt").toFile.slurp() shouldBe Small.File1Contents
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
      (small.srcWithDuplicates / "dup1").toDirectory.files should have size 1
      (small.srcWithDuplicates / "dup1" / "ids3.txt").toFile.slurp() shouldBe Small.File3Contents
      small.dst.toDirectory.files should have size 1
    }
  }
}
