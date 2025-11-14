package com.skraba.skrync

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import org.scalatest.OptionValues._

import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[DigestTask]] */
class DigestTaskSpec extends MultiTaskMainSpec(SkryncGo, Some(DigestTask)) with FileValidator {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(Tmp)

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs()
    itShouldThrowOnMissingOptValue("--srcDir")
    itShouldThrowOnMissingOptValue("--srcDir", "x", "--root")
    itShouldThrowOnMissingOptValue("--srcDir", "x", "--dstDigest")

    describe("without --root") {
      itShouldBeAnExistingPath.args(tag = "Source")("--srcDir", "<>")

      // TODO: itShouldBeACreatableDir
      it("throws an exception when the dst exists") {
        val t = interceptGoDocoptEx("digest", "--srcDir", Small.src.toString, "--dstDigest", ExistingFile)
        t.getMessage shouldBe s"Destination digest already exists: $ExistingFile"
      }

      it("throws an exception when the dstDigest path is inside a file") {
        val fileExists = ExistingFile / "impossible"
        val t = interceptGoDocoptEx("digest", "--srcDir", Small.src.toString, "--dstDigest", fileExists)
        t.getMessage shouldBe s"Destination digest is uncreatable, $ExistingFile exists: $fileExists"
      }

      it("throws an exception when the dst directory folder structure doesn't exist") {
        val fileStructure = Small.root / "does" / "not" / "exist"
        val t = interceptGoDocoptEx("digest", "--srcDir", Small.src.toString, "--dstDigest", fileStructure)
        t.getMessage shouldBe s"Destination digest parent directory doesn't exist: $fileStructure"
      }
    }

    describe("with --root") {

      it("throws an exception when the source doesn't exist (absolute + absolute)") {
        val t = interceptGoDocoptEx("digest", "--root", "/ignored", "--srcDir", "/doesnt-exist")
        t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (relative + absolute)") {
        val t = interceptGoDocoptEx("digest", "--root", "ignored", "--srcDir", "/doesnt-exist")
        t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (absolute + relative)") {
        val t = interceptGoDocoptEx("digest", "--root", "/tmp", "--srcDir", "doesnt-exist")
        t.getMessage shouldBe "Source doesn't exist: /tmp/doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (relative + relative)") {
        val t = interceptGoDocoptEx("digest", "--root", "tmp", "--srcDir", "doesnt-exist")
        // The entire message contains the current user dir.
        t.getMessage should startWith("Source doesn't exist: ")
        t.getMessage should endWith("/tmp/doesnt-exist")
      }

      it("throws an exception when the source is a file") {
        val t = interceptGoDocoptEx("digest", "--root", ExistingFile.parent.path, "--srcDir", ExistingFile.name)
        t.getMessage shouldBe s"Source expected a directory, found file: $ExistingFile"
      }

      it("throws an exception when the dst exists") {
        val dstDigest = (Small.src / "ids.txt").toString()
        val t = interceptGoDocoptEx(
          "digest",
          "--root",
          Small.src.toString,
          "--srcDir",
          Small.src.toString,
          "--dstDigest",
          "ids.txt"
        )
        t.getMessage shouldBe s"Destination digest already exists: $dstDigest"
      }

      it("throws an exception when the dstDigest path is inside a file") {
        val t = interceptGoDocoptEx(
          "digest",
          "--root",
          ExistingFile.parent.path,
          "--srcDir",
          Small.src.path,
          "--dstDigest",
          ExistingFile.name + "/nox"
        )
        t.getMessage shouldBe s"Destination digest is uncreatable, $ExistingFile exists: $ExistingFile/nox"
      }

      it("throws an exception when the dst directory folder structure doesn't exist") {
        val t = interceptGoDocoptEx(
          "digest",
          "--root",
          Small.root.path,
          "--srcDir",
          Small.src.path,
          "--dstDigest",
          "does/not/exist"
        )
        t.getMessage shouldBe s"Destination digest parent directory doesn't exist: ${Small.root / "does" / "not" / "exist"}"
      }

    }

    it("prints to standard out when no destination") {
      // No exception should occur, and output is dumped to the console.
      val (stdout, stderr) = withGo("digest", "--srcDir", Small.src.toString)
      stdout should not have size(0)
      stdout should include("ids.txt")
      stderr shouldBe ""
    }

    it("generates a destination file automatically.") {
      // Run the application and check the system streams.
      val dstDir: Directory = Small.root.resolve("autoDst").toDirectory
      dstDir.createDirectory()
      val (stdout, stderr) = withGo("digest", "--srcDir", Small.src.toString, "--dstDigest", dstDir.toString)
      stdout shouldBe "[![!]]{<.>{<.>}}"
      stderr shouldBe ""

      // One file is created.
      val dst: Seq[Path] = dstDir.list.toSeq
      dst should have size 1

      // It should have an autogenerated name.
      val dstDigestFile = dst.headOption.value.toFile
      dstDigestFile.name should include("_small__")
      dstDigestFile.name should endWith regex """__\d{14}"""

      // This is the actual time taken from the file name in YYYYMMDDHHMMSS format.
      val dstDigestFileTime = """\d{14}$""".r.findFirstIn(dstDigestFile.name)

      // The contents of the file should be readable and match the contents of the directory.
      val analysis = Json.read(dstDigestFile)
      val expected = SkryncDir.scan(Small.src, digest = true).copyWithoutTimes()

      analysis.src shouldBe Small.src
      analysis.info
        .copy(path = analysis.info.path.copy(name = "small"))
        .copyWithoutTimes() shouldBe expected

      // The internal file time should match the filename.
      LocalDateTime
        .ofInstant(Instant.ofEpochMilli(analysis.created), ZoneOffset.UTC)
        .format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss")) shouldBe dstDigestFileTime.value
    }

    it("creates the file when a destination is explicitly specified.") {
      // Run the application and check the system streams.
      val dstDir: Directory = Small.root.resolve("dst").toDirectory
      dstDir.createDirectory()
      val (stdout, stderr) =
        withGo("digest", "--srcDir", Small.src.toString, "--dstDigest", (dstDir / File("output.gz")).toString)
      stdout shouldBe "[![!]]{<.>{<.>}}"
      stderr shouldBe ""

      // One file is created.
      val dst: Seq[Path] = dstDir.list.toSeq
      dst should have size 1

      // It should have the specified name.
      val dstDigestFile = dst.headOption.value.toFile
      dstDigestFile.name shouldBe "output.gz"

      // The contents of the file should be readable.
      val dstRoot = Json.read(dstDigestFile)
      val expected = SkryncDir.scan(Small.src, digest = true).copyWithoutTimes()
      dstRoot.info.copy(path = dstRoot.info.path.copy(name = "small")) shouldBe expected
    }

    it("creates the file without digest information.") {
      // Run the application and check the system streams.
      val dstDir: Directory = Small.root.resolve("dstNoDigest").toDirectory
      dstDir.createDirectory()
      val (stdout, stderr) = withGo(
        "digest",
        "--no-digest",
        "--srcDir",
        Small.src.toString,
        "--dstDigest",
        (dstDir / File("output.gz")).toString
      )
      stdout shouldBe "[![!]]"
      stderr shouldBe ""

      // One file is created.
      val dst: Seq[Path] = dstDir.list.toSeq
      dst should have size 1

      // It should have the specified name.
      val dstDigestFile = dst.headOption.value.toFile
      dstDigestFile.name shouldBe "output.gz"

      // The contents of the file should be readable.
      val dstRoot = Json.read(dstDigestFile)
      val expected = SkryncDir.scan(Small.src, digest = false).copyWithoutTimes()
      dstRoot.info.copy(path = dstRoot.info.path.copy(name = "small")) shouldBe expected
    }
  }

  describe("DigestTask") {
    it("creates a default filename") {
      val defaultName = DigestTask.getDefaultDigestName("/tmp/root\\dir", LocalDateTime.of(1980, 2, 14, 12, 34, 56, 0))
      defaultName shouldBe "tmp_root_dir__19800214123456"
    }
  }
}
