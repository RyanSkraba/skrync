package com.skraba.skrync

import com.skraba.skrync.SkryncGoSpec.{
  interceptSkryncGoDocoptEx,
  interceptSkryncGoDocoptExitEx,
  interceptSkryncGoIAEx,
  withSkryncGo
}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[DigestTask]] */
class DigestTaskSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  describe("SkryncGo digest command line") {

    it("throws an exception with --help") {
      val t = interceptSkryncGoDocoptExitEx("digest", "--help")
      t.getMessage shouldBe DigestTask.Doc
      // t.docopt shouldBe DigestTask.Doc
    }

    describe("when missing information") {
      for (args <- List(List("digest"))) {
        it("throws an exception on missing options: " + args.mkString(" ")) {
          val t = interceptSkryncGoDocoptEx(args: _*)
          t.docopt shouldBe DigestTask.Doc
        }
      }

      for (
        (opt, args) <- List(
          "--srcDir" -> List("digest", "--srcDir"),
          "--root" -> List("digest", "--srcDir", "x", "--root"),
          "--dstDigest" -> List("digest", "--srcDir", "x", "--dstDigest")
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
      val t = interceptSkryncGoDocoptEx("digest", "--garbage")
      t.docopt shouldBe DigestTask.Doc
    }

    describe("without --root") {

      it("throws an exception when the source doesn't exist") {
        val t = interceptSkryncGoIAEx("digest", "--srcDir", Small.DoesntExist)
        t.getMessage shouldBe s"Source doesn't exist: ${Small.DoesntExist}"
      }

      it("throws an exception when the source is a file") {
        val t = interceptSkryncGoIAEx("digest", "--srcDir", Small.ExistingFile.toString)
        t.getMessage shouldBe s"Source is not a directory: ${Small.ExistingFile}"
      }

      it("throws an exception when the dst exists") {
        val t =
          interceptSkryncGoIAEx("digest", "--srcDir", Small.src.toString, "--dstDigest", Small.ExistingFile.toString())
        t.getMessage shouldBe s"Destination digest already exists: ${Small.ExistingFile}"
      }

      it("throws an exception when the dstDigest path is inside a file") {
        val fileExists = (Small.ExistingFile / "impossible").toString()
        val t = interceptSkryncGoIAEx("digest", "--srcDir", Small.src.toString, "--dstDigest", fileExists)
        t.getMessage shouldBe s"Destination digest directory is not a directory: ${Small.ExistingFile}"
      }

      it("throws an exception when the dst directory folder structure doesn't exist") {
        val fileExists = (Small.root / "does" / "not" / "exist").toString()
        val t = interceptSkryncGoIAEx("digest", "--srcDir", Small.src.toString, "--dstDigest", fileExists)
        t.getMessage shouldBe s"Destination digest directory doesn't exist: ${Small.root / "does" / "not"}"
      }
    }

    describe("with --root") {

      it("throws an exception when the source doesn't exist (absolute + absolute)") {
        val t = interceptSkryncGoIAEx("digest", "--root", "/ignored", "--srcDir", "/doesnt-exist")
        t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (relative + absolute)") {
        val t = interceptSkryncGoIAEx("digest", "--root", "ignored", "--srcDir", "/doesnt-exist")
        t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (absolute + relative)") {
        val t = interceptSkryncGoIAEx("digest", "--root", "/tmp", "--srcDir", "doesnt-exist")
        t.getMessage shouldBe "Source doesn't exist: /tmp/doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (relative + relative)") {
        val t = interceptSkryncGoIAEx("digest", "--root", "tmp", "--srcDir", "doesnt-exist")
        // The entire message contains the current user dir.
        t.getMessage should startWith("Source doesn't exist: ")
        t.getMessage should endWith("/tmp/doesnt-exist")
      }

      it("throws an exception when the source is a file") {
        val t =
          interceptSkryncGoIAEx("digest", "--root", Small.ExistingFile.parent.path, "--srcDir", Small.ExistingFile.name)
        t.getMessage shouldBe s"Source is not a directory: ${Small.ExistingFile}"
      }

      it("throws an exception when the dst exists") {
        val dstDigest = (Small.src / "ids.txt").toString()
        val t = interceptSkryncGoIAEx(
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
        val t = interceptSkryncGoIAEx(
          "digest",
          "--root",
          Small.ExistingFile.parent.path,
          "--srcDir",
          Small.src.path,
          "--dstDigest",
          "exists/impossible"
        )
        t.getMessage shouldBe s"Destination digest directory is not a directory: ${Small.ExistingFile}"
      }

      it("throws an exception when the dst directory folder structure doesn't exist") {
        val t = interceptSkryncGoIAEx(
          "digest",
          "--root",
          Small.root.path,
          "--srcDir",
          Small.src.path,
          "--dstDigest",
          "does/not/exist"
        )
        t.getMessage shouldBe s"Destination digest directory doesn't exist: ${Small.root / "does" / "not"}"
      }

    }

    it("prints to standard out when no destination") {
      // No exception should occur, and output is dumped to the console.
      val (stdout, stderr) = withSkryncGo("digest", "--srcDir", Small.src.toString)
      stdout should not have size(0)
      stdout should include("ids.txt")
      stderr shouldBe ""
    }

    it("generates a destination file automatically.") {
      // Run the application and check the system streams.
      val dstDir: Directory = Small.root.resolve("autoDst").toDirectory
      dstDir.createDirectory()
      val (stdout, stderr) = withSkryncGo("digest", "--srcDir", Small.src.toString, "--dstDigest", dstDir.toString)
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
        withSkryncGo("digest", "--srcDir", Small.src.toString, "--dstDigest", (dstDir / File("output.gz")).toString)
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
      val (stdout, stderr) = withSkryncGo(
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
      val expected =
        SkryncDir.scan(Small.src, digest = false).copyWithoutTimes()
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
