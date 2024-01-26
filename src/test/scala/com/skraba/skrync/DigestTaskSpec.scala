package com.skraba.skrync

import com.skraba.skrync.SkryncGo.InternalDocoptException
import com.skraba.skrync.SkryncGoSpec.withSkryncGo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[DigestTask]] */
class DigestTaskSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  /** A pre-existing file outside the small scenario. */
  val ExistingFile: File = Small.root / File("exists")
  Streamable.closing(ExistingFile.outputStream())(_.write(1))

  describe("SkryncGo digest command line") {

    it("throws an exception with --help") {
      val t = intercept[InternalDocoptException] { withSkryncGo("digest", "--help") }
      t.getMessage shouldBe DigestTask.Doc
      t.docopt shouldBe DigestTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("digest"),
        List("digest", "--srcDir"),
        List("digest", "--srcDir", "x", "--srcRoot"),
        List("digest", "--srcDir", "x", "--dstDir")
      )
      for (args <- invalid) {
        val t = intercept[InternalDocoptException] { withSkryncGo(args: _*) }
        t.docopt shouldBe DigestTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = intercept[InternalDocoptException] { withSkryncGo("digest", "--srcDigest") }
      t.docopt shouldBe DigestTask.Doc
    }

    describe("without --srcRoot") {

      it("throws an exception when the source doesn't exist") {
        val t = intercept[IllegalArgumentException] { withSkryncGo("digest", "--srcDir", "/doesnt-exist") }
        t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
      }

      it("throws an exception when the source is a file") {
        val t = intercept[IllegalArgumentException] { withSkryncGo("digest", "--srcDir", ExistingFile.toString) }
        t.getMessage shouldBe s"Source is not a directory: $ExistingFile"
      }

      it("throws an exception when the dst exists") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcDir", Small.src.toString, "--dstDigest", ExistingFile.toString())
        }
        t.getMessage shouldBe s"Destination digest already exists: $ExistingFile"
      }

      it("throws an exception when the dstDigest path is inside a file") {
        val fileExists = (ExistingFile / "impossible").toString()
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcDir", Small.src.toString, "--dstDigest", fileExists)
        }
        t.getMessage shouldBe s"Destination digest directory is not a directory: $ExistingFile"
      }

      it("throws an exception when the dst directory folder structure doesn't exist") {
        val fileExists = (Small.root / "does" / "not" / "exist").toString()
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcDir", Small.src.toString, "--dstDigest", fileExists)
        }
        t.getMessage shouldBe s"Destination digest directory doesn't exist: ${Small.root / "does" / "not"}"
      }
    }

    describe("with --srcRoot") {

      it("throws an exception when the source doesn't exist (absolute + absolute)") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcRoot", "/ignored", "--srcDir", "/doesnt-exist")
        }
        t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (relative + absolute)") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcRoot", "ignored", "--srcDir", "/doesnt-exist")
        }
        t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (absolute + relative)") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcRoot", "/tmp", "--srcDir", "doesnt-exist")
        }
        t.getMessage shouldBe "Source doesn't exist: /tmp/doesnt-exist"
      }

      it("throws an exception when the source doesn't exist (relative + relative)") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcRoot", "tmp", "--srcDir", "doesnt-exist")
        }
        // The entire message contains the current user dir.
        t.getMessage should startWith("Source doesn't exist: ")
        t.getMessage should endWith("/tmp/doesnt-exist")
      }

      it("throws an exception when the source is a file") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo("digest", "--srcRoot", ExistingFile.parent.path, "--srcDir", ExistingFile.name)
        }
        t.getMessage shouldBe s"Source is not a directory: $ExistingFile"
      }

      it("throws an exception when the dst exists") {
        val dstDigest = (Small.src / "ids.txt").toString()
        val t = intercept[IllegalArgumentException] {
          withSkryncGo(
            "digest",
            "--srcRoot",
            Small.src.toString,
            "--srcDir",
            Small.src.toString,
            "--dstDigest",
            "ids.txt"
          )
        }
        t.getMessage shouldBe s"Destination digest already exists: $dstDigest"
      }

      it("throws an exception when the dstDigest path is inside a file") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo(
            "digest",
            "--srcRoot",
            ExistingFile.parent.path,
            "--srcDir",
            Small.src.path,
            "--dstDigest",
            "exists/impossible"
          )
        }
        t.getMessage shouldBe s"Destination digest directory is not a directory: $ExistingFile"
      }

      it("throws an exception when the dst directory folder structure doesn't exist") {
        val t = intercept[IllegalArgumentException] {
          withSkryncGo(
            "digest",
            "--srcRoot",
            Small.root.path,
            "--srcDir",
            Small.src.path,
            "--dstDigest",
            "does/not/exist"
          )
        }
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
