package com.skraba.skrync

import com.skraba.skrync.SkryncGo.{InternalDocoptException, go}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.LocalDateTime
import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[DigestTask]]
  */
class DigestTaskSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSingleFile = new ScenarioSingleFile(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  /** A pre-existing file outside the small scenario. */
  val ExistingFile: File = Small.root / File("exists")
  Streamable.closing(ExistingFile.outputStream())(_.write(1))

  override protected def afterAll(): Unit =
    Small.cleanup()

  describe("SkryncGo digest command line") {

    it("throws an exception with --help") {
      val t = intercept[InternalDocoptException] {
        go("digest", "--help")
      }
      t.getMessage shouldBe DigestTask.Doc
      t.docopt shouldBe DigestTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("digest"),
        List("digest", "--srcDir"),
        List("digest", "--srcDir", "x", "--dstDir")
      )
      for (args <- invalid) {
        val t = intercept[InternalDocoptException] {
          go(args: _*)
        }
        t.docopt shouldBe DigestTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = intercept[InternalDocoptException] {
        go("digest", "--srcDigest")
      }
      t.docopt shouldBe DigestTask.Doc
    }

    it("throws an exception when the source doesn't exist") {
      val t = intercept[IllegalArgumentException] {
        go("digest", "--srcDir", "/doesnt-exist")
      }
      t.getMessage shouldBe "Source doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source is a file") {
      val t = intercept[IllegalArgumentException] {
        go("digest", "--srcDir", ExistingFile.toString)
      }
      t.getMessage shouldBe s"Source is not a directory: $ExistingFile"
    }

    it("prints to standard out when no destination") {
      // No exception should occur, and output is dumped to the console.
      val out = new ByteArrayOutputStream()
      Console.withOut(out) {
        go("digest", "--srcDir", Small.src.toString)
      }

      val stdout = new String(out.toByteArray, StandardCharsets.UTF_8)
      stdout should not have size(0)
      stdout should include("ids.txt")
    }

    it("generates a destination file automatically.") {
      // Run the application and check the system streams.
      val dstDir: Directory = Small.root.resolve("autoDst").toDirectory
      dstDir.createDirectory()
      go(
        "digest",
        "--srcDir",
        Small.src.toString,
        "--dstDigest",
        dstDir.toString
      )

      // One file is created.
      val dst: Seq[Path] = dstDir.list.toSeq
      dst should have size 1

      // It should have an autogenerated name.
      val dstDigestFile = dst.headOption.value.toFile
      dstDigestFile.name should include("_small_")
      dstDigestFile.name should endWith regex """_\d{14}"""

      // The contents of the file should be readable.
      val dstRoot = Json.read(dstDigestFile)
      val expected =
        SkryncDir(Small.src).digest(Small.src).copyWithoutTimes()
      dstRoot.info
        .copy(path = dstRoot.info.path.copy(name = "small"))
        .copyWithoutTimes() should equal(expected)
    }

    it("creates the file when a destination is explicitly specified.") {
      // Run the application and check the system streams.
      val dstDir: Directory = Small.root.resolve("dst").toDirectory
      dstDir.createDirectory()
      go(
        "digest",
        "--srcDir",
        Small.src.toString,
        "--dstDigest",
        (dstDir / File("output.gz")).toString
      )

      // One file is created.
      val dst: Seq[Path] = dstDir.list.toSeq
      dst should have size 1

      // It should have the specified name.
      val dstDigestFile = dst.headOption.value.toFile
      dstDigestFile.name should equal("output.gz")

      // The contents of the file should be readable.
      val dstRoot = Json.read(dstDigestFile)
      val expected =
        SkryncDir(Small.src).digest(Small.src).copyWithoutTimes()
      dstRoot.info.copy(path =
        dstRoot.info.path.copy(name = "small")
      ) should equal(expected)
    }
  }

  describe("DigestTask") {
    it("creates a default filename") {
      val defaultName = DigestTask.getDefaultDigestName(
        "/tmp/root\\dir",
        LocalDateTime.of(1980, 2, 14, 12, 34, 56, 0)
      )
      defaultName shouldBe "tmp_root_dir_19800214123456"
    }
  }
}
