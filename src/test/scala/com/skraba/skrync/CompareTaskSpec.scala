package com.skraba.skrync

import com.skraba.skrync.SkryncGo.InternalDocoptException
import com.skraba.skrync.SkryncGoSpec.withSkryncGo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Unit tests for [[CompareTask]]
  */
class CompareTaskSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  /** A pre-existing file outside the small scenario. */
  val ExistingFile: File = Small.root / File("exists")
  Streamable.closing(ExistingFile.outputStream())(_.write(1))

  describe("SkryncGo compare command line") {
    it("throws an exception with --help") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("compare", "--help")
      }
      t.getMessage shouldBe CompareTask.Doc
      t.docopt shouldBe CompareTask.Doc
    }

    it("throws an exception when arguments are missing") {
      val invalid = List(
        List("compare"),
        List("compare", "--dstDigest"),
        List("compare", "--dstDigest", "x"),
        List("compare", "--dstDigest", "x", "--srcDigest"),
        List("compare", "--srcDigest"),
        List("compare", "--srcDigest", "x", "--dstDigest")
      )
      for (args <- invalid) {
        val t = intercept[InternalDocoptException] {
          withSkryncGo(args: _*)
        }
        t.docopt shouldBe CompareTask.Doc
      }
    }

    it("throws an exception with unknown option") {
      val t = intercept[InternalDocoptException] {
        withSkryncGo("compare", "--garbage")
      }
      t.docopt shouldBe CompareTask.Doc
    }

    it("throws an exception when the source or destination doesn't exist") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "compare",
          "--srcDigest",
          "/doesnt-exist",
          "--dstDigest",
          ExistingFile.toString
        )
      }
      tSrc.getMessage shouldBe "Source doesn't exist: /doesnt-exist"

      val tDst = intercept[IllegalArgumentException] {
        withSkryncGo(
          "compare",
          "--srcDigest",
          ExistingFile.toString,
          "--dstDigest",
          "/doesnt-exist"
        )
      }
      tDst.getMessage shouldBe "Destination doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source or destination is a directory") {
      val tSrc = intercept[IllegalArgumentException] {
        withSkryncGo(
          "compare",
          "--srcDigest",
          Small.src.toString(),
          "--dstDigest",
          ExistingFile.toString
        )
      }
      tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"

      val tDst = intercept[IllegalArgumentException] {
        withSkryncGo(
          "compare",
          "--srcDigest",
          ExistingFile.toString,
          "--dstDigest",
          Small.src.toString()
        )
      }
      tDst.getMessage shouldBe s"Destination is not a file: ${Small.src}"
    }
  }

  describe("SkryncGo compare two identical folders") {

    // Create two analysis files from the same directory.  This is like comparing two identical
    // folders, or the same unchanged folder at two different times.
    val dstDir: Directory = Small.root.resolve("dst").toDirectory
    dstDir.createDirectory()
    withSkryncGo(
      "digest",
      "--srcDir",
      Small.src.toString,
      "--dstDigest",
      (dstDir / File("compare.gz")).toString
    )
    withSkryncGo(
      "digest",
      "--srcDir",
      Small.src.toString,
      "--dstDigest",
      (dstDir / File("compare2.gz")).toString
    )

    it("on the filesystem including digests") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.src, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.modified shouldBe Set()
    }

    it("on the filesystem without digests") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = false),
        SkryncDir.scan(Small.src, digest = false)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.modified shouldBe Set()
    }

    it("on the filesystem with digest mismatches") {
      // This is actually an error condition, when one directory instance contains a digest
      // and another doesn't, they're considered mismatches.
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.src, digest = false)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.modified shouldBe Set(
        Path("small/ids.txt"),
        Path("small/sub/ids2.txt")
      )
    }

    it("via the CLI") {
      // No exception should occur, and output is dumped to the console.
      val (stdout, stderr) = withSkryncGo(
        "compare",
        "--srcDigest",
        (dstDir / File("compare.gz")).toString(),
        "--dstDigest",
        (dstDir / File("compare2.gz")).toString()
      )

      stdout should not have size(0)
      stdout should include("identical: true")
      stderr shouldBe ""
    }
  }

  describe("SkryncGo compare different folders") {

    it("when a file is deleted") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcDeletedFile, digest = true)
      )
      cmp.srcOnly shouldBe Set(Path("small/sub/ids2.txt"))
      cmp.dstOnly shouldBe Set()
      cmp.modified shouldBe Set()
    }

    it("when a file is added") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.srcDeletedFile, digest = true),
        SkryncDir.scan(Small.src, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set(Path("small/sub/ids2.txt"))
      cmp.modified shouldBe Set()
    }

    it("when a file is modified") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcModifiedFile, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.modified shouldBe Set(Path("small/sub/ids2.txt"))
    }

    it("when a file is renamed") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcRenamedFile, digest = true)
      )
      // TODO: This is currently treated as an addition and deletion
      cmp.srcOnly shouldBe Set(Path("small/sub/ids2.txt"))
      cmp.dstOnly shouldBe Set(Path("small/sub/ids2a.txt"))
      cmp.modified shouldBe Set()
    }
  }
}
