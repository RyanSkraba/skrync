package com.skraba.skrync

import com.skraba.skrync.CompareTask.DupFiles
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[CompareTask]] */
class CompareTaskSpec extends MultiTaskMainSpec(SkryncGo, Some(CompareTask)) with TmpDir {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(Tmp)

  describe(s"${Main.Name} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq.empty)
    itShouldThrowOnIncompleteArgs(Seq("--dstDigest", "x"))
    itShouldThrowOnIncompleteArgs(Seq("--srcDigest", "x"))

    itShouldThrowOnMissingFlagValue(Seq("--srcDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--dstDigest", "x", "--srcDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--srcDigest", "x", "--dstDigest"))
    itShouldThrowOnMissingFlagValue(Seq("--dstDigest"))

    it("throws an exception when the source or destination doesn't exist") {
      val tSrc = interceptGo[IllegalArgumentException](
        "compare",
        "--srcDigest",
        "/doesnt-exist",
        "--dstDigest",
        Small.ExistingFile
      )
      tSrc.getMessage shouldBe "Source doesn't exist: /doesnt-exist"

      val tDst = interceptGo[IllegalArgumentException](
        "compare",
        "--srcDigest",
        Small.ExistingFile,
        "--dstDigest",
        "/doesnt-exist"
      )
      tDst.getMessage shouldBe "Destination doesn't exist: /doesnt-exist"
    }

    it("throws an exception when the source or destination is a directory") {
      val tSrc =
        interceptGo[IllegalArgumentException]("compare", "--srcDigest", Small.src, "--dstDigest", Small.ExistingFile)
      tSrc.getMessage shouldBe s"Source is not a file: ${Small.src}"

      val tDst =
        interceptGo[IllegalArgumentException]("compare", "--srcDigest", Small.ExistingFile, "--dstDigest", Small.src)
      tDst.getMessage shouldBe s"Destination is not a file: ${Small.src}"
    }
  }

  describe("SkryncGo compare two identical folders") {

    // Create two analysis files from the same directory.  This is like comparing two identical
    // folders, or the same unchanged folder at two different times.
    val dstDir: Directory = Small.root.resolve("dst").toDirectory
    dstDir.createDirectory()
    withGo("digest", "--srcDir", Small.src, "--dstDigest", dstDir / File("compare.gz"))
    withGo("digest", "--srcDir", Small.src, "--dstDigest", dstDir / File("compare2.gz"))

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
      cmp.modified shouldBe Set(Path("small/ids.txt"), Path("small/sub/ids2.txt"))
    }

    it("via the CLI") {
      // No exception should occur, and output is dumped to the console.
      val (stdout, stderr) =
        withGo("compare", "--srcDigest", dstDir / File("compare.gz"), "--dstDigest", dstDir / File("compare2.gz"))

      stdout should not have size(0)
      stdout should include("\nNo differences have been detected.\n")
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
      cmp.moved shouldBe Set()
      cmp.modified shouldBe Set()
    }

    it("when a file is added") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.srcDeletedFile, digest = true),
        SkryncDir.scan(Small.src, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set(Path("small/sub/ids2.txt"))
      cmp.moved shouldBe Set()
      cmp.modified shouldBe Set()
    }

    it("when a file is modified") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcModifiedFile, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.moved shouldBe Set()
      cmp.modified shouldBe Set(Path("small/sub/ids2.txt"))
    }

    it("when a file is renamed") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcRenamedFile, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.moved shouldBe Set(DupFiles(Set(Path("small/sub/ids2.txt")), Set(Path("small/sub/ids2a.txt"))))
      cmp.modified shouldBe Set()
    }

    it("when a file is moved") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcMovedFile, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.moved shouldBe Set(DupFiles(Set(Path("small/sub/ids2.txt")), Set(Path("small/sub2/ids2.txt"))))
      cmp.modified shouldBe Set()
    }

    it("when one file is duplicated") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcWithDuplicateFile, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.moved shouldBe Set(
        DupFiles(Set(Path("small/ids.txt")), Set(Path("small/ids.txt"), Path("small/sub/ids.txt")))
      )
      cmp.modified shouldBe Set()
    }

    it("when two files are swapped") {
      val cmp = CompareTask.compare(
        SkryncDir.scan(Small.src, digest = true),
        SkryncDir.scan(Small.srcSwappedFiles, digest = true)
      )
      cmp.srcOnly shouldBe Set()
      cmp.dstOnly shouldBe Set()
      cmp.moved shouldBe Set(
        DupFiles(Set(Path("small/ids.txt")), Set(Path("small/sub/ids2.txt"))),
        DupFiles(Set(Path("small/sub/ids2.txt")), Set(Path("small/ids.txt")))
      )
      // TODO: This should be empty, but it's the same as moved.
      // cmp.modified shouldBe Set()
    }
  }
}
