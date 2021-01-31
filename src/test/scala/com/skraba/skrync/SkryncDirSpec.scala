package com.skraba.skrync

import com.skraba.skrync.SkryncDirSpec.Example
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.FileNotFoundException
import java.nio.file.NoSuchFileException
import scala.reflect.io._

/** Unit tests for [[SkryncDir]] using a small generated source directory.
  */
class SkryncDirSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  describe("SkryncDir") {

    it("can initialize itself from a path.") {
      val withoutSha1 = SkryncDir.scan(Small.src)
      withoutSha1.path.name shouldBe "small"
      withoutSha1.path.size shouldBe 27L
      // creation uses the modification time and access is based on the current time.
      withoutSha1.path.creation shouldBe 2000L
      withoutSha1.path.access shouldBe 1000L
      withoutSha1.path.modification shouldBe 2000L
      withoutSha1.path.digest shouldBe None
      withoutSha1.deepFileCount shouldBe 2
      withoutSha1.files should contain(
        SkryncPath("ids.txt", 12, 2000, 1000, 2000, None)
      )
      withoutSha1.dirs should have size 1

      // Only the digest is added by this method.  You have to respecify the location on disk.
      val digestedDir = withoutSha1.path.copy(digest = Some(Small.dirDigest))
      val digestedFile =
        withoutSha1.files.head.copy(digest = Some(Small.fileIdTxtDigest))

      val withSha1 = withoutSha1.digest(Small.src)
      withSha1.path shouldBe digestedDir
      withSha1.files shouldBe List(digestedFile)
      withSha1.dirs.headOption.value.files should have size 1
    }

    it("can be stripped of time information.") {
      val withoutTimes = Example.copyWithoutTimes()
      withoutTimes.path.name shouldBe "dir"
      withoutTimes.path.size shouldBe 54321L
      // creation uses the modification time and access is based on the current time.
      withoutTimes.path.creation shouldBe -1L
      withoutTimes.path.access shouldBe -1L
      withoutTimes.path.modification shouldBe -1L
      withoutTimes.path.digest shouldBe Example.path.digest
      withoutTimes.deepFileCount shouldBe 1
      withoutTimes.files shouldBe
        List(
          SkryncPath("file", 12345, -1, -1, -1, withoutTimes.files.head.digest)
        )
      withoutTimes.dirs should have size 0
    }

    it("fails digesting from a existing file.") {
      // Only the digest is added by this method, and it uses the specified location even if it
      // doesn't match the name.
      val t = intercept[FileNotFoundException] {
        Example.digest(Small.src / File("ids.txt"))
      }
      t.getMessage should include("(Not a directory)")
    }

    it("fails when digesting on a path when the contents do not match.") {
      val t = intercept[FileNotFoundException] {
        Example.digest(Small.src)
      }
      t.getMessage should include("tmp/small/file (No such file or directory)")
    }

    it("fails when creating from a path that doesn't exist.") {
      val t = intercept[NoSuchFileException] {
        SkryncPath(Small.src / Directory("dir-does-not-exist"))
      }
      t.getMessage should include("tmp/small/dir-does-not-exist")
    }

    it("can flatten its path contents.") {
      val src = SkryncDir.scan(Small.src)
      val paths = src.flattenPaths(Path("."))
      paths should have size 2
      paths should contain(Path("./ids.txt") -> src.files.head)
    }
  }
}

object SkryncDirSpec {

  /** An initialized instance to test with. */
  val Example = SkryncDir(
    path = SkryncPath(
      name = "dir",
      size = 54321,
      creation = 65432,
      access = 76543,
      modification = 87654,
      digest = Some(Digests.fromHex("FEDCBA"))
    ),
    deepFileCount = 1,
    files = List(SkryncPathSpec.Example),
    dirs = List.empty
  )
}
