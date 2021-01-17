package com.skraba.skrync

import com.skraba.skrync.SkryncDirSpec.Example
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.FileNotFoundException
import java.nio.file.NoSuchFileException
import scala.reflect.io._

/** Unit tests for [[SkryncDir]] using a small generated source directory.
  */
class SkryncDirSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSingleFile = new ScenarioSingleFile(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  describe("SkryncDir") {

    it("can initialize itself from a path.") {
      val withoutSha1 = SkryncDir(Small.src)
      withoutSha1.path.name should equal("small")
      withoutSha1.path.size should equal(12L)
      // creation uses the modification time and access is based on the current time.
      withoutSha1.path.creation should equal(2000L)
      withoutSha1.path.access should equal(1000L)
      withoutSha1.path.modification should equal(2000L)
      withoutSha1.path.digest should equal(None)
      withoutSha1.deepFileCount should equal(1)
      withoutSha1.files should equal(
        List(SkryncPath("ids.txt", 12, 2000, 1000, 2000, None))
      )
      withoutSha1.dirs should have size 0

      // Only the digest is added by this method.  You have to respecify the location on disk.
      val digestedDir = withoutSha1.path.copy(digest = Some(Small.dirDigest))
      val digestedFile =
        withoutSha1.files.head.copy(digest = Some(Small.fileDigest))

      val withSha1 = withoutSha1.digest(Small.src)
      withSha1 should equal(
        withoutSha1.copy(path = digestedDir, files = List(digestedFile))
      )
    }

    it("can be stripped of time information.") {
      val withoutTimes = Example.copyWithoutTimes()
      withoutTimes.path.name should equal("dir")
      withoutTimes.path.size should equal(54321L)
      // creation uses the modification time and access is based on the current time.
      withoutTimes.path.creation should equal(-1L)
      withoutTimes.path.access should equal(-1L)
      withoutTimes.path.modification should equal(-1L)
      withoutTimes.path.digest should equal(Example.path.digest)
      withoutTimes.deepFileCount should equal(1)
      withoutTimes.files should equal(
        List(
          SkryncPath("file", 12345, -1, -1, -1, withoutTimes.files.head.digest)
        )
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
