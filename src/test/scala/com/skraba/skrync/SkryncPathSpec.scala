package com.skraba.skrync

import com.skraba.skrync.SkryncPathSpec.Example
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.FileNotFoundException
import java.nio.file.NoSuchFileException
import scala.reflect.io._

/** Unit tests for [[SkryncPath]] using a small generated source directory.
  */
class SkryncPathSpec
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

  describe("SkryncPath") {

    it("can initialize itself from a path.") {
      val withoutSha1 = SkryncPath(Small.src / File("ids.txt"))
      withoutSha1.name should equal("ids.txt")
      withoutSha1.size should equal(12L)
      // creation uses the modification time and access is based on the current time.
      withoutSha1.creation should equal(2000L)
      withoutSha1.modification should equal(2000L)
      withoutSha1.digest should equal(None)

      // Only the digest is added by this method.  You have to respecify the location on disk.
      val withSha1 = withoutSha1.digest(Small.src / File("ids.txt"))
      withSha1 should equal(withoutSha1.copy(digest = Some(Small.fileDigest)))
    }

    it("can be stripped of time information.") {
      val withoutTimes = Example.copyWithoutTimes()
      withoutTimes.name should equal("file")
      withoutTimes.size should equal(12345L)
      // creation uses the modification time and access is based on the current time.
      withoutTimes.creation should equal(-1L)
      withoutTimes.access should equal(-1L)
      withoutTimes.modification should equal(-1L)
      withoutTimes.digest should equal(Example.digest)
    }

    it("can be initialised with digest information from an existing file.") {
      // Only the digest is added by this method, and it uses the specified location even if it
      // doesn't match the name.
      val withSha1 = Example.digest(Small.src / File("ids.txt"))
      withSha1 should equal(Example.copy(digest = Some(Small.fileDigest)))
    }

    it("fails when digesting on a path that doesn't exist.") {
      val t = intercept[FileNotFoundException] {
        Example.digest(Small.src / File(Example.name))
      }
      t.getMessage should include("tmp/small/file (No such file or directory)")
    }

    it("fails when creating from a path that doesn't exist.") {
      val t = intercept[NoSuchFileException] {
        SkryncPath(Small.src / File("file-does-not-exist"))
      }
      t.getMessage should include("tmp/small/file-does-not-exist")
    }
  }
}

object SkryncPathSpec {

  /** An initialized instance to test with. */
  val Example = SkryncPath(
    name = "file",
    size = 12345,
    creation = 23456,
    access = 34567,
    modification = 45678,
    digest = Some(Digests.fromHex("ABCDEF"))
  )
}
