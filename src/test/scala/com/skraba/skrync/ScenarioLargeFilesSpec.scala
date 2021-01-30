package com.skraba.skrync

import com.skraba.skrync.SkryncGo.go
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.reflect.io._

/** Unit tests for [[SkryncGo]] using a large generated source directory.
  */
class ScenarioLargeFilesSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Large: ScenarioLargeFiles = new ScenarioLargeFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Large.cleanup()

  describe("Working with digests and files") {

    it("calculates digests correctly") {
      // The large file.
      Digests.getSha1(Large.bigFile) shouldBe Large.bigFileDigest

      Digests.getSha1(
        Large.src / File("3Z0EP1uW/4SIebeyBk/Edna6/0n90T06rcb.bin")
      ) shouldBe Digests.fromHex("ef7035c4aec006dc7d0c9655912b021c11d51a30")

      Digests.getSha1(
        Large.src / File("3Z0EP1uW/4SIebeyBk/Edna6/nvmVfF.txt")
      ) shouldBe
        Digests.fromHex("ff5cfa665f608d53024bdce9520d5abbe78ef27f")

    }

    it("generates a SkryncDir with directory attributes and digest.") {

      val (dirWithoutSha1, scanProgress) =
        Streamable.closing(new ByteArrayOutputStream()) { out =>
          Console.withOut(out) {
            val w = new PrintDigestProgress(Console.out)
            val dir = SkryncDir
              .scan(Large.src, w)
            out.flush()
            (dir, new String(out.toByteArray, StandardCharsets.UTF_8))
          }
        }

      // Ensure that all of the directories and files have been counted.
      scanProgress.groupBy(c => c).mapValues(c => c.length) shouldBe Map(
        '[' -> 798,
        '!' -> 1000,
        ']' -> 798
      )

      dirWithoutSha1.path.name shouldBe "large"
      // creation uses the modification time and access is not affected by reading files.
      // dirWithoutSha1.root.creation shouldBe 2000L
      //dirWithoutSha1.root.access shouldBe 1000L
      //dirWithoutSha1.root.modification shouldBe 2000L
      dirWithoutSha1.path.size shouldBe 229520625L
      dirWithoutSha1.path.digest shouldBe None
      dirWithoutSha1.deepFileCount shouldBe 1000
      dirWithoutSha1.dirs should have size 9
      dirWithoutSha1.files should have size 1

      val fileWithoutSha1 = dirWithoutSha1.files.head
      fileWithoutSha1.name shouldBe Large.bigFile.name
      // creation uses the modification time and access is based on the current time.
      fileWithoutSha1.creation shouldBe 11000L
      fileWithoutSha1.modification shouldBe 11000L
      fileWithoutSha1.size shouldBe Int.MaxValue / 10
      fileWithoutSha1.digest shouldBe None

      // Only digests are added by this method.
      val (dirWithSha1, digestProgress) =
        Streamable.closing(new ByteArrayOutputStream()) { out =>
          Console.withOut(out) {
            val w = new PrintDigestProgress(Console.out)
            val dir = dirWithoutSha1.digest(Large.src, w)
            out.flush()
            (dir, new String(out.toByteArray, StandardCharsets.UTF_8))
          }
        }

      // Ensure that all of the directories and files have been counted.
      digestProgress.groupBy(c => c).mapValues(c => c.length) shouldBe Map(
        '{' -> 798,
        '<' -> 1000,
        '.' -> 1204,
        '>' -> 1000,
        '}' -> 798
      )

      dirWithSha1.files should have size 1
      val fileWithSha1 = dirWithSha1.files.head
      Digests.toHex(fileWithSha1.digest.get) shouldBe Digests.toHex(
        Large.bigFileDigest
      )
      fileWithSha1 shouldBe
        fileWithoutSha1.copy(digest = Some(Large.bigFileDigest))

      Digests.toHex(dirWithSha1.path.digest.get) shouldBe Digests.toHex(
        Large.dirDigest
      )

      // Check flattening the directory.
      val paths = dirWithSha1.flattenPaths(Path("flatten"))
      paths should have size 1000
      paths should contain(
        Path(s"flatten/${Large.bigFile.name}") -> fileWithSha1
      )
    }

    describe("running the digest command") {
      it("generates a file when a destination is explicitly specified.") {
        // Run the application and check the output files.
        val dstDir: Directory = Large.root.resolve("dstDigest").toDirectory
        dstDir.createDirectory()
        go(
          "digest",
          "--srcDir",
          Large.src.toString,
          "--dstDigest",
          (dstDir / File("output.gz")).toString
        )

        // One file is created.
        val dst: Seq[Path] = dstDir.list.toSeq
        dst should have size 1

        // It should have an autogenerated name.
        val dstDigestFile = dst.headOption.value.toFile
        dstDigestFile.name shouldBe "output.gz"

        // The contents of the file should be readable.
        val dstRoot = Json.read(dstDigestFile)
        val expected =
          SkryncDir.scan(Large.src).digest(Large.src).copyWithoutTimes()
        dstRoot.info.copy(path =
          dstRoot.info.path.copy(name = "large")
        ) shouldBe expected
      }
    }
  }

  describe("Comparing two digests") {

    // Create the digest once for reuse
    val dstDir: Directory = Large.root.resolve("dstCompare").toDirectory
    dstDir.createDirectory()
    go(
      "digest",
      "--srcDir",
      Large.src.toString,
      "--dstDigest",
      (dstDir / File("compare.gz")).toString
    )

    describe("compare running the compare command") {
      it(
        "finds no differences when run on digests created on the same directory."
      ) {
        go(
          "digest",
          "--srcDir",
          Large.src.toString,
          "--dstDigest",
          (dstDir / File("compare2.gz")).toString
        )

        // Ignore the time the backup was created.
        val root =
          Json.read(dstDir / File("compare.gz")).copy(created = -1)
        val root2 =
          Json.read(dstDir / File("compare2.gz")).copy(created = -1)
        root shouldBe root2

        go(
          "compare",
          "--srcDigest",
          (dstDir / File("compare.gz")).toString,
          "--dstDigest",
          (dstDir / File("compare2.gz")).toString
        )
      }
    }
  }
}
