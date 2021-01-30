package com.skraba.skrync

import com.skraba.skrync.SkryncDirSpec.Example
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, FileNotFoundException}
import java.nio.charset.StandardCharsets
import java.nio.file.NoSuchFileException
import scala.collection.mutable
import scala.reflect.io._

/** Unit tests for [[DigestProgress]] using a small generated source directory.
  */
class DigestProgressSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  describe("Reporting progress while scanning and digesting") {

    it("can use a short, visual form streaming a character") {
      Streamable.closing(new ByteArrayOutputStream()) { out =>
        Console.withOut(out) {
          val w = new PrintDigestProgress(Console.out)
          SkryncDir
            .scan(Small.src, w)
            .digest(Small.src, w)
        }
        out.flush()
        val stdout = new String(out.toByteArray, StandardCharsets.UTF_8)
        // See the interpretation below.
        stdout shouldBe "[![!]]{<.>{<.>}}"
      }
    }

    it("performs the long form") {
      val w = new MockDigestProgress(Small.src)
      SkryncDir
        .scan(Small.src, w)
        .digest(Small.src, w)

      w.msgs.dequeue() shouldBe "Starting to scan... ()"
      w.msgs.dequeue() shouldBe "Scanned file (ids.txt) : 12"
      w.msgs.dequeue() shouldBe "Starting to scan... (sub)"
      w.msgs.dequeue() shouldBe "Scanned file (sub/ids2.txt) : 15"
      w.msgs.dequeue() shouldBe "Scanned dir (sub) : 15"
      w.msgs.dequeue() shouldBe "Scanned dir () : 27"
      w.msgs.dequeue() shouldBe "Digesting dir... () : None"
      w.msgs.dequeue() shouldBe "Digesting... (ids.txt) : None"
      w.msgs.dequeue() shouldBe "Digesting... : 12"
      w.msgs
        .dequeue() shouldBe s"Digested (ids.txt) : ${Digests.toHex(Small.fileIdTxtDigest)}"
      w.msgs.dequeue() shouldBe "Digesting dir... (sub) : None"
      w.msgs.dequeue() shouldBe "Digesting... (sub/ids2.txt) : None"
      w.msgs.dequeue() shouldBe "Digesting... : 15"
      w.msgs.dequeue() shouldBe "Digested (sub/ids2.txt) : 70EEB052A7DE574CCCA9608D38D64382EDB00F78"
      w.msgs.dequeue() shouldBe "Digested dir (sub) : EA0AE70F1FBBDB58778290AFCCAE3907C2074A46"
      w.msgs
        .dequeue() shouldBe s"Digested dir () : ${Digests.toHex(Small.dirDigest)}"
      w.msgs should have size 0
    }
  }
}

class MockDigestProgress(
    root: Path,
    var msgs: mutable.Queue[String] = mutable.Queue()
) extends DigestProgress {

  override def scanningDir(p: Directory): Unit = {
    msgs.enqueue(s"Starting to scan... (${root.relativize(p)})")
  }
  override def scannedDir(p: Directory, dir: SkryncDir): SkryncDir = {
    msgs.enqueue(s"Scanned dir (${root.relativize(p)}) : ${dir.path.size}")
    dir
  }
  override def scannedFile(p: Path, file: SkryncPath): SkryncPath = {
    msgs.enqueue(s"Scanned file (${root.relativize(p)}) : ${file.size}")
    file
  }
  override def digestingDir(p: Path, dir: SkryncDir): SkryncDir = {
    msgs.enqueue(
      s"Digesting dir... (${root.relativize(p)}) : ${dir.path.digest}"
    )
    dir
  }
  override def digestedDir(p: Path, dir: SkryncDir): SkryncDir = {
    msgs.enqueue(
      s"Digested dir (${root.relativize(p)}) : ${Digests.toHex(dir.path.digest.value)}"
    )
    dir
  }
  override def digestingFile(p: Path, file: SkryncPath): SkryncPath = {
    msgs.enqueue(
      s"Digesting... (${root.relativize(p)}) : ${file.digest}"
    )
    file
  }
  override def digestingFileProgress(len: Long): Long = {
    msgs.enqueue(
      s"Digesting... : ${len}"
    )
    len
  }
  override def digestedFile(p: Path, file: SkryncPath): SkryncPath = {
    msgs.enqueue(
      s"Digested (${root.relativize(p)}) : ${Digests.toHex(file.digest.value)}"
    )
    file
  }
}
