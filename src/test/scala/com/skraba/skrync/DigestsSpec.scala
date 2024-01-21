package com.skraba.skrync

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File}

/** Unit tests for [[Digests]] */
class DigestsSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit = Small.cleanup()

  describe("Hex conversion") {
    it("should convert to and from strings") {
      Digests.fromHex("") shouldBe Seq()
      Digests.fromHex("1") shouldBe Seq(0x01)
      Digests.fromHex("A") shouldBe Seq(0x0a)
      Digests.fromHex("a") shouldBe Seq(0x0a)
      Digests.fromHex("A0") shouldBe Seq(0xa0.toByte)
      Digests.fromHex("a0") shouldBe Seq(0xa0.toByte)
      Digests.fromHex("FF") shouldBe Seq(0xff.toByte)
      Digests.fromHex("0FF0") shouldBe Seq(0x0f, 0xf0.toByte)
      Digests
        .fromHex("000000000FF0") shouldBe Seq(0, 0, 0, 0, 0x0f, 0xf0.toByte)

      Digests.toHex(Seq.empty) shouldBe ""
      Digests.toHex(Seq(0x01)) shouldBe "01"
      Digests.toHex(Seq(0x0a)) shouldBe "0A"
      Digests.toHex(Seq(0xa0.toByte)) shouldBe "A0"
      Digests.toHex(Seq(0xff.toByte)) shouldBe "FF"
      Digests.toHex(Seq(0x0f, 0xf0.toByte)) shouldBe "0FF0"
      Digests.toHex(Seq(0, 0, 0, 0, 0x0f, 0xf0.toByte)) shouldBe "000000000FF0"
    }
  }

  describe("From files") {
    it("calculates digests.") {
      Digests.getSha1(
        Small.src / File("ids.txt")
      ) shouldBe Small.fileIdTxtDigest
    }
  }
}
