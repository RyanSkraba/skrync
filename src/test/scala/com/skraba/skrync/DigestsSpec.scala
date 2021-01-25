package com.skraba.skrync

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.Seq
import scala.reflect.io.{Directory, File}

/** Unit tests for [[Digests]]
  */
class DigestsSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** Temporary directory root for all tests. */
  val Small: ScenarioSmallFiles = new ScenarioSmallFiles(
    Directory.makeTemp(getClass.getSimpleName),
    deleteRootOnCleanup = true
  )

  override protected def afterAll(): Unit =
    Small.cleanup()

  describe("Hex conversion") {
    it("should convert to and from strings") {
      Digests.fromHex("") should equal(Seq())
      Digests.fromHex("1") should equal(Seq(0x01))
      Digests.fromHex("A") should equal(Seq(0x0a))
      Digests.fromHex("a") should equal(Seq(0x0a))
      Digests.fromHex("A0") should equal(Seq(0xa0.toByte))
      Digests.fromHex("a0") should equal(Seq(0xa0.toByte))
      Digests.fromHex("FF") should equal(Seq(0xff.toByte))
      Digests.fromHex("0FF0") should equal(Seq(0x0f, 0xf0.toByte))
      Digests.fromHex("000000000FF0") should equal(
        Seq(0, 0, 0, 0, 0x0f, 0xf0.toByte)
      )
      Digests.toHex(Seq()) should equal("")
      Digests.toHex(Seq(0x01)) should equal("01")
      Digests.toHex(Seq(0x0a)) should equal("0A")
      Digests.toHex(Seq(0xa0.toByte)) should equal("A0")
      Digests.toHex(Seq(0xff.toByte)) should equal("FF")
      Digests.toHex(Seq(0x0f, 0xf0.toByte)) should equal("0FF0")
      Digests.toHex(Seq(0, 0, 0, 0, 0x0f, 0xf0.toByte)) should equal(
        "000000000FF0"
      )
    }
  }

  describe("From files") {
    it("calculates digests.") {
      Digests.getSha1(Small.src / File("ids.txt")) should equal(
        Small.fileIdTxtDigest
      )
    }
  }
}
