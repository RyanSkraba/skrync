package com.skraba.skrync

import java.io.{IOException, InputStream}
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption
import java.security.{DigestInputStream, MessageDigest}
import scala.collection.immutable.BitSet
import scala.reflect.io.{File, Path}

/** Tools and utilities for calculating digests on objects. */
object Digests {

  /** Our digest is simply a byte array. */
  type Digest = Seq[Byte]

  /** Internally the contents of files use SHA-1. */
  private[this] def sha1Digest() = MessageDigest.getInstance("SHA-1")

  def fromHex(in: String): Digest = {
    in.sliding(2, 2).toSeq.map(Integer.parseInt(_, 16).toByte)
  }

  def toHex(in: Digest): String = {
    in.map("%02X" format _).mkString
  }

  private[this] def fromBytes(in: Iterable[Byte]): Digest = in.toSeq

  /** Get a SHA1 digest for a file.
    */
  @throws[IOException]
  def getSha1(file: File): Digest =
    fromBytes(getDigestUsingStream(sha1Digest(), file.inputStream()))

  @throws[IOException]
  private[this] def getDigestUsingStream(
      digest: MessageDigest,
      in: InputStream
  ): Array[Byte] = {
    val din = new DigestInputStream(in, digest)
    try {
      val buffer = new Array[Byte](1 << 20)
      while (din.read(buffer) != -1) {
        // Just scan through the entire stream.
      }
      digest.digest
    } finally if (din != null) din.close()
  }

  /** Get a SHA1 digest for a bunch of bytes.
    */
  @throws[IOException]
  def getSha1(in: Iterable[Iterable[Byte]]): Digest = {
    val digested = in.foldLeft(sha1Digest()) { (sha1, bytes) =>
      {
        sha1.update(bytes.toArray)
        sha1
      }
    }
    fromBytes(digested.digest())
  }
}
