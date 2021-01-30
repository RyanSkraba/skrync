package com.skraba.skrync

import java.io.{IOException, InputStream}
import java.security.{DigestInputStream, MessageDigest}
import scala.reflect.io.{File, Streamable}

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
    * @param file The file to get the SHA1 digest for
    * @param w A watcher to notify with progress on the digest
    */
  @throws[IOException]
  def getSha1(
      file: File,
      w: DigestProgress = IgnoreProgress
  ): Digest =
    fromBytes(getDigestUsingStream(sha1Digest(), file.inputStream(), w))

  @throws[IOException]
  private[this] def getDigestUsingStream(
      digest: MessageDigest,
      in: InputStream,
      w: DigestProgress
  ): Array[Byte] = {
    Streamable.closing(new DigestInputStream(in, digest)) { din =>
      val buffer = new Array[Byte](1 << 20)
      Stream
        .continually(din.read(buffer))
        .takeWhile { len =>
          len != -1
        }
        .foreach(len => w.digestingFileProgress(len))
    }
    digest.digest
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
