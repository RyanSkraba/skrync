package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

import java.util.Random
import scala.reflect.io._

/** Create a directory with many files inside, including at least one really big file..
  *
  * @param root An existing directory.  The "large" directory will be created inside and deleted
  *             on [[cleanup]].
  * @param deleteRootOnCleanup Whether the root directory should also be deleted on cleanup.
  */
class ScenarioLargeFiles(
    val root: Directory,
    val deleteRootOnCleanup: Boolean
) {

  /** Remove the directory inside the root, including the root directory if requested. */
  def cleanup(): Unit =
    try {
      src.deleteRecursively()
      if (deleteRootOnCleanup)
        root.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  /** Simple test directory with many files. */
  val src: Directory = root / Directory("large")

  /** The number of files to generate in the large directory. */
  val srcLargeSize = 1000

  /** The one big binary file in this scenario (deterministically generated in the tool). */
  val bigFile: File = src / File("V3XmHf.bin")

  /** The SHA1 digest of the biggest file. */
  val bigFileDigest: Digest =
    Digests.fromHex("62d3e103aedbf032ac196d4fd1e49ab05bdb4950")

  /** The SHA1 digest of the directory. */
  val dirDigest: Digest =
    Digests.fromHex("360075c4739867dea791892ec96604ea3820a806")

  {
    src.createDirectory(force = false, failIfExists = true)
    RandomFiles.fillDirectory(
      new Random(0L),
      dir = java.nio.file.Paths.get(src.toURI),
      numFiles = srcLargeSize,
      minFiles = 0,
      maxFiles = 10,
      maxDirs = 10,
      oneLarge = Some(Int.MaxValue / 10),
      time = Some(0)
    )
  }
}
