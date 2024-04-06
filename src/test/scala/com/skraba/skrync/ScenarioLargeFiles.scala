package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

import scala.reflect.io._
import scala.util.Random

/** Create a directory with many files inside, including at least one really big file..
  *
  * @param root
  *   An existing directory. The "large" directory will be created inside and deleted on [[cleanup]].
  * @param deleteRootOnCleanup
  *   Whether the root directory should also be deleted on cleanup.
  * @param numFiles
  *   The number of files to include in the scenario.
  */
class ScenarioLargeFiles(val root: Directory, val deleteRootOnCleanup: Boolean, val numFiles: Int = 1000) {

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

  /** The one big binary file in this scenario. */
  val bigFile: File = src / File("bigFile.bin")

  /** The SHA1 digest of the biggest file. */
  val bigFileDigest: Digest =
    Digests.fromHex("FC91B79BF39FAB778E18BA2C63F0DFC897E27F77")

  /** The SHA1 digest of the directory. */
  val dirDigest: Digest =
    Digests.fromHex("9379B4430CB333B06F7480C586C0C1461004AC69")

  {
    src.createDirectory(force = false, failIfExists = true)
    RandomFiles.fillDirectory(
      new Random(0L),
      dir = src,
      numFiles = numFiles,
      minFiles = 0,
      maxFiles = 10,
      maxDirs = 10,
      oneLarge = Some("bigFile.bin", Int.MaxValue / 10),
      time = Some(0)
    )
  }
}
