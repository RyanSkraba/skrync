package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

import scala.reflect.io._

/** Create a directory with a single file inside for simple unit tests.
  *
  * @param root An existing directory.  The "small" directory will be created inside and deleted
  *             on [[cleanup]].
  * @param deleteRootOnCleanup Whether the root directory should also be deleted on cleanup.
  */
class ScenarioSingleFile(
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

  // Simple test directory with one file. Initialized before all of the tests.
  val src: Directory = root / Directory("small")

  // The SHA1 digest of the ids.txt file.
  val fileDigest: Digest =
    Digests.fromHex("d732cfe0c7e77139a6a479a4c385a31aa2907cd2")

  // The SHA1 digest of the directory with ids.txt file.
  val dirDigest: Digest =
    Digests.fromHex("8ea0b84dd0fbc32109125102a87b46ecdb82124c")

  {
    src.createDirectory(force = false, failIfExists = true)
    val file = src.resolve("ids.txt")
    val fos = file.toFile.outputStream(false)
    try {
      fos.write("1;one\n2;two\n".getBytes)
    } finally if (fos != null) fos.close()
    RandomFiles.setTimeAttributes(java.nio.file.Paths.get(file.toURI), 0L)
    RandomFiles.setTimeAttributes(java.nio.file.Paths.get(src.toURI), 0L)
  }
}
