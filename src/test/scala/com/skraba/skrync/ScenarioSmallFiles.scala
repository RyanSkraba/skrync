package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

import scala.reflect.io._
import scala.util.Random

/** Create a small directory with two files inside for simple unit tests.
  *
  * @param root An existing directory.  The small and scenario directories will be created inside
  *              and deleted on [[cleanup]].
  * @param deleteRootOnCleanup Whether the root directory should also be deleted on cleanup.
  */
class ScenarioSmallFiles(
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

  /** A directory with the initial scenario:
    * - small/ids.txt
    * - small/sub/ids2.txt
    */
  val src: Directory = root / Directory("small")

  /** Identical to src without ids2.txt. */
  val srcDeletedFile: Directory = root / Directory("scenario1")

  /** Identical to src with ids2.txt modified. */
  val srcModifiedFile: Directory = root / Directory("scenario2")

  /** Identical to src with ids2.txt renamed to ids3.txt. */
  val srcRenamedFile: Directory = root / Directory("scenario3")

  /** The SHA1 digest of the ids.txt file. */
  val fileIdTxtDigest: Digest =
    Digests.fromHex("D732CFE0C7E77139A6A479A4C385A31AA2907CD2")

  /** The SHA1 digest of the src directory. */
  val dirDigest: Digest =
    Digests.fromHex("439FBA72B89A7D7E34BC58B6DA525DDAB7F77089")

  {
    // Create the ids.txt file in all of the scenario directories.
    val rnd = new Random()
    for (d <- Seq(src, srcDeletedFile, srcModifiedFile, srcRenamedFile)) {
      // This file is in all of the scenarios.
      d.createDirectory(force = false, failIfExists = true)
      RandomFiles.setTimeAttributes(d, 0L)
      RandomFiles.createTxtFileWithContents(
        rnd,
        d,
        "1;one\n2;two\n",
        Some("ids.txt")
      )

      d.resolve("sub").createDirectory(force = false, failIfExists = true)
      RandomFiles.setTimeAttributes(d.resolve("sub"), 0L)
      RandomFiles.setTimeAttributes(d, 0L)
    }

    // Create the second ids2.txt file in the src scenario.
    RandomFiles.createTxtFileWithContents(
      rnd,
      src.resolve("sub"),
      "3;three\n4;four\n",
      Some("ids2.txt")
    )

    // Create it with the same contents but different name in the renamed scenario.
    RandomFiles.createTxtFileWithContents(
      rnd,
      srcRenamedFile.resolve("sub"),
      "3;three\n4;four\n",
      Some("ids3.txt")
    )

    // Create it with different contents in the modified scenario.
    RandomFiles.createTxtFileWithContents(
      rnd,
      srcModifiedFile.resolve("sub"),
      "4;four\n3;three\n",
      Some("ids2.txt")
    )
  }
}
