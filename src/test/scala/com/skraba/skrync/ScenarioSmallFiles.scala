package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

import scala.reflect.io._

/** Create a small directory with two files inside for simple unit tests, and some variations.
  *
  * Given `root` as the base directory:
  *
  *  1. An original scenario with two CSV files.
  *    - `root/original/small/ids.txt`
  *    - `root/original/small/sub/ids2.txt`
  *  1. A variation where one of the files is deleted.
  *    - `root/scenario1/small/ids.txt`
  *  1. A variation where one of the files is modified.
  *    - `root/scenario2/small/ids.txt`
  *    - `root/scenario2/small/sub/ids2.txt`
  *  1. A variation where one of the files is unmodified but renamed.
  *    - `root/scenario3/small/ids.txt`
  *    - `root/scenario3/small/sub/ids3.txt`
  *  1. A variation where one of the files was copied
  *    - `root/scenario4/small/ids.txt`
  *    - `root/scenario4/small/sub/ids.txt`
  *    - `root/scenario4/small/sub/ids2.txt`
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
  val src: Directory = root / "original" / Directory("small")

  /** Identical to src without ids2.txt. */
  val srcDeletedFile: Directory = root / "scenario1" / Directory("small")

  /** Identical to src with ids2.txt modified. */
  val srcModifiedFile: Directory = root / "scenario2" / Directory("small")

  /** Identical to src with ids2.txt renamed to ids3.txt. */
  val srcRenamedFile: Directory = root / "scenario3" / Directory("small")

  /** Identical to src with a duplicate ids.txt file. */
  val srcWithDuplicateFile: Directory = root / "scenario4" / Directory("small")

  /** The SHA1 digest of the ids.txt file. */
  val fileIdTxtDigest: Digest =
    Digests.fromHex("D732CFE0C7E77139A6A479A4C385A31AA2907CD2")

  /** The SHA1 digest of the src directory. */
  val dirDigest: Digest =
    Digests.fromHex("439FBA72B89A7D7E34BC58B6DA525DDAB7F77089")

  {
    // Create the ids.txt file in all of the scenario directories.
    for (
      d <- Seq(
        src,
        srcDeletedFile,
        srcModifiedFile,
        srcRenamedFile,
        srcWithDuplicateFile
      )
    ) {
      // This file is in all of the scenarios.
      d.createDirectory(force = true, failIfExists = true)
      RandomFiles.setTimeAttributes(d, 0L)
      RandomFiles.createTxtFileWithContents(
        d / File("ids.txt"),
        "1;one\n2;two\n"
      )

      d.resolve("sub").createDirectory(force = false, failIfExists = true)
      RandomFiles.setTimeAttributes(d.resolve("sub"), 0L)
      RandomFiles.setTimeAttributes(d, 0L)
    }

    // Create the second ids2.txt file in the src scenario.
    RandomFiles.createTxtFileWithContents(
      src / "sub" / File("ids2.txt"),
      "3;three\n4;four\n"
    )

    // Create it with the same contents but different name in the renamed scenario.
    RandomFiles.createTxtFileWithContents(
      srcRenamedFile / "sub" / File("ids3.txt"),
      "3;three\n4;four\n"
    )

    // Create it with different contents in the modified scenario.
    RandomFiles.createTxtFileWithContents(
      srcModifiedFile / "sub" / File("ids2.txt"),
      "4;four\n3;three\n"
    )

    // Create it with different contents in the modified scenario.
    RandomFiles.createTxtFileWithContents(
      srcWithDuplicateFile / "sub" / File("ids.txt"),
      "1;one\n2;two\n"
    )
    RandomFiles.createTxtFileWithContents(
      srcWithDuplicateFile / "sub" / File("ids2.txt"),
      "4;four\n3;three\n"
    )
  }
}
