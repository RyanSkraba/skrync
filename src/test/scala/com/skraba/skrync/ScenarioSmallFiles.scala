package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.RandomFiles.createTxtContents

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
  *    - `root/scenario2/small/sub/ids2.txt` ```same name but changed```
  *  1. A variation where one of the files is unmodified but renamed.
  *    - `root/scenario3/small/ids.txt`
  *    - `root/scenario3/small/sub/ids2a.txt` ```different name but unchanged```
  *  1. A variation where one of the files was copied
  *    - `root/scenario4/small/ids.txt`
  *    - `root/scenario4/small/sub/ids.txt`
  *    - `root/scenario4/small/sub/ids2.txt`
  *  1. A variation where both files have switched places
  *    - `root/scenario5/small/ids2.txt`
  *    - `root/scenario5/small/sub/ids.txt`
  *  1. A variation to explore duplicates
  *    - `root/scenario6/small/ids.txt`
  *    - `root/scenario6/small/sub/ids2.txt`
  *    - `root/scenario6/small/dup1/ids.txt`
  *    - `root/scenario6/small/dup1/ids3.txt`
  *    - `root/scenario6/small/dup2/ids3.txt`
  *    - `root/scenario6/small/dup2/ids3b.txt`
  *    - `root/scenario6/small/dup3/ids2a.txt`
  *    - `root/scenario6/small/dup3/ids3.txt`
  *    - `root/scenario6/small/dup3/sub/ids3.txt`
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

  /** Identical to src with ids2.txt renamed to ids2a.txt. */
  val srcRenamedFile: Directory = root / "scenario3" / Directory("small")

  /** Identical to src with a duplicate ids.txt file. */
  val srcWithDuplicateFile: Directory = root / "scenario4" / Directory("small")

  /** Identical to src, swapping the directories the two files are in. */
  val srcSwappedFiles: Directory = root / "scenario5" / Directory("small")

  /** Identical to src, with additional directories containing duplicate and unique files. */
  val srcWithDuplicates: Directory = root / "scenario6" / Directory("small")

  /** The SHA1 digest of the ids.txt file. */
  val fileIdTxtDigest: Digest =
    Digests.fromHex("D732CFE0C7E77139A6A479A4C385A31AA2907CD2")

  /** The SHA1 digest of the src directory. */
  val dirDigest: Digest =
    Digests.fromHex("439FBA72B89A7D7E34BC58B6DA525DDAB7F77089")

  {
    createTxtContents(src / File("ids.txt"), "1;one\n2;two\n")
    createTxtContents(src / "sub" / File("ids2.txt"), "3;three\n4;four\n")

    // scenario1 has a deleted file
    createTxtContents(srcDeletedFile / File("ids.txt"), "1;one\n2;two\n")
    srcDeletedFile / Directory("sub") createDirectory ()

    // scenario2 has a modified file
    createTxtContents(srcModifiedFile / File("ids.txt"), "1;one\n2;two\n")
    createTxtContents(
      srcModifiedFile / "sub" / File("ids2.txt"),
      "4;four\n3;three\n"
    )

    // scenario3
    createTxtContents(srcRenamedFile / File("ids.txt"), "1;one\n2;two\n")
    createTxtContents(
      srcRenamedFile / "sub" / File("ids2a.txt"),
      "3;three\n4;four\n"
    )

    // scenario4
    createTxtContents(srcWithDuplicateFile / File("ids.txt"), "1;one\n2;two\n")
    createTxtContents(
      srcWithDuplicateFile / "sub" / File("ids.txt"),
      "1;one\n2;two\n"
    )
    createTxtContents(
      srcWithDuplicateFile / "sub" / File("ids2.txt"),
      "3;three\n4;four\n"
    )

    // scenario5
    createTxtContents(
      srcSwappedFiles / "sub" / File("ids.txt"),
      "1;one\n2;two\n"
    )
    createTxtContents(srcSwappedFiles / File("ids2.txt"), "3;three\n4;four\n")

    // scenario6
    createTxtContents(srcWithDuplicates / File("ids.txt"), "1;one\n2;two\n")
    createTxtContents(
      srcWithDuplicates / "sub" / File("ids2.txt"),
      "3;three\n4;four\n"
    )
    createTxtContents(
      srcWithDuplicates / "dup1" / File("ids.txt"),
      "1;one\n2;two\n"
    )
    createTxtContents(
      srcWithDuplicates / "dup1" / File("ids3.txt"),
      "5;five\n6;six\n"
    )
    createTxtContents(
      srcWithDuplicates / "dup2" / File("ids3.txt"),
      "5;five\n6;six\n"
    )
    createTxtContents(
      srcWithDuplicates / "dup2" / File("ids3a.txt"),
      "5;five\n6;six\n"
    )
    createTxtContents(
      srcWithDuplicates / "dup3" / File("ids2a.txt"),
      "3;three\n4;four\n"
    )
    createTxtContents(
      srcWithDuplicates / "dup3" / File("ids3.txt"),
      "5;five\n6;six\n"
    )
    createTxtContents(
      srcWithDuplicates / "dup3" / "sub" / File("ids3.txt"),
      "5;five\n6;six\n"
    )

    // Set all of the time attributes on the scenario.
    RandomFiles.setTimeAttributes(root, 0L, recursive = true)
  }
}
