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
  *    - `root/scenario2/small/sub/ids2.txt` __same name but changed__
  *  1. A variation where one of the files is unmodified but renamed.
  *    - `root/scenario3/small/ids.txt`
  *    - `root/scenario3/small/sub/ids2a.txt` __different name but unchanged__
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
  *    - `root/scenario6/small/dup2/ids4a.txt`
  *    - `root/scenario6/small/dup2/ids4b.txt`
  *    - `root/scenario6/small/dup3/ids2a.txt`
  *    - `root/scenario6/small/dup3/ids5.txt`
  *    - `root/scenario6/small/dup3/sub/ids5.txt`
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
    *
    *  - small/ids.txt
    *  - small/sub/ids2.txt
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

  val File1Contents = "1;one\n2;two\n"
  val File2Contents = "3;three\n4;four\n"
  val File3Contents = "5;five\n6;six\n"
  val File4Contents = "7;seven\n8;eight\n"
  val File5Contents = "9;nine\n10;ten\n"

  {
    createTxtContents(src / "ids.txt", File1Contents)
    createTxtContents(src / "sub" / "ids2.txt", File2Contents)

    // scenario1 has a deleted file
    val s1: Directory = srcDeletedFile
    createTxtContents(s1 / "ids.txt", File1Contents)
    (s1 / Directory("sub")).createDirectory()

    // scenario2 has a modified file
    val s2: Directory = srcModifiedFile
    createTxtContents(s2 / "ids.txt", File1Contents)
    createTxtContents(s2 / "sub" / "ids2.txt", "4;four\n3;three\n")

    // scenario3
    val s3: Directory = srcRenamedFile
    createTxtContents(s3 / "ids.txt", File1Contents)
    createTxtContents(s3 / "sub" / "ids2a.txt", File2Contents)

    // scenario4
    val s4: Directory = srcWithDuplicateFile
    createTxtContents(s4 / "ids.txt", File1Contents)
    createTxtContents(s4 / "sub" / "ids.txt", File1Contents)
    createTxtContents(s4 / "sub" / "ids2.txt", File2Contents)

    // scenario5
    val s5: Directory = srcSwappedFiles
    createTxtContents(s5 / "sub" / "ids.txt", File1Contents)
    createTxtContents(s5 / File("ids2.txt"), File2Contents)

    // scenario6
    val s6: Directory = srcWithDuplicates
    createTxtContents(s6 / "ids.txt", File1Contents)
    createTxtContents(s6 / "sub" / "ids2.txt", File2Contents)
    createTxtContents(s6 / "dup1" / "ids.txt", File1Contents)
    createTxtContents(s6 / "dup1" / "ids3.txt", File3Contents)
    createTxtContents(s6 / "dup2" / "ids4.txt", File4Contents)
    createTxtContents(s6 / "dup2" / "ids4a.txt", File4Contents)
    createTxtContents(s6 / "dup3" / "ids2a.txt", File2Contents)
    createTxtContents(s6 / "dup3" / "ids5.txt", File5Contents)
    createTxtContents(s6 / "dup3" / "sub" / "ids5.txt", File5Contents)

    // Set all of the time attributes on the scenario.
    RandomFiles.setTimeAttributes(root, 0L, recursive = true)
  }
}
