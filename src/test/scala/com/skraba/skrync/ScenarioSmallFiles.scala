package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.RandomFiles.createTxtContents

import scala.reflect.io._

/** Create a small directory with two files inside for simple unit tests, and some variations.
  *
  * Given `root` as the base directory:
  *
  *   1. An original scenario with two CSV files.
  *      - `root/original/small/ids.txt`
  *      - `root/original/small/sub/ids2.txt`
  *   1. A variation where one of the files is deleted.
  *      - `root/scenario1/small/ids.txt`
  *   1. A variation where one of the files is modified.
  *      - `root/scenario2/small/ids.txt`
  *      - `root/scenario2/small/sub/ids2.txt` __same name but changed__
  *   1. A variation where one of the files is unmodified but renamed.
  *      - `root/scenario3/small/ids.txt`
  *      - `root/scenario3/small/sub/ids2a.txt` __different name but unchanged__
  *   1. A variation where one of the files was copied
  *      - `root/scenario4/small/ids.txt`
  *      - `root/scenario4/small/sub/ids.txt`
  *      - `root/scenario4/small/sub/ids2.txt`
  *   1. A variation where both files have switched places
  *      - `root/scenario5/small/ids2.txt`
  *      - `root/scenario5/small/sub/ids.txt`
  *   1. A variation to explore duplicates
  *      - `root/scenario6/small/ids.txt`
  *      - `root/scenario6/small/sub/ids2.txt`
  *      - `root/scenario6/small/dup1/ids.txt`
  *      - `root/scenario6/small/dup1/ids3.txt`
  *      - `root/scenario6/small/dup2/ids4a.txt`
  *      - `root/scenario6/small/dup2/ids4b.txt`
  *      - `root/scenario6/small/dup3/ids2a.txt`
  *      - `root/scenario6/small/dup3/ids5.txt`
  *      - `root/scenario6/small/dup3/sub/ids5.txt`
  *
  * @param root
  *   An existing directory that will be populated with the above files. This class doesn't clean up the filesystem
  *   after use.
  */
class ScenarioSmallFiles(val root: Directory) {

  /** A pre-existing file outside the small scenario. */
  val ExistingFile: File = root / File("exists")
  Streamable.closing(ExistingFile.outputStream())(_.write(1))

  /** A path inside the small scenario that doesn't exist. */
  val DoesntExist: String = (root / "doesnt-exist").toString()

  /** A directory with the initial scenario:
    *
    *   - small/ids.txt
    *   - small/sub/ids2.txt
    */
  val src: Directory = root / "original" / Directory("small")

  /** A directory that is empty at the beginning of the scenario */
  val dst: Directory = (root / "dst").createDirectory()

  /** Identical to src without ids2.txt. */
  val srcDeletedFile: Directory = root / "srcDeletedFile" / Directory("small")

  /** Identical to src with ids2.txt modified. */
  val srcModifiedFile: Directory = root / "srcModifiedFile" / Directory("small")

  /** Identical to src with ids2.txt renamed to ids2a.txt. */
  val srcRenamedFile: Directory = root / "srcRenamedFile" / Directory("small")

  /** Identical to src with ids2.txt moved to a different directory. */
  val srcMovedFile: Directory = root / "srcMovedFile" / Directory("small")

  /** Identical to src with a duplicate ids.txt file. */
  val srcWithDuplicateFile: Directory = root / "srcWithDuplicateFile" / Directory("small")

  /** Identical to src, swapping the directories the two files are in. */
  val srcSwappedFiles: Directory = root / "srcSwappedFiles" / Directory("small")

  /** Identical to src, with additional directories containing duplicate and unique files. */
  val srcWithDuplicates: Directory = root / "srcWithDuplicates" / Directory("small")

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
    // The original source
    createTxtContents(src / "ids.txt", File1Contents)
    createTxtContents(src / "sub" / "ids2.txt", File2Contents)

    // This scenario has a deleted file
    {
      val scenario: Directory = srcDeletedFile
      createTxtContents(scenario / "ids.txt", File1Contents)
      (scenario / Directory("sub")).createDirectory()
    }

    // This scenario has a modified file
    {
      val scenario: Directory = srcModifiedFile
      createTxtContents(scenario / "ids.txt", File1Contents)
      createTxtContents(scenario / "sub" / "ids2.txt", "4;four\n3;three\n")
    }

    // This scenario has a renamed file
    {
      val scenario: Directory = srcRenamedFile
      createTxtContents(scenario / "ids.txt", File1Contents)
      createTxtContents(scenario / "sub" / "ids2a.txt", File2Contents)
    }

    // This scenario has a moved file
    {
      val scenario: Directory = srcMovedFile
      createTxtContents(scenario / "ids.txt", File1Contents)
      createTxtContents(scenario / "sub2" / "ids2.txt", File2Contents)
    }

    // This scenario has a duplicated file
    {
      val scenario: Directory = srcWithDuplicateFile
      createTxtContents(scenario / "ids.txt", File1Contents)
      createTxtContents(scenario / "sub" / "ids.txt", File1Contents)
      createTxtContents(scenario / "sub" / "ids2.txt", File2Contents)
    }

    // The contents in this scenario have been swapped
    {
      val scenario: Directory = srcSwappedFiles
      createTxtContents(scenario / "ids.txt", File2Contents)
      createTxtContents(scenario / "sub" / "ids2.txt", File1Contents)
    }

    // Different types of duplicate files in this scenario
    {
      val scenario: Directory = srcWithDuplicates
      createTxtContents(scenario / "ids.txt", File1Contents)
      createTxtContents(scenario / "sub" / "ids2.txt", File2Contents)
      createTxtContents(scenario / "dup1" / "ids.txt", File1Contents)
      createTxtContents(scenario / "dup1" / "ids3.txt", File3Contents)
      createTxtContents(scenario / "dup2" / "ids4.txt", File4Contents)
      createTxtContents(scenario / "dup2" / "ids4a.txt", File4Contents)
      createTxtContents(scenario / "dup3" / "ids2a.txt", File2Contents)
      createTxtContents(scenario / "dup3" / "ids5.txt", File5Contents)
      createTxtContents(scenario / "dup3" / "sub" / "ids5.txt", File5Contents)
    }

    // Set all of the time attributes on the scenario.
    RandomFiles.setTimeAttributes(root, 0L, recursive = true)
  }
}
