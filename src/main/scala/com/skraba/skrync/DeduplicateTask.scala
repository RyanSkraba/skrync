package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.SkryncGo.{validateDirectory, validateFile}
import com.skraba.skrync.SkryncPath.isIn

import scala.reflect.io._

/** This task reads a digest file, and makes a comparison with an external directory to find files that are known or
  * unknown in the original digest path.
  *
  * This can be useful for filing new files in the original directory from where the digest was made.
  */
object DeduplicateTask {

  val Cmd = "dedup"

  val Description = "Finds files that are already known or unknown in the digest."

  val Doc: String =
    """%s
      |
      |Usage:
      |  SkryncGo dedup --srcDigest SRC --dedupDir DEDUP_DIR [options]
      |
      |Options:
      |  --srcDigest SRC       The file generated from the source directory.
      |  --dedupDir DEDUP_DIR  Provide a deduplication report on all the files
      |                        in DEDUP_DIR
      |  --mvDir MV_DIR        If present, moves duplicates to MV_DIR
      |
      |Examples:
      |
      | # Provide a deduplication report on the files in the given directory
      | SkryncGo dedup --srcDigest $HOME/skrync/backup1 --dedupDir $HOME/dedup/
      |
      |""".stripMargin
      .format(Description)
      .trim

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  case class Report(duplicateFiles: Seq[Seq[(SkryncPath, Path)]])

  /** Analysis of the contents of one specific directory to find duplicate and unique files.
    *
    * Note that if there is a file that exists twice but only inside the dedupPath, one will appear in the uniques and
    * one will appear in the duplicates.
    *
    * @param src
    *   The prepared files analysis
    * @param dedupPath
    *   The path to deduplicate, relative to the root of the analysis.
    * @param uniques
    *   Files in the dedupPath that do not exist outside the dedupPath.
    * @param duplicates
    *   Files in the dedupPath that exist more than once.
    */
  case class DedupPathReport(
      src: SkryncGo.Analysis,
      dedupPath: Directory,
      uniques: Seq[(Path, SkryncPath)],
      duplicates: Seq[(Path, SkryncPath)]
  )

  object DedupPathReport {

    /** Given the analysis file and the dedup directory, determines which of the files in the directory already exist.
      *
      * @param src
      *   The source analysis of an existing directory.
      * @param dedupPath
      *   A new directory. If this is inside the src directory, then the information is directly used from there.
      * @return
      *   A deduplication report of different and unique files in the dedupPath.
      */
    def apply(src: SkryncGo.Analysis, dedupPath: Directory): DedupPathReport = {

      // All of the files in the analysis, sorted into whether they are in the dedupPath
      val (
        srcInDedup: Seq[(Path, SkryncPath)],
        srcOutDedup: Seq[(Path, SkryncPath)]
      ) =
        src.info
          .flattenPaths(src.src)
          .filter(_._2.digest.nonEmpty)
          .partition(p => isIn(dedupPath, p._1))

      // All of the files in the dedupPath
      val dedupContents =
        if (isIn(src.src, dedupPath)) srcInDedup
        else
          SkryncDir
            .scan(dedupPath, digest = true)
            .flattenPaths(dedupPath)

      // And all of the candidate files sorted by digest
      val srcOutDedupDigests: Map[Digest, Seq[(Path, SkryncPath)]] =
        srcOutDedup.groupBy(_._2.digest.get)

      // All of the files that we're testing for duplication
      val dedupDigests: Map[Digest, Seq[(Path, SkryncPath)]] =
        dedupContents.groupBy(_._2.digest.get)

      // Partition by whether the file is unique or not
      val (uniques, duplicates) = dedupContents
        .partition {
          case (_, SkryncPath(_, _, _, _, _, Some(dig))) if srcOutDedupDigests.contains(dig) =>
            false
          case path @ (_, SkryncPath(_, _, _, _, _, Some(dig))) =>
            // The file doesn't exist in the source, but it might be duplicated inside the dedup path.
            // One should be considered a duplicate, and the other a unique file.
            dedupDigests.getOrElse(dig, Nil).headOption.contains(path)
          case _ => false
        }

      DedupPathReport(
        src,
        dedupPath,
        uniques = uniques,
        duplicates = duplicates
      )
    }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {
    val srcDigest: File = validateFile(
      // TODO
      arg = opts.get("--srcDigest")
    )

    val dedupDir: Directory = validateDirectory(opts.get("--dedupDir"), tag = "Deduplication directory").toAbsolute
    val mvDir: Option[Directory] =
      Option(opts.get("--mvDir")).map(validateDirectory(_, tag = "Duplicate destination directory"))

    // Read all of the information from the two digest files.
    val src: SkryncGo.Analysis = Json.read(srcDigest)
    val r = DedupPathReport(src, dedupDir)

    println("DEDUPLICATION REPORT")
    println("===========")
    println("from: " + srcDigest)
    println("src: " + src.src)
    println("dedup: " + dedupDir)
    println(s"uniques: ${r.uniques.size}")
    println(s"duplicates: ${r.duplicates.size}")
    println()
    r.duplicates.map(_._1).foreach { f =>
      val dst = mvDir.map(_.resolve(f.name)).getOrElse(f.changeExtension("dup." + f.extension))
      System.out.println(
        s"""mv "$f" "$dst" """
      )
    }
    println()
    r.uniques.map(_._1).foreach { f =>
      System.out.println(
        s"""mv "$f" "${f.changeExtension("uniq." + f.extension)}" """
      )
    }
  }
}
