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
      |  --srcRoot SRC_ROOT    Root directory to use as the parent for all file
      |                        options. If not present, its value is taken from
      |                        the SKRYNC_SRC_ROOT environment variable or the
      |                        current working directory.
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

  /** Analysis of the contents of one specific directory to find known (exists in the source digest) and unknown files.
    *
    * @param src
    *   The prepared files analysis
    * @param dedupPath
    *   A new directory. If this is *inside* the analysis directory, then the file information is used from the files in
    *   the analysis. If this is *external* then that directory will be scanned.
    * @param known
    *   Files in the dedupPath that in the source analysis outside of the dedupPath, or are duplicated in the dedupPath.
    * @param unknown
    *   Files in the dedupPath that do not exist outside the dedupPath. If there is a file that exists twice but only
    *   inside the dedupPath, one will appear in the uniques and one will appear as known.
    */
  case class DedupPathReport(
      src: SkryncGo.Analysis,
      dedupPath: Directory,
      known: Seq[(Path, SkryncPath)],
      unknown: Seq[(Path, SkryncPath)]
  )

  object DedupPathReport {

    /** Given the analysis file and the dedup directory, determines which of the files in the directory already exist.
      *
      * @param src
      *   The source analysis of an existing directory.
      * @param dedupPath
      *   A new directory. If this is *inside* the analysis directory, then the file information is used from the files
      *   in the analysis. If this is *external* then that directory will be scanned.
      * @return
      *   A deduplication report of known and unknown files in the dedupPath.
      */
    def apply(src: SkryncGo.Analysis, dedupPath: Directory): DedupPathReport = {

      // All of the files in the analysis, sorted into whether they are in the dedupPath
      val (srcInDedup: Seq[(Path, SkryncPath)], srcOutDedup: Seq[(Path, SkryncPath)]) =
        src.info.flattenPaths(src.src).filter(_._2.digest.nonEmpty).partition(p => isIn(dedupPath, p._1))

      // All of the files in the dedupPath, taken from the analysis or the filesystem
      val dedupContents =
        if (isIn(src.src, dedupPath)) srcInDedup
        else SkryncDir.scan(dedupPath, digest = true).flattenPaths(dedupPath)

      // All of the files in the analysis keyed on digest.  This is the set that determines whether the file in the dedupPath is known or unknown.
      val srcOutDedupDigests: Map[Digest, Seq[(Path, SkryncPath)]] = srcOutDedup.groupBy(_._2.digest.get)

      // All of the files that we're testing
      val dedupDigests: Map[Digest, Seq[(Path, SkryncPath)]] = dedupContents.groupBy(_._2.digest.get)

      // Partition by whether the file is known or not
      val (unknown, known) = dedupContents.partition {
        case (_, SkryncPath(_, _, _, _, _, Some(dgst))) if srcOutDedupDigests.contains(dgst) =>
          false
        case path @ (_, SkryncPath(_, _, _, _, _, Some(dgst))) =>
          // The file doesn't exist in the source, but it might be duplicated inside the dedup path.
          // One should be considered a duplicate, and the other a unique file.
          dedupDigests.getOrElse(dgst, Nil).headOption.contains(path)
        case _ => false
      }

      DedupPathReport(src, dedupPath, known = known, unknown = unknown)
    }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    // A root directory taken from the command line, or from the environment, or from the current working directory.
    val root = Option(opts.get("--srcRoot").asInstanceOf[String])

    // The file resources used by this task
    val srcDigest: File = validateFile(arg = opts.get("--srcDigest"), root)
    val dedupDir: Directory =
      validateDirectory(opts.get("--dedupDir"), root, tag = "Deduplication directory").toAbsolute
    val mvDir: Option[Directory] =
      Option(opts.get("--mvDir")).map(validateDirectory(_, root, tag = "Duplicate destination directory"))

    // Read all of the information from the two digest files.
    val src: SkryncGo.Analysis = Json.read(srcDigest)
    val r = DedupPathReport(src, dedupDir)

    println("DEDUPLICATION REPORT")
    println("===========")
    println("from: " + srcDigest)
    println("src: " + src.src)
    println("dedup: " + dedupDir)
    println(s"new files: ${r.unknown.size}")
    println(s"known files: ${r.known.size}")
    println()
    r.known.map(_._1).foreach { f =>
      val dst = mvDir.map(_.resolve(f.name)).getOrElse(f.changeExtension("known." + f.extension))
      System.out.println(
        s"""mv "$f" "$dst" """
      )
    }
    println()
    r.unknown.map(_._1).foreach { f =>
      System.out.println(
        s"""mv "$f" "${f.changeExtension("unknown." + f.extension)}" """
      )
    }
  }
}
