package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.SkryncPath.isIn

import scala.reflect.io._

/** This task reads a digest file and provides some basic information.
  */
object ReportTask {

  val Cmd = "report"

  val Description = "Reads a digest file and provides some basic info."

  val Doc: String =
    """%s
      |
      |Usage:
      |  SkryncGo report --srcDigest SRC
      |
      |Options:
      |  --srcDigest SRC  The file generated from the source directory.
      |
      |Examples:
      |
      | SkryncGo report --srcDigest $$HOME/skrync/backup1
      |
      |""".stripMargin
      .format(Description)
      .trim

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  case class Report(duplicateFiles: Seq[Seq[(SkryncPath, Path)]])

  /** Analysis of the contents of one specific directory to find duplicate and
    *  unique files.
    *
    * Note that if there is a file that exists twice but only inside the
    * dedupPath, one will appear in the uniques and one will appear in the
    * duplicates.
    *
    * @param src        The prepared files analysis
    * @param dedupPath  The path to deduplicate, relative to the root of the analysis.
    * @param uniques    Files in the dedupPath that do not exist outside the dedupPath.
    * @param duplicates Files in the dedupPath that exist more than once.
    */
  case class DedupPathReport(
      src: SkryncGo.Analysis,
      dedupPath: Directory,
      uniques: Seq[(Path, SkryncPath)],
      duplicates: Seq[(Path, SkryncPath)]
  )

  object DedupPathReport {

    /** Given the analysis file and the dedup directory, determines which of the files in the directory already
      * exist.
      *
      * @param src The source analysis of an existing directory.
      * @param dedupPath  A new directory.  If this is inside the src directory, then the information is directly used
      *                   from there.
      * @return  A deduplication report of different and unique files in the dedupPath.
      */
    def apply(src: SkryncGo.Analysis, dedupPath: Directory): DedupPathReport = {

      // All of the files in the analysis
      val srcContents: Seq[(Path, SkryncPath)] =
        src.info
          .flattenPaths(src.src)
          .filter(_._2.digest.nonEmpty)

      val dedupContents =
        if (isIn(src.src, dedupPath)) Seq.empty
        else
          SkryncDir
            .scan(dedupPath, digest = true)
            .flattenPaths(dedupPath)

      val contents = srcContents ++ dedupContents

      // And all of the files sorted by digest
      val digests: Map[Digest, Seq[(Path, SkryncPath)]] =
        contents.groupBy(_._2.digest.get)

      val (uniques, duplicates) = contents
        .filter(p => isIn(dedupPath, p._1))
        .partition {
          case (_, SkryncPath(_, _, _, _, _, Some(dig))) =>
            digests.getOrElse(dig, Nil).size < 2
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

  def report(src: SkryncGo.Analysis): Report = {
    val digests: Map[Digest, Seq[(Path, SkryncPath)]] =
      src.info
        .flattenPaths(src.src)
        .filter(_._2.digest.nonEmpty)
        .groupBy(_._2.digest.get)

    val duplicates = digests.toSeq
      .filter(_._2.size > 1)
      .sortBy(_._2.head._2.size)
      .map(_._2.map(_.swap))

    Report(duplicates)
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {
    val srcDigestString = opts.get("--srcDigest").asInstanceOf[String]

    val srcDigest: File = File(srcDigestString).toAbsolute
    if (!srcDigest.exists)
      throw new IllegalArgumentException(
        s"Source doesn't exist: $srcDigestString"
      )
    if (!srcDigest.isFile)
      throw new IllegalArgumentException(
        s"Source is not a file: $srcDigestString"
      )

    // Read all of the information from the two digest files.
    val src: SkryncGo.Analysis = Json.read(srcDigest)

    // Check the two digests for differences.
    val r = report(src)

    // TODO: implement
    println("REPORT")
    println("===========")
    println("from: " + srcDigestString)
    println(s"total files: ${src.info.deepFileCount}")
    println()
    println("Duplicates")
    println("----------")
    for (fs <- r.duplicateFiles.takeRight(10)) {
      println(s"${fs.head._1.size} (${fs.size}) -->")
      for (fs2 <- fs) {
        println(s"   ${fs2._2}")
      }
    }

  }
}
