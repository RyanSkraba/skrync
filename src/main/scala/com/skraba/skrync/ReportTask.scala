package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

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

  case class DuplicateReport(
      src: SkryncGo.Analysis,
      dedupPath: Directory,
      uniques: Seq[(Path, SkryncPath)],
      duplicates: Seq[(Path, SkryncPath)]
  )

  object DuplicateReport {
    def apply(src: SkryncGo.Analysis, dedupPath: Directory): DuplicateReport = {

      // All of the files in the analysis
      val contents: Seq[(Path, SkryncPath)] =
        src.info
          .flattenPaths(src.src)
          .filter(_._2.digest.nonEmpty)

      // And all of the files sorted by digest
      val digests: Map[Digest, Seq[(Path, SkryncPath)]] =
        contents.groupBy(_._2.digest.get)

      val dedupPathStr = dedupPath.toString()

      val (uniques, duplicates) = contents
        .filter(_._1.toString.startsWith(dedupPathStr))
        .partition {
          case (_, SkryncPath(_, _, _, _, _, Some(dig))) =>
            digests.getOrElse(dig, Nil).size < 2
          case _ => false
        }

      DuplicateReport(
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
