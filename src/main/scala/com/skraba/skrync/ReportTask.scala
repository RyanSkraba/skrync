package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.SkryncGo.validateFile

import scala.reflect.io._

/** This task reads a digest file and provides some basic information. */
object ReportTask {

  val Cmd = "report"

  val Description = "Reads a digest file and provides some basic info."

  val Doc: String =
    """%s
      |
      |Usage:
      |  SkryncGo report --srcDigest SRC
      |  SkryncGo report --srcDigest SRC --dedupDir DEDUP_DIR [--mvDir MV_DIR]
      |
      |Options:
      |  --srcDigest SRC       The file generated from the source directory.
      |  --dedupDir DEDUP_DIR  Provide a deduplication report on all the files
      |                        in DEDUP_DIR
      |  --mvDir MV_DIR        If present, moves duplicates to MV_DIR
      |
      |Examples:
      |
      | # Provide a basic report on the files in the digest
      | SkryncGo report --srcDigest $HOME/skrync/backup1
      |
      | # Provide a deduplication report on the files in the given directory
      | SkryncGo report --srcDigest $HOME/skrync/backup1 --dedupDir $HOME/dedup/
      |
      |""".stripMargin
      .format(Description)
      .trim

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  case class Report(duplicateFiles: Seq[Seq[(SkryncPath, Path)]])

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
    val dedup = opts.get("--dedupDir").asInstanceOf[String]

    val srcDigest: File = validateFile(
      // TODO
      arg = opts.get("--srcDigest")
    )

    // Check the two digests for differences.
    // Read all of the information from the two digest files.
    val src: SkryncGo.Analysis = Json.read(srcDigest)
    val r = report(src)

    // TODO: implement
    println("REPORT")
    println("===========")
    println("from: " + srcDigest)
    println("src: " + src.src)
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
