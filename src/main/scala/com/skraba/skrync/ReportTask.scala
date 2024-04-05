package com.skraba.skrync

import com.skraba.docoptcli.DocoptCliGo
import com.skraba.docoptcli.DocoptCliGo.Task
import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.SkryncGo.{Line, validateFile}

import scala.reflect.io._

/** This task reads a digest file and provides some basic information. */
object ReportTask extends DocoptCliGo.Task {

  val Cmd = "report"

  val Description = "Reads a digest file and provides some basic info."

  val Doc: String =
    """%s
      |
      |Usage:
      |  SkryncGo report --srcDigest SRC
      |
      |Options:
      |  --srcDigest SRC       The file generated from the source directory.
      |
      |Examples:
      |
      | # Provide a basic report on the files in the digest
      | SkryncGo report --srcDigest $HOME/skrync/backup1
      |""".stripMargin
      .format(Description)
      .trim

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
    println(Line)
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
