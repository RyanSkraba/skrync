package com.skraba.skrync

import com.skraba.skrync.CompareTask.Comparison

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

  case class Report(duplicateFiles: Seq[SkryncPath])

  def report(src: SkryncDir): Report = {
    Report(Seq())
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
    val r = report(src.info)

    // TODO: implement
    println("REPORT")
    println("===========")
    println("srcDigest:" + srcDigestString)
    println("Report:" + r)
  }
}
