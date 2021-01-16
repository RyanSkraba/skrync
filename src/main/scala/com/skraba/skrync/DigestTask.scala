package com.skraba.skrync

import java.io.IOException
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.reflect.io._

/** This task creates a file with digest information for all of the files in a given directory.
  */
object DigestTask {

  val Cmd = "digest"

  val Description =
    "Scan a source directory to save all file information."

  val Doc: String =
    """%s
      |
      |Usage:
      |  SkryncGo digest --srcDir=<SRC_DIR> [--dstDigest=<DST>] [--silent]
      |
      |Options:
      |  --srcDir SRC_DIR    The directory to analyze.
      |  --dstDigest DST     The destination of the persistent file, or a directory to
      |                      auto generate a destination.  If not present, writes to
      |                      STDOUT.
      |  --silent            Do not print any output unless writing to STDOUT.
      |
      |Examples:
      |
      | SkryncGo digest --srcDir /run/media/%s/backup --dstDigest $$HOME/skrync/
      |
      |""".stripMargin
      .format(Description, sys.env.getOrElse("BACKUP_DISK_MAIN", "MYDISK"))
      .trim

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  /** Creates a digest file from the input directory containing the file info and digests.
    *
    * @param srcDirString The source directory to read.
    * @param dstString    The destination, if present.  If this is already a directory, a default
    *                     file name will be generated, including the date.  If not present, dumps
    *                     to stdout.
    */
  @throws[IOException]
  def go(opts: java.util.Map[String, AnyRef]): Unit = {
    val srcDirString = opts.get("--srcDir").asInstanceOf[String]
    val dstString = Option(opts.get("--dstDigest").asInstanceOf[String])

    val srcDir: Directory = Directory(srcDirString).toAbsolute
    if (!srcDir.exists)
      throw new IllegalArgumentException(
        s"Source doesn't exist: $srcDirString"
      )
    if (!srcDir.isDirectory)
      throw new IllegalArgumentException(
        s"Source is not a directory: $srcDirString"
      )

    // TODO: implement
    println("DIGEST")
    println("===========")
    println("srcDir:" + srcDirString)
    println("dstDigest:" + dstString)
  }
}
