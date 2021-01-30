package com.skraba.skrync

import com.skraba.skrync.SkryncGo.Analysis

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

  /** Creates a digest file from the input directory containing the file info and digests. */
  @throws[IOException]
  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    // The source directory to read.
    val srcDirString = opts.get("--srcDir").asInstanceOf[String]
    // The destination, if present.  If this is already a directory, a default file name will be generated, including the date.  If not present, dumps to stdout
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

    // If no destination is specified, this will be null and standard out will be used.
    val dst: Option[File] = dstString
      .map(Path(_))
      .map(p => {
        // If the destination is a directory, auto-create the filename based on the time.
        if (p.exists && p.isDirectory)
          p / File(getDefaultDigestName(srcDirString, LocalDateTime.now))
        else p.toFile
      })

    // Get all of the file information from the source directory, and add the SHA1 digests.
    val w = new PrintDigestProgress(Console.out)
    val source = SkryncDir.scan(srcDir, w).digest(srcDir, w)

    // And write the analysis to disk in gzipped JSON format.
    Json.write(
      Analysis(
        src = srcDir,
        created = System.currentTimeMillis(),
        info = source
      ),
      dst,
      gzipped = true
    )
  }

  /** Create a default filename for a digest file.
    * @param srcDirString The source directory being analysed (included in the file name)
    * @param time The time to include in the file name.
    * @return The filename for the digest file.
    */
  def getDefaultDigestName(
      srcDirString: String,
      time: LocalDateTime
  ): String = {
    var defaultName = srcDirString.replaceAll("""[/\\]""", " ").trim
    defaultName += "_" + time.format(
      DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
    )
    defaultName = defaultName.replace(" ", "_")
    defaultName
  }
}
