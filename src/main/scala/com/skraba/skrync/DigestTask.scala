package com.skraba.skrync

import com.skraba.skrync.SkryncGo.Analysis

import java.io.IOException
import java.time.{LocalDateTime, ZoneOffset}
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
      |      [--no-digest]
      |
      |Options:
      |  --srcDir SRC_DIR  The directory to analyze.
      |  --dstDigest DST   The destination of the persistent file, or a directory to
      |                    auto generate a destination.  If not present, writes to
      |                    STDOUT.
      |  --silent          Do not print any output unless writing to STDOUT.
      |  --no-digest       Skip calculating the digest on the file contents.
      |
      |Examples:
      |
      | SkryncGo digest --srcDir %s --dstDigest $$HOME/skrync/
      |
      |""".stripMargin
      .format(
        Description,
        sys.env.getOrElse("BACKUP_DISK_MAIN", "/run/media/BACKUP/backup")
      )
      .trim

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  /** Creates a digest file from the input directory containing the file info and digests. */
  @throws[IOException]
  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    // The current date/time
    val now = LocalDateTime.now

    // The source directory to read.
    val srcDirString = opts.get("--srcDir").asInstanceOf[String]
    // The destination, if present.  If this is already a directory, a default file name will be generated, including the date.  If not present, dumps to stdout
    val dstString = Option(opts.get("--dstDigest").asInstanceOf[String])
    val silent = opts.get("--silent").asInstanceOf[Boolean]
    val digest = !opts.get("--no-digest").asInstanceOf[Boolean]

    val srcDir: Directory = Directory(srcDirString).toAbsolute
    if (!srcDir.exists)
      throw new IllegalArgumentException(
        s"Source doesn't exist: $srcDirString"
      )
    if (!srcDir.isDirectory)
      throw new IllegalArgumentException(
        s"Source is not a directory: $srcDirString"
      )

    // If no destination is specified, this will be None and standard out will be used.
    val dst: Option[File] = dstString
      .map(Path(_))
      .map(p => {
        // If the destination is a directory, auto-create the filename based on the time.
        if (p.exists && p.isDirectory)
          p / File(getDefaultDigestName(srcDirString, now))
        else p.toFile
      })

    // Whether to write the output to the console as well.
    val consoleOut =
      if (silent || dst.isEmpty) IgnoreProgress
      else new PrintDigestProgress(Console.out)

    // The destination for writing the actual digest.
    val out: DigestProgress = Json.writeProgress(
      src = srcDir,
      dst = dst,
      created = now.toInstant(ZoneOffset.UTC).toEpochMilli,
      gzipped = true,
      wrapped = consoleOut
    )

    // Run the scan, saving the information via the progress
    SkryncDir.scan(srcDir, digest, out)
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
