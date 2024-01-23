package com.skraba.skrync

import com.skraba.skrync.SkryncGo.{validateDirectory, validateFileSystem}

import java.io.IOException
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}
import scala.reflect.io._

/** This task creates a file with digest information for all of the files in a given directory. */
object DigestTask {

  val Cmd = "digest"

  val Description =
    "Scan a source directory to save all file information."

  val Doc: String =
    """%s
      |
      |Usage:
      |  skrync digest --srcDir=<SRC_DIR> [options]
      |
      |Options:
      |  --srcRoot SRC_ROOT  Root directory to use as the parent for all file options.
      |                      If not present, it's value is taken from the
      |                      SKRYNC_SRC_ROOT environment variable or the current
      |                      working directory.
      |  --srcDir SRC_DIR    The directory to analyze. Relative to SRC_ROOT.
      |  --dstDigest DST     The destination of the persistent file, or a directory to
      |                      auto generate a destination.  Relative to SRC_ROOT. If
      |                      not present, writes to STDOUT.
      |  --silent            Do not print any output unless writing to STDOUT.
      |  --no-digest         Skip calculating the digest on the file contents.
      |
      |Examples:
      |
      | skrync digest --srcDir /run/media/BACKUP/backup --dstDigest $$HOME/skrync/
      |
      |""".stripMargin.trim

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  /** Creates a digest file from the input directory containing the file info and digests. */
  @throws[IOException]
  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    // The current date/time
    val now = LocalDateTime.now

    // A root directory taken from the command line, or from the environment, or from the current working directory.
    val srcRootOption = Option(opts.get("--srcRoot").asInstanceOf[String])

    val srcDirString = opts.get("--srcDir").asInstanceOf[String]
    // The destination, if present.  If this is already a directory, a default file name will be generated,
    // including the date.  If not present, dumps to stdout
    val dstString = Option(opts.get("--dstDigest").asInstanceOf[String])
    val silent = opts.get("--silent").asInstanceOf[Boolean]
    val digest = !opts.get("--no-digest").asInstanceOf[Boolean]

    val srcDir = validateDirectory(srcRootOption, srcDirString)

    // If no destination is specified, this will be None and standard out will be used.
    val dst: Option[File] = dstString
      .map(validateFileSystem(srcRootOption, _, mustExist = false))
      .map(p => {
        // If the destination is a directory, auto-create the filename based on the time.
        if (p.exists && p.isDirectory)
          p / File(getDefaultDigestName(srcDir.toString, now))
        else p.toFile
      })
      .map(validateFileSystem(None, _, tag = "Destination digest", mustExist = false, mustNotExist = true).toFile)

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
    * @param srcDirString
    *   The source directory being analysed (included in the file name)
    * @param time
    *   The time to include in the file name.
    * @return
    *   The filename for the digest file.
    */
  def getDefaultDigestName(
      srcDirString: String,
      time: LocalDateTime
  ): String = {
    var defaultName = srcDirString.replaceAll("""[/\\]""", " ").trim
    defaultName += "__" + time.format(
      DateTimeFormatter.ofPattern("yyyyMMddHHmmss")
    )
    defaultName = defaultName.replace(" ", "_")
    defaultName
  }
}
