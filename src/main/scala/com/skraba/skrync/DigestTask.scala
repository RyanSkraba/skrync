package com.skraba.skrync

import com.tinfoiled.docopt4s.{Docopt, DocoptException, PathValidator, Task}

import java.io.IOException
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}
import scala.reflect.io._

/** This task creates a file with digest information for all the files in a given directory. */
object DigestTask extends Task {

  val Cmd = "digest"

  val Description =
    "Scan a source directory to save all file information."

  val Doc: String =
    """%s
      |
      |Usage:
      |  skrync digest --srcDir <SRC_DIR> [options]
      |
      |Options:
      |  --root SRC_ROOT     Root directory to use as the parent for all file
      |                      options. If not present, its value is taken from
      |                      the SKRYNC_SRC_ROOT environment variable or the
      |                      current working directory.
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

  /** Creates a digest file from the input directory containing the file info and digests. */
  @throws[IOException]
  def go(opt: Docopt): Unit = {

    // The current date/time
    val now = LocalDateTime.now

    val silent = opt.flag("--silent")
    val digest = !opt.flag("--no-digest")

    // A root directory taken from the command line, or from the environment, or from the current working directory.
    val root = opt.string.getOption("--root")

    // The file resources used by this task
    val srcDir = opt.dir.get("--srcDir", PathValidator(root).withTag("Source"))

    // The destination can either not be present, or be a file that doesn't exist, or be an existing directory where
    // a default file can be created.

    val dstVld = PathValidator(root).doesntExist().withTag("Destination digest")
    val dst: Option[File] = opt.path.getOption("--dstDigest", dstVld.optionallyExists()) match {
      case Some(p) if p.isFile => opt.file.getOption("--dstDigest", dstVld)
      case Some(p) if p.exists =>
        opt.dir
          .getOption("--dstDigest", dstVld.withTag("Destination digest directory").exists())
          .map(_ / File(getDefaultDigestName(srcDir.toString, now)))
      case Some(_) => opt.file.getOption("--dstDigest", dstVld)
      case _       => None
    }

    // TODO: This should really be part of docopts4s
    dst.foreach { path =>
      val existingParent = LazyList.iterate(path.parent)(_.parent).dropWhile(!_.exists).headOption
      if (existingParent.exists(_.jfile.isFile))
        throw new DocoptException(s"Destination digest is uncreatable, ${existingParent.get} exists: $path")
      if (!path.parent.exists)
        throw new DocoptException(s"Destination digest parent directory doesn't exist: $path")
    }

    val dstInProgress = dst.map(f => f.parent / (f.name + ".running")).map(_.toFile)

    // Whether to write the output to the console as well.
    val consoleOut =
      if (silent || dst.isEmpty) IgnoreProgress
      else new PrintDigestProgress(Console.out)

    // The destination for writing the actual digest.
    val out: DigestProgress = Json.writeProgress(
      src = srcDir,
      dst = dstInProgress,
      created = now.toInstant(ZoneOffset.UTC).toEpochMilli,
      gzipped = true,
      wrapped = consoleOut
    )

    // Run the scan, saving the information via the progress
    SkryncDir.scan(srcDir, digest, out)

    // If the file was created then rename it to the final destination
    if (!dstInProgress.forall(_.jfile.renameTo(dst.get.jfile)))
      throw new IllegalArgumentException(s"Unable to rename ${dstInProgress.get}")
  }

  /** Create a default filename for a digest file.
    * @param srcDirString
    *   The source directory being analysed (included in the file name)
    * @param time
    *   The time to include in the file name.
    * @return
    *   The filename for the digest file.
    */
  def getDefaultDigestName(srcDirString: String, time: LocalDateTime): String = {
    var defaultName = srcDirString.replaceAll("""[/\\]""", " ").trim
    defaultName += "__" + time.format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"))
    defaultName = defaultName.replace(" ", "_")
    defaultName
  }
}
