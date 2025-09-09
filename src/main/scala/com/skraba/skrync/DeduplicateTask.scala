package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.SkryncGo.{Line, validateDirectory, validateFile}
import com.skraba.skrync.SkryncPath.isIn
import com.tinfoiled.docopt4s.{Docopt, Task}

import java.nio.file.{Files, StandardCopyOption}
import scala.reflect.io._

/** This task reads a digest file, and makes a comparison with an external directory to find files that are known or
  * unknown in the original digest path.
  *
  * This can be useful for filing new files in the original directory from where the digest was made.
  */
object DeduplicateTask extends Task {

  // TODO: Add verbose scanning for the input dedup directory

  val Cmd = "dedup"

  val Description = "Finds files that are already known or unknown in the digest."

  val Doc: String =
    """%s
      |
      |All the files in the deduplication directory are categorized as known
      |or unknown.  Known files exist elsewhere, either in the source digest outside
      |of the deduplication directory OR have a duplicate inside the directory.  An
      |unknown file has information that can't be found elsewhere.
      |
      |(If a file is duplicated in the deduplication directory but doesn't exist in
      |the digest, only one will be unknown.)
      |
      |Usage:
      |  SkryncGo dedup --srcDigest SRC --dedupDir DEDUP [options]
      |
      |Options:
      |  --root SRC_ROOT       Root directory to use as the parent for all file
      |                        options. If not present, its value is taken from
      |                        the SKRYNC_SRC_ROOT environment variable or the
      |                        current working directory.
      |  --srcDigest SRC       The file generated from the source directory
      |  --dedupDir DEDUP_DIR  Provide a deduplication report on all the files
      |                        in DEDUP
      |  --knownExt KN_EXT     If present, renames known files by augmenting with this
      |                        extension (for example a.jpg would become a.known.jpg)
      |  --unknownExt UKN_EXT  If present, renames unknown files by augmenting with
      |                        this extension (for example a.jpg would become
      |                        a.unknown.jpg)
      |  --mvDir KN_DIR        If present, moves known files to MV_DIR
      |  --rmKnown             If present, deletes known files from the DEDUP_DIR
      |  --verbose             Print detailed information to stdout
      |  --timing              Print some timing information about the task
      |  --dryRun              If any actions are to be taken, describe them on
      |                        stdout instead of executing them
      |
      |Examples:
      |
      | # Provide a deduplication report on the files in the given directory
      | SkryncGo dedup --srcDigest $HOME/skrync/backup1 --dedupDir $HOME/dedup/
      |
      |""".stripMargin
      .format(Description)
      .trim

  /** Analysis of the contents of one specific directory to find known (exists in the source digest) and unknown files.
    *
    * @param src
    *   The prepared files analysis
    * @param dedupPath
    *   A new directory. If this is *inside* the analysis directory, then the file information is used from the files in
    *   the analysis. If this is *external* then that directory will be scanned.
    * @param known
    *   Files in the dedupPath that in the source analysis outside of the dedupPath, or are duplicated in the dedupPath.
    * @param unknown
    *   Files in the dedupPath that do not exist outside the dedupPath. If there is a file that exists twice but only
    *   inside the dedupPath, one will appear in the unknown and one will appear as known.
    */
  case class DedupPathReport(
      src: SkryncGo.Analysis,
      dedupPath: Directory,
      known: Seq[(Path, SkryncPath)],
      unknown: Seq[(Path, SkryncPath)]
  )

  object DedupPathReport {

    /** Given the analysis file and the dedup directory, determines which of the files in the directory already exist.
      *
      * @param src
      *   The source analysis of an existing directory.
      * @param dedupPath
      *   A new directory. If this is *inside* the analysis directory, then the file information is used from the files
      *   in the analysis. If this is *external* then that directory will be scanned.
      * @return
      *   A deduplication report of known and unknown files in the dedupPath.
      */
    def apply(src: SkryncGo.Analysis, dedupPath: Directory): DedupPathReport = {

      // All the files in the analysis, sorted into whether they are in the dedupPath
      val (srcInDedup: Seq[(Path, SkryncPath)], srcOutDedup: Seq[(Path, SkryncPath)]) =
        src.info.flattenPaths(src.src).filter(_._2.digest.nonEmpty).partition(p => isIn(dedupPath, p._1))

      // All the files in the dedupPath, taken from the analysis or the filesystem
      val dedupContents =
        if (isIn(src.src, dedupPath)) srcInDedup
        else SkryncDir.scan(dedupPath, digest = true).flattenPaths(dedupPath)

      // All the files in the analysis keyed on digest.  This is the set that determines whether the file in the dedupPath is known or unknown.
      val srcOutDedupDigests: Map[Digest, Seq[(Path, SkryncPath)]] = srcOutDedup.groupBy(_._2.digest.get)

      // All the files that we're testing
      val dedupDigests: Map[Digest, Seq[(Path, SkryncPath)]] = dedupContents.groupBy(_._2.digest.get)

      // Partition by whether the file is known or not
      val (unknown, known) = dedupContents.partition {
        case (_, SkryncPath(_, _, _, _, _, Some(dgst))) if srcOutDedupDigests.contains(dgst) =>
          false
        case path @ (_, SkryncPath(_, _, _, _, _, Some(dgst))) =>
          // The file doesn't exist in the source, but it might be duplicated inside the dedup path.
          // One should be considered a duplicate, and the other a unique file.
          dedupDigests.getOrElse(dgst, Nil).headOption.contains(path)
        case _ => false
      }

      DedupPathReport(src, dedupPath, known = known, unknown = unknown)
    }
  }

  def go(opt: Docopt): Unit = {

    // Setup the expected output.
    val verbose = opt.boolean.get("--verbose")
    val dryRun = opt.boolean.get("--dryRun")
    val timing = opt.boolean.get("--timing")

    // A root directory taken from the command line, or from the environment, or from the current working directory.
    val root = opt.string.getOption("--root")

    // The file resources used by this task
    val srcDigest: File = validateFile(arg = opt.string.get("--srcDigest"), root)
    val dedupDir: Directory =
      validateDirectory(opt.string.get("--dedupDir"), root, tag = "Deduplication directory").toAbsolute

    val knownExtension: Option[String] = opt.string.getOption("--knownExt")
    val unknownExtension: Option[String] = opt.string.getOption("--unknownExt")
    val rmKnown = opt.boolean.get("--rmKnown")
    val mvDir: Option[Directory] =
      opt.string.getOption("--mvDir").map(validateDirectory(_, root, tag = "Duplicate destination directory"))

    // Read all the information from the two digest files.
    val start = System.currentTimeMillis()
    val src: SkryncGo.Analysis = Json.read(srcDigest)
    val readTime = System.currentTimeMillis() - start
    val r = DedupPathReport(src, dedupDir)
    val dedupTime = System.currentTimeMillis() - start

    println("DEDUPLICATION REPORT")
    println(Line)
    println("from: " + srcDigest)
    println("src: " + src.src)
    println("dedup: " + dedupDir)
    println(s"new files: ${r.unknown.size}")
    println(s"known files: ${r.known.size}")
    if (timing) println(s"read analysis: ${readTime}ms")
    if (timing) println(s"dedup analysis: ${dedupTime}ms")
    if (verbose) println(s"verbose: true")
    if (dryRun) println(s"dryRun: true (No files will be changed)")
    println()

    // Determine whether there will be console output for either known or unknown files.
    val outputForKnown =
      r.known.nonEmpty && (verbose || dryRun && (mvDir.nonEmpty || knownExtension.nonEmpty || rmKnown))
    val outputForUnknown = r.unknown.nonEmpty && (verbose || dryRun && unknownExtension.nonEmpty)

    // If there's no output expected to be printed to the screen, then indicate how to get
    // output.
    if (!outputForKnown && !outputForUnknown) {
      println("Use --verbose to list the files.")
    }

    if (outputForKnown)
      println(s"""Known files (duplicates)
         |$Line
         |""".stripMargin)

    r.known.map(_._1).foreach { f =>
      val movedDst = mvDir.map(_.resolve(f.name)).getOrElse(f)
      val dst = knownExtension.map(ext => movedDst.changeExtension(ext + "." + f.extension)).getOrElse(movedDst)
      if (rmKnown) {
        if (verbose || dryRun) println(s"""rm "$f"""")
        if (!dryRun)
          Files.delete(f.jfile.toPath)
      } else if (f == dst) {
        if (verbose) println(s"""# $f""")
      } else {
        if (verbose || dryRun) println(s"""mv "$f" "$dst"""")
        if (!dryRun)
          Files.move(f.jfile.toPath, dst.jfile.toPath, StandardCopyOption.ATOMIC_MOVE)
      }
    }

    if (outputForKnown && outputForUnknown) println()

    if (outputForUnknown)
      println(s"""Unknown files (unique)
         |$Line
         |""".stripMargin)

    r.unknown.map(_._1).foreach { f =>
      val dst = unknownExtension.map(ext => f.changeExtension(ext + "." + f.extension)).getOrElse(f)
      if (f == dst) {
        if (verbose) println(s"""# $f""")
      } else {
        if (verbose || dryRun) println(s"""mv "$f" "$dst"""")
        if (!dryRun)
          Files.move(f.jfile.toPath, dst.jfile.toPath, StandardCopyOption.ATOMIC_MOVE)
      }
    }
  }
}
