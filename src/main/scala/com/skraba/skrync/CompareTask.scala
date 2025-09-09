package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.SkryncGo.validateFile
import com.tinfoiled.docopt4s.{Docopt, Task}

import scala.collection.immutable
import scala.reflect.io._

/** This task compares two digest files, a source and destination, and calculates the changes that need to be made to
  * the destination in order for it to be identical to the source.
  */
object CompareTask extends Task {

  val Cmd = "compare"

  val Description = "Compare the digest files from two directories."

  val Doc: String =
    """%s
      |
      |Usage:
      |  SkryncGo compare --srcDigest SRC --dstDigest DST
      |
      |Options:
      |  --srcDigest SRC  The file generated from the source directory.
      |  --dstDigest DST  The file generated from the destination directory.
      |
      |Examples:
      |
      | SkryncGo compare --srcDigest $$HOME/skrync/backup1 \
      |     --dstDigest $$HOME/skrync/backup2
      |
      |""".stripMargin
      .format(Description)
      .trim

  /** Files with the same digest that exist in multiple places
    * @param srcs
    *   All the paths in the source digest that contain files with this digest
    * @param dsts
    *   All the paths in the dst digest that contain files with this digest
    */
  case class DupFiles(srcs: Set[Path], dsts: Set[Path])

  case class Comparison(
      /** Files that exist only in the source directory without any corresponding destination. Should be copied over. */
      srcOnly: Set[Path],
      /** Files that exist only in the destination directory without any corresponding source. Should be removed. */
      dstOnly: Set[Path],
      /** Identical files that exist in the source AND destination but at different paths. Any file in the destination
        * set can be moved or copied to all the paths in the source set.
        */
      moved: Set[DupFiles],
      /** Files that exist in both the source and destination at the same path, but have been changed. The destination
        * needs to be overwritten from the source.
        */
      modified: Set[Path]
  )

  def compare(src: SkryncDir, dst: SkryncDir): Comparison = {

    // All the files found in the source directory, as a flat list of Paths relative to its root,
    // and grouped by digest.
    val srcMap: Map[Path, SkryncPath] = src.copyWithoutTimes().flattenPaths(Path(src.path.name)).toMap
    val srcKeys: Set[Path] = srcMap.keySet
    val srcDigest: Map[Digest, Set[Path]] = srcMap.groupMap(_._2.digest)(_._1).flatMap {
      case (Some(digest), paths) => Some(digest -> paths.toSet)
      case _                     => None
    }

    // All the files found in the dest directory, as a flat list of Paths relative to its root,
    // and grouped by digest.
    val dstMap: Map[Path, SkryncPath] = dst.copyWithoutTimes().flattenPaths(Path(dst.path.name)).toMap
    val dstKeys: Set[Path] = dstMap.keySet
    val dstDigest: Map[Digest, Set[Path]] = dstMap.groupMap(_._2.digest)(_._1).flatMap {
      case (Some(digest), paths) => Some(digest -> paths.toSet)
      case _                     => None
    }

    // All files that are moved exist in both the source and destination, but not at *exactly* the same
    // locations.
    val moved = srcDigest.keySet
      .intersect(dstDigest.keySet)
      .map(mv => DupFiles(srcDigest(mv), dstDigest(mv)))
      .filterNot(dup => dup.dsts == dup.srcs)

    val srcOnly = srcKeys.diff(dstKeys)
    val dstOnly = dstKeys.diff(srcKeys)
    val bothButModified = srcKeys.intersect(dstKeys).filter { p => srcMap(p) != dstMap(p) }

    Comparison(
      srcOnly.diff(moved.flatMap(_.srcs)),
      dstOnly.diff(moved.flatMap(_.dsts)),
      moved,
      bothButModified
    )
  }

  def go(opt: Docopt): Unit = {
    val srcDigest: File = validateFile(arg = opt.string.get("--srcDigest"))
    val dstDigest: File = validateFile(arg = opt.string.get("--dstDigest"), tag = "Destination")

    // Read all the information from the two digest files.
    val src: SkryncGo.Analysis = Json.read(srcDigest)
    val dst: SkryncGo.Analysis = Json.read(dstDigest)

    // Check the two digests for differences.
    val compares = compare(src.info, dst.info)

    println(s"""COMPARE
      |srcDigest: $srcDigest
      |         : ${src.src}
      |dstDigest: $dstDigest
      |         : ${dst.src}
      |""".stripMargin)

    if (compares.srcOnly.isEmpty && compares.dstOnly.isEmpty && compares.moved.isEmpty && compares.modified.isEmpty) {
      println("No differences have been detected.")
    } else {
      // TODO: implement
      println(s"""COMPARE
        |srcOnly: ${compares.srcOnly.size}
        |dstOnly: ${compares.dstOnly.size}
        |moved: ${compares.moved.size}
        |modified: ${compares.modified.size}
        |""".stripMargin)

      println("""FILES""")
      val all: Seq[(Path, String)] =
        (compares.srcOnly.map((_, "<< SRC"))
          ++ compares.dstOnly.map((_, ">> DST"))
          ++ compares.modified.map((_, "!="))).toSeq.sortBy(_._1.toString())
      all.foreach(str => println(str._2 + " " + str._1))
      println()

      println("""DUPS""")
      compares.moved.foreach { dup =>
        val (both, srcOnly) = dup.srcs.partition(dup.dsts)
        val dstOnly = dup.dsts.filterNot(dup.srcs)
        println(both.mkString(","))
        srcOnly.map("  << SRC DUP " + _).foreach(println)
        dstOnly.map("  >> DST DUP " + _).foreach(println)
      }
    }
  }
}
