package com.skraba.skrync

import com.skraba.skrync.Digests.Digest
import com.skraba.skrync.SkryncGo.validateFile

import scala.reflect.io._

/** This task compares two digest files, a source and destination, and calculates the changes that need to be made to
  * the destination in order for it to be identical to the source.
  */
object CompareTask {

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

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  case class Comparison(
      /** Files that exist only in the source directory without any corresponding destination. Should be copied over. */
      srcOnly: Set[Path],
      /** Files that exist only in the destination directory without any corresponding source. Should be removed. */
      dstOnly: Set[Path],
      /** Identical files that exist in the source AND destination but at different paths. Any file in the destination
        * set can be moved or copied to all of the paths in the source set.
        */
      moved: Set[(Set[Path], Set[Path])],
      /** Files that exist in both the source and destination at the same path, but have been changed. The destination
        * needs to be overwritten fron the source.
        */
      modified: Set[Path]
  )

  def compare(src: SkryncDir, dst: SkryncDir): Comparison = {
    val srcMap: Map[Path, SkryncPath] = src.copyWithoutTimes().flattenPaths(Path(src.path.name)).toMap
    val dstMap: Map[Path, SkryncPath] = dst.copyWithoutTimes().flattenPaths(Path(dst.path.name)).toMap

    val srcKeys: Set[Path] = srcMap.keySet
    val dstKeys: Set[Path] = dstMap.keySet

    val srcOnly = srcKeys.diff(dstKeys)
    val dstOnly = dstKeys.diff(srcKeys)
    val bothButModified = srcKeys.intersect(dstKeys).filter { p => srcMap(p) != dstMap(p) }

    val srcDigest: Map[Digest, Set[Path]] =
      (srcOnly ++ bothButModified).groupMap(srcMap(_).digest)(identity).filter(_._1.nonEmpty).map(f => f._1.get -> f._2)
    val dstDigest: Map[Digest, Set[Path]] =
      (dstOnly ++ bothButModified).groupMap(dstMap(_).digest.getOrElse(Seq.empty))(identity).filter(_._1.nonEmpty)
    val moved = srcDigest.keySet.intersect(dstDigest.keySet).map(mv => (srcDigest(mv), dstDigest(mv)))

    Comparison(
      srcOnly.diff(moved.flatMap(_._1)),
      dstOnly.diff(moved.flatMap(_._2)),
      moved,
      bothButModified
    )
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {
    val srcDigest: File = validateFile(arg = opts.get("--srcDigest"))
    val dstDigest: File = validateFile(arg = opts.get("--dstDigest"), tag = "Destination")

    // Read all of the information from the two digest files.
    val src: SkryncGo.Analysis = Json.read(srcDigest)
    val dst: SkryncGo.Analysis = Json.read(dstDigest)

    // Check the two digests for differences.
    val compares = compare(src.info, dst.info)

    println(s"""COMPARE
      |srcDigest: $srcDigest
      |         : ${src.src}
      |dstDigest:" $dstDigest
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

      val all: Seq[(Path, String)] =
        (compares.srcOnly.map((_, "<< SRC"))
          ++ compares.dstOnly.map((_, ">> DST"))
          ++ compares.modified.map((_, "!="))).toSeq.sortBy(_._1.toString())

      all.foreach(str => println(str._2 + " " + str._1))

      compares.moved.foreach(println)
    }
  }
}
