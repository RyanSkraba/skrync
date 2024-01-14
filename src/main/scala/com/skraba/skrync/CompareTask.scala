package com.skraba.skrync

import scala.reflect.io._

/** This task compares two digest files.
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
      srcOnly: Set[Path],
      dstOnly: Set[Path],
      modified: Set[Path]
  )

  def compare(src: SkryncDir, dst: SkryncDir): Comparison = {
    val srcMap = src.copyWithoutTimes().flattenPaths(Path(src.path.name)).toMap
    val dstMap = dst.copyWithoutTimes().flattenPaths(Path(dst.path.name)).toMap

    val srcKeys: Set[Path] = srcMap.keySet
    val dstKeys: Set[Path] = dstMap.keySet

    Comparison(
      srcKeys.diff(dstKeys),
      dstKeys.diff(srcKeys),
      srcKeys.intersect(dstKeys).filter { p =>
        srcMap(p) != dstMap(p)
      }
    )
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {
    val srcDigestString = opts.get("--srcDigest").asInstanceOf[String]
    val dstDigestString = opts.get("--dstDigest").asInstanceOf[String]

    val srcDigest: File = File(srcDigestString).toAbsolute
    val dstDigest: File = File(dstDigestString).toAbsolute
    if (!srcDigest.exists)
      throw new IllegalArgumentException(
        s"Source doesn't exist: $srcDigestString"
      )
    if (!dstDigest.exists)
      throw new IllegalArgumentException(
        s"Destination doesn't exist: $dstDigestString"
      )
    if (!srcDigest.isFile)
      throw new IllegalArgumentException(
        s"Source is not a file: $srcDigestString"
      )
    if (!dstDigest.isFile)
      throw new IllegalArgumentException(
        s"Destination is not a file: $dstDigestString"
      )

    // Read all of the information from the two digest files.
    val src: SkryncGo.Analysis = Json.read(srcDigest)
    val dst: SkryncGo.Analysis = Json.read(dstDigest)

    // Check the two digests for differences.
    val compares = compare(src.info, dst.info)

    // TODO: implement
    println(s"""COMPARE
      |srcDigest: $srcDigestString
      |dstDigest:" $dstDigestString
      |srcOnly: ${compares.srcOnly.size}
      |dstOnly: ${compares.dstOnly.size}
      |modified: ${compares.modified.size}
      |identical: ${compares.srcOnly.isEmpty && compares.dstOnly.isEmpty && compares.modified.isEmpty}
      |
      """.stripMargin)

    val all: Seq[(Path, String)] =
      (compares.srcOnly.map((_, "<<")) ++ compares.dstOnly.map(
        (_, ">>")
      ) ++ compares.modified.map((_, "!="))).toSeq.sortBy(_._1.toString())

    all.foreach(str => println(str._2 + " " + str._1))
  }
}
