package com.skraba.skrync

import java.io.IOException
import scala.reflect.io._

/** This task copies or moves files according to a synchronisation plan.
  */
object ExecuteTask {

  val Cmd = "execute"

  val Description =
    "TODO"

  val Doc: String =
    """%s
      |
      |Usage:
      |  SkryncGo execute --srcDigest SRC --dstDigest DST [--backup BACKUP_DIR]
      |  SkryncGo execute --plan PLAN_JSON [--backup BACKUP_DIR]
      |
      |Options:
      |  --srcDigest SRC       A digest file containing the information from a source
      |                        directory.
      |  --dstDigest DST       A digest file containing the information from a
      |                        destination directory.
      |  --backup BACKUP_DIR   A backup directory to store files that were deleted
      |                        from the destination.
      |  --plan PLAN_JSON      A plan describing the changes to make between two
      |                        directories.
      |      |
      |Examples:
      |
      |TODO
      |
      |""".stripMargin
      .format(Description)
      .trim

  val Task: SkryncGo.Task = SkryncGo.Task(Doc, Cmd, Description, go)

  @throws[IOException]
  def go(opts: java.util.Map[String, AnyRef]): Unit = {
    val backup = Option(opts.get("--backup").asInstanceOf[String])
    Option(opts.get("--plan"))
      .map(plan => goExecute(plan.asInstanceOf[String], backup))
      .getOrElse(
        goExecute(
          opts.get("--srcDigest").asInstanceOf[String],
          opts.get("--dstDigest").asInstanceOf[String],
          backup
        )
      )
  }

  def goExecute(
      srcDigestString: String,
      dstDigestString: String,
      backupDirString: Option[String]
  ): Unit = {
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

    System.out.println("EXECUTE")
    System.out.println("===========")
    System.out.println("srcDigest:" + srcDigestString)
    System.out.println("dstDigest:" + dstDigestString)
    System.out.println("backupDir:" + backupDirString)
  }

  def goExecute(
      planJsonString: String,
      backupDirString: Option[String]
  ): Unit = {
    // TODO: implement
    System.out.println("EXECUTE")
    System.out.println("===========")
    System.out.println("planJson:" + planJsonString)
    System.out.println("backupDir:" + backupDirString)
  }
}
