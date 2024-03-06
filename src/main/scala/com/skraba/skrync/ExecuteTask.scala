package com.skraba.skrync

import com.skraba.skrync.SkryncGo.validateFile

import java.io.IOException
import scala.reflect.io._

/** This task copies or moves files according to a synchronisation plan. */
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
          validateFile(
            // TODO
            arg = opts.get("--srcDigest")
          ),
          validateFile(
            // TODO
            arg = opts.get("--dstDigest"),
            tag = "Destination"
          ),
          backup
        )
      )
  }

  def goExecute(
      srcDigest: File,
      dstDigest: File,
      backupDirString: Option[String]
  ): Unit = {
    println("EXECUTE")
    println("===========")
    println("srcDigest:" + srcDigest)
    println("dstDigest:" + dstDigest)
    println("backupDir:" + backupDirString)
  }

  def goExecute(
      planJsonString: String,
      backupDirString: Option[String]
  ): Unit = {
    // TODO: implement
    println("EXECUTE")
    println("===========")
    println("planJson:" + planJsonString)
    println("backupDir:" + backupDirString)
  }
}
