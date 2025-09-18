package com.skraba.skrync

import com.tinfoiled.docopt4s.{MultiTaskMain, Task}

import scala.reflect.io.{Directory, File, Path}
import scala.util.Properties

/** My synchronization tool. */
object SkryncGo extends MultiTaskMain {

  override lazy val Name: String = "SkryncGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(DigestTask, ReportTask, DeduplicateTask, CompareTask, ExecuteTask)
  override lazy val Doc: String = "My file synchronization and backup tool.\n\n" + SimpleDoc +
    "\n\nAnalyzes and synchronizes changes between two directories."

  val Line: String = "=" * 50

  /** An analysis contains all the information discovered while reading a directory.
    *
    * This can be persisted to disk and used to communicate between the tasks.
    *
    * @param src
    *   The root of the directory that was digested/analysed.
    * @param created
    *   The time that the the analysis was performed.
    * @param info
    *   All the information information discovered at that location.
    */
  case class Analysis(src: Directory, created: Long, info: SkryncDir)
}
