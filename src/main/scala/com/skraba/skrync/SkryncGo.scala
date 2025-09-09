package com.skraba.skrync

import com.tinfoiled.docopt4s.{MultiTaskMain, Task}

import scala.reflect.io.{Directory, File, Path}
import scala.util.Properties

/** My synchronization tool. */
object SkryncGo extends MultiTaskMain {

  override lazy val Name: String = "SkryncGo"

  override lazy val Version: String = "0.0.1-SNAPSHOT"

  override lazy val Tasks: Seq[Task] =
    Seq(DigestTask, ReportTask, DeduplicateTask, CompareTask, ExecuteTask)

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

  /** Helper to validate command line arguments against an expected filesystem state.
    *
    * @param arg
    *   An absolute or relative (to root) directory to use in constructing the path
    * @param root
    *   An absolute directory to use in constructing the path
    * @param tag
    *   A human-readable description for the expected argument
    * @param isDir
    *   Whether to test to ensure the argument must be a Directory or must be a File (or None if it doesn't matter).
    * @param exists
    *   Whether to test to ensure the argument must exist or must not exist (or None if it doesn't matter).
    * @return
    *   The validated path that the argument represents on the filesystem.
    */
  def validateFileSystem(
      arg: String,
      root: Option[AnyRef] = None,
      tag: String = "Source",
      isDir: Option[Boolean] = None,
      exists: Option[Boolean] = Some(true)
  ): Path = {
    val path: Path = Path(
      root
        .map(_.toString)
        .orElse(sys.env.get("SKRYNC_ROOT_DIR"))
        .orElse(Option(Properties.userDir))
        .getOrElse("/")
    ).resolve(Path(arg)).toAbsolute
    if (exists.contains(true) && !path.exists)
      throw new IllegalArgumentException(s"$tag doesn't exist: $path")
    if (exists.contains(false) && path.exists)
      throw new IllegalArgumentException(s"$tag already exists: $path")
    if (isDir.contains(true) && exists.contains(true) && !path.isDirectory)
      throw new IllegalArgumentException(s"$tag is not a directory: $path")
    if (isDir.contains(false) && exists.contains(true) && !path.isFile)
      throw new IllegalArgumentException(s"$tag is not a file: $path")
    path
  }

  /** Helper to validate command line arguments against an expected filesystem directory.
    *
    * @param arg
    *   An absolute or relative (to root) directory to use in constructing the path
    * @param root
    *   An absolute directory to use in constructing the path
    * @param tag
    *   A human readable description for the expected argument
    * @param exists
    *   Whether to test to ensure the argument must exist or must not exist (or None if it doesn't matter).
    * @return
    *   The validated directory that the argument represents on the filesystem.
    */
  def validateDirectory(
      arg: String,
      root: Option[AnyRef] = None,
      tag: String = "Source",
      exists: Option[Boolean] = Some(true)
  ): Directory = validateFileSystem(arg, root, tag, isDir = Some(true), exists = exists).toDirectory

  /** Helper to validate command line arguments against an expected filesystem directory.
    *
    * @param arg
    *   An absolute or relative (to root) directory to use in constructing the path
    * @param root
    *   An absolute directory to use in constructing the path
    * @param tag
    *   A human readable description for the expected argument
    * @param exists
    *   Whether to test to ensure the argument must exist or must not exist (or None if it doesn't matter).
    * @return
    *   The validated directory that the argument represents on the filesystem.
    */
  def validateFile(
      arg: String,
      root: Option[AnyRef] = None,
      tag: String = "Source",
      exists: Option[Boolean] = Some(true)
  ): File = validateFileSystem(arg, root, tag, isDir = Some(false), exists = exists).toFile
}
