package com.skraba.skrync

import java.io.{FileOutputStream, IOException}
import java.nio.file.attribute.{BasicFileAttributeView, FileTime}
import java.nio.file.{Files, Path}
import java.util.Random
import java.util.stream.IntStream

/** Utilities for generating random files in a directory for testing.
  *
  * The files are generated using the PRNG, so the results are deterministic, and POSIX creation,
  * access, and modification times can be set.
  */
object RandomFiles {

  /** Lower case letters. */
  private val AlphabetLower = "abcdefghijklmnopqrstuvwxyz"

  /** Upper case letters. */
  private val AlphabetUpper = AlphabetLower.toUpperCase

  /** Digits. */
  private val Numeric = "0123456789"

  /** Characters for generating random strings. */
  private val Alphanumeric =
    (AlphabetLower + AlphabetUpper + Numeric).toCharArray

  /** Generate a random integer between the two values (inclusive).
    *
    * @param rnd PRNG for repeatability.
    * @param min The minimum value.
    * @param max The maximum value.
    * @return A random alphanumeric string.
    */
  def nextInt(rnd: Random, min: Int, max: Int): Int =
    min + rnd.nextInt(max - min + 1)

  /** Generate a random string.
    *
    * @param rnd    PRNG for repeatability.
    * @param length The exact length of the string to generate.
    * @return A random alphanumeric string.
    */
  def nextString(rnd: Random, length: Int): String = {
    val sb = new StringBuilder
    IntStream
      .range(0, length)
      .forEach((i: Int) =>
        sb.append(Alphanumeric(rnd.nextInt(Alphanumeric.length)))
      )
    sb.toString
  }

  /** Generate a random string.
    *
    * @param rnd       PRNG for repeatability.
    * @param minLength Minimum length of the string.
    * @param maxLength Maximum length of the string.
    * @return A random alphanumeric string.
    */
  def nextString(rnd: Random, minLength: Int, maxLength: Int): String =
    nextString(rnd, nextInt(rnd, minLength, maxLength))

  /** Generate a random file Path with an optional extension, checking that it doesn't exist on the
    * filesystem.
    *
    * @param rnd       PRNG for repeatability.
    * @param dir       The directory to generate the file.
    * @param extension Optional extension for the file
    */
  def getNewFile(rnd: Random, dir: Path, extension: Option[String]): Path =
    Stream
      .continually(nextString(rnd, 5, 10))
      .map(_ + extension.map("." + _).getOrElse(""))
      .map(dir.resolve)
      .filter(!Files.exists(_))
      .head

  /** Create a random text file in the specified directory.
    *
    * @param rnd           PRNG for repeatability.
    * @param dir           The directory to generate the file.
    * @param numLines      The exact number of lines to generate in the file.
    * @param minLineLength The minimum line length.
    * @param maxLineLength The maximum line length.
    * @return the path to the created file.
    */
  @throws[IOException]
  def createTxtFile(
      rnd: Random,
      dir: Path,
      numLines: Int,
      minLineLength: Int,
      maxLineLength: Int
  ): Path = {
    val file = getNewFile(rnd, dir, Option("txt"))
    val fos = new FileOutputStream(file.toFile)
    try {
      (0 until numLines).foreach { _ =>
        fos.write(nextString(rnd, minLineLength, maxLineLength).getBytes)
        fos.write("\n".getBytes)
      }
    } finally if (fos != null) fos.close()
    file
  }

  /** Create a random binary file in the specified directory.
    *
    * @param rnd     PRNG for repeatability.
    * @param dir     The directory to generate the file.
    * @param minSize The minimum size in bytes.
    * @param maxSize The maximum size in bytes.
    * @return the path to the created file.
    */
  @throws[IOException]
  def createBinaryFile(
      rnd: Random,
      dir: Path,
      minSize: Int,
      maxSize: Int
  ): Path = {
    val file = getNewFile(rnd, dir, Option("bin"))
    val size = nextInt(rnd, minSize, maxSize)
    val fos = new FileOutputStream(file.toFile)
    try {
      val buffer = new Array[Byte](1024)
      Stream
        .from(0, buffer.length)
        .takeWhile(_ < size)
        .foreach { blockLen =>
          rnd.nextBytes(buffer)
          fos.write(
            buffer,
            0,
            if (blockLen + buffer.length > size) size % buffer.length
            else buffer.length
          )
        }
    } finally if (fos != null) fos.close()

    file
  }

  /** Populates the given directory with files and subdirectories.
    *
    * @param rnd      PRNG for repeatability.
    * @param dir      The directory to populate.
    * @param numFiles The exact number of files to generate in the directory and all subdirectories.
    * @param minFiles The minimum number of files to place in any directory.
    * @param maxFiles The maximum number of files to place in any directory.
    * @param maxDirs  The maximum number of subdirectories to create in any directory.
    * @param oneLarge If present, one binary file of exactly this size will be created in the root.
    * @param time     If present, a base time to use when generating file attributes.  All files and
    *                 directories will be generated consistently after this time.
    */
  @throws[IOException]
  def fillDirectory(
      rnd: Random,
      dir: Path,
      numFiles: Int,
      minFiles: Int,
      maxFiles: Int,
      maxDirs: Int,
      oneLarge: Option[Int] = None,
      time: Option[Long]
  ): Unit = { // Create files in the given directory.

    // Incrementing path times in this directory.
    var pathTime: Option[Long] = time;

    // The number of files to create inside this directory.  The rest will be placed in subdirs.
    val createFiles = Math.min(numFiles, nextInt(rnd, minFiles, maxFiles))
    for (_ <- 0 until createFiles) {
      val createdFile: Path =
        if (rnd.nextDouble < 0.5)
          createTxtFile(rnd, dir, rnd.nextInt(1000), 20, 80)
        else createBinaryFile(rnd, dir, rnd.nextInt(1000), 10 * 1024)
      pathTime = pathTime.map((t: Long) => {
        setTimeAttributes(createdFile, t)
        t + 1
      })
    }

    // Create subdirectories to place the remaining files.
    var remainingFiles =
      numFiles - createFiles - oneLarge.map(_ => 1).getOrElse(0)
    if (remainingFiles > 0) {
      val createDirs = nextInt(rnd, 1, maxDirs)
      val filesPer = remainingFiles.toDouble / createDirs
      for (i <- 0 until createDirs) {
        // Distribute the files evenly.
        val files =
          if (i == createDirs - 1) remainingFiles
          else (filesPer * (i + 1) + 0.5).toInt - (filesPer * i + 0.5).toInt
        val newDir = getNewFile(rnd, dir, None)
        Files.createDirectory(newDir)
        fillDirectory(
          rnd,
          newDir,
          files,
          minFiles,
          maxFiles,
          maxDirs,
          None,
          pathTime
        )
        remainingFiles -= files
        pathTime = pathTime.map((t: Long) => {
          setTimeAttributes(newDir, t)
          t + 1
        })
      }
    }

    // If requested, create the one large binary file.
    oneLarge
      .map(size => createBinaryFile(rnd, dir, size, size))
      .foreach(largeFile =>
        pathTime.map((t: Long) => {
          setTimeAttributes(largeFile, t)
        })
      )
  }

  /** Sets the time attributes on the path.
    *
    * @param path The file or directory to modify.
    * @param time The creation time to set on the path.  Access time will be one second more, and
    *             modification time will be two seconds more.
    */
  def setTimeAttributes(path: Path, time: Long): Unit = {
    val attributes =
      Files.getFileAttributeView(path, classOf[BasicFileAttributeView])
    // last modified, last access, creation
    attributes.setTimes(
      FileTime.fromMillis((time + 2) * 1000),
      FileTime.fromMillis((time + 1) * 1000),
      FileTime.fromMillis(time * 1000)
    )
  }
}
