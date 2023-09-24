package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

import java.nio.charset.StandardCharsets
import scala.reflect.io.{Directory, File, Path}

/** Basic information about a directory.
  *
  * This includes file system information and information about its contents.  The size is the
  * deep size of all the files it contains, and the digest includes file and directory names
  * information.
  */
case class SkryncDir(
    path: SkryncPath,
    deepFileCount: Int,
    files: List[SkryncPath],
    dirs: List[SkryncDir]
) {

  /** @return a copy of this instance without any time information.
    */
  def copyWithoutTimes(): SkryncDir = {
    SkryncDir(
      path = path.copyWithoutTimes(),
      deepFileCount = deepFileCount,
      files = files.map(_.copyWithoutTimes()),
      dirs = dirs.map(_.copyWithoutTimes())
    )
  }

  /** Recalculate and add the sha1 digest.
    */
  def digest(location: Path, w: DigestProgress = IgnoreProgress): SkryncDir = {
    w.digestingDir(location, this)

    val filesWithDigest = files.map(f => f.digest(location / File(f.name), w))
    val dirsWithDigest =
      dirs.map(d => d.digest(location / Directory(d.path.name), w))

    // For a directory, use the stream of its children digests *and* their names.
    val fileNamesAndSha1: Seq[Stream[Byte]] =
      for (
        file <- filesWithDigest;
        name: String <- Some(file.name);
        digest: Digest <- file.digest
      )
        yield name
          .getBytes(StandardCharsets.UTF_8)
          .toStream append digest.toStream
    val dirNamesAndSha1: Seq[Stream[Byte]] =
      for (
        dir <- dirsWithDigest;
        name: String <- Some(dir.path.name);
        digest: Digest <- dir.path.digest
      )
        yield name
          .getBytes(StandardCharsets.UTF_8)
          .toStream append digest.toStream

    val rootWithDigest = path.copy(digest =
      Some(Digests.getSha1(fileNamesAndSha1 ++ dirNamesAndSha1))
    )

    w.digestedDir(
      location,
      copy(
        path = rootWithDigest,
        files = filesWithDigest,
        dirs = dirsWithDigest
      )
    )
  }

  /** Flattens the contents of this directory recursively so that all of its contents are
    * in the sequence.
    *
    * @param path The path at which this directory is considered to exist.  All of the contents are resolved relative to this path.
    * @return A sequence of Path -> SkryncPath of all of the file contents inside his directory.
    */
  def flattenPaths(path: Path): Seq[(Path, SkryncPath)] = {
    // The files directly in this source directory.
    val fs: Seq[(Path, SkryncPath)] =
      files.map(file => path.resolve(file.name) -> file)
    // And then recursively do the children.
    val ds: Seq[(Path, SkryncPath)] =
      dirs.flatMap(dir => dir.flattenPaths(path.resolve(dir.path.name)))
    // Return the two lists concatenated.
    fs ++ ds
  }
}

object SkryncDir {

  /** Returns that the given string corresponds to a directory on the filesystem.
    * @param argSrcDir The argument
    * @return The validated directory on the filesystem
    */
  def validateSourceDirectory(argSrcDir: String): Directory = {
    val srcDir: Directory = Directory(argSrcDir).toAbsolute
    if (!srcDir.exists)
      throw new IllegalArgumentException(s"Source doesn't exist: $argSrcDir")
    if (!srcDir.isDirectory)
      throw new IllegalArgumentException(
        s"Source is not a directory: $argSrcDir"
      )
    srcDir
  }

  /** Create a [[SkryncDir]] of the basic path elements, including all subdirectories and files
    * recursively.  This does not calculate the digest.
    *
    * @param d The path to get from the filesystem.
    * @param w A class that is notified as the file system is being traversed.
    */
  private def scanInternal(
      d: Directory,
      w: DigestProgress = IgnoreProgress
  ): SkryncDir = {
    val root = SkryncPath(d)
    w.scanningDir(d)

    // Get all of the files and subdirectories.
    val files = (for (f <- d.list.toList if f.isFile)
      yield w.scannedFile(f, SkryncPath(f.toFile))).sortWith(_.name < _.name)
    val dirs = (for (sub <- d.list.toList if sub.isDirectory)
      yield SkryncDir.scanInternal(sub.toDirectory, w))
      .sortWith(_.path.name < _.path.name)

    val deepSize = files.map(_.size).sum + dirs.map(_.path.size).sum
    val deepFileCount = dirs.map(_.deepFileCount).sum + files.size

    w.scannedDir(
      d,
      SkryncDir(
        path = root.copy(size = deepSize),
        deepFileCount = deepFileCount,
        files = files,
        dirs = dirs
      )
    )
  }

  /** Create a [[SkryncDir]] of the basic path elements, including all subdirectories and files
    * recursively.  This only calculates the digest if requested.
    *
    * @param d The path to get from the filesystem.
    * @param w A class that is notified as the file system is being traversed.
    * @param digest Get the digests after scanning the file system.
    */
  def scan(
      d: Directory,
      digest: Boolean,
      w: DigestProgress = IgnoreProgress
  ): SkryncDir = {
    // Do the scan
    val dir = scanInternal(d, w)
    w.done(d, if (digest) dir.digest(d, w) else dir)
  }
}
