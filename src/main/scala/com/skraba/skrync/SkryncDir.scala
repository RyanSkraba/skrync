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
  def digest(location: Path): SkryncDir = {
    val filesWithDigest = files.map(f => f.digest(location / File(f.name)))
    val dirsWithDigest =
      dirs.map(d => d.digest(location / Directory(d.path.name)))

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

    copy(path = rootWithDigest, files = filesWithDigest, dirs = dirsWithDigest)
  }
}

object SkryncDir {

  /** Create a [[SkryncDir]] of the basic path elements, including all subdirectories and files
    * recursively.  This does not calculate the digest.
    *
    * @param d The path to get from the filesystem.
    */
  def apply(d: Directory): SkryncDir = {
    val root = SkryncPath(d)

    // Get all of the files and subdirectories.
    val files = (for (f <- d.list.toList if f.isFile)
      yield SkryncPath(f.toFile)).sortWith(_.name < _.name)
    val dirs = (for (sub <- d.list.toList if sub.isDirectory)
      yield SkryncDir(sub.toDirectory)).sortWith(_.path.name < _.path.name)

    val deepSize = files.map(_.size).sum + dirs.map(_.path.size).sum
    val deepFileCount = dirs.map(_.deepFileCount).sum + files.size

    SkryncDir(
      path = root.copy(size = deepSize),
      deepFileCount = deepFileCount,
      files = files,
      dirs = dirs
    )
  }
}
