package com.skraba.skrync

import com.skraba.skrync.Digests.Digest

import java.nio.file.attribute.BasicFileAttributes
import scala.reflect.io._

/** Basic information about a directory or file. These are taken from the filesystem. When contained in a [[SkryncDir]],
  * the size is the sum of the files underneath it.
  *
  * The digest is based on the file contents only. When contained in a [[SkryncDir]], this is an internal digest that
  * includes its children's digests and names.
  */
case class SkryncPath(
    name: String,
    size: Long,
    creation: Long,
    access: Long,
    modification: Long,
    digest: Option[Digest]
) {

  /** @return a copy of this instance without any time information. */
  def copyWithoutTimes(): SkryncPath = {
    SkryncPath(
      name = name,
      size = size,
      creation = -1,
      access = -1,
      modification = -1,
      digest = digest
    )
  }

  /** Recalculate and add the sha1 digest. */
  def digest(location: Path, w: DigestProgress = IgnoreProgress): SkryncPath = {
    // This should not be called on a directory.
    w.digestingFile(location, this)
    val digest = Digests.getSha1(location.toFile, w)
    w.digestedFile(location, copy(digest = Some(digest)))
  }
}

object SkryncPath {

  /** Create from the basic path elements from the filesystem. This does not calculate the digest.
    *
    * @param p
    *   The path to get from the filesystem.
    */
  def apply(p: Path): SkryncPath = {
    val attributes: BasicFileAttributes = java.nio.file.Files.readAttributes(
      java.nio.file.Paths.get(p.toURI),
      classOf[BasicFileAttributes]
    )
    SkryncPath(
      name = p.name,
      creation = attributes.creationTime().toMillis,
      access = attributes.lastAccessTime().toMillis,
      modification = attributes.lastModifiedTime().toMillis,
      size = attributes.size(),
      digest = None
    )
  }

  /** A helper method to determine whether the child is inside the parent, including resolving .. paths
    *
    * @param parent
    *   An directory, either absolute or relative to the current directory
    * @param child
    *   Any path, to check whether it is inside the parent directory.
    * @return
    *   True if the child refers to anything inside (or including) the parent directory.
    */
  def isIn(parent: Directory, child: Path): Boolean = {
    val canonicalParent = parent.toCanonical
    val canonicalChild = (if (child.isAbsolute) child
                          else canonicalParent.resolve(child)).toCanonical
    canonicalChild.jfile.toPath.startsWith(canonicalParent.jfile.toPath)
  }
}
