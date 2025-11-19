package com.skraba.skrync

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

import scala.reflect.io.{Directory, File, Path}

/** Unit tests for [[SkryncGo]] */
class SkryncGoSpec extends MultiTaskMainSpec(SkryncGo) {
  describe(s"Standard $MainName command line help, versions and exceptions") {
    itShouldHandleVersionAndHelpFlags()
    itShouldThrowOnMissingTaskCommand("--debug")
    itShouldThrowOnUnknownTaskCommand()
  }
}

object SkryncGoSpec extends MultiTaskMainSpec(SkryncGo) {

  /** A helper method used to create an analysis file and read it into memory.
    *
    * @param srcDir
    *   The source directory to read files from.
    * @param dstDigest
    *   Either the exact file to write the analysis to, or a directory to create with a default filename. If the file
    *   exists, it is read without re-scanning.
    * @param mustExist
    *   If true, scanning is never performed and the file is read as it is.
    * @return
    *   The destination digest and the analysis in memory.
    */
  def withSkryncGoAnalysis(
      srcDir: Directory,
      dstDigest: Path,
      mustExist: Boolean = false
  ): (File, SkryncGo.Analysis) = {
    dstDigest match {
      case f: File if f.exists || mustExist => (f, Json.read(f))
      case f: File =>
        f.parent.createDirectory(force = true, failIfExists = false);
        withGo("digest", "--srcDir", srcDir, "--dstDigest", dstDigest)
        (f, Json.read(f))
      case d: Directory =>
        if (!d.exists)
          d.createDirectory(force = true, failIfExists = false)
        if (!mustExist)
          withGo("digest", "--srcDir", srcDir, "--dstDigest", dstDigest)
        val dstDigestDefault: File = d.list.maxBy(_.lastModified).toFile
        (dstDigestDefault, Json.read(dstDigestDefault))
    }
  }
}
