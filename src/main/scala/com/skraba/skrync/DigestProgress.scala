package com.skraba.skrync

import java.io.PrintStream
import scala.reflect.io.{Directory, Path}

/** A trait that is used as a callback for progress as the tool is working. */
trait DigestProgress {
  def scanningDir(p: Directory): Unit = {}
  def scannedDir(p: Directory, dir: SkryncDir): SkryncDir = dir
  def scannedFile(p: Path, file: SkryncPath): SkryncPath = file
  def digestingDir(p: Path, dir: SkryncDir): SkryncDir = dir
  def digestedDir(p: Path, dir: SkryncDir): SkryncDir = dir
  def digestingFile(p: Path, file: SkryncPath): SkryncPath = file
  def digestingFileProgress(len: Long): Long = len
  def digestedFile(p: Path, file: SkryncPath): SkryncPath = file
  def done(p: Directory, dir: SkryncDir): SkryncDir = dir
}

/** An implementation that does nothing. */
object IgnoreProgress extends DigestProgress {}

/** A wrapper for detecting progress that prints to a stream. */
class PrintDigestProgress(
    out: PrintStream,
    wrapped: DigestProgress = IgnoreProgress
) extends DigestProgress {
  override def scanningDir(p: Directory): Unit = {
    out.print('[')
    wrapped.scanningDir(p)
  }
  override def scannedDir(p: Directory, dir: SkryncDir): SkryncDir = {
    out.print(']')
    wrapped.scannedDir(p, dir)
  }
  override def scannedFile(p: Path, file: SkryncPath): SkryncPath = {
    out.print('!')
    wrapped.scannedFile(p, file)
  }
  override def digestingDir(p: Path, dir: SkryncDir): SkryncDir = {
    out.print('{')
    wrapped.digestingDir(p, dir)
  }
  override def digestedDir(p: Path, dir: SkryncDir): SkryncDir = {
    out.print('}')
    wrapped.digestedDir(p, dir)
  }
  override def digestingFile(p: Path, file: SkryncPath): SkryncPath = {
    out.print('<')
    wrapped.digestingFile(p, file)
  }
  override def digestingFileProgress(len: Long): Long = {
    out.print('.')
    wrapped.digestingFileProgress(len)
  }
  override def digestedFile(p: Path, file: SkryncPath): SkryncPath = {
    out.print('>')
    wrapped.digestedFile(p, file)
  }
}
