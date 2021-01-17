package com.skraba.skrync

import com.google.gson.stream.JsonWriter
import com.google.gson.{Gson, JsonArray, JsonObject}
import com.skraba.skrync.SkryncGo.Analysis

import java.io.{BufferedOutputStream, InputStreamReader, PrintWriter}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.reflect.io.{Directory, File, Streamable}

/** Utilities for reading and writing the file to and from JSON */
object Json {

  /** JSON property names for a [[SkryncPath]]. */
  val Name = "name"
  val CreationTime = "c"
  val AccessTime = "a"
  val ModificationTime = "m"
  val Size = "size"
  val Sha1 = "sha1"

  /** JSON property names for a [[SkryncDir]]. */
  val Files = "files"
  val Dirs = "dirs"
  val DeepFileCount = "filecount"

  val BackupCreated = "backupCreated"

  def pathToJson(p: SkryncPath): JsonObject = {
    val json = new JsonObject
    json.addProperty(Name, p.name)
    json.addProperty(CreationTime, p.creation)
    json.addProperty(AccessTime, p.access)
    json.addProperty(ModificationTime, p.modification)
    json.addProperty(Size, p.size)
    // Optionally add the digest.
    p.digest.map(Digests.toHex).foreach(json.addProperty(Sha1, _))
    json
  }

  def pathFromJson(ignoreTimes: Boolean)(json: JsonObject): SkryncPath = {
    SkryncPath(
      name = json.get(Name).getAsString,
      creation = if (ignoreTimes) -1 else json.get(CreationTime).getAsLong,
      access = if (ignoreTimes) -1 else json.get(AccessTime).getAsLong,
      modification =
        if (ignoreTimes) -1 else json.get(ModificationTime).getAsLong,
      size = json.get(Size).getAsLong,
      digest = Option(json.get(Sha1)).map(_.getAsString).map(Digests.fromHex)
    )
  }

  def dirToJson(dir: SkryncDir): JsonObject = {
    val json: JsonObject = pathToJson(dir.path)
    json.addProperty(DeepFileCount, dir.deepFileCount)
    json.add(
      Files,
      dir.files
        .map(pathToJson)
        .foldLeft(new JsonArray)((ja, f) => {
          ja.add(f)
          ja
        })
    )
    json.add(
      Dirs,
      dir.dirs
        .map(dirToJson)
        .foldLeft(new JsonArray)((ja, f) => {
          ja.add(f)
          ja
        })
    )
    json
  }

  def dirFromJson(ignoreTimes: Boolean)(json: JsonObject): SkryncDir = {
    import collection.JavaConverters._
    SkryncDir(
      path = pathFromJson(ignoreTimes)(json),
      deepFileCount = json.get(DeepFileCount).getAsInt,
      files = json
        .getAsJsonArray(Files)
        .iterator
        .asScala
        .toList
        .map(_.getAsJsonObject)
        .map(pathFromJson(ignoreTimes)),
      dirs = json
        .getAsJsonArray(Dirs)
        .iterator
        .asScala
        .toList
        .map(_.getAsJsonObject)
        .map(dirFromJson(ignoreTimes))
    )
  }

  def analysisToJson(analysis: Analysis): JsonObject = {
    val json: JsonObject = dirToJson(analysis.info)
    json.addProperty(Name, analysis.src.toAbsolute.toString())
    json.addProperty(BackupCreated, analysis.created)
    json
  }

  def analysisFromJson(ignoreTimes: Boolean)(in: JsonObject): Analysis = {
    val srcDir = Directory(in.getAsJsonPrimitive(Name).getAsString)
    in.addProperty(Name, ".")
    Analysis(
      src = srcDir,
      created = Option(in.getAsJsonPrimitive(BackupCreated))
        .map(_.getAsLong)
        .getOrElse(-1),
      info = dirFromJson(ignoreTimes)(in)
    )
  }

  /** Write an analysis object with all of its files and directories to a file.
    *
    * @param analysis    The object to write.
    * @param dst     The destination file to write to, or overwrite.
    * @param gzipped Whether to gzip the output.
    */
  def write(analysis: Analysis, dst: Option[File], gzipped: Boolean): Unit = {
    dst match {
      case None =>
        Streamable.closing(new JsonWriter(new PrintWriter(Console.out))) { jw =>
          new Gson().toJson(analysisToJson(analysis), jw)
        }
      case Some(file) =>
        val out = new PrintWriter(
          if (gzipped) new GZIPOutputStream(file.outputStream())
          else new BufferedOutputStream(file.outputStream())
        )

        Streamable.closing(new JsonWriter(out)) { w =>
          new Gson().toJson(analysisToJson(analysis), w)
        }
    }
  }

  /** @param src The source file generated from [[write()]]
    */
  def read(src: File, ignoreTimes: Boolean = true): Analysis = {
    // It's gzipped if the magic header matches these two bytes.
    val gzipped = Streamable.closing(src.inputStream) { in =>
      in.read == 0x1f && in.read == 0x8b
    }

    Streamable.closing(
      new InputStreamReader(
        if (gzipped) new GZIPInputStream(src.inputStream) else src.inputStream
      )
    ) { in =>
      analysisFromJson(ignoreTimes)(
        new Gson().fromJson(in, classOf[JsonObject])
      )
    }
  }
}
