package com.skraba.skrync

import com.google.gson.{Gson, JsonArray, JsonObject}
import com.google.gson.stream.{JsonReader, JsonToken, JsonWriter}
import com.skraba.skrync.SkryncGo.Analysis

import java.io.{BufferedOutputStream, InputStreamReader, PrintWriter}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.reflect.io.{Directory, File, Path, Streamable}

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

  /** Collects "pointers" by path into the JSON object. */
  private[this] def collectByPath(
      current: Path,
      json: JsonObject
  ): Seq[(Path, JsonObject)] = {
    import collection.JavaConverters._
    // Remember this object at the current path
    Seq(current -> json) ++
      // Add all of the files to the list, inside the current path.
      json
        .getAsJsonArray(Files)
        .iterator
        .asScala
        .map(_.getAsJsonObject)
        .map(json => current.resolve(json.get(Name).getAsString) -> json) ++
      // Add all of the files to the list, inside the current path.
      json
        .getAsJsonArray(Dirs)
        .iterator
        .asScala
        .map(_.getAsJsonObject)
        .flatMap(json =>
          collectByPath(current.resolve(json.get(Name).getAsString), json)
        )
  }

  /** @param src The source file generated from [[write()]]
    */
  def read(src: File, ignoreTimes: Boolean = true): Analysis = {

    // It's gzipped if the magic header matches these two bytes.
    val gzipped = Streamable.closing(src.inputStream) { in =>
      in.read == 0x1f && in.read == 0x8b
    }

    // There can be multiple JSON objects in the reader, which Gson requires to be lenient
    val gson = new Gson()
    val r = new JsonReader(
      new InputStreamReader(
        if (gzipped) new GZIPInputStream(src.inputStream) else src.inputStream
      )
    )
    r.setLenient(true)

    // Read all of the json objects from the stream and me
    Streamable.closing(r) { r =>
      // The main tree is the first JSON object in the stream.
      val analysisJson: JsonObject = gson.fromJson(r, classOf[JsonObject])

      // Create a map from all of the paths to the internal JSON objects in the main tree.
      val paths: Map[Path, JsonObject] =
        collectByPath(Path(""), analysisJson).toMap

      // All of the remaining JSON objects in the stream are digest info.
      Stream
        .continually {
          if (r.peek() == JsonToken.END_DOCUMENT) None
          else
            Some(gson.fromJson(r, classOf[JsonObject]).asInstanceOf[JsonObject])
        }
        .takeWhile(_.nonEmpty)
        .map(_.get)
        .foreach { sha1Json =>
          // Update the main analysis object by looking up the name
          paths
            .get(Path(sha1Json.get(Name).getAsString))
            .map(_.addProperty(Sha1, sha1Json.get(Sha1).getAsString))
        }

      // Convert the merged json to an object
      analysisFromJson(ignoreTimes)(analysisJson)
    }
  }

  /** Write an analysis object with all of its files and directories to a file.
    *
    * @param src The root of the directory that was digested/analysed.
    * @param dst The destination file to write to, or None to go write to STDOUT.
    * @param created The time that the the analysis was performed.
    * @param gzipped Whether to gzip the output.
    * @param wrapped An additional digest progress watcher to wrap.
    */
  def writeProgress(
      src: Directory,
      dst: Option[File],
      created: Long,
      gzipped: Boolean,
      wrapped: DigestProgress = IgnoreProgress
  ): DigestProgress = new DigestProgress {

    var root: Option[Directory] = None

    val out = new PrintWriter(dst match {
      case None => Console.out
      case Some(file) =>
        if (gzipped) new GZIPOutputStream(file.outputStream())
        else new BufferedOutputStream(file.outputStream())
    })

    val w = new JsonWriter(out)
    val g = new Gson()

    private[this] def appendJson(json: JsonObject): Unit = {
      g.toJson(json, w)
      if (!gzipped || dst.isEmpty) {
        w.flush()
        out.println()
      }
    }

    override def scanningDir(p: Directory): Unit = {
      if (root.isEmpty)
        root = Some(p)
      wrapped.scanningDir(p)
    }

    override def scannedDir(p: Directory, dir: SkryncDir): SkryncDir = {
      if (root.contains(p)) {
        appendJson(
          analysisToJson(
            Analysis(
              src = src,
              created = created,
              info = dir
            )
          )
        )
        w.flush()
      }
      wrapped.scannedDir(p, dir)
    }

    override def scannedFile(p: Path, file: SkryncPath): SkryncPath =
      wrapped.scannedFile(p, file)

    override def digestingDir(p: Path, dir: SkryncDir): SkryncDir =
      wrapped.digestingDir(p, dir)

    override def digestedDir(p: Path, dir: SkryncDir): SkryncDir = {
      appendJson {
        val json = new JsonObject()
        json.addProperty(Name, root.map(_.relativize(p).toString).getOrElse(""))
        dir.path.digest.map(Digests.toHex).foreach(json.addProperty(Sha1, _))
        json
      }
      wrapped.digestedDir(p, dir)
    }

    override def digestingFile(p: Path, file: SkryncPath): SkryncPath = {
      wrapped.digestingFile(p, file)
    }

    override def digestingFileProgress(len: Long): Long =
      wrapped.digestingFileProgress(len)

    override def digestedFile(p: Path, file: SkryncPath): SkryncPath = {
      appendJson {
        val json = new JsonObject()
        json.addProperty(Name, root.map(_.relativize(p).toString).getOrElse(""))
        file.digest.map(Digests.toHex).foreach(json.addProperty(Sha1, _))
        json
      }
      wrapped.digestedFile(p, file)
    }

    override def done(p: Directory, dir: SkryncDir): SkryncDir = {
      if (root.contains(p)) {
        w.close()
      }
      wrapped.done(p, dir)
    }
  }
}
