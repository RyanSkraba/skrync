package com.skraba.skrync

import com.tinfoiled.docopt4s.Task
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

/** Additional validators for files */
trait FileValidator extends TmpDir { this: MultiTaskMainSpec[? <: Task] =>

  // TODO: Move this into docopt4s MultiTaskMainSpec
  class PathAdapter(dfltTag: String = "Path")(thunk: (String, String, Seq[Any]) => Unit)
      extends Function2[String, String, Function[Seq[String], Unit]] {
    // TODO: Scala 3 can have these as apply instead of named methods
    // def apply(holder: String = "<>", tag: String = dfltTag)(): Unit = thunk(holder, tag, Seq.empty)
    // def apply(holder: String = "<>", tag: String = dfltTag)(in: Any, in2: Any*): Unit = thunk(holder, tag, in +: in2)
    def args(holder: String = "<>", tag: String = dfltTag)(in: Any*): Unit = thunk(holder, tag, in)
    override def apply(repl: String = "<>", tag: String = dfltTag): Function[Seq[Any], Unit] = thunk(repl, tag, _)
  }

  // TODO: Document
  val itShouldBeAnExistingPath: PathAdapter = new PathAdapter("Path")((holder, tag, args) => {
    val nonExistArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) NonExistingPath else arg)
    it("throws an exception when the path doesn't exist: " + nonExistArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(nonExistArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"$tag doesn't exist: $NonExistingPath"
    }
  })

  // TODO: Document
  val itShouldBeAnExistingFile: PathAdapter = new PathAdapter("File")((holder, tag, args) => {
    val nonExistArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) NonExistingPath else arg)
    it("throws an exception when the file doesn't exist: " + nonExistArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(nonExistArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"$tag doesn't exist: $NonExistingPath"
    }

    val dirArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) Tmp else arg)
    it("throws an when the file exists but is a directory: " + dirArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(dirArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"$tag expected a file, found directory: $Tmp"
    }
  })

  // TODO: Document
  val itShouldBeAnExistingDir: PathAdapter = new PathAdapter("Directory")((holder, tag, args) => {
    val nonExistArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) NonExistingPath else arg)
    it("throws an exception when the directory doesn't exist: " + nonExistArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(nonExistArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"$tag doesn't exist: $NonExistingPath"
    }

    val fileArgs = Task.map(_.Cmd).toSeq ++ args.map(arg => if (arg == holder) ExistingFile else arg)
    it("throws an when the directory exists but is a file: " + fileArgs.mkString(" ")) {
      val t = interceptGoDocoptEx(fileArgs: _*)
      t.docopt shouldBe Doc
      t.getMessage shouldBe s"$tag expected a directory, found file: $ExistingFile"
    }
  })
}
