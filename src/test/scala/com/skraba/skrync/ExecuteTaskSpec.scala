package com.skraba.skrync

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[ExecuteTask]] */
class ExecuteTaskSpec extends MultiTaskMainSpec(SkryncGo, Some(ExecuteTask)) with FileValidator {

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs()
    itShouldThrowOnIncompleteArgs("--srcDigest", "x")
    itShouldThrowOnIncompleteArgs("--dstDigest", "x")
    itShouldThrowOnIncompleteArgs("--backup", "x")
    itShouldThrowOnMissingOptValue("--srcDigest")
    itShouldThrowOnMissingOptValue("--dstDigest", "x", "--srcDigest")
    itShouldThrowOnMissingOptValue("--dstDigest")
    itShouldThrowOnMissingOptValue("--srcDigest", "x", "--dstDigest")
    itShouldThrowOnMissingOptValue("--srcDigest", "x", "--dstDigest", "x", "--backup")
    itShouldThrowOnMissingOptValue("--backup")
    itShouldThrowOnMissingOptValue("--plan")
    itShouldThrowOnMissingOptValue("--plan", "x", "--backup")
    itShouldThrowOnMissingOptValue("--backup", "x", "--plan")

    itShouldBeAnExistingFile.args(tag = "Source")("--srcDigest", "<>", "--dstDigest", ExistingFile)
    itShouldBeAnExistingFile.args(tag = "Destination")("--srcDigest", ExistingFile, "--dstDigest", "<>")
  }
}
