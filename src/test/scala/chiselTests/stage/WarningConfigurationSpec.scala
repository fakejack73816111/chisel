// SPDX-License-Identifier: Apache-2.0

package chiselTests.stage

import chisel3._
import chisel3.testers.TestUtils
import chisel3.experimental.SourceInfo
import circt.stage.ChiselStage

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import firrtl.options.OptionsException

object WarningConfigurationSpec {
  class ModuleWithWarning extends RawModule {
    val in = IO(Input(UInt(8.W)))
    val out = IO(Output(UInt(8.W)))
    TestUtils.warn("sample warning")
    out := in
  }
}

class WarningConfigurationSpec extends AnyFunSpec with Matchers with chiselTests.Utils {
  import WarningConfigurationSpec._

  private def checkInvalid(wconf: String, carat: String, msg: String): Unit = {
    val args = Array("--warn-conf", wconf)
    val e = the[OptionsException] thrownBy (ChiselStage.emitCHIRRTL(new ModuleWithWarning, args))
    val msg = e.getMessage()
    val lines = msg.split("\n")
    msg should include(msg)
    lines should contain("  " + wconf)
    lines should contain("  " + carat)
  }

  describe("WarningConfiguration") {

    it("should support warnings as errors") {
      info("In form --warn-conf any:e")
      val args = Array("--warn-conf", "any:e", "--throw-on-first-error")
      val e = the[ChiselException] thrownBy { ChiselStage.emitCHIRRTL(new ModuleWithWarning, args) }
      e.getMessage should include("sample warning")

      info("Also in form --warnings-as-errors")
      val args2 = Array("--warnings-as-errors", "--throw-on-first-error")
      val e2 = the[ChiselException] thrownBy { ChiselStage.emitCHIRRTL(new ModuleWithWarning, args2) }
      e2.getMessage should include("sample warning")
    }

    it("should support message filters") {
      info("For suppressing warnings")
      val args = Array("--warn-conf", "msg=sample warning:s,any:e", "--throw-on-first-error")
      ChiselStage.emitCHIRRTL(new ModuleWithWarning, args)

      info("For keeping them as warnings despite --warnings-as-errors")
      val args2 = Array("--warn-conf", "msg=sample warning:w,any:e", "--throw-on-first-error")
      val (log, _) = grabLog(ChiselStage.emitCHIRRTL(new ModuleWithWarning, args2))
      log should include("sample warning")
      log should include("There were 1 warning(s) during hardware elaboration.")

      info("For elevating individual warnings to errors")
      val args3 = Array("--warn-conf", "msg=sample warning:e", "--throw-on-first-error")
      val e = the[ChiselException] thrownBy { ChiselStage.emitCHIRRTL(new ModuleWithWarning, args3) }
      e.getMessage should include("sample warning")
    }

    it("should support message filter regex") {
      info("as simple partial matches")
      val args = Array("--warn-conf", "msg=le wa:s,any:e", "--throw-on-first-error")
      ChiselStage.emitCHIRRTL(new ModuleWithWarning, args)

      info("as proper regular expressions")
      val args2 = Array("--warn-conf", "msg=sam\\w{3}\\W[warnig]{7}:s,any:e", "--throw-on-first-error")
      ChiselStage.emitCHIRRTL(new ModuleWithWarning, args2)
    }

    it("should support source filters") {
      info("For suppressing warnings")
      val args = Array("--warn-conf", "src=WarningConfigurationSpec.scala:s,any:e", "--throw-on-first-error")
      ChiselStage.emitCHIRRTL(new ModuleWithWarning, args)

      info("For keeping them as warnings despite --warnings-as-errors")
      val args2 = Array("--warn-conf", "src=WarningConfigurationSpec.scala:w,any:e", "--throw-on-first-error")
      val (log, _) = grabLog(ChiselStage.emitCHIRRTL(new ModuleWithWarning, args2))
      log should include("sample warning")
      log should include("There were 1 warning(s) during hardware elaboration.")

      info("For elevating individual warnings to errors")
      val args3 = Array("--warn-conf", "src=WarningConfigurationSpec.scala:e", "--throw-on-first-error")
      val e = the[ChiselException] thrownBy { ChiselStage.emitCHIRRTL(new ModuleWithWarning, args3) }
      e.getMessage should include("sample warning")
    }

    it("should support source filter regex") {
      info("as simple extension matches")
      val args = Array("--warn-conf", "src=.*.scala:s,any:e", "--throw-on-first-error")
      ChiselStage.emitCHIRRTL(new ModuleWithWarning, args)

      info("as rooted in arbitrary directories")
      val args2 = Array("--warn-conf", "src=stage/WarningConfigurationSpec.scala:s,any:e", "--throw-on-first-error")
      ChiselStage.emitCHIRRTL(new ModuleWithWarning, args2)

      info("as rooted in arbitrary directories including regex")
      val args3 = Array("--warn-conf", "src=scala/.*/WarningConfigurationSpec.scala:s,any:e", "--throw-on-first-error")
      ChiselStage.emitCHIRRTL(new ModuleWithWarning, args3)

      info("but not as partial matches on directories")
      val args4 = Array("--warn-conf", "src=age/WarningConfigurationSpec.scala:s,any:e", "--throw-on-first-error")
      val e = the[ChiselException] thrownBy { ChiselStage.emitCHIRRTL(new ModuleWithWarning, args4) }
      e.getMessage should include("sample warning")
    }

    it("should error on a missing action") {
      val wconf = "potato"
      val carat = "     ^"
      val msg = "Filter 'potato' is missing an action"
      checkInvalid(wconf, carat, msg)
    }

    it("should error on an invalid action") {
      val wconf = "msg=sample warning:x"
      val carat = "                  ^"
      val msg = "Invalid action ':x'"
      checkInvalid(wconf, carat, msg)

      info("and be able to point to an invalid action in a later filter")
      val wconf2 = "msg=other warning:s,msg=sample warning:x"
      val carat2 = "                                      ^"
      val msg2 = "Invalid action ':x'"
      checkInvalid(wconf2, carat2, msg2)
    }

    it("should error on an invalid category") {
      val wconf = "msg=other warning:s,msg=hi&foo=sample warning:e"
      val carat = "                           ^"
      val msg = "Invalid category 'foo'"
      checkInvalid(wconf, carat, msg)
    }

    it("should error on a duplicate msg") {
      val wconf = "msg=other warning:s,msg=hi&msg=bye:e"
      val carat = "                           ^"
      val msg = "Cannot have duplicates of the same category"
      checkInvalid(wconf, carat, msg)
    }

    it("should error on a duplicate src") {
      val wconf = "msg=other warning:s,src=hi&src=bye:e"
      val carat = "                           ^"
      val msg = "Cannot have duplicates of the same category"
      checkInvalid(wconf, carat, msg)
    }

    it("should error when combining any other category with 'any'") {
      val wconf = "msg=other warning:s,any&src=bye:e"
      val carat = "                        ^"
      val msg = "'any' cannot be combined with other filters"
      checkInvalid(wconf, carat, msg)

      info("In any order with any")
      val wconf2 = "msg=other warning:s,src=bye&any:e"
      val carat2 = "                            ^"
      val msg2 = "'any' cannot be combined with other filters"
      checkInvalid(wconf2, carat2, msg2)

      info("Even multiple 'any'")
      val wconf3 = "msg=other warning:s,any&any:e"
      val carat3 = "                        ^"
      val msg3 = "'any' cannot be combined with other filters"
      checkInvalid(wconf3, carat3, msg3)
    }
  }
}
