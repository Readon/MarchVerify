package uestsg.march

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class MarchTester extends AnyFunSuite {
  var compiled: SimCompiled[March] = null
  test("compile") {
    compiled = SimConfig.withConfig(SpinalConfig()).compile {
      val ops = Array[String]("01", "10", "01")
      val elements = Seq((0, true), (0, false), (3, false))
      val dut = new March(elements, ops, 2)
      dut
    }
  }

  test("basic") {
    compiled.doSim("testBasicTiming") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling(1000)
      simSuccess()
    }
  }
}
