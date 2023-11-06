package uestsg.march

import spinal.core._
import spinal.core.sim._
import spinal.tester._

class MarchTester extends SpinalAnyFunSuite {
  test("compile") {
    val compiled = SimConfig.withConfig(SpinalConfig()).withFstWave.compile {
      val ops = Array[String]("01", "11", "00")
      val elements = Seq((0, true), (1, false), (ops.length, false))
      val dut = new March(elements, ops, 2)
      dut
    }
  }

  test("basic") {
    val compiled = SimConfig.withConfig(SpinalConfig()).withFstWave.compile {
      val ops = Array[String]("01", "11", "00")
      val elements = Seq((0, true), (1, false), (ops.length, false))
      val dut = new March(elements, ops, 2)
      dut
    }

    compiled.doSim("testBasicTiming") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling(1000)
      simSuccess()
    }
  }

  test("March C-") {
    val compiled = SimConfig.withConfig(SpinalConfig()).withFstWave.compile {
      val ops = Array[String](
        "00",
        "10",
        "01",
        "11",
        "00",
        "10",
        "01",
        "11",
        "00",
        "10"
      )
      val elements = Seq(
        (0, true),
        (1, true),
        (1, true),
        (1, false),
        (1, false),
        (0, false),
        (ops.length, false)
      )
      val dut = new March(elements, ops, 3)
      dut
    }

    compiled.doSim("testBasicTiming") { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.clockDomain.waitSampling(1000)
      simSuccess()
    }
  }
}
