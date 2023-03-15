package uestsg.march

import spinal.core._
import spinal.core.formal._
import spinal.lib.formal._

class MarchChecker extends SpinalFormalFunSuite {
  val opsMarchCm = Array[String](
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
  val elementsMarchCm = Seq(
    (0, true),
    (1, true),
    (1, true),
    (1, false),
    (1, false),
    (0, false),
    (opsMarchCm.length, false)
  )

  test("withNoFault") {
    FormalConfig
      .withCover(200)
      // .addEngin(SmtBmc(solver=SmtBmcSolver.Z3))
      .doVerify(new Component {

        val dut = FormalDut(new March(elementsMarchCm, opsMarchCm, 3))
        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val working = CombInit(dut.checkLogic.checking | dut.meLogic.output.valid)
        when(working){
          assert(dut.io.faults === 0)
        }
        cover(fell(working))
      })
  }
  
  test("withSAF") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      // .addEngin(SmtBmc(stbv = true, solver=SmtBmcSolver.Yices))
      // .withDebug
      .doVerify(new Component {
        val pos = 4
        val value = anyconst(Bool)
        val dut = FormalDut(March(elementsMarchCm, opsMarchCm, 3))
        dut.accessLogic.rework {
          import dut.accessLogic._
          ram.write(U(pos), value.pull, True)
        }

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val working = CombInit(dut.checkLogic.checking | dut.meLogic.output.valid)
        when(fell(working)){
          assert(dut.io.faults === (1<<pos))
        }
        cover(fell(working))
      })
  }
}
