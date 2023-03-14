package uestsg.march

import spinal.core._
import spinal.core.formal._
import spinal.lib.formal._

class MarchChecker extends SpinalFormalFunSuite {
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

  test("withNoFault") {
    FormalConfig
      .withCover(200)
      // .addEngin(SmtBmc(solver=SmtBmcSolver.Z3))
      .doVerify(new Component {

        val dut = FormalDut(new March(elements, ops, 3))
        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        val working = CombInit(dut.checkLogic.checking | dut.meLogic.output.valid)
        when(working){
          assert(dut.io.faults === 0)
        }
        cover(fell(working))
      })
  }
}
