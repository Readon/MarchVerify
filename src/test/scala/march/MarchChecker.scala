package uestsg.march

import spinal.core._
import spinal.core.formal._
import spinal.lib.formal._

class MarchChecker extends SpinalFormalFunSuite {
  val opsMarchCm = Array[String](
    March.W0,
    March.R0,
    March.W1,
    March.R1,
    March.W0,
    March.R0,
    March.W1,
    March.R1,
    March.W0,
    March.R0
  )
  val elementsMarchCm = Seq(
    (0, March.UP),
    (1, March.UP),
    (1, March.UP),
    (1, March.DOWN),
    (1, March.DOWN),
    (0, March.DOWN),
    (opsMarchCm.length, false)
  )

  def createLogic(
      elements: Seq[(Int, Boolean)],
      ops: Array[String],
      memWidth: Int = 3,
      getExpected: (UInt) => Bits,
      faultEmulate: (March, UInt, Bool) => Unit
  ) = {
    val pos = anyconst(UInt(memWidth bits))
    val value = anyconst(Bool)
    val dut = FormalDut(March(elements, ops, memWidth))
    val working = CombInit(dut.checkLogic.checking | dut.meLogic.output.valid)
    val reset = ClockDomain.current.isResetActive
    assumeInitial(reset)

    val expected = getExpected(pos)
    when(dut.io.faults =/= 0 || fell(working)) {
      assert(dut.io.faults === expected)
    }
    cover(fell(working))
    faultEmulate(dut, pos, value)
  }

  test("withNoFault") {
    FormalConfig
      .withCover(120)
      .doVerify(new Component {
        createLogic(elementsMarchCm, opsMarchCm, 3, (_) => 0, (_, _, _) => {})
      })
  }

  test("withSAF") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      // .addEngin(SmtBmc(stbv = true, solver=SmtBmcSolver.Yices))
      // .withDebug
      .doVerify(new Component {
        val memWidth = 3
        createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth),
          (dut, pos, value) => {
            dut.accessLogic.rework {
              import dut.accessLogic._
              ram.write(pos.pull, value.pull, True)
            }
          }
        )
      })
  }
}
