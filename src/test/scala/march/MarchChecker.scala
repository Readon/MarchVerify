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
  ) = new Area {
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
        val inject = createLogic(elementsMarchCm, opsMarchCm, 3, (_) => 0, (_, _, _) => {})
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
        val inject = createLogic(
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

  test("withCFdsxw!x") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      .doVerify(new Component {
        val memWidth = 3
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._
              // val valueHist = Reg(Bool)
              // when(pastValidAfterReset && past(input.addr === pos.pull)){
              //   valueHist := data
              // }
              val attackPos = pos.pull
              val victimPos = pos.pull + 1
              val attackCond = (input.addr === attackPos)
              val victimCond = (input.addr === victimPos)

              val attackHist =
                RegNextWhen(data, pastValidAfterReset && past(attackCond))
              val victimHist =
                RegNextWhen(data, pastValidAfterReset && past(victimCond))

              when(attackCond && !input.isRead && input.value =/= attackHist) {
                ram.write(victimPos, !victimHist, input.fire)
              }
            }
          }
        )
      })
  }

  test("withCFdsxwx") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      .doVerify(new Component {
        val memWidth = 3
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._
              // val valueHist = Reg(Bool)
              // when(pastValidAfterReset && past(input.addr === pos.pull)){
              //   valueHist := data
              // }
              val attackPos = pos.pull
              val victimPos = pos.pull + 1
              val attackCond = (input.addr === attackPos)
              val victimCond = (input.addr === victimPos)

              val attackHist =
                RegNextWhen(data, pastValidAfterReset && past(attackCond))
              val victimHist =
                RegNextWhen(data, pastValidAfterReset && past(victimCond))

              when(attackCond && !input.isRead && input.value === attackHist) {
                ram.write(victimPos, !victimHist, input.fire)
              }
            }
          }
        )
      })
  }

  test("withCFdsrx") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      .doVerify(new Component {
        val memWidth = 3
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._
              // val valueHist = Reg(Bool)
              // when(pastValidAfterReset && past(input.addr === pos.pull)){
              //   valueHist := data
              // }
              val attackPos = pos.pull
              val victimPos = pos.pull + 1
              val attackCond = (input.addr === attackPos)
              val victimCond = (input.addr === victimPos)

              val attackHist =
                RegNextWhen(data, pastValidAfterReset && past(attackCond))
              val victimHist =
                RegNextWhen(data, pastValidAfterReset && past(victimCond))

              when(attackCond && input.isRead && input.value === attackHist) {
                ram.write(victimPos, !victimHist, input.fire)
              }
            }
          }
        )
      })
  }

  test("withCFtr") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      .doVerify(new Component {
        val memWidth = 3
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._
              // val valueHist = Reg(Bool)
              // when(pastValidAfterReset && past(input.addr === pos.pull)){
              //   valueHist := data
              // }
              val attackPos = pos.pull
              val victimPos = pos.pull + 1
              val attackCond = (input.addr === attackPos)
              val victimCond = (input.addr === victimPos)

              val attackHist =
                RegNextWhen(data, pastValidAfterReset && past(attackCond))
              val victimHist =
                RegNextWhen(data, pastValidAfterReset && past(victimCond))

              when(victimCond && !input.isRead && input.value =/= victimHist) {
                ram.write(victimPos, !input.value, input.fire)
              }
            }
          }
        )
      })
  }

  test("withCFwd") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      .doVerify(new Component {
        val memWidth = 3
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._
              // val valueHist = Reg(Bool)
              // when(pastValidAfterReset && past(input.addr === pos.pull)){
              //   valueHist := data
              // }
              val attackPos = pos.pull
              val victimPos = pos.pull + 1
              val attackCond = (input.addr === attackPos)
              val victimCond = (input.addr === victimPos)

              val attackHist =
                RegNextWhen(data, pastValidAfterReset && past(attackCond))
              val victimHist =
                RegNextWhen(data, pastValidAfterReset && past(victimCond))

              when(victimCond && !input.isRead && input.value === victimHist) {
                ram.write(victimPos, !input.value, input.fire)
              }
            }
          }
        )
      })
  }

  test("withCFrd") {
    FormalConfig
      .withBMC(120)
      .withCover(120)
      .doVerify(new Component {
        val memWidth = 3
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._
              // val valueHist = Reg(Bool)
              // when(pastValidAfterReset && past(input.addr === pos.pull)){
              //   valueHist := data
              // }
              val attackPos = pos.pull
              val victimPos = pos.pull + 1
              val attackCond = (input.addr === attackPos)
              val victimCond = (input.addr === victimPos)

              val attackHist =
                RegNextWhen(data, pastValidAfterReset && past(attackCond))
              val victimHist =
                RegNextWhen(data, pastValidAfterReset && past(victimCond))

              when(victimCond && input.isRead && input.value === victimHist) {
                ram.write(victimPos, !input.value, input.fire)
              }
            }
          }
        )
      })
  }

  test("withCFdrd") {
    FormalConfig
      .withBMC(64)
      .withCover(64)
      .doVerify(new Component {
        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._
              
              val victimPos = pos.pull
              val victimCond = (input.addr === victimPos)

              val victimHist =
                RegNextWhen(data, pastValidAfterReset && past(victimCond && input.fire))

              val injectCond = pastValidAfterReset && past(victimCond && input.isRead && input.fire)
              val injectEnable = injectCond && value.pull === data && data === past(victimHist)
              ram.write(victimPos, !value.pull, injectEnable)
            }
          }
        )
      })
  }
}
