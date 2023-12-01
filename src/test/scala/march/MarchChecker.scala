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
      .withCover(88)
      .withDebug
      .doVerify(new Component {
        val memWidth = 3
        val inject = createLogic(elementsMarchCm, opsMarchCm, memWidth, (_) => B(0, 1 << memWidth bits), (_, _, _) => {})
      })
  }
  
  test("withSAF") {
    FormalConfig
      .withBMC(88)
      .withCover(88)
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
    shouldFail(FormalConfig
      .withBMC(88)
      .withCover(88)
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

              val attackHist = ram(attackPos)
              val victimHist = ram(victimPos)

              val injectCond = attackCond && !input.isRead && input.fire
              val injectEnable = injectCond && value.pull === !input.value && value.pull === attackHist
              ram.write(victimPos, !victimHist, injectEnable)
            }
          }
        )
      }))
  }

  test("withCFdsxwx") {
    shouldFail(FormalConfig
      .withBMC(88)
      .withCover(88)
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

              val attackHist = ram(attackPos)
              val victimHist = ram(victimPos)

              val injectCond = attackCond && !input.isRead && input.fire
              val injectEnable = injectCond && value.pull === input.value && value.pull === attackHist
              ram.write(victimPos, !victimHist, injectEnable)
            }
          }
        )
      }))
  }

  test("withCFdsrx") {
    shouldFail(FormalConfig
      .withBMC(88)
      .withCover(88)
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

              val attackHist = ram(attackPos)
              val victimHist = ram(victimPos)

              val injectCond = attackCond && input.isRead && input.fire
              val injectEnable = injectCond && value.pull === input.value && value.pull === attackHist
              ram.write(victimPos, !victimHist, injectEnable)
            }
          }
        )
      }))
  }

  test("withIRF") {
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
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

              val victimHist = ram(victimPos)

              val injectCond = victimCond && input.isRead && input.fire
              val injectEnable = injectCond && value.pull === input.value && value.pull === victimHist
              // ram.write(victimPos, !value.pull, injectEnable)

              when(past(injectEnable)) {
                data := !value.pull
              }
            }
          }
        )
      }))
  }

  test("withCFwd") {
    FormalConfig
      .withBMC(88)
      .withCover(88)
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
      .withBMC(88)
      .withCover(88)
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
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
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

              val injectCond = victimCond && input.isRead && input.fire
              val injectEnable = injectCond && value.pull === input.value && value.pull === past(victimHist)
              ram.write(victimPos, !value.pull, injectEnable)
            }
          }
        )
      }))
  }
}
