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
    when(fell(working)) {
      assert(dut.io.faults === expected)
    }
    when(dut.io.faults =/= 0) {
      assert(dut.io.faults === expected)
    }
    cover(fell(working))
    faultEmulate(dut, pos, value)
  }

  test("withNoFault") {
    FormalConfig
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val memWidth = 2
        val inject = createLogic(elementsMarchCm, opsMarchCm, memWidth, (_) => B(0, 1 << memWidth bits), (_, _, _) => {})
      })
  }
  
  test("withSAF") {
    FormalConfig
      .withBMC(48)
      .withCover(48)
      // .addEngin(SmtBmc(stbv = true, solver=SmtBmcSolver.Yices))
      .withDebug
      .doVerify(new Component {
        val memWidth = 2
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
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === value.pull && opOnAttack && writeOpCond(!value.pull)
              val victimCond = victimState === yvalue.pull

              val injectEnable = victimCond && attackCond
              ram.write(victimPos, !victimState, injectEnable)
            }
          }
        )
      }))
  }

  test("withCFdsxwx") {
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === value.pull && opOnAttack && writeOpCond(value.pull)
              val victimCond = victimState === yvalue.pull

              val injectEnable = victimCond && attackCond
              ram.write(victimPos, !victimState, injectEnable)
            }
          }
        )
      }))
  }

  test("withCFdsrx") {
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === value.pull && opOnAttack && readOpCond(value.pull)
              val victimCond = victimState === yvalue.pull

              val injectEnable = victimCond && attackCond
              ram.write(victimPos, !victimState, injectEnable)
            }
          }
        )
      }))
  }

  test("withIRF") {
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
      .withDebug
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
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnVictim = input.addr === victimPos
              val victimCond = victimState === value.pull && opOnVictim && readOpCond(value.pull)

              val injectEnable = victimCond
              // ram.write(victimPos, !value.pull, injectEnable)

              when(past(injectEnable)) {
                data := !value.pull
              }
            }
          }
        )
      }))
  }
  test("withCFir") {
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === yvalue.pull
              val victimCond = victimState === value.pull && opOnVictim && readOpCond(value.pull)

              val injectEnable = victimCond && attackCond
              // ram.write(victimPos, !value.pull, injectEnable)

              when(past(injectEnable)) {
                data := !value.pull
              }
            }
          }
        )
      }))
  }

  test("withRDF") {
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
      .withDebug
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
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnVictim = input.addr === victimPos
              val victimCond = victimState === value.pull && opOnVictim && readOpCond(value.pull)

              val injectEnable = victimCond
              ram.write(victimPos, !value.pull, injectEnable)

              when(past(injectEnable)) {
                data := !value.pull
              }
            }
          }
        )
      }))
  }

  test("withCFrd") {
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === yvalue.pull
              val victimCond = victimState === value.pull && opOnVictim && readOpCond(value.pull)

              val injectEnable = victimCond && attackCond
              ram.write(victimPos, !value.pull, injectEnable)

              when(past(injectEnable)) {
                data := !value.pull
              }
            }
          }
        )
      }))
  }

  test("withTF") {
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
      .withDebug
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
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnVictim = input.addr === victimPos
              val victimCond = victimState === value.pull && opOnVictim && writeOpCond(!value.pull)

              val injectEnable = victimCond
              ram.write(victimPos, value.pull, injectEnable)
            }
          }
        )
      }))
  }
  test("withCFtr") {
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === yvalue.pull
              val victimCond = victimState === value.pull && opOnVictim && writeOpCond(!value.pull)

              val injectEnable = victimCond && attackCond
              ram.write(victimPos, value.pull, injectEnable)
            }
          }
        )
      }))
  }

  test("withWDF") {
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
      .withDebug
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
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnVictim = input.addr === victimPos
              val victimCond = victimState === value.pull && opOnVictim && writeOpCond(value.pull)

              val injectEnable = victimCond
              ram.write(victimPos, !value.pull, injectEnable)
            }
          }
        )
      }))
  }
  test("withCFwd") {
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === yvalue.pull
              val victimCond = victimState === value.pull && opOnVictim && writeOpCond(value.pull)

              val injectEnable = victimCond && attackCond
              ram.write(victimPos, !value.pull, injectEnable)
            }
          }
        )
      }))
  }

  test("withDRDF") {
    shouldFail(FormalConfig
      .withBMC(48)
      .withCover(48)
      .withDebug
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
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnVictim = input.addr === victimPos
              val victimCond = victimState === value.pull && opOnVictim && readOpCond(value.pull)

              val injectEnable = victimCond
              ram.write(victimPos, !value.pull, injectEnable)
            }
          }
        )
      }))
  }
  test("withCFdrd") {
      .withBMC(48)
      .withCover(48)
      .withDebug
      .doVerify(new Component {
        val posOffId = anyconst(UInt(1 bits))
        val availablePosOff = Vec(S(1), S(-1))
        val posOff = availablePosOff(posOffId)
        val yvalue = anyconst(Bool)

        val memWidth = 2
        val inject = createLogic(
          elementsMarchCm,
          opsMarchCm,
          memWidth,
          (pos) => (B(1) << pos + posOff.pull.asUInt).resize(1 << memWidth), // 响应
          (dut, pos, value) => { // 激励
            dut.accessLogic.rework {
              import dut.accessLogic._

              val attackPos = pos.pull
              val victimPos = pos.pull + posOff.pull.asUInt
              val attackState = ram(attackPos)
              val victimState = ram(victimPos)

              def writeOpCond(x: Bool) = !input.isRead && input.fire && input.value === x
              def readOpCond(x: Bool = value.pull) = input.isRead && input.fire && input.value === x
              val opOnAttack = input.addr === attackPos
              val opOnVictim = input.addr === victimPos

              val attackCond = attackState === yvalue.pull
              val victimCond = victimState === value.pull && opOnVictim && readOpCond(value.pull)

              val injectEnable = victimCond && attackCond
              ram.write(victimPos, !value.pull, injectEnable)
            }
          }
        )
      }))
  }
}
