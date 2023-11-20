package uestsg.march

import spinal.core._
import spinal.lib._

case class Instruction(addrWidth: Int) extends Bundle {
  val value = Bool()
  val isRead = Bool()
  val addr = UInt(addrWidth bit)
}

case class Element(maxOps: Int) extends Bundle {
  val count = UInt(log2Up(maxOps) bits)
  val opBase = UInt(log2Up(maxOps) bits)
  val isUpDir = Bool()
}

case class AddrElement(maxOps: Int, addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth bits)
  val element = Element(maxOps)
}

case class OpElement(maxOps: Int, addrWidth: Int) extends Bundle {
  val addr = UInt(addrWidth bits)
  val opAddr = UInt(log2Up(maxOps) bits)
}

case class Check(input: Stream[Instruction], data: Bool) extends Area {
  val writeStream = cloneOf(input)
  val readStream = cloneOf(input)
  val hasFault = Vec(Reg(Bool) init (False), 1 << input.addr.getBitsWidth)

  Vec(writeStream, readStream) <> StreamDemux[Instruction](
    input,
    input.isRead.asUInt,
    2
  )
  val checkStream = cloneOf(readStream)
  val checking = checkStream.fire
  val termStream = writeStream.haltWhen(checking)
  termStream.ready := True

  checkStream <-< readStream
  checkStream.ready := True

  when(checking & data =/= checkStream.value) {
    hasFault(checkStream.addr) := True
  }
}

case class Access(input: Stream[Instruction]) extends Area {
  val addrWidth = input.addr.getBitsWidth
  val ram = Mem(Bool, 1 << addrWidth)
  val data =
    ram.readWriteSync(input.addr, input.value, input.fire, !input.isRead)
}

case class MarchElement(
    addrWidth: Int,
    input: Stream[Element],
    opRam: Vec[Bits]
) extends Area {
  val addrCount = U((1 << addrWidth) - 1)
  val addrPreStream = input.map(p => {
    val to = new AddrElement(input.maxOps, addrWidth)
    to.addr := 0
    to.element := p
    to
  })

  val addrStream = StreamTransactionExtender(addrPreStream, addrCount) {
    (id, p, _) =>
      val to = cloneOf(p)
      to.addr := id
      to.element := p.element
      to
  }

  val addrPostStream = addrStream.map(p => {
    val to = cloneOf(p)
    to.addr := p.addr
    when(!p.element.isUpDir) {
      to.addr := addrCount - p.addr
    }
    to.element := p.element
    to
  })

  val opPreStream = addrPostStream.map(p => {
    val to = OpElement(p.maxOps, p.addrWidth)
    to.addr := p.addr
    to.opAddr := p.element.opBase
    to
  })
  val opStream =
    StreamTransactionExtender(opPreStream, addrPostStream.element.count) {
      (id, p, _) =>
        val to = cloneOf(p)
        to.addr := p.addr
        to.opAddr := p.opAddr + id
        to
    }

  val data = opRam(opStream.opAddr)
  val output = opStream
    .map(p => {
      val to = new Instruction(p.addrWidth)
      to.addr := p.addr
      to.isRead := data(1)
      to.value := data(0)
      to
    })
}

case class SavedElement(maxOps: Int) extends Bundle {
  val count = UInt(log2Up(maxOps) bits)
  val isUpDir = Bool()
}

object March {
  val W0 = "00"
  val W1 = "01"
  val R0 = "10"
  val R1 = "11"
  val UP = true
  val DOWN = false
}

case class March(
    elements: Seq[(Int, Boolean)],
    ops: Seq[String],
    addrWidth: Int
) extends Component {
  val io = new Bundle {
    val faults = out Bits ((1 << addrWidth) bits)
  }

  val maxElements = elements.length
  val maxOps = ops.length
  val elemAddrWidth = log2Up(maxElements)
  val opAddrWidth = log2Up(maxOps)
  val opBase = Reg(UInt(opAddrWidth bits)) init (0)
  val input = Stream(Element(maxOps))
  input.payload := input.payload.getZero

  val elemAddr = Counter(maxElements, inc = input.fire)
  val elemInit = elements.map {
    case (count, dir) => {
      val elem = SavedElement(maxOps)
      elem.count := U(count)
      elem.isUpDir := Bool(dir)
      elem
    }
  }
  val elemRam = Vec(elemInit)
  val element = elemRam(elemAddr)

  val elementStream = input
    .map(p => {
      val to = cloneOf(p)
      to.count := element.count
      to.opBase := opBase
      to.isUpDir := element.isUpDir
      to
    })
  when(elementStream.fire) { opBase := opBase + element.count + 1 }

  val start = RegInit(False)
  start.setWhen(start === False)
  val endCond = element.count >= maxOps
  val valid = RegNext(start).clearWhen(endCond)
  input.valid := valid & !endCond

  val opInit = ops.map {
    case (op) => {
      assert(op.length == 2)
      B(op)
    }
  }
  val opRam = Vec(opInit)
  val meLogic = MarchElement(addrWidth, elementStream, opRam)

  val accessLogic = Access(meLogic.output)

  val checkLogic = Check(meLogic.output, accessLogic.data)
  io.faults := checkLogic.hasFault.asBits
}

object MarchVerilog {
  def main(args: Array[String]) {
    SpinalVerilog({
      val ops = Array[String]("01", "10", "01")
      val elements = Seq((0, true), (0, false), (3, false))
      new March(elements, ops, 2)
    })
  }
}
