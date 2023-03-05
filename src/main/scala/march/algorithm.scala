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
  writeStream.haltWhen(checking).throwWhen(True)

  checkStream <-< readStream
  checkStream.throwWhen(True)

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
    opRam: Mem[Bits]
) extends Area {
  val addrCount = U(1 << addrWidth - 1)
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
      to
  }

  val addrPostStream = addrStream.map(p => {
    val to = cloneOf(p)
    to.addr := p.addr
    when(!p.element.isUpDir) {
      to.addr := addrCount - p.addr
    }
    to
  })

  // val opRam = Mem(Bits(2 bits), input.maxOps)

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
        to.opAddr := p.opAddr + id
        to
    }

  val data = opRam.readSync(opStream.opAddr, opStream.fire)
  val output = opStream
    .stage()
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

case class March(maxElements: Int, maxOps: Int, addrWidth: Int)
    extends Component {
  val elemAddrWidth = log2Up(maxElements)
  val opAddrWidth = log2Up(maxOps)
  val opBase = Reg(UInt(opAddrWidth bits)) init (0)
  val input = Stream(Element(maxOps))
  input.payload := input.payload.getZero

  val elemAddr = Counter(maxElements, inc = input.fire)
  val elemRam = Mem(SavedElement(maxOps), maxElements)
  val element = elemRam.readSync(elemAddr, input.valid)

  val elementStream = input
    .stage()
    .map(p => {
      val to = cloneOf(p)
      to.count := element.count
      to.opBase := opBase
      to.isUpDir := element.isUpDir
      to
    })
  when(elementStream.fire) { opBase := opBase + element.count }

  val start = RegInit(True)
  val valid = RegNext(start).clearWhen(element.count === maxOps)
  input.valid := valid

  val opRam = Mem(Bits(2 bits), maxOps)
  val meLogic = MarchElement(addrWidth, elementStream, opRam)

  val accessLogic = Access(meLogic.output)

  val checkLogic = Check(meLogic.output, accessLogic.data)
}
