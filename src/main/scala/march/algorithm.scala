package uestsg.march

import spinal.core._
import spinal.lib._

case class Instruction(addrWidth: Int) extends Bundle {
  val value = Bool()
  val isRead = Bool()
  val addr = UInt(addrWidth bit)
}

case class Check[T <: Instruction](input: Stream[Instruction], data: Bool)
    extends Area {
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

