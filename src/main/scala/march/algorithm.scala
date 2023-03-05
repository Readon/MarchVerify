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
  val hasFault = Vec(Reg(Bool) init (False), log2Up(input.addr.getBitsWidth))

  Vec(writeStream, readStream) <> StreamDemux[Instruction](
    input,
    input.isRead.asUInt,
    2
  )
  writeStream.throwWhen(True)

  val checkStream = cloneOf(readStream)
  checkStream <-< readStream
  checkStream.throwWhen(True)
  val checking = checkStream.fire

  when(checking & data =/= checkStream.value) {
    hasFault(checkStream.addr) := True
  }
}

