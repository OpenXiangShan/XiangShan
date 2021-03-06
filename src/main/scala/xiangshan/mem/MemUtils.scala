package xiangshan.mem

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.cache._
import xiangshan.backend.fu.FenceToSbuffer

object genWmask {
  def apply(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)).asUInt()
  }
}

object genWdata {
  def apply(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }
}

class LsPipelineBundle extends XSBundle {
  val vaddr = UInt(VAddrBits.W)
  val paddr = UInt(PAddrBits.W)
  val func = UInt(6.W) //fixme???
  val mask = UInt(8.W)
  val data = UInt((XLEN+1).W)
  val uop = new MicroOp

  val miss = Bool()
  val tlbMiss = Bool()
  val ptwBack = Bool()
  val mmio = Bool()
  val rsIdx = UInt(log2Up(IssQueSize).W)

  val forwardMask = Vec(8, Bool())
  val forwardData = Vec(8, UInt(8.W))
}

class LoadForwardQueryIO extends XSBundle {
  val paddr = Output(UInt(PAddrBits.W))
  val mask = Output(UInt(8.W))
  val uop = Output(new MicroOp) // for replay
  val pc = Output(UInt(VAddrBits.W)) //for debug
  val valid = Output(Bool()) //for debug

  val forwardMask = Input(Vec(8, Bool()))
  val forwardData = Input(Vec(8, UInt(8.W)))

  // val lqIdx = Output(UInt(LoadQueueIdxWidth.W))
  val sqIdx = Output(new SqPtr)
}

class MaskedLoadForwardQueryIO extends XSBundle {
  val paddr = Output(UInt(PAddrBits.W))
  val mask = Output(UInt(8.W))
  val uop = Output(new MicroOp) // for replay
  val pc = Output(UInt(VAddrBits.W)) //for debug
  val valid = Output(Bool()) //for debug

  val forwardMask = Input(Vec(8, Bool()))
  val forwardData = Input(Vec(8, UInt(8.W)))

  val sqIdx = Output(new SqPtr) // for debug
  // sqIdxMask is calcuated in earlier stage for better timing
  val sqIdxMask = Output(UInt(StoreQueueSize.W))
}
