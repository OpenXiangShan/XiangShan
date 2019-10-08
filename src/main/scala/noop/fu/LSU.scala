package noop
//TODO(rv64)
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

object LSUOpType {
  def lb   = "b0000".U
  def lh   = "b0001".U
  def lw   = "b0010".U
  def ld   = "b0011".U
  def lbu  = "b0100".U
  def lhu  = "b0101".U
  def lwu  = "b0110".U
  def sb   = "b1000".U
  def sh   = "b1001".U
  def sw   = "b1010".U
  def sd   = "b1011".U

  def isStore(func: UInt): Bool = func(3)
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(XLEN.W))
  val dmem = new SimpleBusUC
  val mmio = new SimpleBusUC
  val isMMIO = Output(Bool())
}

class LSU extends NOOPModule {
  val io = IO(new LSUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
      "b11".U -> 0xff.U //11111111
    )) << addr(2, 0)
  }
  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(8, data(7, 0)),
      "b01".U -> Fill(4, data(15, 0)),
      "b10".U -> Fill(2, data(31, 0)),
      "b11".U -> data
    ))
  }

  val dmem = io.dmem
  val addr = src1 + src2
  val addrLatch = RegNext(addr)
  val isStore = valid && LSUOpType.isStore(func)

  val s_idle :: s_wait_resp :: s_partialLoad :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val mmio = AddressSpace.isMMIO(addr)
  val partialLoad = !isStore && (func =/= LSUOpType.ld)

  switch (state) {
    is (s_idle) { when (valid) {
      when (Mux(mmio, io.mmio.req.fire(), dmem.req.fire())) { state := Mux(isStore && !mmio, s_partialLoad, s_wait_resp) }
    }}
    is (s_wait_resp) {
      when (Mux(mmio, io.mmio.resp.fire(), dmem.resp.fire())) { state := Mux(partialLoad, s_partialLoad, s_idle) }
    }
    is (s_partialLoad) { state := s_idle }
  }

  val size = func(1,0)
  dmem.req.bits.apply(addr = addr, size = size, wdata = genWdata(io.wdata, size),
    wmask = genWmask(addr, size), cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read))
  dmem.req.valid := valid && (state === s_idle) && !mmio
  dmem.resp.ready := true.B

  io.mmio.req.bits := dmem.req.bits
  io.mmio.req.valid := valid && (state === s_idle) && mmio
  io.mmio.resp.ready := true.B

  io.out.valid := Mux(isStore && !mmio, state === s_partialLoad, Mux(partialLoad, state === s_partialLoad,
    Mux(mmio, io.mmio.resp.fire(), dmem.resp.fire() && (state === s_wait_resp))))
  io.in.ready := (state === s_idle)

  val mmioLatch = RegNext(mmio)
  val rdata = Mux(mmioLatch, io.mmio.resp.bits.rdata, dmem.resp.bits.rdata)
  val rdataLatch = RegNext(rdata)
  val rdataSel = LookupTree(addrLatch(2, 0), List(
    "b000".U -> rdataLatch(63, 0),
    "b001".U -> rdataLatch(63, 8),
    "b010".U -> rdataLatch(63, 16),
    "b011".U -> rdataLatch(63, 24),
    "b100".U -> rdataLatch(63, 32),
    "b101".U -> rdataLatch(63, 40),
    "b110".U -> rdataLatch(63, 48),
    "b111".U -> rdataLatch(63, 56)
  ))
  val rdataPartialLoad = LookupTree(func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
      LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  ))

  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata)
  io.isMMIO := mmio && valid

  BoringUtils.addSource(dmem.isRead() && dmem.req.fire(), "perfCntCondMloadInstr")
  BoringUtils.addSource(BoolStopWatch(dmem.isRead(), dmem.resp.fire()), "perfCntCondMloadStall")
  BoringUtils.addSource(BoolStopWatch(dmem.isWrite(), dmem.resp.fire()), "perfCntCondMstoreStall")
  BoringUtils.addSource(io.mmio.req.fire(), "perfCntCondMmmioInstr")
}
