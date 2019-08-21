package noop

import chisel3._
import chisel3.util._

import utils._
import bus.simplebus.SimpleBus

trait HasLSUOpType {
  val LsuOpTypeNum  = 10

  def LsuLb   = "b0000".U
  def LsuLh   = "b0001".U
  def LsuLw   = "b0010".U
  def LsuLbu  = "b0100".U
  def LsuLhu  = "b0101".U
  def LsuSb   = "b1000".U
  def LsuSh   = "b1001".U
  def LsuSw   = "b1010".U

  def funcIsStore(func: UInt): Bool = func(3)
}

object LSUInstr extends HasDecodeConst {
  def LB      = BitPat("b????????????_?????_000_?????_0000011")
  def LH      = BitPat("b????????????_?????_001_?????_0000011")
  def LW      = BitPat("b????????????_?????_010_?????_0000011")
  def LBU     = BitPat("b????????????_?????_100_?????_0000011")
  def LHU     = BitPat("b????????????_?????_101_?????_0000011")
  def SB      = BitPat("b???????_?????_?????_000_?????_0100011")
  def SH      = BitPat("b???????_?????_?????_001_?????_0100011")
  def SW      = BitPat("b???????_?????_?????_010_?????_0100011")

  val table = Array(
    LB             -> List(InstrI, FuLsu, LsuLb ),
    LH             -> List(InstrI, FuLsu, LsuLh ),
    LW             -> List(InstrI, FuLsu, LsuLw ),
    LBU            -> List(InstrI, FuLsu, LsuLbu),
    LHU            -> List(InstrI, FuLsu, LsuLhu),
    SB             -> List(InstrS, FuLsu, LsuSb ),
    SH             -> List(InstrS, FuLsu, LsuSh ),
    SW             -> List(InstrS, FuLsu, LsuSw)
  )
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(32.W))
  val dmem = new SimpleBus
  val mmio = new SimpleBus
  val isMMIO = Output(Bool())
}

class LSU extends Module with HasLSUOpType {
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
      "b00".U -> 0x1.U,
      "b01".U -> 0x3.U,
      "b10".U -> 0xf.U
    )) << addr(1, 0)
  }
  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(4, data(7, 0)),
      "b01".U -> Fill(2, data(15, 0)),
      "b10".U -> data
    ))
  }

  val dmem = io.dmem
  val addr = src1 + src2
  val addrLatch = RegNext(addr)
  val isStore = valid && funcIsStore(func)

  val s_idle :: s_wait_resp :: s_partialLoad :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val mmio = AddressSpace.isMMIO(addr)
  val partialLoad = !isStore && (func =/= LsuLw)

  switch (state) {
    is (s_idle) { when (valid) {
      when (Mux(mmio, io.mmio.req.fire(), dmem.req.fire())) { state := Mux(isStore && !mmio, s_partialLoad, s_wait_resp) }
    }}
    is (s_wait_resp) {
      when (Mux(mmio, io.mmio.resp.fire(), dmem.resp.fire())) { state := Mux(partialLoad, s_partialLoad, s_idle) }
    }
    is (s_partialLoad) { state := s_idle }
  }

  dmem.req.bits.addr := addr
  dmem.req.bits.size := func(1, 0)
  dmem.req.valid := valid && (state === s_idle) && !mmio
  dmem.req.bits.wen := isStore
  dmem.req.bits.wdata := genWdata(io.wdata, func(1, 0))
  dmem.req.bits.wmask := genWmask(addr, func(1, 0))
  dmem.resp.ready := true.B

  io.mmio.req.bits := dmem.req.bits
  io.mmio.req.valid := valid && (state === s_idle) && mmio
  io.mmio.resp.ready := true.B

  io.out.valid := Mux(isStore && !mmio, state === s_partialLoad, Mux(partialLoad, state === s_partialLoad,
    Mux(mmio, io.mmio.resp.fire(), dmem.resp.fire() && (state === s_wait_resp))))
  io.in.ready := (state === s_idle)

  val rdata = Mux(mmio, io.mmio.resp.bits.rdata, dmem.resp.bits.rdata)
  val rdataLatch = RegNext(rdata)
  val rdataSel = LookupTree(addrLatch(1, 0), List(
    "b00".U -> rdataLatch,
    "b01".U -> rdataLatch(15, 8),
    "b10".U -> rdataLatch(31, 16),
    "b11".U -> rdataLatch(31, 24)
  ))
  val rdataPartialLoad = LookupTree(func, List(
      LsuLb   -> Cat(Fill(24, rdataSel(7)), rdataSel(7, 0)),
      LsuLh   -> Cat(Fill(16, rdataSel(15)), rdataSel(15, 0)),
      LsuLbu  -> Cat(0.U(24.W), rdataSel(7, 0)),
      LsuLhu  -> Cat(0.U(16.W), rdataSel(15, 0))
  ))

  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata)
  io.isMMIO := mmio && valid
}
