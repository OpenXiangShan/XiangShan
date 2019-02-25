package noop

import chisel3._
import chisel3.util._

import utils._
import memory.SimpleBus

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
  val isLoad = Output(Bool())
  val loadStall = Output(Bool())
  val storeStall = Output(Bool())
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
  val isStore = valid && funcIsStore(func)

  val s_idle :: s_wait_resp :: Nil = Enum(2)
  val state = RegInit(s_idle)

  switch (state) {
    is (s_idle) {
      when (dmem.a.fire()) { state := Mux(isStore || dmem.r.fire(), s_idle, s_wait_resp) }
    }

    is (s_wait_resp) {
      when (dmem.r.fire()) { state := s_idle }
    }
  }

  dmem.a.bits.addr := addr
  dmem.a.bits.size := func(1, 0)
  dmem.a.valid := valid && (state === s_idle)
  dmem.w.valid := isStore
  dmem.w.bits.data := genWdata(io.wdata, func(1, 0))
  dmem.w.bits.mask := genWmask(addr, func(1, 0))
  dmem.r.ready := true.B

  io.out.valid := Mux(isStore, dmem.a.fire(), dmem.r.fire())
  io.in.ready := (state === s_idle)

  val rdataFromBus = io.dmem.r.bits.data
  val rdata = LookupTree(addr(1, 0), List(
    "b00".U -> rdataFromBus,
    "b01".U -> rdataFromBus(15, 8),
    "b10".U -> rdataFromBus(31, 16),
    "b11".U -> rdataFromBus(31, 24)
  ))
  io.out.bits := LookupTree(func, List(
      LsuLb   -> Cat(Fill(24, rdata(7)), rdata(7, 0)),
      LsuLh   -> Cat(Fill(16, rdata(15)), rdata(15, 0)),
      LsuLw   -> rdata,
      LsuLbu  -> Cat(0.U(24.W), rdata(7, 0)),
      LsuLhu  -> Cat(0.U(16.W), rdata(15, 0))
  ))

  // perfcnt
  io.isLoad := io.out.fire() && isStore
  io.loadStall := BoolStopWatch(dmem.a.valid && !isStore, dmem.r.fire())
  io.storeStall := BoolStopWatch(dmem.a.valid && isStore, dmem.a.fire())
}
