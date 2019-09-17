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
  def lw   = "b0111".U
  def ld   = "b0010".U
  def lbu  = "b0100".U
  def lhu  = "b0101".U
  def lwu  = "b0110".U
  def sb   = "b1000".U
  def sh   = "b1001".U
  def sw   = "b1010".U
  def sd   = "b1011".U

  def isStore(func: UInt): Bool = func(3)
}

object LSUInstr extends HasInstrType {
  def LB      = BitPat("b????????????_?????_000_?????_0000011")
  def LH      = BitPat("b????????????_?????_001_?????_0000011")
  def LW      = BitPat("b????????????_?????_010_?????_0000011")
  def LBU     = BitPat("b????????????_?????_100_?????_0000011")
  def LHU     = BitPat("b????????????_?????_101_?????_0000011")
  def SB      = BitPat("b???????_?????_?????_000_?????_0100011")
  def SH      = BitPat("b???????_?????_?????_001_?????_0100011")
  def SW      = BitPat("b???????_?????_?????_010_?????_0100011")
  def LWU     = BitPat("b???????_?????_?????_110_?????_0000011")
  def LD      = BitPat("b???????_?????_?????_011_?????_0000011")
  def SD      = BitPat("b???????_?????_?????_011_?????_0100011")

  val table = Array(
    LB             -> List(InstrI, FuType.lsu, LSUOpType.lb ),
    LH             -> List(InstrI, FuType.lsu, LSUOpType.lh ),
    LW             -> List(InstrI, FuType.lsu, LSUOpType.lw ),
    LD             -> List(InstrI, FuType.lsu, LSUOpType.ld ),
    LBU            -> List(InstrI, FuType.lsu, LSUOpType.lbu),
    LHU            -> List(InstrI, FuType.lsu, LSUOpType.lhu),
    LWU            -> List(InstrI, FuType.lsu, LSUOpType.lwu),
    SB             -> List(InstrS, FuType.lsu, LSUOpType.sb ),
    SH             -> List(InstrS, FuType.lsu, LSUOpType.sh ),
    SW             -> List(InstrS, FuType.lsu, LSUOpType.sw),
    SD             -> List(InstrS, FuType.lsu, LSUOpType.sd)
  )
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(64.W))
  val dmem = new SimpleBusUH
  val mmio = new SimpleBusUL
  val isMMIO = Output(Bool())
}

class LSU extends Module {
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

  dmem.req.bits.addr := addr
  dmem.req.bits.burst := false.B
  dmem.req.bits.size := func(2, 0)
  dmem.req.valid := valid && (state === s_idle) && !mmio
  dmem.req.bits.cmd := Mux(isStore, SimpleBusCmd.cmdWrite, SimpleBusCmd.cmdRead)
  dmem.req.bits.wdata := genWdata(io.wdata, func(2, 0))
  dmem.req.bits.wmask := genWmask(addr, func(2, 0))
  dmem.req.bits.wlast := true.B
  dmem.req.bits.user := 0.U
  dmem.resp.ready := true.B

  io.mmio.req.bits := dmem.asInstanceOf[SimpleBusUL].req.bits
  io.mmio.req.valid := valid && (state === s_idle) && mmio
  io.mmio.resp.ready := true.B

  Debug(true){
    // when(isStore && (dmem.req.bits.wdata(31,0) === "h00003f00".U)){
    //   printf("TIME %d addr: %x dmem.req.bits.wdata %x, dmem.req.bits.wmask %x\n", GTimer(), addr, dmem.req.bits.wdata, dmem.req.bits.wmask)
    // }
    // when(isStore && (dmem.req.bits.wdata(31,0) === "h8018b120".U)){
    //   printf("TIME %d addr: %x dmem.req.bits.wdata %x, dmem.req.bits.wmask %x\n", GTimer(), addr, dmem.req.bits.wdata, dmem.req.bits.wmask)
    // }

    // when(isStore && (addr(31,0) === "h40600000".U)){
    //   printf("TIME %d addr: %x dmem.req.bits.wdata %x, dmem.req.bits.wmask %x im %x\n", GTimer(), addr, dmem.req.bits.wdata, dmem.req.bits.wmask, mmio)
    // }

  }

  Debug(){
    when(dmem.req.fire()){
      printf("[LSU] (req) addr:%x data:%x wen:%b\n",addr, dmem.req.bits.wdata, isStore)
    } 
  
  
    when(dmem.resp.fire()){
      printf("[LSU] (resp) addr:%x data:%x wen:%b\n",addr, io.out.bits, isStore)
      // printf("%x\n", rdata)
    }
  
    when(io.mmio.req.fire()){
      printf("[LSU] (mmio req) addr:%x data:%x wen:%b\n",addr, dmem.req.bits.wdata, isStore)
    } 
  
  
    when(io.mmio.resp.fire()){
      printf("[LSU] (mmio resp) addr:%x data:%x wen:%b\n",addr, io.out.bits, isStore)
      // printf("%x\n", rdata)
    }
  
    when(state===s_partialLoad){
      printf("[LSU] (partialLoad) addr:%x data:%x wen:%b\n",addr, io.out.bits, isStore)
    }
  }
  
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
      LSUOpType.lb   -> Cat(Fill(24+32, rdataSel(7)), rdataSel(7, 0)),
      LSUOpType.lh   -> Cat(Fill(16+32, rdataSel(15)), rdataSel(15, 0)),
      LSUOpType.lw   -> Cat(Fill(32, rdataSel(31)), rdataSel(31, 0)),
      LSUOpType.lbu  -> Cat(0.U((24+32).W), rdataSel(7, 0)),
      LSUOpType.lhu  -> Cat(0.U((16+32).W), rdataSel(15, 0)),
      LSUOpType.lwu  -> Cat(0.U((32).W), rdataSel(31, 0))
  ))

  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata)
  io.isMMIO := mmio && valid

  BoringUtils.addSource(dmem.isRead() && dmem.req.fire(), "perfCntCondMloadInstr")
  BoringUtils.addSource(BoolStopWatch(dmem.isRead(), dmem.resp.fire()), "perfCntCondMloadStall")
  BoringUtils.addSource(BoolStopWatch(dmem.isWrite(), dmem.resp.fire()), "perfCntCondMstoreStall")
  BoringUtils.addSource(io.mmio.req.fire(), "perfCntCondMmmioInstr")
}
