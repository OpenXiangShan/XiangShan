package noop
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._
import bus.simplebus._

object LSUOpType {
  def lb   = "b000000".U
  def lh   = "b000001".U
  def lw   = "b000010".U
  def ld   = "b000011".U
  def lbu  = "b000100".U
  def lhu  = "b000101".U
  def lwu  = "b000110".U
  def sb   = "b001000".U
  def sh   = "b001001".U
  def sw   = "b001010".U
  def sd   = "b001011".U

  def lr      = "b100000".U
  def sc      = "b100001".U
  def amoswap = "b100010".U
  def amoadd  = "b100011".U
  def amoxor  = "b100100".U
  def amoand  = "b100101".U
  def amoor   = "b100110".U
  def amomin  = "b110111".U
  def amomax  = "b110000".U
  def amominu = "b110001".U
  def amomaxu = "b110010".U
  
  def isStore(func: UInt): Bool = func(3)
  def isAtom(func: UInt): Bool = func(5)
  def isLoad(func: UInt): Bool = !isStore(func) & !isAtom(func)

  def atomW = "010".U
  def atomD = "011".U
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(XLEN.W))
  val dmem = new SimpleBusUC
  val isMMIO = Output(Bool())
}

class StoreQueueEntry extends NOOPBundle{
  val src1  = UInt(XLEN.W)
  val src2  = UInt(XLEN.W)
  val wdata = UInt(XLEN.W)
  val func  = UInt(6.W)
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
    val lsExecUnit = Module(new LSExecUnit)
    
    val storeReq = valid & LSUOpType.isStore(func)
    val loadReq = valid  & LSUOpType.isLoad(func)
    val atomReq = valid  & LSUOpType.isAtom(func)

    val atomWidthW = true.B  //TODO: should be funct3 === w
    val atomWidthD = false.B //TODO: should be funct3 === d

    // LSU control FSM state
    val s_idle :: s_load :: s_lr :: s_sc :: s_amo_l :: s_amo_a :: s_amo_s :: Nil = Enum(7)

    // LSU control FSM
    val state = RegInit(s_idle)
    val atomReg = Reg(UInt(XLEN.W))
    
    // StoreQueue
    // TODO: inst fence needs storeQueue to be finished
    val storeQueue = Module(new Queue(new StoreQueueEntry, 4))
    storeQueue.io.enq.valid := state === s_idle && storeReq
    storeQueue.io.enq.bits.src1 := src1
    storeQueue.io.enq.bits.src2 := src2
    storeQueue.io.enq.bits.wdata := io.wdata
    storeQueue.io.enq.bits.func := func
    storeQueue.io.deq.ready := lsExecUnit.io.out.fire()
    
    lsExecUnit.io.in.valid     := false.B
    lsExecUnit.io.out.ready    := DontCare
    lsExecUnit.io.in.bits.src1 := DontCare
    lsExecUnit.io.in.bits.src2 := DontCare
    lsExecUnit.io.in.bits.func := DontCare
    lsExecUnit.io.wdata        := DontCare
    io.out.valid               := false.B
    io.in.ready                := false.B

    switch (state) {
      is(s_idle){
        lsExecUnit.io.in.valid     := Mux(storeQueue.io.deq.valid, storeQueue.io.deq.valid, io.in.valid)
        lsExecUnit.io.out.ready    := io.out.ready 
        lsExecUnit.io.in.bits.src1 := Mux(storeQueue.io.deq.valid, storeQueue.io.deq.bits.src1, src1)
        lsExecUnit.io.in.bits.src2 := Mux(storeQueue.io.deq.valid, storeQueue.io.deq.bits.src2, src2)
        lsExecUnit.io.in.bits.func := Mux(storeQueue.io.deq.valid, storeQueue.io.deq.bits.func, func)
        lsExecUnit.io.wdata        := Mux(storeQueue.io.deq.valid, storeQueue.io.deq.bits.wdata, io.wdata)
        io.in.ready                := Mux(storeReq, storeQueue.io.enq.ready, false.B)
        io.out.valid               := Mux(storeReq, storeQueue.io.enq.ready, false.B)

        when(storeReq){
          state := s_idle
        }
        when(loadReq){
          state := Mux(storeQueue.io.deq.valid, s_idle, s_load)
        }
        when(atomReq){
          state := Mux(storeQueue.io.deq.valid, s_idle, s_amo_l)
        }
      }

      is(s_load){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := io.out.ready 
        lsExecUnit.io.in.bits.src1 := src1
        lsExecUnit.io.in.bits.src2 := src2
        lsExecUnit.io.in.bits.func := func
        lsExecUnit.io.wdata        := DontCare
        io.in.ready                := lsExecUnit.io.out.fire()
        io.out.valid               := lsExecUnit.io.out.valid
        when(lsExecUnit.io.out.fire()){state := s_idle}//load finished
      }

      is(s_amo_l){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := true.B 
        lsExecUnit.io.in.bits.src1 := src1
        lsExecUnit.io.in.bits.src2 := 0.U
        lsExecUnit.io.in.bits.func := Mux(atomWidthD, LSUOpType.ld, LSUOpType.lw)
        lsExecUnit.io.wdata        := DontCare
        io.in.ready                := false.B
        io.out.valid               := false.B
        when(lsExecUnit.io.out.fire()){state := s_amo_a}
        atomReg := lsExecUnit.io.out.bits
      }

      is(s_amo_a){
        lsExecUnit.io.in.valid     := false.B
        lsExecUnit.io.out.ready    := false.B 
        lsExecUnit.io.in.bits.src1 := DontCare
        lsExecUnit.io.in.bits.src2 := DontCare
        lsExecUnit.io.in.bits.func := DontCare
        lsExecUnit.io.wdata        := DontCare
        io.in.ready                := false.B
        io.out.valid               := false.B
        state := s_amo_s
        // atomReg := AtomALU(atomReg, src2, func) [TODO]
      }

      is(s_amo_s){
        lsExecUnit.io.in.valid     := true.B
        lsExecUnit.io.out.ready    := io.out.ready
        lsExecUnit.io.in.bits.src1 := src1
        lsExecUnit.io.in.bits.src2 := 0.U
        lsExecUnit.io.in.bits.func := Mux(atomWidthD, LSUOpType.sd, LSUOpType.sw)
        lsExecUnit.io.wdata        := atomReg
        io.in.ready                := lsExecUnit.io.out.fire()
        io.out.valid               := lsExecUnit.io.out.valid
        when(lsExecUnit.io.out.fire()){state := s_idle}
      }
    }


    // controled by FSM 
    // io.in.ready := lsExecUnit.io.in.ready
    // lsExecUnit.io.wdata := io.wdata
    // io.out.valid := lsExecUnit.io.out.valid 

    io.dmem <> lsExecUnit.io.dmem
    io.out.bits <> lsExecUnit.io.out.bits

    val addr = Mux(atomReq, src1, src1 + src2)
    io.isMMIO := AddressSpace.isMMIO(addr) && io.out.valid
    // io.isMMIO := lsExecUnit.io.isMMIO
}

class LSExecUnit extends NOOPModule {
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
  val partialLoad = !isStore && (func =/= LSUOpType.ld)

  val s_idle :: s_wait_resp :: s_partialLoad :: Nil = Enum(3)
  val state = RegInit(s_idle)

  switch (state) {
    is (s_idle) { when (dmem.req.fire()) { state := Mux(isStore, s_partialLoad, s_wait_resp) } }
    is (s_wait_resp) { when (dmem.resp.fire()) { state := Mux(partialLoad, s_partialLoad, s_idle) } }
    is (s_partialLoad) { state := s_idle }
  }

  val size = func(1,0)
  dmem.req.bits.apply(addr = addr, size = size, wdata = genWdata(io.wdata, size),
    wmask = genWmask(addr, size), cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read))
  dmem.req.valid := valid && (state === s_idle)
  dmem.resp.ready := true.B

  io.out.valid := Mux(isStore || partialLoad, state === s_partialLoad, dmem.resp.fire() && (state === s_wait_resp))
  io.in.ready := (state === s_idle)

  val rdata = dmem.resp.bits.rdata
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
  io.isMMIO := AddressSpace.isMMIO(addr) && io.out.valid

  BoringUtils.addSource(dmem.isRead() && dmem.req.fire(), "perfCntCondMloadInstr")
  BoringUtils.addSource(BoolStopWatch(dmem.isRead(), dmem.resp.fire()), "perfCntCondMloadStall")
  BoringUtils.addSource(BoolStopWatch(dmem.isWrite(), dmem.resp.fire()), "perfCntCondMstoreStall")
  BoringUtils.addSource(io.isMMIO, "perfCntCondMmmioInstr")
}
