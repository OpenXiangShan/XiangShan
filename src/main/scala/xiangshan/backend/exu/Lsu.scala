package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import xiangshan._
import xiangshan.FuType._
import xiangshan.utils._
import xiangshan.backend.regfile.RfWritePort
import utils._
import bus.simplebus._
import noop.AddressSpace

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

  def lr      = "b100010".U
  def sc      = "b100011".U
  def amoswap = "b100001".U
  def amoadd  = "b100000".U
  def amoxor  = "b100100".U
  def amoand  = "b101100".U
  def amoor   = "b101000".U
  def amomin  = "b110000".U
  def amomax  = "b110100".U
  def amominu = "b111000".U
  def amomaxu = "b111100".U
  
  def isStore(func: UInt): Bool = func(3)
  def isAtom(func: UInt): Bool = func(5)

  def atomW = "010".U
  def atomD = "011".U
}

class StoreQueueEntry extends XSBundle{
  val src1  = UInt(XLEN.W)
  val src2  = UInt(XLEN.W)
  val addr  = UInt(XLEN.W)
  val src3  = UInt(XLEN.W)
  val wdata = UInt(XLEN.W)
  val func  = UInt(6.W)
  val pc  = UInt(VAddrBits.W) //for debug
  val brMask = UInt(BrqSize.W) //FIXIT
}

class Lsu extends Exu(
  FuType.ldu.litValue(),
  readIntRf = true,
  readFpRf = true,
  writeIntRf = true,
  writeFpRf = true
) with NeedImpl {
  override def toString: String = "Lsu"

  // store buffer
  val stqData = Reg(Vec(8, new StoreQueueEntry))
  val stqValid = RegInit(VecInit(List.fill(8)(false.B)))
  val stqPtr = Reg(Vec(8, UInt(3.W)))
  val stqHead = RegInit(0.U(3.W))
  val stqTail = stqPtr(0)
  val stqCommited = RegInit(0.U(3.W))
  val stqFull = stqHead === 7.U //stq_valid.reduce(_.valid && _.valid)
  val emptySlot = PriorityMux(~stqValid.asUInt, VecInit(List.tabulate(8)(_.U)))

  // when retiringStore, block all input insts
  val isStoreIn = io.in.valid && LSUOpType.isStore(io.in.bits.uop.ctrl.fuOpType)
  val retiringStore = Wire(Bool()) //RegInit(false.B)
  val (validIn, src1In, src2In, src3In, funcIn) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.src3, io.in.bits.uop.ctrl.fuOpType)
  val (valid, src1, src2, wdata, func) = 
  (
    Mux(retiringStore, stqValid(stqTail), validIn && !isStoreIn),
    Mux(retiringStore, stqData(stqTail).src1, src1In),
    Mux(retiringStore, stqData(stqTail).src2, src2In),
    Mux(retiringStore, stqData(stqTail).src3, src3In),
    Mux(retiringStore, stqData(stqTail).func, funcIn)
  )
  assert(!(retiringStore && !stqValid(stqTail)))

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
  dmem.req.bits.apply(addr = addr, size = size, wdata = genWdata(wdata, size),
    wmask = genWmask(addr, size), cmd = Mux(isStore, SimpleBusCmd.write, SimpleBusCmd.read))
  dmem.req.valid := valid && (state === s_idle)
  dmem.resp.ready := true.B

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

  // pop store queue if insts have been commited and dmem req fired successfully
  val stqDequeue = retiringStore && state === s_partialLoad || !stqValid(stqTail) && stqHead > 0.U
  when(stqDequeue){
    stqValid(stqTail) := false.B
    // update stq ptr
    for(i <- 1 until 8){
      stqPtr(i-1) := stqPtr(i) 
    }
  }

  // if store, add it to store queue
  val stqEnqueue = validIn && isStoreIn && !stqFull && !retiringStore
  when(stqEnqueue){
    stqPtr(stqHead) := emptySlot
    stqData(emptySlot).src1 := src1In
    stqData(emptySlot).src2 := src2In
    stqData(emptySlot).addr := src1In + src2In
    stqData(emptySlot).src3 := src3In
    stqData(emptySlot).pc := io.in.bits.uop.cf.pc
    stqData(emptySlot).func := funcIn
    stqValid(emptySlot) := true.B
  }

  // if store insts have been commited, send dmem req
  retiringStore := stqCommited > 0.U && stqValid(stqTail)

  // update stqTail, stqCommited
  stqCommited := stqCommited + io.scommit - stqDequeue
  stqHead := stqHead + stqEnqueue - stqDequeue

  // Store addr forward match
  // If match, get data from store queue
  val loadResult = Mux(partialLoad, rdataPartialLoad, rdata)
  val dataBackVec = Wire(Vec(XLEN/8, (UInt((XLEN/8).W))))
  for(j <- (0 to (XLEN/8 - 1))){
    dataBackVec(j) := loadResult(8*(j+1)-1, 8*j)
  }
  
  for(i <- 0 until 8){
    when(stqValid(stqPtr(i)) && i.U < stqHead){
      when(addr(PAddrBits-1, log2Up(XLEN/8)) === stqData(stqPtr(i)).addr(PAddrBits-1, log2Up(XLEN/8))){
        for(j <- (0 to (XLEN/8 - 1))){
          when(genWmask(stqData(stqPtr(i)).addr, stqData(stqPtr(i)).func(1, 0))(j)){
            dataBackVec(j) := stqData(stqPtr(i)).src3(8*(j+1)-1, 8*j)
          }
        }
      }
      XSDebug("sbuffer id %d ptr %d pc %x addr %x data %x func %x wmask %b\n", 
        i.U, stqPtr(i), stqData(stqPtr(i)).pc, stqData(stqPtr(i)).src1 + stqData(stqPtr(i)).src2, stqData(stqPtr(i)).src3, stqData(stqPtr(i)).func, genWmask(stqData(stqPtr(i)).addr, stqData(stqPtr(i)).func(1, 0))
      )
    }
  }

  val expRedirect = io.redirect.valid && io.redirect.bits.isException
  val brRedirect = io.redirect.valid && !io.redirect.bits.isException
  for(i <- 0 until 8){
    when(expRedirect || brRedirect && (UIntToOH(io.redirect.bits.brTag) & stqData(i).brMask(i)).orR && stqValid(i)){
      stqValid(i) := false.B
    }
    XSDebug("sptrtable: id %d ptr %d valid  %d\n", i.U, stqPtr(i), stqValid(stqPtr(i)))
  }
  when(expRedirect){
    //invalidate uncommited store
    //FIXME
  }

  io.in.ready := io.out.fire()

  io.out.valid := (!isStoreIn && !retiringStore && Mux(partialLoad, state === s_partialLoad, dmem.resp.fire() && (state === s_wait_resp)) || stqEnqueue) && io.in.valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := dataBackVec.asUInt
  // io.out.bits.debug.isMMIO := AddressSpace.isMMIO(addr) && io.out.valid
  io.out.bits.debug.isMMIO := AddressSpace.isMMIO(addr) //for debug
  when(io.out.fire()){
    XSDebug("LSU fire: pc %x addr %x mmio %x isStoreIn %x retiringStore %x partialLoad %x dmem %x stqEnqueue %x state %x dmemres %x fwdres %x\n",
      io.in.bits.uop.cf.pc,
      addr,
      io.out.bits.debug.isMMIO,
      isStoreIn,
      retiringStore,
      partialLoad,
      dmem.resp.fire(),
      stqEnqueue,
      state,
      loadResult,
      io.out.bits.data
    )
  }

  // debug
  XSDebug("state: %d (valid, ready): in (%d,%d) out (%d,%d)\n", state, io.in.valid, io.in.ready, io.out.valid, io.out.ready)
  XSDebug("stqinfo: stqValid.asUInt %b stqHead %d stqTail %d stqCommited %d emptySlot %d\n", stqValid.asUInt, stqHead, stqTail, stqCommited, emptySlot)
  XSDebug(retiringStore, "retiringStore now...\n")
  XSInfo(io.dmem.req.fire() && io.dmem.req.bits.cmd =/= SimpleBusCmd.write, "[DMEM LOAD  REQ] addr 0x%x wdata 0x%x size %d\n", dmem.req.bits.addr, dmem.req.bits.wdata, dmem.req.bits.size)
  XSInfo(io.dmem.req.fire() && io.dmem.req.bits.cmd === SimpleBusCmd.write, "[DMEM STORE REQ] addr 0x%x wdata 0x%x size %d\n", dmem.req.bits.addr, dmem.req.bits.wdata, dmem.req.bits.size)
  XSInfo(io.dmem.resp.fire(), "[DMEM RESP] data %x\n", rdata)
}
