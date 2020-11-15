package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils._
import xiangshan.cache._

trait HasIFUConst { this: XSModule =>
  val resetVector = 0x80000000L//TODO: set reset vec
  val groupAlign = log2Up(FetchWidth * 4 * 2)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  // each 1 bit in mask stands for 2 Bytes
  def mask(pc: UInt): UInt = (Fill(PredictWidth * 2, 1.U(1.W)) >> pc(groupAlign - 1, 1))(PredictWidth - 1, 0)
  def snpc(pc: UInt): UInt = pc + (PopCount(mask(pc)) << 1)
  
  val IFUDebug = true
}

class GlobalHistoryInfo() extends XSBundle {
  val sawNTBr = Bool()
  val takenOnBr = Bool()
  val saveHalfRVI = Bool()
  def shifted = takenOnBr || sawNTBr
  def newPtr(ptr: UInt) = Mux(shifted, ptr - 1.U, ptr)
  implicit val name = "IFU"
  def debug = XSDebug("[GHInfo] sawNTBr=%d, takenOnBr=%d, saveHalfRVI=%d\n", sawNTBr, takenOnBr, saveHalfRVI)
  // override def toString(): String = "histPtr=%d, sawNTBr=%d, takenOnBr=%d, saveHalfRVI=%d".format(histPtr, sawNTBr, takenOnBr, saveHalfRVI)
}

class IFUIO extends XSBundle
{
  val fetchPacket = DecoupledIO(new FetchPacket)
  val redirect = Flipped(ValidIO(new Redirect))
  val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val icacheReq = DecoupledIO(new ICacheReq)
  val icacheResp = Flipped(DecoupledIO(new ICacheResp))
  val icacheFlush = Output(UInt(2.W))
  // val loopBufPar = Flipped(new LoopBufferParameters)
}

class IFU extends XSModule with HasIFUConst
{
  val io = IO(new IFUIO)
  val bpu = BPU(EnableBPU)
  val pd = Module(new PreDecode)
  val loopBuffer = if(EnableLB) { Module(new LoopBuffer) } else { Module(new FakeLoopBuffer) }

  val if2_redirect, if3_redirect, if4_redirect = WireInit(false.B)
  val if1_flush, if2_flush, if3_flush, if4_flush = WireInit(false.B)

  val loopBufPar = loopBuffer.io.loopBufPar
  val inLoop = WireInit(loopBuffer.io.out.valid)
  val icacheResp = WireInit(Mux(inLoop, loopBuffer.io.out.bits, io.icacheResp.bits))

  if4_flush := io.redirect.valid || loopBufPar.LBredirect.valid
  if3_flush := if4_flush || if4_redirect
  if2_flush := if3_flush || if3_redirect
  if1_flush := if2_flush || if2_redirect

  loopBuffer.io.flush := io.redirect.valid

  //********************** IF1 ****************************//
  val if1_valid = !reset.asBool && GTimer() > 500.U
  val if1_npc = WireInit(0.U(VAddrBits.W))
  val if2_ready = WireInit(false.B)
  val if1_fire = if1_valid && (if2_ready || if1_flush) && (inLoop || io.icacheReq.ready)


  val if1_histPtr, if2_histPtr, if3_histPtr, if4_histPtr = Wire(UInt(log2Up(ExtHistoryLength).W))
  val if2_newPtr, if3_newPtr, if4_newPtr = Wire(UInt(log2Up(ExtHistoryLength).W))

  val extHist = RegInit(VecInit(Seq.fill(ExtHistoryLength)(0.U(1.W))))
  val shiftPtr = WireInit(false.B)
  val newPtr = Wire(UInt(log2Up(ExtHistoryLength).W))
  val ptr = Mux(shiftPtr, newPtr, if1_histPtr)
  val hist = Wire(Vec(HistoryLength, UInt(1.W)))
  for (i <- 0 until HistoryLength) {
    hist(i) := extHist(ptr + i.U)
  }

  shiftPtr := false.B
  newPtr := if1_histPtr

  def wrapGHInfo(bp: BranchPrediction) = {
    val ghi = Wire(new GlobalHistoryInfo())
    ghi.sawNTBr     := bp.hasNotTakenBrs
    ghi.takenOnBr   := bp.takenOnBr
    ghi.saveHalfRVI := bp.saveHalfRVI
    ghi
  }

  //********************** IF2 ****************************//
  val if2_valid = RegEnable(next = if1_valid, init = false.B, enable = if1_fire)
  val if3_ready = WireInit(false.B)
  val if2_fire = if2_valid && if3_ready && !if2_flush
  val if2_pc = RegEnable(next = if1_npc, init = resetVector.U, enable = if1_fire)
  val if2_snpc = snpc(if2_pc)
  val if2_predHistPtr = RegEnable(ptr, enable=if1_fire)
  if2_ready := if2_fire || !if2_valid || if2_flush
  when (if2_flush) { if2_valid := if1_fire }
  .elsewhen (if1_fire) { if2_valid := if1_valid }
  .elsewhen (if2_fire) { if2_valid := false.B }

  when (RegNext(reset.asBool) && !reset.asBool) {
    if1_npc := resetVector.U(VAddrBits.W)
  }.elsewhen (if2_fire) {
    if1_npc := if2_snpc
  }.otherwise {
    if1_npc := RegNext(if1_npc)
  }

  val if2_bp = bpu.io.out(0).bits
  // if taken, bp_redirect should be true
  // when taken on half RVI, we suppress this redirect signal
  if2_redirect := if2_fire && bpu.io.out(0).valid && if2_bp.redirect && !if2_bp.saveHalfRVI
  when (if2_redirect) {
    if1_npc := if2_bp.target
  }

  val if2_GHInfo = wrapGHInfo(if2_bp)

  when (if2_fire && if2_GHInfo.shifted) {
    shiftPtr := true.B
    newPtr := if2_newPtr
  }
  when (if2_GHInfo.shifted && if2_newPtr >= ptr) {
    hist(if2_newPtr-ptr) := if2_GHInfo.takenOnBr.asUInt
  }



  //********************** IF3 ****************************//
  val if3_valid = RegEnable(next = if2_valid, init = false.B, enable = if2_fire)
  val if4_ready = WireInit(false.B)
  val if3_fire = if3_valid && if4_ready && (inLoop || io.icacheResp.valid) && !if3_flush
  val if3_pc = RegEnable(if2_pc, if2_fire)
  val if3_predHistPtr = RegEnable(if2_predHistPtr, enable=if2_fire)
  if3_ready := if3_fire || !if3_valid || if3_flush
  when (if3_flush) { if3_valid := false.B }
  .elsewhen (if2_fire) { if3_valid := if2_valid }
  .elsewhen (if3_fire) { if3_valid := false.B }

  val if3_bp = bpu.io.out(1).bits

  val if3_GHInfo = wrapGHInfo(if3_bp)

  class PrevHalfInstr extends Bundle {
    val valid = Bool()
    val taken = Bool()
    val ghInfo = new GlobalHistoryInfo()
    val fetchpc = UInt(VAddrBits.W) // only for debug
    val idx = UInt(VAddrBits.W) // only for debug
    val pc = UInt(VAddrBits.W)
    val target = UInt(VAddrBits.W)
    val instr = UInt(16.W)
    val ipf = Bool()
    val newPtr = UInt(log2Up(ExtHistoryLength).W)
  }

  val if3_prevHalfInstr = RegInit(0.U.asTypeOf(new PrevHalfInstr))
  val if4_prevHalfInstr = Wire(new PrevHalfInstr)
  // 32-bit instr crosses 2 pages, and the higher 16-bit triggers page fault
  val crossPageIPF = WireInit(false.B)
  when (if4_prevHalfInstr.valid) {
    if3_prevHalfInstr := if4_prevHalfInstr
  }
  val prevHalfInstr = Mux(if4_prevHalfInstr.valid, if4_prevHalfInstr, if3_prevHalfInstr)

  // the previous half of RVI instruction waits until it meets its last half
  val if3_hasPrevHalfInstr = prevHalfInstr.valid && (prevHalfInstr.pc + 2.U) === if3_pc
  // set to invalid once consumed or redirect from backend
  val prevHalfConsumed = if3_hasPrevHalfInstr && if3_fire || if4_flush
  when (prevHalfConsumed) {
    if3_prevHalfInstr.valid := false.B
  }

  // when bp signal a redirect, we distinguish between taken and not taken
  // if taken and saveHalfRVI is true, we do not redirect to the target
  if3_redirect := if3_fire && bpu.io.out(1).valid && (if3_hasPrevHalfInstr && prevHalfInstr.taken || if3_bp.redirect && (if3_bp.taken && !if3_bp.saveHalfRVI || !if3_bp.taken) )

  when (if3_redirect) {
    when (!(if3_hasPrevHalfInstr && prevHalfInstr.taken)) {
      if1_npc := if3_bp.target
      when (if3_GHInfo.shifted){
        shiftPtr := true.B
        newPtr := if3_newPtr
      }
    }
  }

  // when it does not redirect, we still need to modify hist(wire)
  when(if3_GHInfo.shifted && if3_newPtr >= ptr) {
    hist(if3_newPtr-ptr) := if3_GHInfo.takenOnBr
  }
  when (if3_hasPrevHalfInstr && prevHalfInstr.ghInfo.shifted && prevHalfInstr.newPtr >= ptr) {
    hist(prevHalfInstr.newPtr-ptr) := prevHalfInstr.ghInfo.takenOnBr
  }

  //********************** IF4 ****************************//
  val if4_pd = RegEnable(pd.io.out, if3_fire)
  val if4_ipf = RegEnable(icacheResp.ipf || if3_hasPrevHalfInstr && prevHalfInstr.ipf, if3_fire)
  val if4_crossPageIPF = RegEnable(crossPageIPF, if3_fire)
  val if4_valid = RegInit(false.B)
  val if4_fire = if4_valid && io.fetchPacket.ready
  val if4_pc = RegEnable(if3_pc, if3_fire)

  val if4_predHistPtr = RegEnable(if3_predHistPtr, enable=if3_fire)
  if4_ready := (if4_fire || !if4_valid || if4_flush) && GTimer() > 500.U
  when (if4_flush)     { if4_valid := false.B }
  .elsewhen (if3_fire) { if4_valid := if3_valid }
  .elsewhen(if4_fire)  { if4_valid := false.B }

  val if4_bp = Wire(new BranchPrediction)
  if4_bp := bpu.io.out(2).bits

  val if4_GHInfo = wrapGHInfo(if4_bp)

  val if4_cfi_jal = if4_pd.instrs(if4_bp.jmpIdx)
  val if4_cfi_jal_tgt = if4_pd.pc(if4_bp.jmpIdx) + Mux(if4_pd.pd(if4_bp.jmpIdx).isRVC,
    SignExt(Cat(if4_cfi_jal(12), if4_cfi_jal(8), if4_cfi_jal(10, 9), if4_cfi_jal(6), if4_cfi_jal(7), if4_cfi_jal(2), if4_cfi_jal(11), if4_cfi_jal(5, 3), 0.U(1.W)), XLEN),
    SignExt(Cat(if4_cfi_jal(31), if4_cfi_jal(19, 12), if4_cfi_jal(20), if4_cfi_jal(30, 21), 0.U(1.W)), XLEN))
  if4_bp.target := Mux(if4_pd.pd(if4_bp.jmpIdx).isJal && if4_bp.taken, if4_cfi_jal_tgt, bpu.io.out(2).bits.target)
  if4_bp.redirect := bpu.io.out(2).bits.redirect || if4_pd.pd(if4_bp.jmpIdx).isJal && if4_bp.taken && if4_cfi_jal_tgt =/= bpu.io.out(2).bits.target

  if4_prevHalfInstr := 0.U.asTypeOf(new PrevHalfInstr)
  when (bpu.io.out(2).valid && if4_fire && if4_bp.saveHalfRVI) {
    if4_prevHalfInstr.valid := true.B
    if4_prevHalfInstr.taken := if4_bp.taken
    if4_prevHalfInstr.ghInfo := if4_GHInfo
    // Make sure shifted can work
    if4_prevHalfInstr.ghInfo.saveHalfRVI := false.B
    if4_prevHalfInstr.newPtr := if4_newPtr
    if4_prevHalfInstr.fetchpc := if4_pc
    if4_prevHalfInstr.idx := PopCount(mask(if4_pc)) - 1.U
    if4_prevHalfInstr.pc := if4_pd.pc(if4_prevHalfInstr.idx)
    if4_prevHalfInstr.target := if4_bp.target
    if4_prevHalfInstr.instr := if4_pd.instrs(if4_prevHalfInstr.idx)(15, 0)
    if4_prevHalfInstr.ipf := if4_ipf
  }

  // Redirect and npc logic for if4
  when (bpu.io.out(2).valid && if4_fire && if4_bp.redirect) {
    if4_redirect := true.B
    when (if4_bp.saveHalfRVI) {
      if1_npc := snpc(if4_pc)
    }.otherwise {
      if1_npc := if4_bp.target
    }
  }

  // This should cover the if4 redirect to snpc when saveHalfRVI
  when (if3_redirect) {
    when (if3_hasPrevHalfInstr && prevHalfInstr.taken) {
      if1_npc := prevHalfInstr.target
    }
  }

  // history logic for if4
  when (bpu.io.out(2).valid && if4_fire && if4_bp.redirect) {
    shiftPtr := true.B
    newPtr := if4_newPtr
  }

  when (if4_GHInfo.shifted && if4_newPtr >= ptr) {
    hist(if4_newPtr-ptr) := if4_GHInfo.takenOnBr
  }

  when (if3_redirect) {
    // when redirect and if3_hasPrevHalfInstr, this prevHalfInstr should only be taken
    when (if3_hasPrevHalfInstr && prevHalfInstr.ghInfo.shifted) {
      shiftPtr := true.B
      newPtr := prevHalfInstr.newPtr
      extHist(prevHalfInstr.newPtr) := prevHalfInstr.ghInfo.takenOnBr
    }
  }

  // modify GHR at the end of a prediction lifetime
  when (if4_fire && if4_GHInfo.shifted) {
    extHist(if4_newPtr) := if4_GHInfo.takenOnBr
  }

  // This is a histPtr which is only modified when a prediction
  // is sent, so that it can get the final prediction info
  val finalPredHistPtr = RegInit(0.U(log2Up(ExtHistoryLength).W))
  if4_histPtr := finalPredHistPtr
  if4_newPtr  := if3_histPtr
  when (if4_fire && if4_GHInfo.shifted) {
    finalPredHistPtr := if4_newPtr
  }

  if3_histPtr := Mux(if4_GHInfo.shifted && if4_valid && !if4_flush, if4_histPtr - 1.U, if4_histPtr)
  if3_newPtr  := if2_histPtr

  if2_histPtr := Mux(if3_GHInfo.shifted && if3_valid && !if3_flush, if3_histPtr - 1.U, if3_histPtr)
  if2_newPtr  := if1_histPtr

  if1_histPtr := Mux(if2_GHInfo.shifted && if2_valid && !if2_flush, if2_histPtr - 1.U, if2_histPtr)




  when (io.outOfOrderBrInfo.valid && io.outOfOrderBrInfo.bits.isMisPred) {
    val b = io.outOfOrderBrInfo.bits
    val oldPtr = b.brInfo.histPtr
    shiftPtr := true.B
    when (!b.pd.isBr && !b.brInfo.sawNotTakenBranch) {
      // If mispredicted cfi is not a branch,
      // and there wasn't any not taken branch before it,
      // we should only recover the pointer to an unshifted state
      newPtr := oldPtr
      finalPredHistPtr := oldPtr
    }.otherwise {
      newPtr := oldPtr - 1.U
      finalPredHistPtr := oldPtr - 1.U
      hist(0) := Mux(b.pd.isBr, b.taken, 0.U)
      extHist(newPtr) := Mux(b.pd.isBr, b.taken, 0.U)
    }
  }

  when (loopBufPar.LBredirect.valid) {
    if1_npc := loopBufPar.LBredirect.bits
  }

  when (io.redirect.valid) {
    if1_npc := io.redirect.bits.target
  }

  when(inLoop) {
    io.icacheReq.valid := if4_flush
  }.otherwise {
    io.icacheReq.valid := if1_valid && if2_ready
  }
  io.icacheResp.ready := if4_ready
  io.icacheReq.bits.addr := if1_npc

  // when(if4_bp.taken) {
  //   when(if4_bp.saveHalfRVI) {
  //     io.loopBufPar.LBReq := snpc(if4_pc)
  //   }.otherwise {
  //     io.loopBufPar.LBReq := if4_bp.target
  //   }
  // }.otherwise {
  //   io.loopBufPar.LBReq := snpc(if4_pc)
  //   XSDebug(p"snpc(if4_pc)=${Hexadecimal(snpc(if4_pc))}\n")
  // }
  loopBufPar.fetchReq := if3_pc
  
  io.icacheReq.bits.mask := mask(if1_npc)
  
  io.icacheFlush := Cat(if3_flush, if2_flush)

  val inOrderBrHist = Wire(Vec(HistoryLength, UInt(1.W)))
  (0 until HistoryLength).foreach(i => inOrderBrHist(i) := extHist(i.U + io.inOrderBrInfo.bits.brInfo.predHistPtr))
  bpu.io.inOrderBrInfo.valid := io.inOrderBrInfo.valid
  bpu.io.inOrderBrInfo.bits := BranchUpdateInfoWithHist(io.inOrderBrInfo.bits, inOrderBrHist.asUInt)
  bpu.io.outOfOrderBrInfo.valid := io.outOfOrderBrInfo.valid
  bpu.io.outOfOrderBrInfo.bits := BranchUpdateInfoWithHist(io.outOfOrderBrInfo.bits, inOrderBrHist.asUInt) // Dont care about hist

  // bpu.io.flush := Cat(if4_flush, if3_flush, if2_flush)
  bpu.io.flush := VecInit(if2_flush, if3_flush, if4_flush)
  bpu.io.cacheValid := (inLoop || io.icacheResp.valid)
  bpu.io.in.valid := if1_fire
  bpu.io.in.bits.pc := if1_npc
  bpu.io.in.bits.hist := hist.asUInt
  bpu.io.in.bits.histPtr := ptr
  bpu.io.in.bits.inMask := mask(if1_npc)
  bpu.io.out(0).ready := if2_fire
  bpu.io.out(1).ready := if3_fire
  bpu.io.out(2).ready := if4_fire
  bpu.io.predecode.valid := if4_valid
  bpu.io.predecode.bits.mask := if4_pd.mask
  bpu.io.predecode.bits.pd := if4_pd.pd
  bpu.io.predecode.bits.isFetchpcEqualFirstpc := if4_pc === if4_pd.pc(0)
  bpu.io.branchInfo.ready := if4_fire

  pd.io.in := icacheResp
  when(inLoop) {
    pd.io.in.mask := loopBuffer.io.out.bits.mask & mask(loopBuffer.io.out.bits.pc) // TODO: Maybe this is unnecessary
    // XSDebug("Fetch from LB\n")
    // XSDebug(p"pc=${Hexadecimal(io.loopBufPar.LBResp.pc)}\n")
    // XSDebug(p"data=${Hexadecimal(io.loopBufPar.LBResp.data)}\n")
    // XSDebug(p"mask=${Hexadecimal(io.loopBufPar.LBResp.mask)}\n")
  }

  pd.io.prev.valid := if3_hasPrevHalfInstr
  pd.io.prev.bits := prevHalfInstr.instr
  // if a fetch packet triggers page fault, set the pf instruction to nop
  when (!if3_hasPrevHalfInstr && icacheResp.ipf) {
    val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
    (0 until FetchWidth).foreach(i => instrs(i) := ZeroExt("b0010011".U, 32)) // nop
    pd.io.in.data := instrs.asUInt
  }.elsewhen (if3_hasPrevHalfInstr && (prevHalfInstr.ipf || icacheResp.ipf)) {
    pd.io.prev.bits := ZeroExt("b0010011".U, 16)
    val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
    (0 until FetchWidth).foreach(i => instrs(i) := Cat(ZeroExt("b0010011".U, 16), Fill(16, 0.U(1.W))))
    pd.io.in.data := instrs.asUInt

    when (icacheResp.ipf && !prevHalfInstr.ipf) { crossPageIPF := true.B } // higher 16 bits page fault
  }

  //Performance Counter
  // if (!env.FPGAPlatform ) {
  //   ExcitingUtils.addSource(io.fetchPacket.fire && !inLoop, "CntFetchFromICache", Perf)
  //   ExcitingUtils.addSource(io.fetchPacket.fire && inLoop, "CntFetchFromLoopBuffer", Perf)
  // }

  val fetchPacketValid = if4_valid && !io.redirect.valid
  val fetchPacketWire = Wire(new FetchPacket)

  // io.fetchPacket.valid := if4_valid && !io.redirect.valid
  fetchPacketWire.instrs := if4_pd.instrs
  fetchPacketWire.mask := if4_pd.mask & (Fill(PredictWidth, !if4_bp.taken) | (Fill(PredictWidth, 1.U(1.W)) >> (~if4_bp.jmpIdx)))
  loopBufPar.noTakenMask := if4_pd.mask
  fetchPacketWire.pc := if4_pd.pc
  (0 until PredictWidth).foreach(i => fetchPacketWire.pnpc(i) := if4_pd.pc(i) + Mux(if4_pd.pd(i).isRVC, 2.U, 4.U))
  when (if4_bp.taken) {
    fetchPacketWire.pnpc(if4_bp.jmpIdx) := if4_bp.target
  }
  fetchPacketWire.brInfo := bpu.io.branchInfo.bits
  (0 until PredictWidth).foreach(i => fetchPacketWire.brInfo(i).histPtr := finalPredHistPtr)
  (0 until PredictWidth).foreach(i => fetchPacketWire.brInfo(i).predHistPtr := if4_predHistPtr)
  fetchPacketWire.pd := if4_pd.pd
  fetchPacketWire.ipf := if4_ipf
  fetchPacketWire.crossPageIPFFix := if4_crossPageIPF

  // predTaken Vec
  fetchPacketWire.predTaken := if4_bp.taken

  loopBuffer.io.in.bits := fetchPacketWire
  io.fetchPacket.bits := fetchPacketWire
  io.fetchPacket.valid := fetchPacketValid
  loopBuffer.io.in.valid := io.fetchPacket.fire

  // debug info
  if (IFUDebug) {
    XSDebug(RegNext(reset.asBool) && !reset.asBool, "Reseting...\n")
    XSDebug(io.icacheFlush(0).asBool, "Flush icache stage2...\n")
    XSDebug(io.icacheFlush(1).asBool, "Flush icache stage3...\n")
    XSDebug(io.redirect.valid, "Redirect from backend! isExcp=%d isFpp:%d isMisPred=%d isReplay=%d pc=%x\n",
      io.redirect.bits.isException, io.redirect.bits.isFlushPipe, io.redirect.bits.isMisPred, io.redirect.bits.isReplay, io.redirect.bits.pc)
    XSDebug(io.redirect.valid, p"Redirect from backend! target=${Hexadecimal(io.redirect.bits.target)} brTag=${io.redirect.bits.brTag}\n")

    XSDebug("[IF1] v=%d     fire=%d            flush=%d pc=%x ptr=%d mask=%b\n", if1_valid, if1_fire, if1_flush, if1_npc, ptr, mask(if1_npc))
    XSDebug("[IF2] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x ptr=%d snpc=%x\n", if2_valid, if2_ready, if2_fire, if2_redirect, if2_flush, if2_pc, if2_histPtr, if2_snpc)
    XSDebug("[IF3] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x ptr=%d crossPageIPF=%d sawNTBrs=%d\n", if3_valid, if3_ready, if3_fire, if3_redirect, if3_flush, if3_pc, if3_histPtr, crossPageIPF, if3_GHInfo.sawNTBr)
    XSDebug("[IF4] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x ptr=%d crossPageIPF=%d sawNTBrs=%d\n", if4_valid, if4_ready, if4_fire, if4_redirect, if4_flush, if4_pc, if4_histPtr, if4_crossPageIPF, if4_GHInfo.sawNTBr)
    XSDebug("[IF1][icacheReq] v=%d r=%d addr=%x\n", io.icacheReq.valid, io.icacheReq.ready, io.icacheReq.bits.addr)
    XSDebug("[IF1][ghr] headPtr=%d shiftPtr=%d newPtr=%d ptr=%d\n", if1_histPtr, shiftPtr, newPtr, ptr)
    XSDebug("[IF1][ghr] hist=%b\n", hist.asUInt)
    XSDebug("[IF1][ghr] extHist=%b\n\n", extHist.asUInt)

    XSDebug("[IF2][bp] redirect=%d taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n\n", if2_bp.redirect, if2_bp.taken, if2_bp.jmpIdx, if2_bp.hasNotTakenBrs, if2_bp.target, if2_bp.saveHalfRVI)
    if2_GHInfo.debug

    XSDebug("[IF3][icacheResp] v=%d r=%d pc=%x mask=%b\n", io.icacheResp.valid, io.icacheResp.ready, io.icacheResp.bits.pc, io.icacheResp.bits.mask)
    XSDebug("[IF3][bp] redirect=%d taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if3_bp.redirect, if3_bp.taken, if3_bp.jmpIdx, if3_bp.hasNotTakenBrs, if3_bp.target, if3_bp.saveHalfRVI)
    // XSDebug("[IF3][prevHalfInstr] v=%d redirect=%d fetchpc=%x idx=%d tgt=%x taken=%d instr=%x\n\n",
    //   prev_half_valid, prev_half_redirect, prev_half_fetchpc, prev_half_idx, prev_half_tgt, prev_half_taken, prev_half_instr)
    XSDebug("[IF3][    prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x tgt=%x instr=%x ipf=%d\n",
      prevHalfInstr.valid, prevHalfInstr.taken, prevHalfInstr.fetchpc, prevHalfInstr.idx, prevHalfInstr.pc, prevHalfInstr.target, prevHalfInstr.instr, prevHalfInstr.ipf)
    XSDebug("[IF3][if3_prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x tgt=%x instr=%x ipf=%d\n\n",
      if3_prevHalfInstr.valid, if3_prevHalfInstr.taken, if3_prevHalfInstr.fetchpc, if3_prevHalfInstr.idx, if3_prevHalfInstr.pc, if3_prevHalfInstr.target, if3_prevHalfInstr.instr, if3_prevHalfInstr.ipf)
    if3_GHInfo.debug

    XSDebug("[IF4][predecode] mask=%b\n", if4_pd.mask)
    XSDebug("[IF4][bp] redirect=%d taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if4_bp.redirect, if4_bp.taken, if4_bp.jmpIdx, if4_bp.hasNotTakenBrs, if4_bp.target, if4_bp.saveHalfRVI)
    XSDebug(if4_pd.pd(if4_bp.jmpIdx).isJal && if4_bp.taken, "[IF4] cfi is jal!  instr=%x target=%x\n", if4_cfi_jal, if4_cfi_jal_tgt)
    XSDebug("[IF4][if4_prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x tgt=%x instr=%x ipf=%d\n",
      if4_prevHalfInstr.valid, if4_prevHalfInstr.taken, if4_prevHalfInstr.fetchpc, if4_prevHalfInstr.idx, if4_prevHalfInstr.pc, if4_prevHalfInstr.target, if4_prevHalfInstr.instr, if4_prevHalfInstr.ipf)
    if4_GHInfo.debug
    XSDebug(io.fetchPacket.fire(), "[IF4][fetchPacket] v=%d r=%d mask=%b ipf=%d crossPageIPF=%d\n",
      io.fetchPacket.valid, io.fetchPacket.ready, io.fetchPacket.bits.mask, io.fetchPacket.bits.ipf, io.fetchPacket.bits.crossPageIPFFix)
    for (i <- 0 until PredictWidth) {
      XSDebug(io.fetchPacket.fire(), "[IF4][fetchPacket] %b %x pc=%x pnpc=%x pd: rvc=%d brType=%b call=%d ret=%d\n",
        io.fetchPacket.bits.mask(i),
        io.fetchPacket.bits.instrs(i),
        io.fetchPacket.bits.pc(i),
        io.fetchPacket.bits.pnpc(i),
        io.fetchPacket.bits.pd(i).isRVC,
        io.fetchPacket.bits.pd(i).brType,
        io.fetchPacket.bits.pd(i).isCall,
        io.fetchPacket.bits.pd(i).isRet
      )
    }
  }
}