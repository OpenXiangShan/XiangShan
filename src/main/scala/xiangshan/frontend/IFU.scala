package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils._
import xiangshan.cache._
import chisel3.experimental.chiselName

trait HasIFUConst extends HasXSParameter {
  val resetVector = 0x80000000L//TODO: set reset vec
  def align(pc: UInt, bytes: Int): UInt = Cat(pc(VAddrBits-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  val groupBytes = FetchWidth * 4 * 2 // correspond to cache line size
  val groupOffsetBits = log2Ceil(groupBytes)
  val nBanksInPacket = 2
  val bankBytes = PredictWidth * 2 / nBanksInPacket
  val nBanksInGroup = groupBytes / bankBytes
  val bankWidth = PredictWidth / nBanksInPacket
  val bankOffsetBits = log2Ceil(bankBytes)
  // (0, nBanksInGroup-1)
  def bankInGroup(pc: UInt) = pc(groupOffsetBits-1,bankOffsetBits)
  def isInLastBank(pc: UInt) = bankInGroup(pc) === (nBanksInGroup-1).U
  // (0, bankBytes/2-1)
  def offsetInBank(pc: UInt) = pc(bankOffsetBits-1,1)
  def bankAligned(pc: UInt)  = align(pc, bankBytes)
  def groupAligned(pc: UInt) = align(pc, groupBytes)
  // each 1 bit in mask stands for 2 Bytes
  // 8 bits, in which only the first 7 bits could be 0
  def maskFirstHalf(pc: UInt): UInt = ((~(0.U(bankWidth.W))) >> offsetInBank(pc))(bankWidth-1,0)
  // when in loop(buffer), we need to make use of the full packet
  // and get the real mask in iCacheResp from loop buffer
  // we may make predictions on more instructions than we could get from loop buffer
  // and this will be handled in if4
  def maskLastHalf(pc: UInt, inLoop: Bool = false.B): UInt = Mux(isInLastBank(pc) && !inLoop, 0.U(bankWidth.W), ~0.U(bankWidth.W))
  def mask(pc: UInt, inLoop: Bool = false.B): UInt = Reverse(Cat(maskFirstHalf(pc), maskLastHalf(pc, inLoop)))
  def snpc(pc: UInt, inLoop: Bool = false.B): UInt = pc + (PopCount(mask(pc, inLoop)) << 1)

  val enableGhistRepair = true
  val IFUDebug = true
}

class GlobalHistoryInfo() extends XSBundle {
  val nowPtr = UInt(log2Ceil(ExtHistoryLength).W)
  val sawNTBr = Bool()
  val takenOnBr = Bool()
  // val saveHalfRVI = Bool()
  def shifted = takenOnBr || sawNTBr
  def newPtr(ptr: UInt = nowPtr): UInt = Mux(shifted, ptr - 1.U, ptr)

  final def === (that: GlobalHistoryInfo): Bool = {
    shifted === that.shifted &&
    takenOnBr === that.takenOnBr &&
    nowPtr === that.nowPtr
  }

  final def =/= (that: GlobalHistoryInfo): Bool = !(this === that)

  // def update(): GlobalHistoryInfo = {
  //   val g = WireInit(this)
  //   g.nowPtr := nowPtr - Mux(shifted, 1.U, 0.U)
  //   g.sawNTBr := Mux(saveHalfRVI, sawNTBr, false.B)
  //   g.takenOnBr := Mux(saveHalfRVI, takenOnBr, false.B)
  //   // g.saveHalfRVI := false.B
  //   g
  // }

  implicit val name = "IFU"
  def debug(where: String) = XSDebug(p"[${where}_GHInfo] sawNTBr=${sawNTBr}, takenOnBr=${takenOnBr}\n")
  // override def toString(): String = "histPtr=%d, sawNTBr=%d, takenOnBr=%d, saveHalfRVI=%d".format(histPtr, sawNTBr, takenOnBr, saveHalfRVI)
}

class IFUIO extends XSBundle
{
  val fetchPacket = DecoupledIO(new FetchPacket)
  val redirect = Flipped(ValidIO(UInt(VAddrBits.W)))
  val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val icacheReq = DecoupledIO(new ICacheReq)
  val icacheResp = Flipped(DecoupledIO(new ICacheResp))
  val icacheFlush = Output(UInt(2.W))
  // val loopBufPar = Flipped(new LoopBufferParameters)
}

class PrevHalfInstr extends XSBundle {
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

@chiselName
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


  // val if2_newPtr, if3_newPtr, if4_newPtr = Wire(UInt(log2Up(ExtHistoryLength).W))
  
  val extHist = RegInit(VecInit(Seq.fill(ExtHistoryLength)(0.U(1.W))))
  val updatePtr = WireInit(false.B)
  val newPtr = Wire(UInt(log2Up(ExtHistoryLength).W))
  val if1_histPtr = RegEnable(next=newPtr, init=0.U(log2Up(ExtHistoryLength).W), enable=updatePtr)
  val ptr = Mux(updatePtr, newPtr, if1_histPtr)
  val hist = Wire(Vec(HistoryLength, UInt(1.W)))
  for (i <- 0 until HistoryLength) {
    hist(i) := extHist(ptr + i.U)
  }

  updatePtr := false.B
  newPtr := if1_histPtr

  

  def wrapGHInfo(bp: BranchPrediction, ptr: UInt) = {
    val ghi = Wire(new GlobalHistoryInfo())
    ghi.sawNTBr     := bp.hasNotTakenBrs
    ghi.takenOnBr   := bp.takenOnBr
    // ghi.saveHalfRVI := bp.saveHalfRVI
    ghi.nowPtr      := ptr
    ghi
  }

  //********************** IF2 ****************************//
  val if2_valid = RegInit(init = false.B)
  val if3_ready = WireInit(false.B)
  val if2_fire = if2_valid && if3_ready && !if2_flush
  val if2_pc = RegEnable(next = if1_npc, init = resetVector.U, enable = if1_fire)
  val if2_snpc = snpc(if2_pc, inLoop)
  val if2_predHistPtr = RegEnable(ptr, enable=if1_fire)
  if2_ready := if2_fire || !if2_valid || if2_flush
  when (if1_fire)       { if2_valid := if1_valid }
  .elsewhen (if2_flush) { if2_valid := false.B }
  .elsewhen (if2_fire)  { if2_valid := false.B }

  when (RegNext(reset.asBool) && !reset.asBool) {
    if1_npc := resetVector.U(VAddrBits.W)
  }.elsewhen (if2_fire) {
    if1_npc := if2_snpc
  }.otherwise {
    if1_npc := RegNext(if1_npc)
  }

  val if2_bp = bpu.io.out(0)
  
  val if2_GHInfo = wrapGHInfo(if2_bp, if2_predHistPtr)
  // if taken, bp_redirect should be true
  // when taken on half RVI, we suppress this redirect signal
  if2_redirect := if2_fire && if2_bp.taken
  when (if2_redirect) {
    if1_npc := if2_bp.target
  }
  when (if2_fire && if2_GHInfo.shifted) {
    val if2_newPtr = if2_GHInfo.newPtr()
    updatePtr := true.B
    newPtr := if2_newPtr
    extHist(if2_newPtr) := if2_GHInfo.takenOnBr.asUInt
  }



  //********************** IF3 ****************************//
  val if3_valid = RegInit(init = false.B)
  val if4_ready = WireInit(false.B)
  val if3_fire = if3_valid && if4_ready && (inLoop || io.icacheResp.valid) && !if3_flush
  val if3_pc = RegEnable(if2_pc, if2_fire)
  val if3_predHistPtr = RegEnable(if2_predHistPtr, enable=if2_fire)
  val if3_lastGHInfo = RegEnable(if2_GHInfo, enable=if2_fire)
  // val if3_nextValidPC = Mux(if2_valid)
  if3_ready := if3_fire || !if3_valid || if3_flush
  when (if3_flush)     { if3_valid := false.B }
  .elsewhen (if2_fire) { if3_valid := true.B }
  .elsewhen (if3_fire) { if3_valid := false.B }

  val if3_bp = bpu.io.out(1)

  val if3_GHInfo = wrapGHInfo(if3_bp, if3_predHistPtr)

  val prevHalfInstrReq = Wire(new PrevHalfInstr)
  // only valid when if4_fire
  val hasPrevHalfInstrReq = prevHalfInstrReq.valid

  val if3_prevHalfInstr = RegInit(0.U.asTypeOf(new PrevHalfInstr))
  // val if4_prevHalfInstr = Wire(new PrevHalfInstr)
  // 32-bit instr crosses 2 pages, and the higher 16-bit triggers page fault
  val crossPageIPF = WireInit(false.B)
  
  val if3_pendingPrevHalfInstr = if3_prevHalfInstr.valid
  
  // the previous half of RVI instruction waits until it meets its last half
  val if3_prevHalfInstrMet = if3_pendingPrevHalfInstr && (if3_prevHalfInstr.pc + 2.U) === if3_pc && if3_valid
  // set to invalid once consumed or redirect from backend
  val if3_prevHalfConsumed = if3_prevHalfInstrMet && if3_fire
  val if3_prevHalfFlush = if4_flush
  when (hasPrevHalfInstrReq) {
    if3_prevHalfInstr := prevHalfInstrReq
  }.elsewhen (if3_prevHalfConsumed || if3_prevHalfFlush) {
    if3_prevHalfInstr.valid := false.B
  }

  // when bp signal a redirect, we distinguish between taken and not taken
  // if taken and saveHalfRVI is true, we do not redirect to the target

  def if3_nextValidPCNotEquals(pc: UInt) = !if2_valid || if2_valid && if2_pc =/= pc
  val if3_prevHalfMetRedirect    = if3_pendingPrevHalfInstr && if3_prevHalfInstrMet && if3_prevHalfInstr.taken && if3_nextValidPCNotEquals(if3_prevHalfInstr.target)
  val if3_prevHalfNotMetRedirect = if3_pendingPrevHalfInstr && !if3_prevHalfInstrMet && if3_nextValidPCNotEquals(if3_prevHalfInstr.pc + 2.U)
  val if3_predTakenRedirect    = !if3_pendingPrevHalfInstr && if3_bp.taken && if3_nextValidPCNotEquals(if3_bp.target)
  val if3_predNotTakenRedirect = !if3_pendingPrevHalfInstr && !if3_bp.taken && if3_nextValidPCNotEquals(snpc(if3_pc, inLoop))
  // when pendingPrevHalfInstr, if3_GHInfo is set to the info of last prev half instr
  val if3_ghInfoNotIdenticalRedirect = !if3_pendingPrevHalfInstr && if3_GHInfo =/= if3_lastGHInfo && enableGhistRepair.B

  if3_redirect := if3_fire && (
                    // prevHalf is consumed but the next packet is not where it meant to be
                    // we do not handle this condition because of the burden of building a correct GHInfo
                    // prevHalfMetRedirect ||
                    // prevHalf does not match if3_pc and the next fetch packet is not snpc
                    if3_prevHalfNotMetRedirect ||
                    // pred taken and next fetch packet is not the predicted target
                    if3_predTakenRedirect ||
                    // pred not taken and next fetch packet is not snpc
                    if3_predNotTakenRedirect ||
                    // GHInfo from last pred does not corresponds with this packet
                    if3_ghInfoNotIdenticalRedirect
                  )
  
  val if3_target = WireInit(snpc(if3_pc))

  /* when (prevHalfMetRedirect) {
    if1_npc := if3_prevHalfInstr.target
  }.else */
  when (if3_prevHalfNotMetRedirect) {
    if3_target := if3_prevHalfInstr.pc + 2.U
  }.elsewhen (if3_predTakenRedirect) {
    if3_target := if3_bp.target
  }.elsewhen (if3_predNotTakenRedirect) {
    if3_target := snpc(if3_pc)
  }.elsewhen (if3_ghInfoNotIdenticalRedirect) {
    if3_target := Mux(if3_bp.taken, if3_bp.target, snpc(if3_pc))
  }

  when (if3_redirect) {
    if1_npc := if3_target
    val if3_newPtr = if3_GHInfo.newPtr()
    updatePtr := true.B
    newPtr := if3_newPtr
    extHist(if3_newPtr) := if3_GHInfo.takenOnBr.asUInt
  }

  //********************** IF4 ****************************//
  val if4_pd = RegEnable(pd.io.out, if3_fire)
  val if4_ipf = RegEnable(icacheResp.ipf || if3_prevHalfInstrMet && if3_prevHalfInstr.ipf, if3_fire)
  val if4_crossPageIPF = RegEnable(crossPageIPF, if3_fire)
  val if4_valid = RegInit(false.B)
  val if4_fire = if4_valid && io.fetchPacket.ready
  val if4_pc = RegEnable(if3_pc, if3_fire)
  val if4_lastGHInfo = RegEnable(if3_GHInfo, if3_fire)
  // This is the real mask given from icache or loop buffer
  val if4_mask = RegEnable(icacheResp.mask, if3_fire)
  val if4_snpc = Mux(inLoop, if4_pc + (PopCount(if4_mask) << 1), snpc(if4_pc))

  
  val if4_predHistPtr = RegEnable(if3_predHistPtr, enable=if3_fire)
  // wait until prevHalfInstr written into reg
  if4_ready := (if4_fire && !hasPrevHalfInstrReq || !if4_valid || if4_flush) && GTimer() > 500.U
  when (if4_flush)     { if4_valid := false.B }
  .elsewhen (if3_fire) { if4_valid := true.B }
  .elsewhen (if4_fire) { if4_valid := false.B }
  
  val if4_bp = Wire(new BranchPrediction)
  if4_bp := bpu.io.out(2)
  if4_bp.takens  := bpu.io.out(2).takens & if4_mask
  if4_bp.brMask  := bpu.io.out(2).brMask & if4_mask
  if4_bp.jalMask := bpu.io.out(2).jalMask & if4_mask
  
  val if4_GHInfo = wrapGHInfo(if4_bp, if4_predHistPtr)
  
  def cal_jal_tgt(inst: UInt, rvc: Bool): UInt = {
    Mux(rvc,
      SignExt(Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W)), XLEN),
      SignExt(Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)), XLEN)
    )
  }
  val if4_instrs = if4_pd.instrs
  val if4_jals = if4_bp.jalMask
  val if4_jal_tgts = VecInit((0 until PredictWidth).map(i => if4_pd.pc(i) + cal_jal_tgt(if4_instrs(i), if4_pd.pd(i).isRVC)))

  (0 until PredictWidth).foreach {i =>
    when (if4_jals(i)) {
      if4_bp.targets(i) := if4_jal_tgts(i)
    }
  }
  
  // we need this to tell BPU the prediction of prev half
  // because the prediction is with the start of each inst
  val if4_prevHalfInstr = RegInit(0.U.asTypeOf(new PrevHalfInstr))
  val if4_pendingPrevHalfInstr = if4_prevHalfInstr.valid
  val if4_prevHalfInstrMet = if4_pendingPrevHalfInstr && (if4_prevHalfInstr.pc + 2.U) === if4_pc && if4_valid
  val if4_prevHalfConsumed = if4_prevHalfInstrMet && if4_fire
  val if4_prevHalfFlush = if4_flush

  val if4_takenPrevHalf = WireInit(if4_prevHalfInstrMet && if4_prevHalfInstr.taken)
  when (if3_prevHalfConsumed) {
    if4_prevHalfInstr := if3_prevHalfInstr
  }.elsewhen (if4_prevHalfConsumed || if4_prevHalfFlush) {
    if4_prevHalfInstr.valid := false.B
  }

  prevHalfInstrReq := 0.U.asTypeOf(new PrevHalfInstr)
  when (if4_fire && if4_bp.saveHalfRVI) {
    val idx = if4_bp.lastHalfRVIIdx
    prevHalfInstrReq.valid := true.B
    // this is result of the last half RVI
    prevHalfInstrReq.taken := if4_bp.lastHalfRVITaken
    prevHalfInstrReq.ghInfo := if4_GHInfo
    prevHalfInstrReq.newPtr := if4_GHInfo.newPtr()
    prevHalfInstrReq.fetchpc := if4_pc
    prevHalfInstrReq.idx := idx
    prevHalfInstrReq.pc := if4_pd.pc(idx)
    prevHalfInstrReq.target := if4_bp.lastHalfRVITarget
    prevHalfInstrReq.instr := if4_pd.instrs(idx)(15, 0)
    prevHalfInstrReq.ipf := if4_ipf
  }

  def if4_nextValidPCNotEquals(pc: UInt) = if3_valid  && if3_pc =/= pc ||
                                           !if3_valid && (if2_valid && if2_pc =/= pc) ||
                                           !if3_valid && !if2_valid

  val if4_prevHalfNextNotMet = hasPrevHalfInstrReq && if4_nextValidPCNotEquals(prevHalfInstrReq.pc+2.U)
  val if4_predTakenRedirect = !hasPrevHalfInstrReq && if4_bp.taken && if4_nextValidPCNotEquals(if4_bp.target)
  val if4_predNotTakenRedirect = !hasPrevHalfInstrReq && !if4_bp.taken && if4_nextValidPCNotEquals(if4_snpc)
  val if4_ghInfoNotIdenticalRedirect = if4_GHInfo =/= if4_lastGHInfo && enableGhistRepair.B

  if4_redirect := if4_fire && (
                    // when if4 has a lastHalfRVI, but the next fetch packet is not snpc
                    if4_prevHalfNextNotMet ||
                    // when if4 preds taken, but the pc of next fetch packet is not the target
                    if4_predTakenRedirect ||
                    // when if4 preds not taken, but the pc of next fetch packet is not snpc
                    if4_predNotTakenRedirect ||
                    // GHInfo from last pred does not corresponds with this packet
                    if4_ghInfoNotIdenticalRedirect
                  )

  val if4_target = WireInit(if4_snpc)

  when (if4_prevHalfNextNotMet) {
    if4_target := prevHalfInstrReq.pc+2.U
  }.elsewhen (if4_predTakenRedirect) {
    if4_target := if4_bp.target
  }.elsewhen (if4_predNotTakenRedirect) {
    if4_target := if4_snpc
  }.elsewhen (if4_ghInfoNotIdenticalRedirect) {
    if4_target := Mux(if4_bp.taken, if4_bp.target, if4_snpc)
  }
  when (if4_redirect) {
    if1_npc := if4_target
    val if4_newPtr = if4_GHInfo.newPtr()
    updatePtr := true.B
    newPtr := if4_newPtr
    extHist(if4_newPtr) := if4_GHInfo.takenOnBr.asUInt
  }


  when (io.outOfOrderBrInfo.valid && io.outOfOrderBrInfo.bits.isMisPred) {
    val b = io.outOfOrderBrInfo.bits
    val oldPtr = b.brInfo.histPtr
    updatePtr := true.B
    when (!b.pd.isBr && !b.brInfo.sawNotTakenBranch) {
      // If mispredicted cfi is not a branch,
      // and there wasn't any not taken branch before it,
      // we should only recover the pointer to an unshifted state
      newPtr := oldPtr
      // finalPredHistPtr := oldPtr
    }.otherwise {
      newPtr := oldPtr - 1.U
      // finalPredHistPtr := oldPtr - 1.U
      // hist(0) := Mux(b.pd.isBr, b.taken, 0.U)
      extHist(newPtr) := Mux(b.pd.isBr, b.taken, 0.U)
    }
  }

  when (loopBufPar.LBredirect.valid) {
    if1_npc := loopBufPar.LBredirect.bits
  }

  when (io.redirect.valid) {
    if1_npc := io.redirect.bits
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
  bpu.io.inFire(0) := if1_fire
  bpu.io.inFire(1) := if2_fire
  bpu.io.inFire(2) := if3_fire
  bpu.io.inFire(3) := if4_fire
  bpu.io.in.pc := if1_npc
  bpu.io.in.hist := hist.asUInt
  bpu.io.in.histPtr := ptr
  bpu.io.in.inMask := mask(if1_npc)
  bpu.io.predecode.mask := if4_pd.mask
  bpu.io.predecode.lastHalf := if4_pd.lastHalf
  bpu.io.predecode.pd := if4_pd.pd
  bpu.io.predecode.hasLastHalfRVI := if4_pc =/= if4_pd.pc(0)
  bpu.io.realMask := if4_mask
  bpu.io.prevHalf := if4_prevHalfInstr

  pd.io.in := icacheResp
  when(inLoop) {
    pd.io.in.mask := loopBuffer.io.out.bits.mask // TODO: Maybe this is unnecessary
    // XSDebug("Fetch from LB\n")
    // XSDebug(p"pc=${Hexadecimal(io.loopBufPar.LBResp.pc)}\n")
    // XSDebug(p"data=${Hexadecimal(io.loopBufPar.LBResp.data)}\n")
    // XSDebug(p"mask=${Hexadecimal(io.loopBufPar.LBResp.mask)}\n")
  }

  pd.io.prev.valid := if3_prevHalfInstrMet
  pd.io.prev.bits := if3_prevHalfInstr.instr
  // if a fetch packet triggers page fault, set the pf instruction to nop
  when (!if3_prevHalfInstrMet && icacheResp.ipf) {
    val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
    (0 until FetchWidth).foreach(i => instrs(i) := ZeroExt("b0010011".U, 32)) // nop
    pd.io.in.data := instrs.asUInt
  }.elsewhen (if3_prevHalfInstrMet && (if3_prevHalfInstr.ipf || icacheResp.ipf)) {
    pd.io.prev.bits := ZeroExt("b0010011".U, 16)
    val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
    (0 until FetchWidth).foreach(i => instrs(i) := Cat(ZeroExt("b0010011".U, 16), Fill(16, 0.U(1.W))))
    pd.io.in.data := instrs.asUInt

    when (icacheResp.ipf && !if3_prevHalfInstr.ipf) { crossPageIPF := true.B } // higher 16 bits page fault
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
  fetchPacketWire.brInfo := bpu.io.branchInfo
  (0 until PredictWidth).foreach(i => fetchPacketWire.brInfo(i).histPtr := if4_predHistPtr)
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
    XSDebug(io.redirect.valid, p"Redirect from backend! target=${Hexadecimal(io.redirect.bits)}\n")

    XSDebug("[IF1] v=%d     fire=%d            flush=%d pc=%x ptr=%d mask=%b\n", if1_valid, if1_fire, if1_flush, if1_npc, ptr, mask(if1_npc))
    XSDebug("[IF2] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x ptr=%d snpc=%x\n", if2_valid, if2_ready, if2_fire, if2_redirect, if2_flush, if2_pc, if2_predHistPtr, if2_snpc)
    XSDebug("[IF3] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x ptr=%d crossPageIPF=%d sawNTBrs=%d\n", if3_valid, if3_ready, if3_fire, if3_redirect, if3_flush, if3_pc, if3_predHistPtr, crossPageIPF, if3_GHInfo.sawNTBr)
    XSDebug("[IF4] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x ptr=%d crossPageIPF=%d sawNTBrs=%d\n", if4_valid, if4_ready, if4_fire, if4_redirect, if4_flush, if4_pc, if4_predHistPtr, if4_crossPageIPF, if4_GHInfo.sawNTBr)
    XSDebug("[IF1][icacheReq] v=%d r=%d addr=%x\n", io.icacheReq.valid, io.icacheReq.ready, io.icacheReq.bits.addr)
    XSDebug("[IF1][ghr] headPtr=%d updatePtr=%d newPtr=%d ptr=%d\n", if1_histPtr, updatePtr, newPtr, ptr)
    XSDebug("[IF1][ghr] hist=%b\n", hist.asUInt)
    XSDebug("[IF1][ghr] extHist=%b\n\n", extHist.asUInt)

    XSDebug("[IF2][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n\n", if2_bp.taken, if2_bp.jmpIdx, if2_bp.hasNotTakenBrs, if2_bp.target, if2_bp.saveHalfRVI)
    if2_GHInfo.debug("if2")

    XSDebug("[IF3][icacheResp] v=%d r=%d pc=%x mask=%b\n", io.icacheResp.valid, io.icacheResp.ready, io.icacheResp.bits.pc, io.icacheResp.bits.mask)
    XSDebug("[IF3][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if3_bp.taken, if3_bp.jmpIdx, if3_bp.hasNotTakenBrs, if3_bp.target, if3_bp.saveHalfRVI)
    XSDebug("[IF3][redirect]: v=%d, prevMet=%d, prevNMet=%d, predT=%d, predNT=%d, ghInfo=%d\n", if3_redirect, if3_prevHalfMetRedirect, if3_prevHalfNotMetRedirect, if3_predTakenRedirect, if3_predNotTakenRedirect, if3_ghInfoNotIdenticalRedirect)
    // XSDebug("[IF3][prevHalfInstr] v=%d redirect=%d fetchpc=%x idx=%d tgt=%x taken=%d instr=%x\n\n",
    //   prev_half_valid, prev_half_redirect, prev_half_fetchpc, prev_half_idx, prev_half_tgt, prev_half_taken, prev_half_instr)
    XSDebug("[IF3][    prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x tgt=%x instr=%x ipf=%d\n",
      if3_prevHalfInstr.valid, if3_prevHalfInstr.taken, if3_prevHalfInstr.fetchpc, if3_prevHalfInstr.idx, if3_prevHalfInstr.pc, if3_prevHalfInstr.target, if3_prevHalfInstr.instr, if3_prevHalfInstr.ipf)
    XSDebug("[IF3][if3_prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x tgt=%x instr=%x ipf=%d\n\n",
      if3_prevHalfInstr.valid, if3_prevHalfInstr.taken, if3_prevHalfInstr.fetchpc, if3_prevHalfInstr.idx, if3_prevHalfInstr.pc, if3_prevHalfInstr.target, if3_prevHalfInstr.instr, if3_prevHalfInstr.ipf)
    if3_GHInfo.debug("if3")

    XSDebug("[IF4][predecode] mask=%b\n", if4_pd.mask)
    XSDebug("[IF4][snpc]: %x, realMask=%b\n", if4_snpc, if4_mask)
    XSDebug("[IF4][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if4_bp.taken, if4_bp.jmpIdx, if4_bp.hasNotTakenBrs, if4_bp.target, if4_bp.saveHalfRVI)
    XSDebug("[IF4][redirect]: v=%d, prevNotMet=%d, predT=%d, predNT=%d, ghInfo=%d\n", if4_redirect, if4_prevHalfNextNotMet, if4_predTakenRedirect, if4_predNotTakenRedirect, if4_ghInfoNotIdenticalRedirect)
    XSDebug(if4_pd.pd(if4_bp.jmpIdx).isJal && if4_bp.taken, "[IF4] cfi is jal!  instr=%x target=%x\n", if4_instrs(if4_bp.jmpIdx), if4_jal_tgts(if4_bp.jmpIdx))
    XSDebug("[IF4][if4_prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x tgt=%x instr=%x ipf=%d\n",
      if4_prevHalfInstr.valid, if4_prevHalfInstr.taken, if4_prevHalfInstr.fetchpc, if4_prevHalfInstr.idx, if4_prevHalfInstr.pc, if4_prevHalfInstr.target, if4_prevHalfInstr.instr, if4_prevHalfInstr.ipf)
    if4_GHInfo.debug("if4")
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