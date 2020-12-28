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
  def maskLastHalf(pc: UInt): UInt = Mux(isInLastBank(pc), 0.U(bankWidth.W), ~0.U(bankWidth.W))
  def mask(pc: UInt): UInt = Reverse(Cat(maskFirstHalf(pc), maskLastHalf(pc)))
  def snpc(pc: UInt): UInt = bankAligned(pc) + Mux(isInLastBank(pc), bankBytes.U, (bankBytes*2).U)

  val enableGhistRepair = true
  val IFUDebug = true
}

class GlobalHistory extends XSBundle {
  val predHist = UInt(HistoryLength.W)
  // val sawNTBr = Bool()
  // val takenOnBr = Bool()
  // val saveHalfRVI = Bool()
  // def shifted = takenOnBr || sawNTBr
  // def newPtr(ptr: UInt = nowPtr): UInt = Mux(shifted, ptr - 1.U, ptr)
  def update(sawNTBr: Bool, takenOnBr: Bool, hist: UInt = predHist): GlobalHistory = {
    val g = Wire(new GlobalHistory)
    val shifted = takenOnBr || sawNTBr
    g.predHist := Mux(shifted, (hist << 1) | takenOnBr.asUInt, hist)
    g
  }

  final def === (that: GlobalHistory): Bool = {
    predHist === that.predHist
  }

  final def =/= (that: GlobalHistory): Bool = !(this === that)

  implicit val name = "IFU"
  def debug(where: String) = XSDebug(p"[${where}_GlobalHistory] hist=${Binary(predHist)}\n")
  // override def toString(): String = "histPtr=%d, sawNTBr=%d, takenOnBr=%d, saveHalfRVI=%d".format(histPtr, sawNTBr, takenOnBr, saveHalfRVI)
}


class IFUIO extends XSBundle
{
  // to ibuffer
  val fetchPacket = DecoupledIO(new FetchPacket)
  // from backend
  val redirect = Flipped(ValidIO(UInt(VAddrBits.W)))
  val cfiUpdateInfo = Flipped(ValidIO(new CfiUpdateInfo))
  // to icache
  val icacheMemGrant = Flipped(DecoupledIO(new L1plusCacheResp))
  val fencei = Input(Bool())
  // from icache
  val icacheMemAcq = DecoupledIO(new L1plusCacheReq)
  val l1plusFlush = Output(Bool())
  // to tlb
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  // from tlb
  val ptw = new TlbPtwIO
}

class PrevHalfInstr extends XSBundle {
  val taken = Bool()
  val ghInfo = new GlobalHistory()
  val fetchpc = UInt(VAddrBits.W) // only for debug
  val idx = UInt(VAddrBits.W) // only for debug
  val pc = UInt(VAddrBits.W)
  val npc = UInt(VAddrBits.W)
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
  val icache = Module(new ICache)

  val pd = Module(new PreDecode)
  io.ptw <> TLB(
    in = Seq(icache.io.tlb),
    sfence = io.sfence,
    csr = io.tlbCsr,
    width = 1,
    isDtlb = false,
    shouldBlock = true
  )

  val if2_redirect, if3_redirect, if4_redirect = WireInit(false.B)
  val if1_flush, if2_flush, if3_flush, if4_flush = WireInit(false.B)

  val icacheResp = icache.io.resp.bits

  if4_flush := io.redirect.valid
  if3_flush := if4_flush || if4_redirect
  if2_flush := if3_flush || if3_redirect
  if1_flush := if2_flush || if2_redirect

  //********************** IF1 ****************************//
  val if1_valid = !reset.asBool && GTimer() > 500.U
  val if1_npc = WireInit(0.U(VAddrBits.W))
  val if2_ready = WireInit(false.B)
  val if2_allReady = WireInit(if2_ready && icache.io.req.ready)
  val if1_fire = if1_valid && (if2_allReady || if2_flush)


  // val if2_newPtr, if3_newPtr, if4_newPtr = Wire(UInt(log2Up(ExtHistoryLength).W))

  val if1_gh, if2_gh, if3_gh, if4_gh = Wire(new GlobalHistory)
  val if2_predicted_gh, if3_predicted_gh, if4_predicted_gh = Wire(new GlobalHistory)
  val final_gh = RegInit(0.U.asTypeOf(new GlobalHistory))
  val final_gh_bypass = WireInit(0.U.asTypeOf(new GlobalHistory))
  val flush_final_gh = WireInit(false.B)

  //********************** IF2 ****************************//
  val if2_valid = RegInit(init = false.B)
  val if2_allValid = if2_valid && icache.io.tlb.resp.valid
  val if3_ready = WireInit(false.B)
  val if2_fire = if2_allValid && if3_ready
  val if2_pc = RegEnable(next = if1_npc, init = resetVector.U, enable = if1_fire)
  val if2_snpc = snpc(if2_pc)
  val if2_predHist = RegEnable(if1_gh.predHist, enable=if1_fire)
  if2_ready := if3_ready && icache.io.tlb.resp.valid || !if2_valid
  when (if1_fire)       { if2_valid := true.B }
  .elsewhen (if2_flush) { if2_valid := false.B }
  .elsewhen (if2_fire)  { if2_valid := false.B }

  val npcGen = new PriorityMuxGenerator[UInt]
  npcGen.register(true.B, RegNext(if1_npc))
  npcGen.register(if2_fire, if2_snpc)
  val if2_bp = bpu.io.out(0)
  
  // if taken, bp_redirect should be true
  // when taken on half RVI, we suppress this redirect signal
  if2_redirect := if2_valid && if2_bp.taken
  npcGen.register(if2_redirect, if2_bp.target)

  if2_predicted_gh := if2_gh.update(if2_bp.hasNotTakenBrs, if2_bp.takenOnBr)

  //********************** IF3 ****************************//
  // if3 should wait for instructions resp to arrive
  val if3_valid = RegInit(init = false.B)
  val if4_ready = WireInit(false.B)
  val if3_allValid = if3_valid && icache.io.resp.valid
  val if3_fire = if3_allValid && if4_ready
  val if3_pc = RegEnable(if2_pc, if2_fire)
  val if3_predHist = RegEnable(if2_predHist, enable=if2_fire)
  if3_ready := if4_ready && icache.io.resp.valid || !if3_valid
  when (if3_flush) {
    if3_valid := false.B
  }.elsewhen (if2_fire && !if2_flush) {
    if3_valid := true.B
  }.elsewhen (if3_fire) {
    if3_valid := false.B
  }

  val if3_bp = bpu.io.out(1)
  if3_predicted_gh := if3_gh.update(if3_bp.hasNotTakenBrs, if3_bp.takenOnBr)


  val prevHalfInstrReq = WireInit(0.U.asTypeOf(ValidUndirectioned(new PrevHalfInstr)))
  // only valid when if4_fire
  val hasPrevHalfInstrReq = prevHalfInstrReq.valid

  val if3_prevHalfInstr = RegInit(0.U.asTypeOf(ValidUndirectioned(new PrevHalfInstr)))

  // 32-bit instr crosses 2 pages, and the higher 16-bit triggers page fault
  val crossPageIPF = WireInit(false.B)

  val if3_pendingPrevHalfInstr = if3_prevHalfInstr.valid

  // the previous half of RVI instruction waits until it meets its last half
  val if3_prevHalfInstrMet = if3_pendingPrevHalfInstr && if3_prevHalfInstr.bits.npc === if3_pc && if3_valid
  // set to invalid once consumed or redirect from backend
  val if3_prevHalfConsumed = if3_prevHalfInstrMet && if3_fire
  val if3_prevHalfFlush = if4_flush
  when (hasPrevHalfInstrReq && !if3_prevHalfFlush) {
    if3_prevHalfInstr.valid := true.B
  }.elsewhen (if3_prevHalfConsumed || if3_prevHalfFlush) {
    if3_prevHalfInstr.valid := false.B
  }
  when (hasPrevHalfInstrReq) {
    if3_prevHalfInstr.bits := prevHalfInstrReq.bits
  }
  // when bp signal a redirect, we distinguish between taken and not taken
  // if taken and saveHalfRVI is true, we do not redirect to the target

  def if3_nextValidPCNotEquals(pc: UInt) = !if2_valid || if2_valid && if2_pc =/= pc
  val if3_prevHalfMetRedirect    = if3_pendingPrevHalfInstr && if3_prevHalfInstrMet && if3_prevHalfInstr.bits.taken && if3_nextValidPCNotEquals(if3_prevHalfInstr.bits.target)
  val if3_prevHalfNotMetRedirect = if3_pendingPrevHalfInstr && !if3_prevHalfInstrMet && if3_nextValidPCNotEquals(if3_prevHalfInstr.bits.npc)
  val if3_predTakenRedirect    = !if3_pendingPrevHalfInstr && if3_bp.taken && if3_nextValidPCNotEquals(if3_bp.target)
  val if3_predNotTakenRedirect = !if3_pendingPrevHalfInstr && !if3_bp.taken && if3_nextValidPCNotEquals(snpc(if3_pc))
  // when pendingPrevHalfInstr, if3_GHInfo is set to the info of last prev half instr
  // val if3_ghInfoNotIdenticalRedirect = !if3_pendingPrevHalfInstr && if3_GHInfo =/= if3_lastGHInfo && enableGhistRepair.B

  if3_redirect := if3_valid && (
                    // prevHalf is consumed but the next packet is not where it meant to be
                    // we do not handle this condition because of the burden of building a correct GHInfo
                    // prevHalfMetRedirect ||
                    // prevHalf does not match if3_pc and the next fetch packet is not snpc
                    if3_prevHalfNotMetRedirect ||
                    // pred taken and next fetch packet is not the predicted target
                    if3_predTakenRedirect ||
                    // pred not taken and next fetch packet is not snpc
                    if3_predNotTakenRedirect
                    // GHInfo from last pred does not corresponds with this packet
                    // if3_ghInfoNotIdenticalRedirect
                  )

  val if3_target = WireInit(snpc(if3_pc))

  /* when (prevHalfMetRedirect) {
    if1_npc := if3_prevHalfInstr.target
  }.else */
  when (if3_prevHalfNotMetRedirect) {
    if3_target := if3_prevHalfInstr.bits.npc
  }.elsewhen (if3_predTakenRedirect) {
    if3_target := if3_bp.target
  }.elsewhen (if3_predNotTakenRedirect) {
    if3_target := snpc(if3_pc)
  }
  // }.elsewhen (if3_ghInfoNotIdenticalRedirect) {
  //   if3_target := Mux(if3_bp.taken, if3_bp.target, snpc(if3_pc))
  // }
  npcGen.register(if3_redirect, if3_target)

  // when (if3_redirect) {
  //   if1_npc := if3_target
  // }

  //********************** IF4 ****************************//
  val if4_pd = RegEnable(pd.io.out, if3_fire)
  val if4_ipf = RegEnable(icacheResp.ipf || if3_prevHalfInstrMet && if3_prevHalfInstr.bits.ipf, if3_fire)
  val if4_acf = RegEnable(icacheResp.acf, if3_fire)
  val if4_crossPageIPF = RegEnable(crossPageIPF, if3_fire)
  val if4_valid = RegInit(false.B)
  val if4_fire = if4_valid && io.fetchPacket.ready
  val if4_pc = RegEnable(if3_pc, if3_fire)
  // This is the real mask given from icache
  val if4_mask = RegEnable(icacheResp.mask, if3_fire)
  val if4_snpc = snpc(if4_pc)


  val if4_predHist = RegEnable(if3_predHist, enable=if3_fire)
  // wait until prevHalfInstr written into reg
  if4_ready := (io.fetchPacket.ready && !hasPrevHalfInstrReq || !if4_valid) && GTimer() > 500.U
  when (if4_flush) {
    if4_valid := false.B
  }.elsewhen (if3_fire && !if3_flush) {
    if4_valid := Mux(if3_pendingPrevHalfInstr, if3_prevHalfInstrMet, true.B)
  }.elsewhen (if4_fire) {
    if4_valid := false.B
  }

  val if4_bp = Wire(new BranchPrediction)
  if4_bp := bpu.io.out(2)
  if4_bp.takens  := bpu.io.out(2).takens & if4_mask
  if4_bp.brMask  := bpu.io.out(2).brMask & if4_mask
  if4_bp.jalMask := bpu.io.out(2).jalMask & if4_mask

  if4_predicted_gh := if4_gh.update(if4_bp.hasNotTakenBrs, if4_bp.takenOnBr)

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
  val if4_prevHalfInstr = RegInit(0.U.asTypeOf(ValidUndirectioned(new PrevHalfInstr)))
  val if4_pendingPrevHalfInstr = if4_prevHalfInstr.valid
  val if4_prevHalfInstrMet = if4_pendingPrevHalfInstr && if4_prevHalfInstr.bits.npc === if4_pc && if4_valid
  val if4_prevHalfConsumed = if4_prevHalfInstrMet && if4_fire
  val if4_prevHalfFlush = if4_flush

  val if4_takenPrevHalf = WireInit(if4_prevHalfInstrMet && if4_prevHalfInstr.bits.taken)
  when (if3_prevHalfConsumed) {
    if4_prevHalfInstr.valid := if3_prevHalfInstr.valid
  }.elsewhen (if4_prevHalfConsumed || if4_prevHalfFlush) {
    if4_prevHalfInstr.valid := false.B
  }

  when (if3_prevHalfConsumed) {
    if4_prevHalfInstr.bits := if3_prevHalfInstr.bits
  }

  prevHalfInstrReq.valid := if4_fire && if4_bp.saveHalfRVI
  val idx = if4_bp.lastHalfRVIIdx
  
  // this is result of the last half RVI
  prevHalfInstrReq.bits.taken := if4_bp.lastHalfRVITaken
  prevHalfInstrReq.bits.ghInfo := if4_gh
  prevHalfInstrReq.bits.newPtr := DontCare
  prevHalfInstrReq.bits.fetchpc := if4_pc
  prevHalfInstrReq.bits.idx := idx
  prevHalfInstrReq.bits.pc := if4_pd.pc(idx)
  prevHalfInstrReq.bits.npc := if4_pd.pc(idx) + 2.U
  prevHalfInstrReq.bits.target := if4_bp.lastHalfRVITarget
  prevHalfInstrReq.bits.instr := if4_pd.instrs(idx)(15, 0)
  prevHalfInstrReq.bits.ipf := if4_ipf

  def if4_nextValidPCNotEquals(pc: UInt) = if3_valid  && if3_pc =/= pc ||
                                           !if3_valid && (if2_valid && if2_pc =/= pc) ||
                                           !if3_valid && !if2_valid

  val if4_prevHalfNextNotMet = hasPrevHalfInstrReq && if4_nextValidPCNotEquals(prevHalfInstrReq.bits.pc+2.U)
  val if4_predTakenRedirect = !hasPrevHalfInstrReq && if4_bp.taken && if4_nextValidPCNotEquals(if4_bp.target)
  val if4_predNotTakenRedirect = !hasPrevHalfInstrReq && !if4_bp.taken && if4_nextValidPCNotEquals(if4_snpc)
  // val if4_ghInfoNotIdenticalRedirect = if4_GHInfo =/= if4_lastGHInfo && enableGhistRepair.B

  if4_redirect := if4_valid && (
                    // when if4 has a lastHalfRVI, but the next fetch packet is not snpc
                    // if4_prevHalfNextNotMet ||
                    // when if4 preds taken, but the pc of next fetch packet is not the target
                    if4_predTakenRedirect ||
                    // when if4 preds not taken, but the pc of next fetch packet is not snpc
                    if4_predNotTakenRedirect
                    // GHInfo from last pred does not corresponds with this packet
                    // if4_ghInfoNotIdenticalRedirect
                  )

  val if4_target = WireInit(if4_snpc)

  // when (if4_prevHalfNextNotMet) {
  //   if4_target := prevHalfInstrReq.pc+2.U
  // }.else
  when (if4_predTakenRedirect) {
    if4_target := if4_bp.target
  }.elsewhen (if4_predNotTakenRedirect) {
    if4_target := if4_snpc
  }
  // }.elsewhen (if4_ghInfoNotIdenticalRedirect) {
  //   if4_target := Mux(if4_bp.taken, if4_bp.target, if4_snpc)
  // }
  npcGen.register(if4_redirect, if4_target)

  when (if4_fire) {
    final_gh := if4_predicted_gh
  }
  if4_gh := Mux(flush_final_gh, final_gh_bypass, final_gh)
  if3_gh := Mux(if4_valid && !if4_flush, if4_predicted_gh, if4_gh)
  if2_gh := Mux(if3_valid && !if3_flush, if3_predicted_gh, if3_gh)
  if1_gh := Mux(if2_valid && !if2_flush, if2_predicted_gh, if2_gh)




  val cfiUpdate = io.cfiUpdateInfo
  when (cfiUpdate.valid && (cfiUpdate.bits.isMisPred || cfiUpdate.bits.isReplay)) {
    val b = cfiUpdate.bits
    val oldGh = b.bpuMeta.hist
    val sawNTBr = b.bpuMeta.sawNotTakenBranch
    val isBr = b.pd.isBr
    val taken = Mux(cfiUpdate.bits.isReplay, b.bpuMeta.predTaken, b.taken)
    val updatedGh = oldGh.update(sawNTBr, isBr && taken)
    final_gh := updatedGh
    final_gh_bypass := updatedGh
    flush_final_gh := true.B
  }

  npcGen.register(io.redirect.valid, io.redirect.bits)
  npcGen.register(RegNext(reset.asBool) && !reset.asBool, resetVector.U(VAddrBits.W))

  if1_npc := npcGen()


  icache.io.req.valid := if1_valid && (if2_ready || if2_flush)
  icache.io.resp.ready := if4_ready
  icache.io.req.bits.addr := if1_npc
  icache.io.req.bits.mask := mask(if1_npc)
  icache.io.flush := Cat(if3_flush, if2_flush)
  icache.io.mem_grant <> io.icacheMemGrant
  icache.io.fencei := io.fencei
  io.icacheMemAcq <> icache.io.mem_acquire
  io.l1plusFlush := icache.io.l1plusflush

  bpu.io.cfiUpdateInfo <> io.cfiUpdateInfo

  // bpu.io.flush := Cat(if4_flush, if3_flush, if2_flush)
  bpu.io.flush := VecInit(if2_flush, if3_flush, if4_flush)
  bpu.io.inFire(0) := if1_fire
  bpu.io.inFire(1) := if2_fire
  bpu.io.inFire(2) := if3_fire
  bpu.io.inFire(3) := if4_fire
  bpu.io.in.pc := if1_npc
  bpu.io.in.hist := if1_gh.asUInt
  // bpu.io.in.histPtr := ptr
  bpu.io.in.inMask := mask(if1_npc)
  bpu.io.predecode.mask := if4_pd.mask
  bpu.io.predecode.lastHalf := if4_pd.lastHalf
  bpu.io.predecode.pd := if4_pd.pd
  bpu.io.predecode.hasLastHalfRVI := if4_prevHalfInstrMet
  bpu.io.realMask := if4_mask
  bpu.io.prevHalf := if4_prevHalfInstr

  pd.io.in := icacheResp

  pd.io.prev.valid := if3_prevHalfInstrMet
  pd.io.prev.bits := if3_prevHalfInstr.bits.instr
  // if a fetch packet triggers page fault, set the pf instruction to nop
  when (!if3_prevHalfInstrMet && icacheResp.ipf) {
    val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
    (0 until FetchWidth).foreach(i => instrs(i) := ZeroExt("b0010011".U, 32)) // nop
    pd.io.in.data := instrs.asUInt
  }.elsewhen (if3_prevHalfInstrMet && (if3_prevHalfInstr.bits.ipf || icacheResp.ipf)) {
    pd.io.prev.bits := ZeroExt("b0010011".U, 16)
    val instrs = Wire(Vec(FetchWidth, UInt(32.W)))
    (0 until FetchWidth).foreach(i => instrs(i) := Cat(ZeroExt("b0010011".U, 16), Fill(16, 0.U(1.W))))
    pd.io.in.data := instrs.asUInt

    when (icacheResp.ipf && !if3_prevHalfInstr.bits.ipf) { crossPageIPF := true.B } // higher 16 bits page fault
  }

  val fetchPacketValid = if4_valid && !io.redirect.valid
  val fetchPacketWire = Wire(new FetchPacket)

  // io.fetchPacket.valid := if4_valid && !io.redirect.valid
  fetchPacketWire.instrs := if4_pd.instrs
  fetchPacketWire.mask := if4_pd.mask & (Fill(PredictWidth, !if4_bp.taken) | (Fill(PredictWidth, 1.U(1.W)) >> (~if4_bp.jmpIdx)))
  fetchPacketWire.pdmask := if4_pd.mask

  fetchPacketWire.pc := if4_pd.pc
  (0 until PredictWidth).foreach(i => fetchPacketWire.pnpc(i) := if4_pd.pc(i) + Mux(if4_pd.pd(i).isRVC, 2.U, 4.U))
  when (if4_bp.taken) {
    fetchPacketWire.pnpc(if4_bp.jmpIdx) := if4_bp.target
  }
  fetchPacketWire.bpuMeta := bpu.io.bpuMeta
  (0 until PredictWidth).foreach(i => {
    val meta = fetchPacketWire.bpuMeta(i)
    meta.hist := final_gh
    meta.predHist := if4_predHist.asTypeOf(new GlobalHistory)
    meta.predTaken := if4_bp.takens(i)
  })
  fetchPacketWire.pd := if4_pd.pd
  fetchPacketWire.ipf := if4_ipf
  fetchPacketWire.acf := if4_acf
  fetchPacketWire.crossPageIPFFix := if4_crossPageIPF

  // predTaken Vec
  fetchPacketWire.predTaken := if4_bp.taken

  io.fetchPacket.bits := fetchPacketWire
  io.fetchPacket.valid := fetchPacketValid

  // debug info
  if (IFUDebug) {
    XSDebug(RegNext(reset.asBool) && !reset.asBool, "Reseting...\n")
    XSDebug(icache.io.flush(0).asBool, "Flush icache stage2...\n")
    XSDebug(icache.io.flush(1).asBool, "Flush icache stage3...\n")
    XSDebug(io.redirect.valid, p"Redirect from backend! target=${Hexadecimal(io.redirect.bits)}\n")

    XSDebug("[IF1] v=%d     fire=%d            flush=%d pc=%x mask=%b\n", if1_valid, if1_fire, if1_flush, if1_npc, mask(if1_npc))
    XSDebug("[IF2] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x snpc=%x\n", if2_valid, if2_ready, if2_fire, if2_redirect, if2_flush, if2_pc, if2_snpc)
    XSDebug("[IF3] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x crossPageIPF=%d sawNTBrs=%d\n", if3_valid, if3_ready, if3_fire, if3_redirect, if3_flush, if3_pc, crossPageIPF, if3_bp.hasNotTakenBrs)
    XSDebug("[IF4] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x crossPageIPF=%d sawNTBrs=%d\n", if4_valid, if4_ready, if4_fire, if4_redirect, if4_flush, if4_pc, if4_crossPageIPF, if4_bp.hasNotTakenBrs)
    XSDebug("[IF1][icacheReq] v=%d r=%d addr=%x\n", icache.io.req.valid, icache.io.req.ready, icache.io.req.bits.addr)
    XSDebug("[IF1][ghr] hist=%b\n", if1_gh.asUInt)
    XSDebug("[IF1][ghr] extHist=%b\n\n", if1_gh.asUInt)

    XSDebug("[IF2][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n\n", if2_bp.taken, if2_bp.jmpIdx, if2_bp.hasNotTakenBrs, if2_bp.target, if2_bp.saveHalfRVI)
    if2_gh.debug("if2")

    XSDebug("[IF3][icacheResp] v=%d r=%d pc=%x mask=%b\n", icache.io.resp.valid, icache.io.resp.ready, icache.io.resp.bits.pc, icache.io.resp.bits.mask)
    XSDebug("[IF3][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if3_bp.taken, if3_bp.jmpIdx, if3_bp.hasNotTakenBrs, if3_bp.target, if3_bp.saveHalfRVI)
    XSDebug("[IF3][redirect]: v=%d, prevMet=%d, prevNMet=%d, predT=%d, predNT=%d\n", if3_redirect, if3_prevHalfMetRedirect, if3_prevHalfNotMetRedirect, if3_predTakenRedirect, if3_predNotTakenRedirect)
    // XSDebug("[IF3][prevHalfInstr] v=%d redirect=%d fetchpc=%x idx=%d tgt=%x taken=%d instr=%x\n\n",
    //   prev_half_valid, prev_half_redirect, prev_half_fetchpc, prev_half_idx, prev_half_tgt, prev_half_taken, prev_half_instr)
    XSDebug("[IF3][if3_prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x npc=%x tgt=%x instr=%x ipf=%d\n\n",
    if3_prevHalfInstr.valid, if3_prevHalfInstr.bits.taken, if3_prevHalfInstr.bits.fetchpc, if3_prevHalfInstr.bits.idx, if3_prevHalfInstr.bits.pc, if3_prevHalfInstr.bits.npc, if3_prevHalfInstr.bits.target, if3_prevHalfInstr.bits.instr, if3_prevHalfInstr.bits.ipf)
    if3_gh.debug("if3")
    
    XSDebug("[IF4][predecode] mask=%b\n", if4_pd.mask)
    XSDebug("[IF4][snpc]: %x, realMask=%b\n", if4_snpc, if4_mask)
    XSDebug("[IF4][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if4_bp.taken, if4_bp.jmpIdx, if4_bp.hasNotTakenBrs, if4_bp.target, if4_bp.saveHalfRVI)
    XSDebug("[IF4][redirect]: v=%d, prevNotMet=%d, predT=%d, predNT=%d\n", if4_redirect, if4_prevHalfNextNotMet, if4_predTakenRedirect, if4_predNotTakenRedirect)
    XSDebug(if4_pd.pd(if4_bp.jmpIdx).isJal && if4_bp.taken, "[IF4] cfi is jal!  instr=%x target=%x\n", if4_instrs(if4_bp.jmpIdx), if4_jal_tgts(if4_bp.jmpIdx))
    XSDebug("[IF4][ prevHalfInstrReq] v=%d taken=%d fetchpc=%x idx=%d pc=%x npc=%x tgt=%x instr=%x ipf=%d\n",
      prevHalfInstrReq.valid, prevHalfInstrReq.bits.taken, prevHalfInstrReq.bits.fetchpc, prevHalfInstrReq.bits.idx, prevHalfInstrReq.bits.pc, prevHalfInstrReq.bits.npc, prevHalfInstrReq.bits.target, prevHalfInstrReq.bits.instr, prevHalfInstrReq.bits.ipf)
    XSDebug("[IF4][if4_prevHalfInstr] v=%d taken=%d fetchpc=%x idx=%d pc=%x npc=%x tgt=%x instr=%x ipf=%d\n",
      if4_prevHalfInstr.valid, if4_prevHalfInstr.bits.taken, if4_prevHalfInstr.bits.fetchpc, if4_prevHalfInstr.bits.idx, if4_prevHalfInstr.bits.pc, if4_prevHalfInstr.bits.npc, if4_prevHalfInstr.bits.target, if4_prevHalfInstr.bits.instr, if4_prevHalfInstr.bits.ipf)
    if4_gh.debug("if4")
    XSDebug(io.fetchPacket.fire(), "[IF4][fetchPacket] v=%d r=%d mask=%b ipf=%d acf=%d crossPageIPF=%d\n",
      io.fetchPacket.valid, io.fetchPacket.ready, io.fetchPacket.bits.mask, io.fetchPacket.bits.ipf, io.fetchPacket.bits.acf, io.fetchPacket.bits.crossPageIPFFix)
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