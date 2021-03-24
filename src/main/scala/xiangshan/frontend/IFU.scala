package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils._
import xiangshan.cache._
import chisel3.experimental.chiselName
import freechips.rocketchip.tile.HasLazyRoCC
import chisel3.ExcitingUtils._
import xiangshan.backend.ftq.FtqPtr
import xiangshan.backend.decode.WaitTableParameters
import system.L1CacheErrorInfo

trait HasInstrMMIOConst extends HasXSParameter with HasIFUConst{
  def mmioBusWidth = 64
  def mmioBusBytes = mmioBusWidth /8
  def mmioBeats = FetchWidth * 4 * 8 / mmioBusWidth
  def mmioMask  = VecInit(List.fill(PredictWidth)(true.B)).asUInt
  def mmioBusAligned(pc :UInt): UInt = align(pc, mmioBusBytes)
}

trait HasIFUConst extends HasXSParameter {
  val resetVector = 0x10000000L//TODO: set reset vec
  def align(pc: UInt, bytes: Int): UInt = Cat(pc(VAddrBits-1, log2Ceil(bytes)), 0.U(log2Ceil(bytes).W))
  val groupBytes = 64 // correspond to cache line size
  val groupOffsetBits = log2Ceil(groupBytes)
  val groupWidth = groupBytes / instBytes
  val packetBytes = PredictWidth * instBytes
  val packetOffsetBits = log2Ceil(packetBytes)
  def offsetInPacket(pc: UInt) = pc(packetOffsetBits-1, instOffsetBits)
  def packetIdx(pc: UInt) = pc(VAddrBits-1, log2Ceil(packetBytes))
  def groupAligned(pc: UInt)  = align(pc, groupBytes)
  def packetAligned(pc: UInt) = align(pc, packetBytes)
  def mask(pc: UInt): UInt = ((~(0.U(PredictWidth.W))) << offsetInPacket(pc))(PredictWidth-1,0)
  def snpc(pc: UInt): UInt = packetAligned(pc) + packetBytes.U

  val enableGhistRepair = true
  val IFUDebug = true
}

class GlobalHistory extends XSBundle {
  val predHist = UInt(HistoryLength.W)
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
  val redirect = Flipped(ValidIO(new Redirect))
  val bp_ctrl = Input(new BPUCtrl)
  val commitUpdate = Flipped(ValidIO(new FtqEntry))
  val ftqEnqPtr = Input(new FtqPtr)
  val ftqLeftOne = Input(Bool())
  // to backend
  val toFtq = DecoupledIO(new FtqEntry)
  // to icache
  val icacheMemGrant = Flipped(DecoupledIO(new L1plusCacheResp))
  val fencei = Input(Bool())
  // from icache
  val icacheMemAcq = DecoupledIO(new L1plusCacheReq)
  val l1plusFlush = Output(Bool())
  val prefetchTrainReq = ValidIO(new IcacheMissReq)
  val error = new L1CacheErrorInfo
  // to tlb
  val sfence = Input(new SfenceBundle)
  val tlbCsr = Input(new TlbCsrBundle)
  // from tlb
  val ptw = new TlbPtwIO
  // icache uncache
  val mmio_acquire = DecoupledIO(new InsUncacheReq)
  val mmio_grant  = Flipped(DecoupledIO(new InsUncacheResp))
  val mmio_flush = Output(Bool())
}

class PrevHalfInstr extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val npc = UInt(VAddrBits.W)
  val instr = UInt(16.W)
  val ipf = Bool()
}

@chiselName
class IFU extends XSModule with HasIFUConst with HasCircularQueuePtrHelper with WaitTableParameters
{
  val io = IO(new IFUIO)
  val bpu = BPU(EnableBPU)
  val icache = Module(new ICache)

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
  val if2_valid = RegInit(init = false.B)
  val if2_allReady = WireInit(if2_ready && icache.io.req.ready && bpu.io.in_ready)
  val if1_fire = if1_valid &&  if2_allReady

  val if1_gh, if2_gh, if3_gh, if4_gh = Wire(new GlobalHistory)
  val if2_predicted_gh, if3_predicted_gh, if4_predicted_gh = Wire(new GlobalHistory)
  val final_gh = RegInit(0.U.asTypeOf(new GlobalHistory))

  //********************** IF2 ****************************//
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
  npcGen.register(true.B, RegNext(if1_npc), Some("stallPC"))
  val if2_bp = bpu.io.out(0)

  // if taken, bp_redirect should be true
  // when taken on half RVI, we suppress this redirect signal

  npcGen.register(if2_valid, Mux(if2_bp.taken, if2_bp.target, if2_snpc), Some("if2_target"))

  if2_predicted_gh := if2_gh.update(if2_bp.hasNotTakenBrs, if2_bp.takenOnBr)

  //********************** IF3 ****************************//
  // if3 should wait for instructions resp to arrive
  val if3_valid = RegInit(init = false.B)
  val if4_ready = WireInit(false.B)
  val if3_allValid = if3_valid && icache.io.resp.valid
  val if3_fire = if3_allValid && if4_ready
  val if3_pc = RegEnable(if2_pc, if2_fire)
  val if3_snpc = RegEnable(if2_snpc, if2_fire)
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
  val hasPrevHalfInstrReq = prevHalfInstrReq.valid && HasCExtension.B

  val if3_prevHalfInstr = RegInit(0.U.asTypeOf(ValidUndirectioned(new PrevHalfInstr)))

  // 32-bit instr crosses 2 pages, and the higher 16-bit triggers page fault
  val crossPageIPF = WireInit(false.B)

  val if3_pendingPrevHalfInstr = if3_prevHalfInstr.valid && HasCExtension.B

  // the previous half of RVI instruction waits until it meets its last half
  val if3_prevHalfInstrMet = if3_pendingPrevHalfInstr && if3_prevHalfInstr.bits.npc === if3_pc && if3_valid
  // set to invalid once consumed or redirect from backend
  val if3_prevHalfConsumed = if3_prevHalfInstrMet && if3_fire
  val if3_prevHalfFlush = if4_flush
  when (if3_prevHalfFlush) {
    if3_prevHalfInstr.valid := false.B
  }.elsewhen (hasPrevHalfInstrReq) {
    if3_prevHalfInstr.valid := true.B
  }.elsewhen (if3_prevHalfConsumed) {
    if3_prevHalfInstr.valid := false.B
  }
  when (hasPrevHalfInstrReq) {
    if3_prevHalfInstr.bits := prevHalfInstrReq.bits
  }
  // when bp signal a redirect, we distinguish between taken and not taken
  // if taken and saveHalfRVI is true, we do not redirect to the target

  class IF3_PC_COMP extends XSModule {
    val io = IO(new Bundle {
      val if2_pc = Input(UInt(VAddrBits.W))
      val pc     = Input(UInt(VAddrBits.W))
      val if2_valid = Input(Bool())
      val res = Output(Bool())
    })
    io.res := !io.if2_valid || io.if2_valid && io.if2_pc =/= io.pc
  }
  def if3_nextValidPCNotEquals(pc: UInt) = {
    val comp = Module(new IF3_PC_COMP)
    comp.io.if2_pc := if2_pc
    comp.io.pc     := pc
    comp.io.if2_valid := if2_valid
    comp.io.res
  }

  val if3_prevHalfNotMetRedirect = if3_pendingPrevHalfInstr && !if3_prevHalfInstrMet && if3_nextValidPCNotEquals(if3_prevHalfInstr.bits.npc)
  val if3_predTakenRedirect    = !if3_pendingPrevHalfInstr && if3_bp.taken && if3_nextValidPCNotEquals(if3_bp.target)
  val if3_predNotTakenRedirect = !if3_pendingPrevHalfInstr && !if3_bp.taken && if3_nextValidPCNotEquals(if3_snpc)
  // when pendingPrevHalfInstr, if3_GHInfo is set to the info of last prev half instr
  // val if3_ghInfoNotIdenticalRedirect = !if3_pendingPrevHalfInstr && if3_GHInfo =/= if3_lastGHInfo && enableGhistRepair.B

  if3_redirect := if3_valid && (
                    // prevHalf does not match if3_pc and the next fetch packet is not snpc
                    if3_prevHalfNotMetRedirect && HasCExtension.B ||
                    // pred taken and next fetch packet is not the predicted target
                    if3_predTakenRedirect ||
                    // pred not taken and next fetch packet is not snpc
                    if3_predNotTakenRedirect
                    // GHInfo from last pred does not corresponds with this packet
                    // if3_ghInfoNotIdenticalRedirect
                  )

  val if3_target = WireInit(if3_snpc)

  if3_target := Mux1H(Seq((if3_prevHalfNotMetRedirect -> if3_prevHalfInstr.bits.npc),
                          (if3_predTakenRedirect      -> if3_bp.target),
                          (if3_predNotTakenRedirect   -> if3_snpc)))

  npcGen.register(if3_redirect, if3_target, Some("if3_target"))


  //********************** IF4 ****************************//
  val ftqEnqBuf_ready = Wire(Bool())
  val if4_ftqEnqPtr = Wire(new FtqPtr)
  val if4_pd = RegEnable(icache.io.pd_out, if3_fire)
  val if4_ipf = RegEnable(icacheResp.ipf || if3_prevHalfInstrMet && if3_prevHalfInstr.bits.ipf, if3_fire)
  val if4_acf = RegEnable(icacheResp.acf, if3_fire)
  val if4_crossPageIPF = RegEnable(crossPageIPF, if3_fire)
  val if4_valid = RegInit(false.B)
  val if4_fire = if4_valid && io.fetchPacket.ready && ftqEnqBuf_ready
  val if4_pc = RegEnable(if3_pc, if3_fire)
  val if4_snpc = RegEnable(if3_snpc, if3_fire)
  // This is the real mask given from icache
  val if4_mask = RegEnable(icacheResp.mask, if3_fire)


  val if4_predHist = RegEnable(if3_predHist, enable=if3_fire)
  // wait until prevHalfInstr written into reg
  if4_ready := (io.fetchPacket.ready && !hasPrevHalfInstrReq && ftqEnqBuf_ready || !if4_valid) && GTimer() > 500.U
  when (if4_flush) {
    if4_valid := false.B
  }.elsewhen (if3_fire && !if3_flush) {
    if4_valid := Mux(if3_pendingPrevHalfInstr, if3_prevHalfInstrMet, true.B)
  }.elsewhen (if4_fire) {
    if4_valid := false.B
  }

  val if4_bp = Wire(new BranchPrediction)
  if4_bp := bpu.io.out(2)

  if4_predicted_gh := if4_gh.update(if4_bp.hasNotTakenBrs, if4_bp.takenOnBr)

  def jal_offset(inst: UInt, rvc: Bool): SInt = {
    Mux(rvc,
      Cat(inst(12), inst(8), inst(10, 9), inst(6), inst(7), inst(2), inst(11), inst(5, 3), 0.U(1.W)).asSInt(),
      Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)).asSInt()
    )
  }
  def br_offset(inst: UInt, rvc: Bool): SInt = {
    Mux(rvc,
      Cat(inst(12), inst(6, 5), inst(2), inst(11, 10), inst(4, 3), 0.U(1.W)).asSInt,
      Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)).asSInt()
    )
  }
  val if4_instrs = if4_pd.instrs
  val if4_jals = if4_bp.jalMask
  val if4_jal_tgts = VecInit((0 until PredictWidth).map(i => (if4_pd.pc(i).asSInt + jal_offset(if4_instrs(i), if4_pd.pd(i).isRVC)).asUInt))
  val if4_brs = if4_bp.brMask
  val if4_br_tgts = VecInit((0 until PredictWidth).map(i => (if4_pd.pc(i).asSInt + br_offset(if4_instrs(i), if4_pd.pd(i).isRVC)).asUInt))
  (0 until PredictWidth).foreach {i =>
    when (if4_jals(i)) {
      if4_bp.targets(i) := if4_jal_tgts(i)
    }.elsewhen (if4_brs(i)) {
      if4_bp.targets(i) := if4_br_tgts(i)
    }
  }

  // we need this to tell BPU the prediction of prev half
  // because the prediction is with the start of each inst
  val if4_prevHalfInstr = RegInit(0.U.asTypeOf(ValidUndirectioned(new PrevHalfInstr)))
  val if4_pendingPrevHalfInstr = if4_prevHalfInstr.valid && HasCExtension.B
  val if4_prevHalfInstrMet = if4_pendingPrevHalfInstr && if4_valid
  val if4_prevHalfConsumed = if4_prevHalfInstrMet && if4_fire
  val if4_prevHalfFlush = if4_flush

  when (if4_prevHalfFlush) {
    if4_prevHalfInstr.valid := false.B
  }.elsewhen (if3_prevHalfConsumed) {
    if4_prevHalfInstr.valid := if3_prevHalfInstr.valid
  }.elsewhen (if4_prevHalfConsumed) {
    if4_prevHalfInstr.valid := false.B
  }

  when (if3_prevHalfConsumed) {
    if4_prevHalfInstr.bits := if3_prevHalfInstr.bits
  }

  prevHalfInstrReq.valid := if4_fire && if4_bp.saveHalfRVI && HasCExtension.B

  // // this is result of the last half RVI
  prevHalfInstrReq.bits.pc := if4_pd.pc(PredictWidth-1)
  prevHalfInstrReq.bits.npc := snpc(if4_pc)
  prevHalfInstrReq.bits.instr := if4_pd.instrs(PredictWidth-1)(15, 0)
  prevHalfInstrReq.bits.ipf := if4_ipf

  class IF4_PC_COMP extends XSModule {
    val io = IO(new Bundle {
      val if2_pc = Input(UInt(VAddrBits.W))
      val if3_pc = Input(UInt(VAddrBits.W))
      val pc     = Input(UInt(VAddrBits.W))
      val if2_valid = Input(Bool())
      val if3_valid = Input(Bool())
      val res = Output(Bool())
    })
    io.res := io.if3_valid  && io.if3_pc =/= io.pc ||
              !io.if3_valid && (io.if2_valid && io.if2_pc =/= io.pc) ||
              !io.if3_valid && !io.if2_valid
  }
  def if4_nextValidPCNotEquals(pc: UInt) = {
    val comp = Module(new IF4_PC_COMP)
    comp.io.if2_pc := if2_pc
    comp.io.if3_pc := if3_pc
    comp.io.pc     := pc
    comp.io.if2_valid := if2_valid
    comp.io.if3_valid := if3_valid
    comp.io.res
  }

  val if4_prevHalfNextNotMet = hasPrevHalfInstrReq && if4_nextValidPCNotEquals(prevHalfInstrReq.bits.pc+2.U)
  val if4_predTakenRedirect = if4_bp.taken && if4_nextValidPCNotEquals(if4_bp.target)
  val if4_predNotTakenRedirect = !if4_bp.taken && if4_nextValidPCNotEquals(if4_snpc)
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

  if4_target := Mux(if4_bp.taken, if4_bp.target, if4_snpc)

  npcGen.register(if4_redirect, if4_target, Some("if4_target"))

  when (if4_fire) {
    final_gh := if4_predicted_gh
  }
  if4_gh := final_gh
  if3_gh := Mux(if4_valid, if4_predicted_gh, if4_gh)
  if2_gh := Mux(if3_valid && !if3_flush, if3_predicted_gh, if3_gh)
  if1_gh := Mux(if2_valid && !if2_flush, if2_predicted_gh, if2_gh)

  // ***************** Ftq enq buffer ********************
  val toFtqBuf = Wire(new FtqEntry)
  val ftqEnqBuf = RegEnable(toFtqBuf, enable=if4_fire)
  val ftqEnqBuf_valid = RegInit(false.B)
  val ftqLeftOne = WireInit(false.B) // TODO: to be replaced
  ftqEnqBuf_ready := io.toFtq.ready && !(io.ftqLeftOne && ftqEnqBuf_valid)
  if4_ftqEnqPtr := Mux(ftqEnqBuf_valid, io.ftqEnqPtr+1.U, io.ftqEnqPtr)
  when (io.redirect.valid)  { ftqEnqBuf_valid := false.B }
  .elsewhen (if4_fire)      { ftqEnqBuf_valid := true.B }
  .elsewhen (io.toFtq.fire) { ftqEnqBuf_valid := false.B }

  io.toFtq.valid := ftqEnqBuf_valid
  io.toFtq.bits  := ftqEnqBuf

  toFtqBuf := DontCare
  toFtqBuf.ftqPC    := if4_pc
  toFtqBuf.lastPacketPC.valid := if4_pendingPrevHalfInstr
  toFtqBuf.lastPacketPC.bits  := if4_prevHalfInstr.bits.pc

  toFtqBuf.hist     := final_gh
  toFtqBuf.predHist := if4_predHist.asTypeOf(new GlobalHistory)
  toFtqBuf.rasSp    := bpu.io.brInfo.rasSp
  toFtqBuf.rasTop   := bpu.io.brInfo.rasTop
  toFtqBuf.specCnt  := bpu.io.brInfo.specCnt
  toFtqBuf.metas    := bpu.io.brInfo.metas

  // For perf counters
  toFtqBuf.pd    := if4_pd.pd


  val if4_jmpIdx = WireInit(if4_bp.jmpIdx)
  val if4_taken = WireInit(if4_bp.taken)
  val if4_real_valids = if4_pd.mask &
    (Fill(PredictWidth, !if4_taken) |
      (Fill(PredictWidth, 1.U(1.W)) >> (~if4_jmpIdx)))

  val cfiIsCall = if4_pd.pd(if4_jmpIdx).isCall
  val cfiIsRet  = if4_pd.pd(if4_jmpIdx).isRet
  val cfiIsRVC  = if4_pd.pd(if4_jmpIdx).isRVC
  val cfiIsJalr = if4_pd.pd(if4_jmpIdx).isJalr
  toFtqBuf.cfiIsCall := cfiIsCall
  toFtqBuf.cfiIsRet  := cfiIsRet
  toFtqBuf.cfiIsJalr := cfiIsJalr
  toFtqBuf.cfiIsRVC  := cfiIsRVC
  toFtqBuf.cfiIndex.valid := if4_taken
  toFtqBuf.cfiIndex.bits  := if4_jmpIdx

  toFtqBuf.br_mask   := if4_bp.brMask.asTypeOf(Vec(PredictWidth, Bool()))
  toFtqBuf.rvc_mask  := VecInit(if4_pd.pd.map(_.isRVC))
  toFtqBuf.valids    := if4_real_valids.asTypeOf(Vec(PredictWidth, Bool()))
  toFtqBuf.target := Mux(if4_taken, if4_target, if4_snpc)



  val r = io.redirect
  val cfiUpdate = io.redirect.bits.cfiUpdate
  when (r.valid) {
    val isMisPred = r.bits.level === 0.U
    val b = cfiUpdate
    val oldGh = b.hist
    val sawNTBr = b.sawNotTakenBranch
    val isBr = b.pd.isBr
    val taken = Mux(isMisPred, b.taken, b.predTaken)
    val updatedGh = oldGh.update(sawNTBr, isBr && taken)
    final_gh := updatedGh
    if1_gh := updatedGh
  }

  npcGen.register(io.redirect.valid, io.redirect.bits.cfiUpdate.target, Some("backend_redirect"))
  npcGen.register(RegNext(reset.asBool) && !reset.asBool, resetVector.U(VAddrBits.W), Some("reset_vector"))

  if1_npc := npcGen()


  icache.io.req.valid := if1_fire
  icache.io.resp.ready := if4_ready
  icache.io.req.bits.addr := if1_npc
  icache.io.req.bits.mask := mask(if1_npc)
  icache.io.flush := Cat(if3_flush, if2_flush)
  icache.io.mem_grant <> io.icacheMemGrant
  icache.io.fencei := io.fencei
  icache.io.prev.valid := if3_prevHalfInstrMet
  icache.io.prev.bits := if3_prevHalfInstr.bits.instr
  icache.io.prev_ipf := if3_prevHalfInstr.bits.ipf
  icache.io.prev_pc := if3_prevHalfInstr.bits.pc
  icache.io.mmio_acquire <> io.mmio_acquire
  icache.io.mmio_grant <> io.mmio_grant
  icache.io.mmio_flush <> io.mmio_flush
  io.icacheMemAcq <> icache.io.mem_acquire
  io.l1plusFlush := icache.io.l1plusflush
  io.prefetchTrainReq := icache.io.prefetchTrainReq
  io.error <> icache.io.error

  bpu.io.ctrl := RegNext(io.bp_ctrl)
  bpu.io.commit <> io.commitUpdate
  bpu.io.redirect <> io.redirect

  bpu.io.inFire(0) := if1_fire
  bpu.io.inFire(1) := if2_fire
  bpu.io.inFire(2) := if3_fire
  bpu.io.inFire(3) := if4_fire
  bpu.io.in.pc := if1_npc
  bpu.io.in.hist := if1_gh.asUInt
  bpu.io.in.inMask := mask(if1_npc)
  bpu.io.predecode.mask := if4_pd.mask
  bpu.io.predecode.lastHalf := if4_pd.lastHalf
  bpu.io.predecode.pd := if4_pd.pd
  bpu.io.predecode.hasLastHalfRVI := if4_prevHalfInstrMet


  when (if3_prevHalfInstrMet && icacheResp.ipf && !if3_prevHalfInstr.bits.ipf) {
    crossPageIPF := true.B // higher 16 bits page fault
  }

  val fetchPacketValid = if4_valid && !io.redirect.valid && ftqEnqBuf_ready
  val fetchPacketWire = Wire(new FetchPacket)

  fetchPacketWire.mask := if4_real_valids
  //RVC expand
  val expandedInstrs = Wire(Vec(PredictWidth, UInt(32.W)))
  for(i <- 0 until PredictWidth){
      val expander = Module(new RVCExpander)
      expander.io.in := if4_pd.instrs(i)
      expandedInstrs(i) := expander.io.out.bits
  }
  fetchPacketWire.instrs := expandedInstrs

  fetchPacketWire.pc := if4_pd.pc
  fetchPacketWire.foldpc := if4_pd.pc.map(i => XORFold(i(VAddrBits-1,1), WaitTableAddrWidth))

  fetchPacketWire.pdmask := if4_pd.mask
  fetchPacketWire.pd := if4_pd.pd
  fetchPacketWire.ipf := if4_ipf
  fetchPacketWire.acf := if4_acf
  fetchPacketWire.crossPageIPFFix := if4_crossPageIPF
  fetchPacketWire.ftqPtr := if4_ftqEnqPtr

  // predTaken Vec
  fetchPacketWire.pred_taken := if4_bp.takens

  io.fetchPacket.bits := fetchPacketWire
  io.fetchPacket.valid := fetchPacketValid

  if (!env.FPGAPlatform && env.EnablePerfDebug) {
    val predictor_s3 = RegEnable(Mux(if3_redirect, 1.U(log2Up(4).W), 0.U(log2Up(4).W)), if3_fire)
    val predictor_s4 = Mux(if4_redirect, 2.U, predictor_s3)
    val predictor = predictor_s4
    toFtqBuf.metas.map(_.predictor := predictor)

    toFtqBuf.metas.zipWithIndex.foreach{ case(x,i) =>
      x.predictor := predictor

      x.ubtbAns := bpu.io.brInfo.metas(i).ubtbAns
      x.btbAns := bpu.io.brInfo.metas(i).btbAns
      x.tageAns := bpu.io.brInfo.metas(i).tageAns
      x.rasAns := bpu.io.brInfo.metas(i).rasAns // Is this right?
      x.loopAns := bpu.io.brInfo.metas(i).loopAns
    }
  }

  // TODO: perfs
  // frontend redirect from each stage
  XSPerf("if2_redirect", if2_valid && if2_bp.taken && !if2_flush)
  XSPerf("if2_redirect_fired", if2_fire && if2_bp.taken && !if2_flush)
  XSPerf("if3_redirect", if3_valid && if3_redirect && !if3_flush)
  XSPerf("if3_redirect_fired", if3_fire && if3_redirect && !if3_flush)
  XSPerf("if4_redirect", if4_valid && if4_redirect && !if4_flush)
  XSPerf("if4_redirect_fired", if4_fire && if4_redirect && !if4_flush)
  
  XSPerf("if1_total_stall", !if2_allReady && if1_valid)
  XSPerf("if1_stall_from_icache_req", !icache.io.req.ready && if1_valid)
  XSPerf("if1_stall_from_if2", !if2_ready && if1_valid)
  XSPerf("if1_stall_from_bpu", !bpu.io.in_ready && if1_valid)
  XSPerf("itlb_stall", if2_valid && if3_ready && !icache.io.tlb.resp.valid)
  XSPerf("icache_resp_stall", if3_valid && if4_ready && !icache.io.resp.valid)
  XSPerf("if4_stall", if4_valid && !if4_fire)
  XSPerf("if4_stall_ibuffer", if4_valid && !io.fetchPacket.ready && ftqEnqBuf_ready)
  XSPerf("if4_stall_ftq", if4_valid && io.fetchPacket.ready && !ftqEnqBuf_ready)

  XSPerf("if3_prevHalfConsumed", if3_prevHalfConsumed)
  XSPerf("if4_prevHalfConsumed", if4_prevHalfConsumed)
  

  // debug info
  if (IFUDebug) {
    XSDebug(RegNext(reset.asBool) && !reset.asBool, "Reseting...\n")
    XSDebug(icache.io.flush(0).asBool, "Flush icache stage2...\n")
    XSDebug(icache.io.flush(1).asBool, "Flush icache stage3...\n")
    XSDebug(io.redirect.valid, p"Redirect from backend! target=${Hexadecimal(io.redirect.bits.cfiUpdate.target)}\n")

    XSDebug("[IF1] v=%d      fire=%d             flush=%d pc=%x mask=%b\n", if1_valid, if1_fire, if1_flush, if1_npc, mask(if1_npc))
    XSDebug("[IF2] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x snpc=%x\n", if2_valid, if2_ready, if2_fire, if2_redirect, if2_flush, if2_pc, if2_snpc)
    XSDebug("[IF3] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x crossPageIPF=%d sawNTBrs=%d\n", if3_valid, if3_ready, if3_fire, if3_redirect, if3_flush, if3_pc, crossPageIPF, if3_bp.hasNotTakenBrs)
    XSDebug("[IF4] v=%d r=%d fire=%d redirect=%d flush=%d pc=%x crossPageIPF=%d sawNTBrs=%d\n", if4_valid, if4_ready, if4_fire, if4_redirect, if4_flush, if4_pc, if4_crossPageIPF, if4_bp.hasNotTakenBrs)
    XSDebug("[IF1][icacheReq] v=%d r=%d addr=%x\n", icache.io.req.valid, icache.io.req.ready, icache.io.req.bits.addr)
    XSDebug("[IF1][ghr] hist=%b\n", if1_gh.asUInt)

    XSDebug("[IF2][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n\n", if2_bp.taken, if2_bp.jmpIdx, if2_bp.hasNotTakenBrs, if2_bp.target, if2_bp.saveHalfRVI)
    if2_gh.debug("if2")

    XSDebug("[IF3][icacheResp] v=%d r=%d pc=%x mask=%b\n", icache.io.resp.valid, icache.io.resp.ready, icache.io.resp.bits.pc, icache.io.resp.bits.mask)
    XSDebug("[IF3][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if3_bp.taken, if3_bp.jmpIdx, if3_bp.hasNotTakenBrs, if3_bp.target, if3_bp.saveHalfRVI)
    XSDebug("[IF3][redirect]: v=%d, prevNMet=%d, predT=%d, predNT=%d\n", if3_redirect, if3_prevHalfNotMetRedirect, if3_predTakenRedirect, if3_predNotTakenRedirect)
    // XSDebug("[IF3][prevHalfInstr] v=%d redirect=%d fetchpc=%x idx=%d tgt=%x taken=%d instr=%x\n\n",
    //   prev_half_valid, prev_half_redirect, prev_half_fetchpc, prev_half_idx, prev_half_tgt, prev_half_taken, prev_half_instr)
    XSDebug("[IF3][if3_prevHalfInstr] v=%d pc=%x npc=%x  instr=%x ipf=%d\n\n",
    if3_prevHalfInstr.valid, if3_prevHalfInstr.bits.pc, if3_prevHalfInstr.bits.npc, if3_prevHalfInstr.bits.instr, if3_prevHalfInstr.bits.ipf)
    if3_gh.debug("if3")

    XSDebug("[IF4][predecode] mask=%b\n", if4_pd.mask)
    XSDebug("[IF4][snpc]: %x, realMask=%b\n", if4_snpc, if4_mask)
    XSDebug("[IF4][bp] taken=%d jmpIdx=%d hasNTBrs=%d target=%x saveHalfRVI=%d\n", if4_bp.taken, if4_bp.jmpIdx, if4_bp.hasNotTakenBrs, if4_bp.target, if4_bp.saveHalfRVI)
    XSDebug("[IF4][redirect]: v=%d, prevNotMet=%d, predT=%d, predNT=%d\n", if4_redirect, if4_prevHalfNextNotMet, if4_predTakenRedirect, if4_predNotTakenRedirect)
    XSDebug(if4_pd.pd(if4_bp.jmpIdx).isJal && if4_bp.taken, "[IF4] cfi is jal!  instr=%x target=%x\n", if4_instrs(if4_bp.jmpIdx), if4_jal_tgts(if4_bp.jmpIdx))
    XSDebug("[IF4][ prevHalfInstrReq] v=%d pc=%x npc=%x instr=%x ipf=%d\n",
      prevHalfInstrReq.valid, prevHalfInstrReq.bits.pc, prevHalfInstrReq.bits.npc, prevHalfInstrReq.bits.instr, prevHalfInstrReq.bits.ipf)
    XSDebug("[IF4][if4_prevHalfInstr] v=%d pc=%x npc=%x instr=%x ipf=%d\n",
      if4_prevHalfInstr.valid, if4_prevHalfInstr.bits.pc, if4_prevHalfInstr.bits.npc, if4_prevHalfInstr.bits.instr, if4_prevHalfInstr.bits.ipf)
    if4_gh.debug("if4")
    XSDebug(io.fetchPacket.fire(), "[IF4][fetchPacket] v=%d r=%d mask=%b ipf=%d acf=%d crossPageIPF=%d\n",
      io.fetchPacket.valid, io.fetchPacket.ready, io.fetchPacket.bits.mask, io.fetchPacket.bits.ipf, io.fetchPacket.bits.acf, io.fetchPacket.bits.crossPageIPFFix)
    for (i <- 0 until PredictWidth) {
      XSDebug(io.fetchPacket.fire(), "[IF4][fetchPacket] %b %x pc=%x pd: rvc=%d brType=%b call=%d ret=%d\n",
        io.fetchPacket.bits.mask(i),
        io.fetchPacket.bits.instrs(i),
        io.fetchPacket.bits.pc(i),
        io.fetchPacket.bits.pd(i).isRVC,
        io.fetchPacket.bits.pd(i).brType,
        io.fetchPacket.bits.pd(i).isCall,
        io.fetchPacket.bits.pd(i).isRet
      )
    }
    val b = ftqEnqBuf
    XSDebug("[FtqEnqBuf] v=%d r=%d pc=%x cfiIndex(%d)=%d cfiIsCall=%d cfiIsRet=%d cfiIsJalr=%d cfiIsRVC=%d\n",
      ftqEnqBuf_valid, ftqEnqBuf_ready, b.ftqPC, b.cfiIndex.valid, b.cfiIndex.bits, b.cfiIsCall, b.cfiIsRet, b.cfiIsJalr, b.cfiIsRVC)
    XSDebug("[FtqEnqBuf] valids=%b br_mask=%b rvc_mask=%b hist=%x predHist=%x rasSp=%d rasTopAddr=%x rasTopCtr=%d\n",
      b.valids.asUInt, b.br_mask.asUInt, b.rvc_mask.asUInt, b.hist.asUInt, b.predHist.asUInt, b.rasSp, b.rasTop.retAddr, b.rasTop.ctr)
    XSDebug("[ToFTQ] v=%d r=%d leftOne=%d ptr=%d\n", io.toFtq.valid, io.toFtq.ready, io.ftqLeftOne, io.ftqEnqPtr.value)
  }

}
