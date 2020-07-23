 package xiangshan.frontend

import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._
import utils._

trait HasIFUConst { this: XSModule =>
  val resetVector = 0x80000000L//TODO: set reset vec
  val groupAlign = log2Up(FetchWidth * 4)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def snpc(pc: UInt): UInt = pc + (1 << groupAlign).U
  def maskExp(mask: UInt): UInt = Cat(mask.asBools.map(Fill(2,_)).reverse)
  
}

class IFUIO extends XSBundle
{
    val fetchPacket = DecoupledIO(new FetchPacket)
    val redirectInfo = Input(new RedirectInfo)
    val icacheReq = DecoupledIO(new FakeIcacheReq)
    val icacheResp = Flipped(DecoupledIO(new FakeIcacheResp))
}


class FakeBPU extends XSModule{
  val io = IO(new Bundle() {
    val redirectInfo = Input(new RedirectInfo)
    val in = new Bundle { val pc = Flipped(Valid(UInt(VAddrBits.W))) }
    val btbOut = ValidIO(new BranchPrediction)
    val tageOut = Decoupled(new BranchPrediction)
    val predecode = Flipped(ValidIO(new Predecode))
  })

  io.btbOut.valid := true.B
  io.btbOut.bits <> DontCare
  io.btbOut.bits.redirect := false.B
  io.tageOut.valid := false.B
  io.tageOut.bits <> DontCare
}


class IFU extends XSModule with HasIFUConst
{
    val io = IO(new IFUIO)
    val bpu = if(EnableBPU) Module(new BPU) else Module(new FakeBPU)

    //-------------------------
    //      IF1  PC update
    //-------------------------
    //local
    val if1_npc = WireInit(0.U(VAddrBits.W))
    val if1_valid = !reset.asBool 
    val if1_pc = RegInit(resetVector.U(VAddrBits.W))
    //next
    val if2_ready = WireInit(false.B)
    val if2_snpc = snpc(if1_pc) //TODO: calculate snpc according to mask of current fetch packet
    val needflush = WireInit(false.B)
    // when an RVI instruction is predicted as taken and it crosses over two fetch packets,
    // IFU should not take this branch but fetch the latter half of the instruction sequentially,
    // and take the jump target in the next fetch cycle
    val if2_lateJumpLatch = WireInit(false.B)
    val if2_lateJumpTarget = RegInit(0.U(VAddrBits.W))
    val if4_lateJumpLatch = WireInit(false.B)
    val if4_lateJumpTarget = RegInit(0.U(VAddrBits.W))

    //pipe fire
    val if1_fire = if1_valid && if2_ready || needflush
    val if1_pcUpdate = if1_fire || needflush

    when(if1_pcUpdate)
    { 
      if1_pc := if1_npc
    }

    bpu.io.in.pc.valid := if1_fire
    bpu.io.in.pc.bits := if1_npc
    bpu.io.redirectInfo := io.redirectInfo

    XSDebug("[IF1]if1_valid:%d  ||  if1_npc:0x%x  || if1_pcUpdate:%d if1_pc:0x%x  || if2_ready:%d",if1_valid,if1_npc,if1_pcUpdate,if1_pc,if2_ready)
    XSDebug(false,if1_fire,"------IF1->fire!!!")
    XSDebug(false,true.B,"\n")
    //-------------------------
    //      IF2  btb response 
    //           icache visit
    //-------------------------
    //local
    val if2_valid = RegEnable(next=if1_valid,init=false.B,enable=if1_fire)
    val if2_pc = if1_pc
    val if2_btb_taken = bpu.io.btbOut.valid && bpu.io.btbOut.bits.redirect
    val if2_btb_lateJump = WireInit(false.B)
    val if2_btb_insMask = Mux(if2_btb_taken, bpu.io.btbOut.bits.instrValid.asUInt, Fill(FetchWidth*2, 1.U(1.W))) // TODO: FIX THIS
    val if2_btb_target = Mux(if2_btb_lateJump, if2_snpc, bpu.io.btbOut.bits.target)

    if2_lateJumpLatch := BoolStopWatch(if2_btb_lateJump, if1_fire, startHighPriority = true)
    // since late jump target should be taken after the latter half of late jump instr is fetched, we need to latch this target
    when (if2_btb_lateJump) {
      if2_lateJumpTarget := bpu.io.btbOut.bits.target
    }

    //next
    val if3_ready = WireInit(false.B)


    //pipe fire
    val if2_fire = if2_valid && if3_ready && io.icacheReq.fire()
    if2_ready := (if2_fire) || !if2_valid

    io.icacheReq.valid := if2_valid
    io.icacheReq.bits.addr := if2_pc

    when(RegNext(reset.asBool) && !reset.asBool){
      XSDebug("RESET....\n")
      if1_npc := resetVector.U(VAddrBits.W)
    }.elsewhen (if2_fire) {
      if1_npc := Mux(if4_lateJumpLatch, if4_lateJumpTarget, Mux(if2_lateJumpLatch, if2_lateJumpTarget, if2_snpc))
    }.otherwise {
      if1_npc := if1_pc
    }

    //redirect: when if2 fire and if2 redirects, update npc
    when(if2_fire && if2_btb_taken)
    {
      if1_npc := if2_btb_target
    }

    bpu.io.in.pc.valid := if1_fire && !if2_btb_lateJump

    XSDebug("[IF2]if2_valid:%d  ||  if2_pc:0x%x   || if3_ready:%d                                        ",if2_valid,if2_pc,if3_ready)
    XSDebug(false,if2_fire,"------IF2->fire!!!")
    XSDebug(false,true.B,"\n")
    XSDebug("[IF2-Icache-Req] icache_in_valid:%d  icache_in_ready:%d\n",io.icacheReq.valid,io.icacheReq.ready)
    XSDebug("[IF2-BPU-out]if2_btbTaken:%d || if2_btb_insMask:%b || if2_btb_target:0x%x \n",if2_btb_taken,if2_btb_insMask.asUInt,if2_btb_target)
    //-------------------------
    //      IF3  icache hit check
    //-------------------------
    //local
    val if3_valid = RegEnable(next=if2_valid,init=false.B,enable=if2_fire)
    val if3_pc = RegEnable(if2_pc,if2_fire)
    val if3_npc = RegEnable(if1_npc, if2_fire)
    val if3_btb_target = RegEnable(Mux(if2_lateJumpLatch, if2_lateJumpTarget, Mux(if2_btb_lateJump, bpu.io.btbOut.bits.target, if2_btb_target)), if2_fire)
    val if3_btb_taken = RegEnable(Mux(if2_lateJumpLatch, true.B, if2_btb_taken), if2_fire)
    val if3_btb_insMask = RegEnable(Mux(if2_lateJumpLatch, 1.U((FetchWidth*2).W), if2_btb_insMask), if2_fire)
    val if3_btb_lateJump = RegEnable(if2_btb_lateJump, if2_fire)

    //next
    val if4_ready = WireInit(false.B)

    //pipe fire
    val if3_fire = if3_valid && if4_ready
    if3_ready := if3_fire  || !if3_valid


    XSDebug("[IF3]if3_valid:%d  ||  if3_pc:0x%x   if3_npc:0x%x || if4_ready:%d                    ",if3_valid,if3_pc,if3_npc,if4_ready)
    XSDebug(false,if3_fire,"------IF3->fire!!!")
    XSDebug(false,true.B,"\n")
    XSDebug("[IF3]if3_btb_taken:%d if3_btb_insMask:%b if3_btb_lateJump:%d if3_btb_target:0x%x\n",if3_btb_taken, if3_btb_insMask, if3_btb_lateJump, if3_btb_target)
    
    //-------------------------
    //      IF4  icache response   
    //           RAS result
    //           taget result
    //-------------------------
    val if4_valid = RegEnable(next=if3_valid,init=false.B,enable=if3_fire)
    val if4_pc = RegEnable(if3_pc,if3_fire)
    val if4_btb_target = RegEnable(if3_btb_target,if3_fire)
    val if4_btb_taken = RegEnable(if3_btb_taken,if3_fire)
    val if4_btb_insMask = RegEnable(if3_btb_insMask, if3_fire)
    val if4_btb_lateJump = RegEnable(if3_btb_lateJump, if3_fire)
    val if4_start_ready = io.fetchPacket.ready && (GTimer() > 500.U)
    //from BPU Stage3
    val if4_tage_redirect = bpu.io.tageOut.valid && bpu.io.tageOut.bits.redirect
    val if4_tage_lateJump = if4_tage_redirect && bpu.io.tageOut.bits.lateJump && !io.redirectInfo.flush()
    val if4_tage_insMask = bpu.io.tageOut.bits.instrValid
    val if4_snpc = if4_pc + (PopCount(if4_tage_insMask) << 1.U)
    val if4_tage_target = Mux(if4_tage_lateJump, if4_snpc, bpu.io.tageOut.bits.target)
    //frome predecode
    val if4_predec_mask = io.icacheResp.bits.predecode.mask
    val if4_predec_isRVC = io.icacheResp.bits.predecode.isRVC

    if4_ready := (io.fetchPacket.fire() || !if4_valid) && if4_start_ready 
  

    if2_btb_lateJump := if2_btb_taken && bpu.io.btbOut.bits.lateJump && !io.redirectInfo.flush() && !if4_tage_redirect

    if4_lateJumpLatch := BoolStopWatch(if4_tage_lateJump, if1_fire, startHighPriority = true)
    when (if4_tage_lateJump) {
      if4_lateJumpTarget := bpu.io.tageOut.bits.target
    }

    bpu.io.in.pc.valid := if1_fire && !if2_btb_lateJump && !if4_tage_lateJump

    XSDebug("[IF4]if4_valid:%d  ||  if4_pc:0x%x \n",if4_valid,if4_pc)
    XSDebug("[IF4]          if4_btb_taken:%d  if4_btb_lateJump:%d  if4_btb_insMask:%b  if4_btb_target:0x%x\n",if4_btb_taken, if4_btb_lateJump, if4_btb_insMask.asUInt, if4_btb_target)
    XSDebug("[IF4-TAGE-out]if4_tage_redirect:%d if4_tage_lateJump:%d if4_tage_insMask:%b if4_tage_target:0x%x\n",if4_tage_redirect,if4_tage_lateJump,if4_tage_insMask.asUInt,if4_tage_target)
    XSDebug("[IF4-ICACHE-RESP]icacheResp.valid:%d   icacheResp.ready:%d\n",io.icacheResp.valid,io.icacheResp.ready)

    //redirect: when tage result differ from btb
    when(if4_tage_redirect)
    {
      if1_npc := if4_tage_target
    }

    //redirect: miss predict
    when(io.redirectInfo.flush())
    {
      if1_npc := io.redirectInfo.redirect.target
    }
    XSDebug(io.redirectInfo.flush(),"[IFU-REDIRECT] target:0x%x  \n",io.redirectInfo.redirect.target.asUInt)
    
  
    //flush pipline
    needflush := if4_tage_redirect || io.redirectInfo.flush()
    when(needflush){
      if3_valid := false.B
      if4_valid := false.B
    }

    //flush ICache register
    io.icacheReq.bits.flush := needflush

    //to BPU
    bpu.io.predecode.valid := io.icacheResp.fire() && if4_valid
    bpu.io.predecode.bits <> io.icacheResp.bits.predecode
    //TODO: consider RVC && consider cross cacheline fetch
    bpu.io.predecode.bits.mask := if4_predec_mask
    bpu.io.predecode.bits.isRVC := if4_predec_isRVC
    bpu.io.redirectInfo := io.redirectInfo
    io.icacheResp.ready := if4_start_ready

    //-------------------------
    //      Output  fetch packet   
    //           -> Ibuffer
    //-------------------------
    io.fetchPacket.valid := if4_valid && io.icacheResp.valid && !io.redirectInfo.flush()
    io.fetchPacket.bits.instrs := io.icacheResp.bits.icacheOut
    io.fetchPacket.bits.mask := Mux(if4_lateJumpLatch,  1.U((FetchWidth*2).W),
                                Mux(if4_tage_redirect,  if4_predec_mask.asUInt & if4_tage_insMask.asUInt,
                                                        if4_predec_mask.asUInt & if4_btb_insMask.asUInt))
    io.fetchPacket.bits.pc := if4_pc

    XSDebug(io.fetchPacket.fire,"[IFU-Out-FetchPacket] starPC:0x%x   GroupPC:0x%xn\n",if4_pc.asUInt,groupPC(if4_pc).asUInt)
    XSDebug(io.fetchPacket.fire,"[IFU-Out-FetchPacket] instrmask %b\n",io.fetchPacket.bits.mask.asUInt)
    for(i <- 0 until (FetchWidth*2)) {
      when (if4_btb_taken && !if4_tage_redirect && i.U === OHToUInt(HighestBit(if4_btb_insMask.asUInt, FetchWidth*2))) {
        io.fetchPacket.bits.pnpc(i) := if4_btb_target
        if (i != 0) {
          when (!io.icacheResp.bits.predecode.isRVC(i) && !if4_btb_lateJump) {
            io.fetchPacket.bits.pnpc(i-1) := if4_btb_target
          }
        }
      }.elsewhen (if4_tage_redirect && i.U === OHToUInt(HighestBit(if4_tage_insMask.asUInt, FetchWidth*2))) {
        io.fetchPacket.bits.pnpc(i) := Mux(if4_tage_lateJump, bpu.io.tageOut.bits.target, if4_tage_target)
        if (i != 0) {
          when (!io.icacheResp.bits.predecode.isRVC(i) && !if4_tage_lateJump) {
            io.fetchPacket.bits.pnpc(i-1) := if4_tage_target
          }
        }
      }.otherwise {
        io.fetchPacket.bits.pnpc(i) := if4_pc + (i.U << 1.U) + Mux(io.icacheResp.bits.predecode.isRVC(i), 2.U, 4.U)
      }
      XSDebug(io.fetchPacket.fire,"[IFU-Out-FetchPacket] instruction %x    pnpc:0x%x\n",
        Mux((i.U)(0), io.fetchPacket.bits.instrs(i>>1)(31,16), io.fetchPacket.bits.instrs(i>>1)(15,0)),
        io.fetchPacket.bits.pnpc(i))
    }
    io.fetchPacket.bits.hist := bpu.io.tageOut.bits.hist
    io.fetchPacket.bits.predCtr := bpu.io.tageOut.bits.predCtr
    io.fetchPacket.bits.btbHit := bpu.io.tageOut.bits.btbHit
    io.fetchPacket.bits.tageMeta := bpu.io.tageOut.bits.tageMeta
    io.fetchPacket.bits.rasSp := bpu.io.tageOut.bits.rasSp
    io.fetchPacket.bits.rasTopCtr := bpu.io.tageOut.bits.rasTopCtr
    bpu.io.tageOut.ready := if4_start_ready


}

