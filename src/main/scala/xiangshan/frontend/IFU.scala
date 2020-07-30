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
  // def snpc(pc: UInt): UInt = pc + (1 << groupAlign).U
  def mask(pc: UInt): UInt = (Fill(PredictWidth * 2, 1.U(1.W)) >> pc(groupAlign - 1, 1))(PredictWidth - 1, 0)
  def snpc(pc: UInt): UInt = pc + (PopCount(mask(pc)) << 1)
}

class IFUIO extends XSBundle
{
  val fetchPacket = DecoupledIO(new FetchPacket)
  val redirect = Flipped(ValidIO(new Redirect))
  val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  val icacheReq = DecoupledIO(new FakeIcacheReq)
  val icacheResp = Flipped(DecoupledIO(new FakeIcacheResp))
}


class IFU extends XSModule with HasIFUConst
{
  val io = IO(new IFUIO)
  val bpu = if (EnableBPD) Module(new BPU) else Module(new FakeBPU)

  val if2_redirect, if3_redirect, if4_redirect = WireInit(false.B)
  val if1_flush, if2_flush, if3_flush, if4_flush = WireInit(false.B)

  //********************** IF1 ****************************//
  val if1_valid = !reset.asBool
  val if1_npc = WireInit(0.U(VAddrBits.W))
  val if2_ready = WireInit(false.B)
  val if1_fire = if1_valid && (if2_ready || if1_flush) && io.icacheReq.ready

  val extHist = RegInit(Vec(ExtHistoryLength, 0.U(1.W)))
  val headPtr = RegInit(0.U(log2Up(ExtHistoryLength).W))
  val updateHist = io.outOfOrderBrInfo.valid
  val newPtr = Wire(UInt(log2Up(ExtHistoryLength).W))
  val ptr = Mux(updateHist, newPtr, headPtr)
  when (updateHist) { headPtr := newPtr }
  val hist = Wire(Vec(HistoryLength, UInt(1.W)))
  for (i <- 0 until HistoryLength) {
    hist(i) := extHist(ptr + i.U)
  }

  //********************** IF2 ****************************//
  val if2_valid = RegEnable(next = if1_valid, init = false.B, enable = if1_fire)
  val if3_ready = WireInit(false.B)
  val if2_fire = if2_valid && if3_ready
  val if2_pc = RegEnable(next = if1_npc, init = resetVector.U, enable = if1_fire)
  val if2_snpc = snpc(if2_pc)
  if2_ready := if2_fire || !if2_valid

  when (RegNext(reset.asBool) && !reset.asBool) {
    if1_npc := resetVector.U(VAddrBits.W)
  }.elsewhen (if2_fire) {
    if1_npc := if2_snpc
  }.otherwise {
    if1_npc := RegNext(if1_npc)
  }

  val if2_bp = bpu.io.out(0).bits
  val if2_prev_half_valid = RegInit(false.B)
  val if2_prev_half_redirect = RegInit(false.B)
  val if2_prev_half_fetchpc = Reg(UInt(VAddrBits.W))
  val if2_prev_half_idx = Reg(UInt(log2Up(PredictWidth).W))
  val if2_prev_half_tgt = Reg(UInt(VAddrBits.W))
  val if2_prev_half_taken = RegInit(false.B)
  when (if2_flush) {
    if2_prev_half_valid := false.B
    if2_prev_half_redirect := false.B
  }.elsewhen (if2_fire && if2_bp.saveHalfRVI) {
    if2_prev_half_valid := true.B
    if2_prev_half_redirect := if2_bp.redirect && bpu.io.out(0).valid
    if2_prev_half_fetchpc := if2_pc
    if2_prev_half_idx := Mux(if2_bp.redirect && bpu.io.out(0).valid, if2_bp.jmpIdx, PopCount(mask(if2_pc)) - 1.U)
    if2_prev_half_tgt := if2_bp.target
    if2_prev_half_taken := if2_bp.taken
  }.elsewhen (if2_fire) {
    if2_prev_half_valid := false.B
    if2_prev_half_redirect := false.B
  }

  if2_redirect := if2_fire && (if2_prev_half_valid && if2_prev_half_redirect || bpu.io.out(0).valid && if2_bp.redirect && !if2_bp.saveHalfRVI)
  when (if2_redirect) { 
    if1_npc := Mux(if2_prev_half_valid && if2_prev_half_redirect, if2_prev_half_tgt, if2_bp.target)
  }

  //********************** IF3 ****************************//
  val if3_valid = RegEnable(next = if2_valid, init = false.B, enable = if2_fire)
  val if4_ready = WireInit(false.B)
  val if3_fire = if3_valid && if4_ready && io.icacheResp.valid


  //********************** IF4 ****************************//



  io.icacheReq.valid := if1_valid
  io.icacheReq.bits.addr := if1_npc
  // io.icacheReq.bits.flush := 
  io.icacheResp.ready := if3_valid

  bpu.io.inOrderBrInfo <> io.inOrderBrInfo
  bpu.io.flush := Cat(if4_flush, if3_flush, if2_flush)
  bpu.io.in.valid := if1_fire
  bpu.io.in.bits.pc := if1_npc
  bpu.io.in.bits.hist := hist.asUInt
  bpu.io.in.bits.inMask := mask(if1_npc)
  bpu.io.out(0).ready := if2_fire
  // bpu.io.out(1).ready
  // bpu.io.out(2).ready
}