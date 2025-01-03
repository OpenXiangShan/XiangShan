/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
*
*
* Acknowledgement
*
* This implementation is inspired by several key papers:
* [1] Glenn Reinman, Todd Austin, and Brad Calder. "[A scalable front-end architecture for fast instruction delivery.]
* (https://doi.org/10.1109/ISCA.1999.765954)" 26th International Symposium on Computer Architecture (ISCA). 1999.
*
***************************************************************************************/

package xiangshan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.ChiselDB
import utils._
import xiangshan._
import xiangshan.backend.CtrlToFtqIO
import xiangshan.backend.decode.ImmUnion
import xiangshan.frontend.icache._

class FtqDebugBundle extends Bundle {
  val pc        = UInt(39.W)
  val target    = UInt(39.W)
  val isBr      = Bool()
  val isJmp     = Bool()
  val isCall    = Bool()
  val isRet     = Bool()
  val misPred   = Bool()
  val isTaken   = Bool()
  val predStage = UInt(2.W)
}

class FtqPtr(entries: Int) extends CircularQueuePtr[FtqPtr](
      entries
    ) {
  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).FtqSize)
}

object FtqPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): FtqPtr = {
    val ptr = Wire(new FtqPtr)
    ptr.flag  := f
    ptr.value := v
    ptr
  }
  def inverse(ptr: FtqPtr)(implicit p: Parameters): FtqPtr =
    apply(!ptr.flag, ptr.value)
}

class FtqNRSRAM[T <: Data](gen: T, numRead: Int)(implicit p: Parameters) extends XSModule {

  val io = IO(new Bundle() {
    val raddr = Input(Vec(numRead, UInt(log2Up(FtqSize).W)))
    val ren   = Input(Vec(numRead, Bool()))
    val rdata = Output(Vec(numRead, gen))
    val waddr = Input(UInt(log2Up(FtqSize).W))
    val wen   = Input(Bool())
    val wdata = Input(gen)
  })

  for (i <- 0 until numRead) {
    val sram = Module(new SRAMTemplate(gen, FtqSize, withClockGate = true))
    sram.io.r.req.valid       := io.ren(i)
    sram.io.r.req.bits.setIdx := io.raddr(i)
    io.rdata(i)               := sram.io.r.resp.data(0)
    sram.io.w.req.valid       := io.wen
    sram.io.w.req.bits.setIdx := io.waddr
    sram.io.w.req.bits.data   := VecInit(io.wdata)
  }

}

class Ftq_RF_Components(implicit p: Parameters) extends XSBundle with BPUUtils {
  val startAddr     = UInt(VAddrBits.W)
  val nextLineAddr  = UInt(VAddrBits.W)
  val isNextMask    = Vec(PredictWidth, Bool())
  val fallThruError = Bool()
  // val carry = Bool()
  def getPc(offset: UInt) = {
    def getHigher(pc: UInt) = pc(VAddrBits - 1, log2Ceil(PredictWidth) + instOffsetBits + 1)
    def getOffset(pc: UInt) = pc(log2Ceil(PredictWidth) + instOffsetBits, instOffsetBits)
    Cat(
      getHigher(Mux(isNextMask(offset) && startAddr(log2Ceil(PredictWidth) + instOffsetBits), nextLineAddr, startAddr)),
      getOffset(startAddr) + offset,
      0.U(instOffsetBits.W)
    )
  }
  def fromBranchPrediction(resp: BranchPredictionBundle) = {
    def carryPos(addr: UInt) = addr(instOffsetBits + log2Ceil(PredictWidth) + 1)
    this.startAddr    := resp.pc(3)
    this.nextLineAddr := resp.pc(3) + (FetchWidth * 4 * 2).U // may be broken on other configs
    this.isNextMask := VecInit((0 until PredictWidth).map(i =>
      (resp.pc(3)(log2Ceil(PredictWidth), 1) +& i.U)(log2Ceil(PredictWidth)).asBool
    ))
    this.fallThruError := resp.fallThruError(3)
    this
  }
  override def toPrintable: Printable =
    p"startAddr:${Hexadecimal(startAddr)}"
}

class Ftq_pd_Entry(implicit p: Parameters) extends XSBundle {
  val brMask    = Vec(PredictWidth, Bool())
  val jmpInfo   = ValidUndirectioned(Vec(3, Bool()))
  val jmpOffset = UInt(log2Ceil(PredictWidth).W)
  val jalTarget = UInt(VAddrBits.W)
  val rvcMask   = Vec(PredictWidth, Bool())
  def hasJal    = jmpInfo.valid && !jmpInfo.bits(0)
  def hasJalr   = jmpInfo.valid && jmpInfo.bits(0)
  def hasCall   = jmpInfo.valid && jmpInfo.bits(1)
  def hasRet    = jmpInfo.valid && jmpInfo.bits(2)

  def fromPdWb(pdWb: PredecodeWritebackBundle) = {
    val pds = pdWb.pd
    this.brMask        := VecInit(pds.map(pd => pd.isBr && pd.valid))
    this.jmpInfo.valid := VecInit(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid)).asUInt.orR
    this.jmpInfo.bits := ParallelPriorityMux(
      pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid),
      pds.map(pd => VecInit(pd.isJalr, pd.isCall, pd.isRet))
    )
    this.jmpOffset := ParallelPriorityEncoder(pds.map(pd => (pd.isJal || pd.isJalr) && pd.valid))
    this.rvcMask   := VecInit(pds.map(pd => pd.isRVC))
    this.jalTarget := pdWb.jalTarget
  }

  def toPd(offset: UInt) = {
    require(offset.getWidth == log2Ceil(PredictWidth))
    val pd = Wire(new PreDecodeInfo)
    pd.valid := true.B
    pd.isRVC := rvcMask(offset)
    val isBr   = brMask(offset)
    val isJalr = offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(0)
    pd.brType := Cat(offset === jmpOffset && jmpInfo.valid, isJalr || isBr)
    pd.isCall := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(1)
    pd.isRet  := offset === jmpOffset && jmpInfo.valid && jmpInfo.bits(2)
    pd
  }
}

class PrefetchPtrDB(implicit p: Parameters) extends Bundle {
  val fromFtqPtr = UInt(log2Up(p(XSCoreParamsKey).FtqSize).W)
  val fromIfuPtr = UInt(log2Up(p(XSCoreParamsKey).FtqSize).W)
}

class Ftq_Redirect_SRAMEntry(implicit p: Parameters) extends SpeculativeInfo {
  val sc_disagree = if (!env.FPGAPlatform) Some(Vec(numBr, Bool())) else None
}

class Ftq_1R_SRAMEntry(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val meta      = UInt(MaxMetaLength.W)
  val ftb_entry = new FTBEntry
}

class Ftq_Pred_Info(implicit p: Parameters) extends XSBundle {
  val target   = UInt(VAddrBits.W)
  val cfiIndex = ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))
}

class FtqRead[T <: Data](private val gen: T)(implicit p: Parameters) extends XSBundle {
  val valid  = Output(Bool())
  val ptr    = Output(new FtqPtr)
  val offset = Output(UInt(log2Ceil(PredictWidth).W))
  val data   = Input(gen)
  def apply(valid: Bool, ptr: FtqPtr, offset: UInt) = {
    this.valid  := valid
    this.ptr    := ptr
    this.offset := offset
    this.data
  }
}

class FtqToBpuIO(implicit p: Parameters) extends XSBundle {
  val redirect       = Valid(new BranchPredictionRedirect)
  val update         = Valid(new BranchPredictionUpdate)
  val enq_ptr        = Output(new FtqPtr)
  val redirctFromIFU = Output(Bool())
}

class BpuFlushInfo(implicit p: Parameters) extends XSBundle with HasCircularQueuePtrHelper {
  // when ifu pipeline is not stalled,
  // a packet from bpu s3 can reach f1 at most
  val s2 = Valid(new FtqPtr)
  val s3 = Valid(new FtqPtr)
  def shouldFlushBy(src: Valid[FtqPtr], idx_to_flush: FtqPtr) =
    src.valid && !isAfter(src.bits, idx_to_flush)
  def shouldFlushByStage2(idx: FtqPtr) = shouldFlushBy(s2, idx)
  def shouldFlushByStage3(idx: FtqPtr) = shouldFlushBy(s3, idx)
}

class FtqToIfuIO(implicit p: Parameters) extends XSBundle {
  val req              = Decoupled(new FetchRequestBundle)
  val redirect         = Valid(new BranchPredictionRedirect)
  val topdown_redirect = Valid(new BranchPredictionRedirect)
  val flushFromBpu     = new BpuFlushInfo
}

class FtqToICacheIO(implicit p: Parameters) extends XSBundle {
  // NOTE: req.bits must be prepare in T cycle
  // while req.valid is set true in T + 1 cycle
  val req = Decoupled(new FtqToICacheRequestBundle)
}

class FtqToPrefetchIO(implicit p: Parameters) extends XSBundle {
  val req              = Decoupled(new FtqICacheInfo)
  val flushFromBpu     = new BpuFlushInfo
  val backendException = UInt(ExceptionType.width.W)
}

class FtqToCtrlIO(implicit p: Parameters) extends XSBundle {
  // write to backend pc mem
  val pc_mem_wen   = Output(Bool())
  val pc_mem_waddr = Output(UInt(log2Ceil(FtqSize).W))
  val pc_mem_wdata = Output(new Ftq_RF_Components)
  // newest target
  val newest_entry_en     = Output(Bool())
  val newest_entry_target = Output(UInt(VAddrBits.W))
  val newest_entry_ptr    = Output(new FtqPtr)
}

class FTBEntryGen(implicit p: Parameters) extends XSModule with HasBPUParameter {
  val io = IO(new Bundle {
    val start_addr     = Input(UInt(VAddrBits.W))
    val old_entry      = Input(new FTBEntry)
    val pd             = Input(new Ftq_pd_Entry)
    val cfiIndex       = Flipped(Valid(UInt(log2Ceil(PredictWidth).W)))
    val target         = Input(UInt(VAddrBits.W))
    val hit            = Input(Bool())
    val mispredict_vec = Input(Vec(PredictWidth, Bool()))

    val new_entry         = Output(new FTBEntry)
    val new_br_insert_pos = Output(Vec(numBr, Bool()))
    val taken_mask        = Output(Vec(numBr, Bool()))
    val jmp_taken         = Output(Bool())
    val mispred_mask      = Output(Vec(numBr + 1, Bool()))

    // for perf counters
    val is_init_entry           = Output(Bool())
    val is_old_entry            = Output(Bool())
    val is_new_br               = Output(Bool())
    val is_jalr_target_modified = Output(Bool())
    val is_strong_bias_modified = Output(Bool())
    val is_br_full              = Output(Bool())
  })

  // no mispredictions detected at predecode
  val hit = io.hit
  val pd  = io.pd

  val init_entry = WireInit(0.U.asTypeOf(new FTBEntry))

  val cfi_is_br       = pd.brMask(io.cfiIndex.bits) && io.cfiIndex.valid
  val entry_has_jmp   = pd.jmpInfo.valid
  val new_jmp_is_jal  = entry_has_jmp && !pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_jalr = entry_has_jmp && pd.jmpInfo.bits(0) && io.cfiIndex.valid
  val new_jmp_is_call = entry_has_jmp && pd.jmpInfo.bits(1) && io.cfiIndex.valid
  val new_jmp_is_ret  = entry_has_jmp && pd.jmpInfo.bits(2) && io.cfiIndex.valid
  val last_jmp_rvi    = entry_has_jmp && pd.jmpOffset === (PredictWidth - 1).U && !pd.rvcMask.last
  // val last_br_rvi = cfi_is_br && io.cfiIndex.bits === (PredictWidth-1).U && !pd.rvcMask.last

  val cfi_is_jal  = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jal
  val cfi_is_jalr = io.cfiIndex.bits === pd.jmpOffset && new_jmp_is_jalr

  def carryPos = log2Ceil(PredictWidth) + instOffsetBits
  def getLower(pc: UInt) = pc(carryPos - 1, instOffsetBits)
  // if not hit, establish a new entry
  init_entry.valid := true.B
  // tag is left for ftb to assign

  // case br
  val init_br_slot = init_entry.getSlotForBr(0)
  when(cfi_is_br) {
    init_br_slot.valid  := true.B
    init_br_slot.offset := io.cfiIndex.bits
    init_br_slot.setLowerStatByTarget(io.start_addr, io.target, numBr == 1)
    init_entry.strong_bias(0) := true.B // set to strong bias on init
  }

  // case jmp
  when(entry_has_jmp) {
    init_entry.tailSlot.offset := pd.jmpOffset
    init_entry.tailSlot.valid  := new_jmp_is_jal || new_jmp_is_jalr
    init_entry.tailSlot.setLowerStatByTarget(io.start_addr, Mux(cfi_is_jalr, io.target, pd.jalTarget), isShare = false)
    init_entry.strong_bias(numBr - 1) := new_jmp_is_jalr // set strong bias for the jalr on init
  }

  val jmpPft = getLower(io.start_addr) +& pd.jmpOffset +& Mux(pd.rvcMask(pd.jmpOffset), 1.U, 2.U)
  init_entry.pftAddr := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft, getLower(io.start_addr))
  init_entry.carry   := Mux(entry_has_jmp && !last_jmp_rvi, jmpPft(carryPos - instOffsetBits), true.B)

  require(
    isPow2(PredictWidth),
    "If PredictWidth does not satisfy the power of 2," +
      "pftAddr := getLower(io.start_addr) and carry := true.B  not working!!"
  )

  init_entry.isJalr := new_jmp_is_jalr
  init_entry.isCall := new_jmp_is_call
  init_entry.isRet  := new_jmp_is_ret
  // that means fall thru points to the middle of an inst
  init_entry.last_may_be_rvi_call := pd.jmpOffset === (PredictWidth - 1).U && !pd.rvcMask(pd.jmpOffset)

  // if hit, check whether a new cfi(only br is possible) is detected
  val oe              = io.old_entry
  val br_recorded_vec = oe.getBrRecordedVec(io.cfiIndex.bits)
  val br_recorded     = br_recorded_vec.asUInt.orR
  val is_new_br       = cfi_is_br && !br_recorded
  val new_br_offset   = io.cfiIndex.bits
  // vec(i) means new br will be inserted BEFORE old br(i)
  val allBrSlotsVec = oe.allSlotsForBr
  val new_br_insert_onehot = VecInit((0 until numBr).map {
    i =>
      i match {
        case 0 =>
          !allBrSlotsVec(0).valid || new_br_offset < allBrSlotsVec(0).offset
        case idx =>
          allBrSlotsVec(idx - 1).valid && new_br_offset > allBrSlotsVec(idx - 1).offset &&
          (!allBrSlotsVec(idx).valid || new_br_offset < allBrSlotsVec(idx).offset)
      }
  })

  val old_entry_modified = WireInit(io.old_entry)
  for (i <- 0 until numBr) {
    val slot = old_entry_modified.allSlotsForBr(i)
    when(new_br_insert_onehot(i)) {
      slot.valid  := true.B
      slot.offset := new_br_offset
      slot.setLowerStatByTarget(io.start_addr, io.target, i == numBr - 1)
      old_entry_modified.strong_bias(i) := true.B
    }.elsewhen(new_br_offset > oe.allSlotsForBr(i).offset) {
      old_entry_modified.strong_bias(i) := false.B
      // all other fields remain unchanged
    }.otherwise {
      // case i == 0, remain unchanged
      if (i != 0) {
        val noNeedToMoveFromFormerSlot = (i == numBr - 1).B && !oe.brSlots.last.valid
        when(!noNeedToMoveFromFormerSlot) {
          slot.fromAnotherSlot(oe.allSlotsForBr(i - 1))
          old_entry_modified.strong_bias(i) := oe.strong_bias(i)
        }
      }
    }
  }

  // two circumstances:
  // 1. oe: | br | j  |, new br should be in front of j, thus addr of j should be new pft
  // 2. oe: | br | br |, new br could be anywhere between, thus new pft is the addr of either
  //        the previous last br or the new br
  val may_have_to_replace = oe.noEmptySlotForNewBr
  val pft_need_to_change  = is_new_br && may_have_to_replace
  // it should either be the given last br or the new br
  when(pft_need_to_change) {
    val new_pft_offset =
      Mux(!new_br_insert_onehot.asUInt.orR, new_br_offset, oe.allSlotsForBr.last.offset)

    // set jmp to invalid
    old_entry_modified.pftAddr              := getLower(io.start_addr) + new_pft_offset
    old_entry_modified.carry                := (getLower(io.start_addr) +& new_pft_offset).head(1).asBool
    old_entry_modified.last_may_be_rvi_call := false.B
    old_entry_modified.isCall               := false.B
    old_entry_modified.isRet                := false.B
    old_entry_modified.isJalr               := false.B
  }

  val old_entry_jmp_target_modified = WireInit(oe)
  val old_target      = oe.tailSlot.getTarget(io.start_addr) // may be wrong because we store only 20 lowest bits
  val old_tail_is_jmp = !oe.tailSlot.sharing
  val jalr_target_modified = cfi_is_jalr && (old_target =/= io.target) && old_tail_is_jmp // TODO: pass full jalr target
  when(jalr_target_modified) {
    old_entry_jmp_target_modified.setByJmpTarget(io.start_addr, io.target)
    old_entry_jmp_target_modified.strong_bias := 0.U.asTypeOf(Vec(numBr, Bool()))
  }

  val old_entry_strong_bias    = WireInit(oe)
  val strong_bias_modified_vec = Wire(Vec(numBr, Bool())) // whether modified or not
  for (i <- 0 until numBr) {
    when(br_recorded_vec(0)) {
      old_entry_strong_bias.strong_bias(0) :=
        oe.strong_bias(0) && io.cfiIndex.valid && oe.brValids(0) && io.cfiIndex.bits === oe.brOffset(0)
    }.elsewhen(br_recorded_vec(numBr - 1)) {
      old_entry_strong_bias.strong_bias(0) := false.B
      old_entry_strong_bias.strong_bias(numBr - 1) :=
        oe.strong_bias(numBr - 1) && io.cfiIndex.valid && oe.brValids(numBr - 1) && io.cfiIndex.bits === oe.brOffset(
          numBr - 1
        )
    }
    strong_bias_modified_vec(i) := oe.strong_bias(i) && oe.brValids(i) && !old_entry_strong_bias.strong_bias(i)
  }
  val strong_bias_modified = strong_bias_modified_vec.reduce(_ || _)

  val derived_from_old_entry =
    Mux(is_new_br, old_entry_modified, Mux(jalr_target_modified, old_entry_jmp_target_modified, old_entry_strong_bias))

  io.new_entry := Mux(!hit, init_entry, derived_from_old_entry)

  io.new_br_insert_pos := new_br_insert_onehot
  io.taken_mask := VecInit((io.new_entry.brOffset zip io.new_entry.brValids).map {
    case (off, v) => io.cfiIndex.bits === off && io.cfiIndex.valid && v
  })
  io.jmp_taken := io.new_entry.jmpValid && io.new_entry.tailSlot.offset === io.cfiIndex.bits
  for (i <- 0 until numBr) {
    io.mispred_mask(i) := io.new_entry.brValids(i) && io.mispredict_vec(io.new_entry.brOffset(i))
  }
  io.mispred_mask.last := io.new_entry.jmpValid && io.mispredict_vec(pd.jmpOffset)

  // for perf counters
  io.is_init_entry           := !hit
  io.is_old_entry            := hit && !is_new_br && !jalr_target_modified && !strong_bias_modified
  io.is_new_br               := hit && is_new_br
  io.is_jalr_target_modified := hit && jalr_target_modified
  io.is_strong_bias_modified := hit && strong_bias_modified
  io.is_br_full              := hit && is_new_br && may_have_to_replace
}

class FtqPcMemWrapper(numOtherReads: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ifuPtr_w           = Input(new FtqPtr)
    val ifuPtrPlus1_w      = Input(new FtqPtr)
    val ifuPtrPlus2_w      = Input(new FtqPtr)
    val pfPtr_w            = Input(new FtqPtr)
    val pfPtrPlus1_w       = Input(new FtqPtr)
    val commPtr_w          = Input(new FtqPtr)
    val commPtrPlus1_w     = Input(new FtqPtr)
    val ifuPtr_rdata       = Output(new Ftq_RF_Components)
    val ifuPtrPlus1_rdata  = Output(new Ftq_RF_Components)
    val ifuPtrPlus2_rdata  = Output(new Ftq_RF_Components)
    val pfPtr_rdata        = Output(new Ftq_RF_Components)
    val pfPtrPlus1_rdata   = Output(new Ftq_RF_Components)
    val commPtr_rdata      = Output(new Ftq_RF_Components)
    val commPtrPlus1_rdata = Output(new Ftq_RF_Components)

    val wen   = Input(Bool())
    val waddr = Input(UInt(log2Ceil(FtqSize).W))
    val wdata = Input(new Ftq_RF_Components)
  })

  val num_pc_read = numOtherReads + 5
  val mem         = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, num_pc_read, 1, "FtqPC"))
  mem.io.wen(0)   := io.wen
  mem.io.waddr(0) := io.waddr
  mem.io.wdata(0) := io.wdata

  // read one cycle ahead for ftq local reads
  val raddr_vec = VecInit(Seq(
    io.ifuPtr_w.value,
    io.ifuPtrPlus1_w.value,
    io.ifuPtrPlus2_w.value,
    io.pfPtr_w.value,
    io.pfPtrPlus1_w.value,
    io.commPtrPlus1_w.value,
    io.commPtr_w.value
  ))

  mem.io.raddr := raddr_vec

  io.ifuPtr_rdata       := mem.io.rdata.dropRight(6).last
  io.ifuPtrPlus1_rdata  := mem.io.rdata.dropRight(5).last
  io.ifuPtrPlus2_rdata  := mem.io.rdata.dropRight(4).last
  io.pfPtr_rdata        := mem.io.rdata.dropRight(3).last
  io.pfPtrPlus1_rdata   := mem.io.rdata.dropRight(2).last
  io.commPtrPlus1_rdata := mem.io.rdata.dropRight(1).last
  io.commPtr_rdata      := mem.io.rdata.last
}

class Ftq(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper
    with BPUUtils with HasBPUConst with HasPerfEvents
    with HasICacheParameters {
  val io = IO(new Bundle {
    val fromBpu     = Flipped(new BpuToFtqIO)
    val fromIfu     = Flipped(new IfuToFtqIO)
    val fromBackend = Flipped(new CtrlToFtqIO)

    val toBpu       = new FtqToBpuIO
    val toIfu       = new FtqToIfuIO
    val toICache    = new FtqToICacheIO
    val toBackend   = new FtqToCtrlIO
    val toPrefetch  = new FtqToPrefetchIO
    val icacheFlush = Output(Bool())

    val bpuInfo = new Bundle {
      val bpRight = Output(UInt(XLEN.W))
      val bpWrong = Output(UInt(XLEN.W))
    }

    val mmioCommitRead = Flipped(new mmioCommitRead)

    // for perf
    val ControlBTBMissBubble = Output(Bool())
    val TAGEMissBubble       = Output(Bool())
    val SCMissBubble         = Output(Bool())
    val ITTAGEMissBubble     = Output(Bool())
    val RASMissBubble        = Output(Bool())
  })
  io.bpuInfo := DontCare

  val topdown_stage = RegInit(0.U.asTypeOf(new FrontendTopDownBundle))
  // only driven by clock, not valid-ready
  topdown_stage                  := io.fromBpu.resp.bits.topdown_info
  io.toIfu.req.bits.topdown_info := topdown_stage

  val ifuRedirected = RegInit(VecInit(Seq.fill(FtqSize)(false.B)))

  // io.fromBackend.ftqIdxAhead: bju(BjuCnt) + ldReplay + exception
  val ftqIdxAhead = VecInit(Seq.tabulate(FtqRedirectAheadNum)(i => io.fromBackend.ftqIdxAhead(i))) // only bju
  val ftqIdxSelOH = io.fromBackend.ftqIdxSelOH.bits(FtqRedirectAheadNum - 1, 0)

  val aheadValid         = ftqIdxAhead.map(_.valid).reduce(_ | _) && !io.fromBackend.redirect.valid
  val realAhdValid       = io.fromBackend.redirect.valid && (ftqIdxSelOH > 0.U) && RegNext(aheadValid)
  val backendRedirect    = Wire(Valid(new BranchPredictionRedirect))
  val backendRedirectReg = Wire(Valid(new BranchPredictionRedirect))
  backendRedirectReg.valid := RegNext(Mux(realAhdValid, false.B, backendRedirect.valid))
  backendRedirectReg.bits  := RegEnable(backendRedirect.bits, backendRedirect.valid)
  val fromBackendRedirect = Wire(Valid(new BranchPredictionRedirect))
  fromBackendRedirect := Mux(realAhdValid, backendRedirect, backendRedirectReg)

  val stage2Flush  = backendRedirect.valid
  val backendFlush = stage2Flush || RegNext(stage2Flush)
  val ifuFlush     = Wire(Bool())

  val flush = stage2Flush || RegNext(stage2Flush)

  val allowBpuIn, allowToIfu = WireInit(false.B)
  val flushToIfu             = !allowToIfu
  allowBpuIn := !ifuFlush && !backendRedirect.valid && !backendRedirectReg.valid
  allowToIfu := !ifuFlush && !backendRedirect.valid && !backendRedirectReg.valid

  def copyNum = 5
  val bpuPtr, ifuPtr, pfPtr, ifuWbPtr, commitPtr: FtqPtr      = RegInit(FtqPtr(false.B, 0.U))
  val ifuPtrPlus1:                                FtqPtr      = RegInit(FtqPtr(false.B, 1.U))
  val ifuPtrPlus2:                                FtqPtr      = RegInit(FtqPtr(false.B, 2.U))
  val pfPtrPlus1:                                 FtqPtr      = RegInit(FtqPtr(false.B, 1.U))
  val commitPtrPlus1:                             FtqPtr      = RegInit(FtqPtr(false.B, 1.U))
  val copied_ifu_ptr:                             Seq[FtqPtr] = Seq.fill(copyNum)(RegInit(FtqPtr(false.B, 0.U)))
  val copied_bpu_ptr:                             Seq[FtqPtr] = Seq.fill(copyNum)(RegInit(FtqPtr(false.B, 0.U)))
  require(FtqSize >= 4)
  val ifuPtr_write:         FtqPtr = WireInit(ifuPtr)
  val ifuPtrPlus1_write:    FtqPtr = WireInit(ifuPtrPlus1)
  val ifuPtrPlus2_write:    FtqPtr = WireInit(ifuPtrPlus2)
  val pfPtr_write:          FtqPtr = WireInit(pfPtr)
  val pfPtrPlus1_write:     FtqPtr = WireInit(pfPtrPlus1)
  val ifuWbPtr_write:       FtqPtr = WireInit(ifuWbPtr)
  val commitPtr_write:      FtqPtr = WireInit(commitPtr)
  val commitPtrPlus1_write: FtqPtr = WireInit(commitPtrPlus1)
  ifuPtr         := ifuPtr_write
  ifuPtrPlus1    := ifuPtrPlus1_write
  ifuPtrPlus2    := ifuPtrPlus2_write
  pfPtr          := pfPtr_write
  pfPtrPlus1     := pfPtrPlus1_write
  ifuWbPtr       := ifuWbPtr_write
  commitPtr      := commitPtr_write
  commitPtrPlus1 := commitPtrPlus1_write
  copied_ifu_ptr.map { ptr =>
    ptr := ifuPtr_write
    dontTouch(ptr)
  }
  val validEntries  = distanceBetween(bpuPtr, commitPtr)
  val backendCommit = Wire(Bool())

  // Instruction page fault and instruction access fault are sent from backend with redirect requests.
  // When IPF and IAF are sent, backendPcFaultIfuPtr points to the FTQ entry whose first instruction
  // raises IPF or IAF, which is ifuWbPtr_write or IfuPtr_write.
  // Only when IFU has written back that FTQ entry can backendIpf and backendIaf be false because this
  // makes sure that IAF and IPF are correctly raised instead of being flushed by redirect requests.
  val backendException  = RegInit(ExceptionType.none)
  val backendPcFaultPtr = RegInit(FtqPtr(false.B, 0.U))
  when(fromBackendRedirect.valid) {
    backendException := ExceptionType.fromOH(
      has_pf = fromBackendRedirect.bits.cfiUpdate.backendIPF,
      has_gpf = fromBackendRedirect.bits.cfiUpdate.backendIGPF,
      has_af = fromBackendRedirect.bits.cfiUpdate.backendIAF
    )
    when(
      fromBackendRedirect.bits.cfiUpdate.backendIPF || fromBackendRedirect.bits.cfiUpdate.backendIGPF ||
        fromBackendRedirect.bits.cfiUpdate.backendIAF
    ) {
      backendPcFaultPtr := ifuWbPtr_write
    }
  }.elsewhen(ifuWbPtr =/= backendPcFaultPtr) {
    backendException := ExceptionType.none
  }

  // **********************************************************************
  // **************************** enq from bpu ****************************
  // **********************************************************************
  val new_entry_ready = validEntries < FtqSize.U || backendCommit
  io.fromBpu.resp.ready := new_entry_ready

  val bpu_s2_resp     = io.fromBpu.resp.bits.s2
  val bpu_s3_resp     = io.fromBpu.resp.bits.s3
  val bpu_s2_redirect = bpu_s2_resp.valid(3) && bpu_s2_resp.hasRedirect(3)
  val bpu_s3_redirect = bpu_s3_resp.valid(3) && bpu_s3_resp.hasRedirect(3)

  io.toBpu.enq_ptr := bpuPtr
  val enq_fire    = io.fromBpu.resp.fire && allowBpuIn // from bpu s1
  val bpu_in_fire = (io.fromBpu.resp.fire || bpu_s2_redirect || bpu_s3_redirect) && allowBpuIn

  val bpu_in_resp     = io.fromBpu.resp.bits.selectedResp
  val bpu_in_stage    = io.fromBpu.resp.bits.selectedRespIdxForFtq
  val bpu_in_resp_ptr = Mux(bpu_in_stage === BP_S1, bpuPtr, bpu_in_resp.ftq_idx)
  val bpu_in_resp_idx = bpu_in_resp_ptr.value

  // read ports:      pfReq1 + pfReq2 ++  ifuReq1 + ifuReq2 + ifuReq3 + commitUpdate2 + commitUpdate
  val ftq_pc_mem = Module(new FtqPcMemWrapper(2))
  // resp from uBTB
  ftq_pc_mem.io.wen   := bpu_in_fire
  ftq_pc_mem.io.waddr := bpu_in_resp_idx
  ftq_pc_mem.io.wdata.fromBranchPrediction(bpu_in_resp)

  //                                                            ifuRedirect + backendRedirect + commit
  val ftq_redirect_mem = Module(new SyncDataModuleTemplate(
    new Ftq_Redirect_SRAMEntry,
    FtqSize,
    IfuRedirectNum + FtqRedirectAheadNum + 1,
    1,
    hasRen = true
  ))
  // these info is intended to enq at the last stage of bpu
  ftq_redirect_mem.io.wen(0)   := io.fromBpu.resp.bits.lastStage.valid(3)
  ftq_redirect_mem.io.waddr(0) := io.fromBpu.resp.bits.lastStage.ftq_idx.value
  ftq_redirect_mem.io.wdata(0) := io.fromBpu.resp.bits.last_stage_spec_info
  println(f"ftq redirect MEM: entry ${ftq_redirect_mem.io.wdata(0).getWidth} * ${FtqSize} * 3")

  val ftq_meta_1r_sram = Module(new FtqNRSRAM(new Ftq_1R_SRAMEntry, 1))
  // these info is intended to enq at the last stage of bpu
  ftq_meta_1r_sram.io.wen             := io.fromBpu.resp.bits.lastStage.valid(3)
  ftq_meta_1r_sram.io.waddr           := io.fromBpu.resp.bits.lastStage.ftq_idx.value
  ftq_meta_1r_sram.io.wdata.meta      := io.fromBpu.resp.bits.last_stage_meta
  ftq_meta_1r_sram.io.wdata.ftb_entry := io.fromBpu.resp.bits.last_stage_ftb_entry
  //                                                            ifuRedirect + backendRedirect (commit moved to ftq_meta_1r_sram)
  val ftb_entry_mem = Module(new SyncDataModuleTemplate(
    new FTBEntry_FtqMem,
    FtqSize,
    IfuRedirectNum + FtqRedirectAheadNum,
    1,
    hasRen = true
  ))
  ftb_entry_mem.io.wen(0)   := io.fromBpu.resp.bits.lastStage.valid(3)
  ftb_entry_mem.io.waddr(0) := io.fromBpu.resp.bits.lastStage.ftq_idx.value
  ftb_entry_mem.io.wdata(0) := io.fromBpu.resp.bits.last_stage_ftb_entry

  // multi-write
  val update_target = Reg(Vec(FtqSize, UInt(VAddrBits.W))) // could be taken target or fallThrough //TODO: remove this
  val newest_entry_target          = Reg(UInt(VAddrBits.W))
  val newest_entry_target_modified = RegInit(false.B)
  val newest_entry_ptr             = Reg(new FtqPtr)
  val newest_entry_ptr_modified    = RegInit(false.B)
  val cfiIndex_vec                 = Reg(Vec(FtqSize, ValidUndirectioned(UInt(log2Ceil(PredictWidth).W))))
  val mispredict_vec               = Reg(Vec(FtqSize, Vec(PredictWidth, Bool())))
  val pred_stage                   = Reg(Vec(FtqSize, UInt(2.W)))
  val pred_s1_cycle                = if (!env.FPGAPlatform) Some(Reg(Vec(FtqSize, UInt(64.W)))) else None

  val f_to_send :: f_sent :: Nil = Enum(2)
  val entry_fetch_status         = RegInit(VecInit(Seq.fill(FtqSize)(f_sent)))

  val h_not_hit :: h_false_hit :: h_hit :: Nil = Enum(3)
  val entry_hit_status                         = RegInit(VecInit(Seq.fill(FtqSize)(h_not_hit)))

  // modify registers one cycle later to cut critical path
  val last_cycle_bpu_in       = RegNext(bpu_in_fire)
  val last_cycle_bpu_in_ptr   = RegEnable(bpu_in_resp_ptr, bpu_in_fire)
  val last_cycle_bpu_in_idx   = last_cycle_bpu_in_ptr.value
  val last_cycle_bpu_target   = RegEnable(bpu_in_resp.getTarget(3), bpu_in_fire)
  val last_cycle_cfiIndex     = RegEnable(bpu_in_resp.cfiIndex(3), bpu_in_fire)
  val last_cycle_bpu_in_stage = RegEnable(bpu_in_stage, bpu_in_fire)

  val copied_last_cycle_bpu_in = VecInit(Seq.fill(copyNum)(RegNext(bpu_in_fire)))

  newest_entry_target_modified := false.B
  newest_entry_ptr_modified    := false.B
  when(last_cycle_bpu_in) {
    entry_fetch_status(last_cycle_bpu_in_idx) := f_to_send
    cfiIndex_vec(last_cycle_bpu_in_idx)       := last_cycle_cfiIndex
    pred_stage(last_cycle_bpu_in_idx)         := last_cycle_bpu_in_stage

    update_target(last_cycle_bpu_in_idx) := last_cycle_bpu_target // TODO: remove this
    newest_entry_target_modified         := true.B
    newest_entry_target                  := last_cycle_bpu_target
    newest_entry_ptr_modified            := true.B
    newest_entry_ptr                     := last_cycle_bpu_in_ptr
  }

  // reduce fanout by delay write for a cycle
  when(RegNext(last_cycle_bpu_in)) {
    mispredict_vec(RegEnable(last_cycle_bpu_in_idx, last_cycle_bpu_in)) :=
      WireInit(VecInit(Seq.fill(PredictWidth)(false.B)))
  }

  // record s1 pred cycles
  pred_s1_cycle.map { vec =>
    when(bpu_in_fire && (bpu_in_stage === BP_S1)) {
      vec(bpu_in_resp_ptr.value) := bpu_in_resp.full_pred(0).predCycle.getOrElse(0.U)
    }
  }

  bpuPtr := bpuPtr + enq_fire
  copied_bpu_ptr.map(_ := bpuPtr + enq_fire)
  when(io.toIfu.req.fire && allowToIfu) {
    ifuPtr_write      := ifuPtrPlus1
    ifuPtrPlus1_write := ifuPtrPlus2
    ifuPtrPlus2_write := ifuPtrPlus2 + 1.U
  }
  when(io.toPrefetch.req.fire && allowToIfu) {
    pfPtr_write      := pfPtrPlus1
    pfPtrPlus1_write := pfPtrPlus1 + 1.U
  }

  // only use ftb result to assign hit status
  when(bpu_s2_resp.valid(3)) {
    entry_hit_status(bpu_s2_resp.ftq_idx.value) := Mux(bpu_s2_resp.full_pred(3).hit, h_hit, h_not_hit)
  }

  io.toIfu.flushFromBpu.s2.valid      := bpu_s2_redirect
  io.toIfu.flushFromBpu.s2.bits       := bpu_s2_resp.ftq_idx
  io.toPrefetch.flushFromBpu.s2.valid := bpu_s2_redirect
  io.toPrefetch.flushFromBpu.s2.bits  := bpu_s2_resp.ftq_idx
  when(bpu_s2_redirect) {
    bpuPtr := bpu_s2_resp.ftq_idx + 1.U
    copied_bpu_ptr.map(_ := bpu_s2_resp.ftq_idx + 1.U)
    // only when ifuPtr runs ahead of bpu s2 resp should we recover it
    when(!isBefore(ifuPtr, bpu_s2_resp.ftq_idx)) {
      ifuPtr_write      := bpu_s2_resp.ftq_idx
      ifuPtrPlus1_write := bpu_s2_resp.ftq_idx + 1.U
      ifuPtrPlus2_write := bpu_s2_resp.ftq_idx + 2.U
    }
    when(!isBefore(pfPtr, bpu_s2_resp.ftq_idx)) {
      pfPtr_write      := bpu_s2_resp.ftq_idx
      pfPtrPlus1_write := bpu_s2_resp.ftq_idx + 1.U
    }
  }

  io.toIfu.flushFromBpu.s3.valid      := bpu_s3_redirect
  io.toIfu.flushFromBpu.s3.bits       := bpu_s3_resp.ftq_idx
  io.toPrefetch.flushFromBpu.s3.valid := bpu_s3_redirect
  io.toPrefetch.flushFromBpu.s3.bits  := bpu_s3_resp.ftq_idx
  when(bpu_s3_redirect) {
    bpuPtr := bpu_s3_resp.ftq_idx + 1.U
    copied_bpu_ptr.map(_ := bpu_s3_resp.ftq_idx + 1.U)
    // only when ifuPtr runs ahead of bpu s2 resp should we recover it
    when(!isBefore(ifuPtr, bpu_s3_resp.ftq_idx)) {
      ifuPtr_write      := bpu_s3_resp.ftq_idx
      ifuPtrPlus1_write := bpu_s3_resp.ftq_idx + 1.U
      ifuPtrPlus2_write := bpu_s3_resp.ftq_idx + 2.U
    }
    when(!isBefore(pfPtr, bpu_s3_resp.ftq_idx)) {
      pfPtr_write      := bpu_s3_resp.ftq_idx
      pfPtrPlus1_write := bpu_s3_resp.ftq_idx + 1.U
    }
  }

  XSError(isBefore(bpuPtr, ifuPtr) && !isFull(bpuPtr, ifuPtr), "\nifuPtr is before bpuPtr!\n")
  XSError(isBefore(bpuPtr, pfPtr) && !isFull(bpuPtr, pfPtr), "\npfPtr is before bpuPtr!\n")
  XSError(isBefore(ifuWbPtr, commitPtr) && !isFull(ifuWbPtr, commitPtr), "\ncommPtr is before ifuWbPtr!\n")

  (0 until copyNum).map(i => XSError(copied_bpu_ptr(i) =/= bpuPtr, "\ncopiedBpuPtr is different from bpuPtr!\n"))

  // ****************************************************************
  // **************************** to ifu ****************************
  // ****************************************************************
  // 0  for ifu, and 1-4 for ICache
  val bpu_in_bypass_buf         = RegEnable(ftq_pc_mem.io.wdata, bpu_in_fire)
  val copied_bpu_in_bypass_buf  = VecInit(Seq.fill(copyNum)(RegEnable(ftq_pc_mem.io.wdata, bpu_in_fire)))
  val bpu_in_bypass_buf_for_ifu = bpu_in_bypass_buf
  val bpu_in_bypass_ptr         = RegEnable(bpu_in_resp_ptr, bpu_in_fire)
  val last_cycle_to_ifu_fire    = RegNext(io.toIfu.req.fire)
  val last_cycle_to_pf_fire     = RegNext(io.toPrefetch.req.fire)

  val copied_bpu_in_bypass_ptr      = VecInit(Seq.fill(copyNum)(RegEnable(bpu_in_resp_ptr, bpu_in_fire)))
  val copied_last_cycle_to_ifu_fire = VecInit(Seq.fill(copyNum)(RegNext(io.toIfu.req.fire)))

  // read pc and target
  ftq_pc_mem.io.ifuPtr_w       := ifuPtr_write
  ftq_pc_mem.io.ifuPtrPlus1_w  := ifuPtrPlus1_write
  ftq_pc_mem.io.ifuPtrPlus2_w  := ifuPtrPlus2_write
  ftq_pc_mem.io.pfPtr_w        := pfPtr_write
  ftq_pc_mem.io.pfPtrPlus1_w   := pfPtrPlus1_write
  ftq_pc_mem.io.commPtr_w      := commitPtr_write
  ftq_pc_mem.io.commPtrPlus1_w := commitPtrPlus1_write

  io.toIfu.req.bits.ftqIdx := ifuPtr

  val toICachePcBundle               = Wire(Vec(copyNum, new Ftq_RF_Components))
  val toICacheEntryToSend            = Wire(Vec(copyNum, Bool()))
  val nextCycleToPrefetchPcBundle    = Wire(new Ftq_RF_Components)
  val nextCycleToPrefetchEntryToSend = Wire(Bool())
  val toPrefetchPcBundle             = RegNext(nextCycleToPrefetchPcBundle)
  val toPrefetchEntryToSend          = RegNext(nextCycleToPrefetchEntryToSend)
  val toIfuPcBundle                  = Wire(new Ftq_RF_Components)
  val entry_is_to_send               = WireInit(entry_fetch_status(ifuPtr.value) === f_to_send)
  val entry_ftq_offset               = WireInit(cfiIndex_vec(ifuPtr.value))
  val entry_next_addr                = Wire(UInt(VAddrBits.W))

  val pc_mem_ifu_ptr_rdata   = VecInit(Seq.fill(copyNum)(RegNext(ftq_pc_mem.io.ifuPtr_rdata)))
  val pc_mem_ifu_plus1_rdata = VecInit(Seq.fill(copyNum)(RegNext(ftq_pc_mem.io.ifuPtrPlus1_rdata)))
  val diff_entry_next_addr   = WireInit(update_target(ifuPtr.value)) // TODO: remove this

  val copied_ifu_plus1_to_send = VecInit(Seq.fill(copyNum)(RegNext(
    entry_fetch_status(ifuPtrPlus1.value) === f_to_send
  ) || RegNext(last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtrPlus1)))
  val copied_ifu_ptr_to_send = VecInit(Seq.fill(copyNum)(RegNext(
    entry_fetch_status(ifuPtr.value) === f_to_send
  ) || RegNext(last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtr)))

  for (i <- 0 until copyNum) {
    when(copied_last_cycle_bpu_in(i) && copied_bpu_in_bypass_ptr(i) === copied_ifu_ptr(i)) {
      toICachePcBundle(i)    := copied_bpu_in_bypass_buf(i)
      toICacheEntryToSend(i) := true.B
    }.elsewhen(copied_last_cycle_to_ifu_fire(i)) {
      toICachePcBundle(i)    := pc_mem_ifu_plus1_rdata(i)
      toICacheEntryToSend(i) := copied_ifu_plus1_to_send(i)
    }.otherwise {
      toICachePcBundle(i)    := pc_mem_ifu_ptr_rdata(i)
      toICacheEntryToSend(i) := copied_ifu_ptr_to_send(i)
    }
  }

  // Calculate requests sent to prefetcher one cycle in advance to cut critical path
  when(bpu_in_fire && bpu_in_resp_ptr === pfPtr_write) {
    nextCycleToPrefetchPcBundle    := ftq_pc_mem.io.wdata
    nextCycleToPrefetchEntryToSend := true.B
  }.elsewhen(io.toPrefetch.req.fire) {
    nextCycleToPrefetchPcBundle := ftq_pc_mem.io.pfPtrPlus1_rdata
    nextCycleToPrefetchEntryToSend := entry_fetch_status(pfPtrPlus1.value) === f_to_send ||
      last_cycle_bpu_in && bpu_in_bypass_ptr === pfPtrPlus1
  }.otherwise {
    nextCycleToPrefetchPcBundle := ftq_pc_mem.io.pfPtr_rdata
    nextCycleToPrefetchEntryToSend := entry_fetch_status(pfPtr.value) === f_to_send ||
      last_cycle_bpu_in && bpu_in_bypass_ptr === pfPtr // reduce potential bubbles
  }

  // TODO: reconsider target address bypass logic
  when(last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtr) {
    toIfuPcBundle        := bpu_in_bypass_buf_for_ifu
    entry_is_to_send     := true.B
    entry_next_addr      := last_cycle_bpu_target
    entry_ftq_offset     := last_cycle_cfiIndex
    diff_entry_next_addr := last_cycle_bpu_target // TODO: remove this
  }.elsewhen(last_cycle_to_ifu_fire) {
    toIfuPcBundle := RegNext(ftq_pc_mem.io.ifuPtrPlus1_rdata)
    entry_is_to_send := RegNext(entry_fetch_status(ifuPtrPlus1.value) === f_to_send) ||
      RegNext(last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtrPlus1) // reduce potential bubbles
    entry_next_addr := Mux(
      last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtrPlus1,
      bpu_in_bypass_buf_for_ifu.startAddr,
      Mux(ifuPtr === newest_entry_ptr, newest_entry_target, RegNext(ftq_pc_mem.io.ifuPtrPlus2_rdata.startAddr))
    ) // ifuPtr+2
  }.otherwise {
    toIfuPcBundle := RegNext(ftq_pc_mem.io.ifuPtr_rdata)
    entry_is_to_send := RegNext(entry_fetch_status(ifuPtr.value) === f_to_send) ||
      RegNext(last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtr) // reduce potential bubbles
    entry_next_addr := Mux(
      last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtrPlus1,
      bpu_in_bypass_buf_for_ifu.startAddr,
      Mux(ifuPtr === newest_entry_ptr, newest_entry_target, RegNext(ftq_pc_mem.io.ifuPtrPlus1_rdata.startAddr))
    ) // ifuPtr+1
  }

  io.toIfu.req.valid              := entry_is_to_send && ifuPtr =/= bpuPtr
  io.toIfu.req.bits.nextStartAddr := entry_next_addr
  io.toIfu.req.bits.ftqOffset     := entry_ftq_offset
  io.toIfu.req.bits.fromFtqPcBundle(toIfuPcBundle)

  io.toICache.req.valid := entry_is_to_send && ifuPtr =/= bpuPtr
  io.toICache.req.bits.readValid.zipWithIndex.map { case (copy, i) =>
    copy := toICacheEntryToSend(i) && copied_ifu_ptr(i) =/= copied_bpu_ptr(i)
  }
  io.toICache.req.bits.pcMemRead.zipWithIndex.foreach { case (copy, i) =>
    copy.fromFtqPcBundle(toICachePcBundle(i))
    copy.ftqIdx := ifuPtr
  }
  io.toICache.req.bits.backendException := ExceptionType.hasException(backendException) && backendPcFaultPtr === ifuPtr

  io.toPrefetch.req.valid := toPrefetchEntryToSend && pfPtr =/= bpuPtr
  io.toPrefetch.req.bits.fromFtqPcBundle(toPrefetchPcBundle)
  io.toPrefetch.req.bits.ftqIdx  := pfPtr
  io.toPrefetch.backendException := Mux(backendPcFaultPtr === pfPtr, backendException, ExceptionType.none)
  // io.toICache.req.bits.bypassSelect := last_cycle_bpu_in && bpu_in_bypass_ptr === ifuPtr
  // io.toICache.req.bits.bpuBypassWrite.zipWithIndex.map{case(bypassWrtie, i) =>
  //   bypassWrtie.startAddr := bpu_in_bypass_buf.tail(i).startAddr
  //   bypassWrtie.nextlineStart := bpu_in_bypass_buf.tail(i).nextLineAddr
  // }

  // TODO: remove this
  XSError(
    io.toIfu.req.valid && diff_entry_next_addr =/= entry_next_addr,
    p"\nifu_req_target wrong! ifuPtr: ${ifuPtr}, entry_next_addr: ${Hexadecimal(entry_next_addr)} diff_entry_next_addr: ${Hexadecimal(diff_entry_next_addr)}\n"
  )

  // when fall through is smaller in value than start address, there must be a false hit
  when(toIfuPcBundle.fallThruError && entry_hit_status(ifuPtr.value) === h_hit) {
    when(io.toIfu.req.fire &&
      !(bpu_s2_redirect && bpu_s2_resp.ftq_idx === ifuPtr) &&
      !(bpu_s3_redirect && bpu_s3_resp.ftq_idx === ifuPtr)) {
      entry_hit_status(ifuPtr.value) := h_false_hit
      // XSError(true.B, "FTB false hit by fallThroughError, startAddr: %x, fallTHru: %x\n", io.toIfu.req.bits.startAddr, io.toIfu.req.bits.nextStartAddr)
    }
  }
  XSDebug(
    toIfuPcBundle.fallThruError && entry_hit_status(ifuPtr.value) === h_hit,
    "fallThruError! start:%x, fallThru:%x\n",
    io.toIfu.req.bits.startAddr,
    io.toIfu.req.bits.nextStartAddr
  )

  XSPerfAccumulate(
    f"fall_through_error_to_ifu",
    toIfuPcBundle.fallThruError && entry_hit_status(ifuPtr.value) === h_hit &&
      io.toIfu.req.fire && !(bpu_s2_redirect && bpu_s2_resp.ftq_idx === ifuPtr) && !(bpu_s3_redirect && bpu_s3_resp.ftq_idx === ifuPtr)
  )

  val ifu_req_should_be_flushed =
    io.toIfu.flushFromBpu.shouldFlushByStage2(io.toIfu.req.bits.ftqIdx) ||
      io.toIfu.flushFromBpu.shouldFlushByStage3(io.toIfu.req.bits.ftqIdx)

  when(io.toIfu.req.fire && !ifu_req_should_be_flushed) {
    entry_fetch_status(ifuPtr.value) := f_sent
  }

  // *********************************************************************
  // **************************** wb from ifu ****************************
  // *********************************************************************
  val pdWb         = io.fromIfu.pdWb
  val pds          = pdWb.bits.pd
  val ifu_wb_valid = pdWb.valid
  val ifu_wb_idx   = pdWb.bits.ftqIdx.value
  // read ports:                                                         commit update
  val ftq_pd_mem =
    Module(new SyncDataModuleTemplate(new Ftq_pd_Entry, FtqSize, FtqRedirectAheadNum + 1, 1, hasRen = true))
  ftq_pd_mem.io.wen(0)   := ifu_wb_valid
  ftq_pd_mem.io.waddr(0) := pdWb.bits.ftqIdx.value
  ftq_pd_mem.io.wdata(0).fromPdWb(pdWb.bits)

  val hit_pd_valid       = entry_hit_status(ifu_wb_idx) === h_hit && ifu_wb_valid
  val hit_pd_mispred     = hit_pd_valid && pdWb.bits.misOffset.valid
  val hit_pd_mispred_reg = RegNext(hit_pd_mispred, init = false.B)
  val pd_reg             = RegEnable(pds, pdWb.valid)
  val start_pc_reg       = RegEnable(pdWb.bits.pc(0), pdWb.valid)
  val wb_idx_reg         = RegEnable(ifu_wb_idx, pdWb.valid)

  when(ifu_wb_valid) {
    ifuWbPtr_write := ifuWbPtr + 1.U
  }

  XSError(ifu_wb_valid && isAfter(pdWb.bits.ftqIdx, ifuPtr), "IFU returned a predecode before its req, check IFU")

  ftb_entry_mem.io.ren.get.head := ifu_wb_valid
  ftb_entry_mem.io.raddr.head   := ifu_wb_idx
  val has_false_hit = WireInit(false.B)
  when(RegNext(hit_pd_valid)) {
    // check for false hit
    val pred_ftb_entry = ftb_entry_mem.io.rdata.head
    val brSlots        = pred_ftb_entry.brSlots
    val tailSlot       = pred_ftb_entry.tailSlot
    // we check cfis that bpu predicted

    // bpu predicted branches but denied by predecode
    val br_false_hit =
      brSlots.map {
        s => s.valid && !(pd_reg(s.offset).valid && pd_reg(s.offset).isBr)
      }.reduce(_ || _) ||
        (tailSlot.valid && pred_ftb_entry.tailSlot.sharing &&
          !(pd_reg(tailSlot.offset).valid && pd_reg(tailSlot.offset).isBr))

    val jmpOffset = tailSlot.offset
    val jmp_pd    = pd_reg(jmpOffset)
    val jal_false_hit = pred_ftb_entry.jmpValid &&
      ((pred_ftb_entry.isJal && !(jmp_pd.valid && jmp_pd.isJal)) ||
        (pred_ftb_entry.isJalr && !(jmp_pd.valid && jmp_pd.isJalr)) ||
        (pred_ftb_entry.isCall && !(jmp_pd.valid && jmp_pd.isCall)) ||
        (pred_ftb_entry.isRet && !(jmp_pd.valid && jmp_pd.isRet)))

    has_false_hit := br_false_hit || jal_false_hit || hit_pd_mispred_reg
    // assert(!has_false_hit)
  }
  XSDebug(
    RegNext(hit_pd_valid) && has_false_hit,
    "FTB false hit by br or jal or hit_pd, startAddr: %x\n",
    pdWb.bits.pc(0)
  )

  when(has_false_hit) {
    entry_hit_status(wb_idx_reg) := h_false_hit
  }

  // *******************************************************************************
  // **************************** redirect from backend ****************************
  // *******************************************************************************

  // redirect read cfiInfo, couples to redirectGen s2
  // ftqIdxAhead(0-3) => ftq_redirect_mem(1-4), reuse ftq_redirect_mem(1)
  val ftq_redirect_rdata = Wire(Vec(FtqRedirectAheadNum, new Ftq_Redirect_SRAMEntry))
  val ftb_redirect_rdata = Wire(Vec(FtqRedirectAheadNum, new FTBEntry_FtqMem))

  val ftq_pd_rdata = Wire(Vec(FtqRedirectAheadNum, new Ftq_pd_Entry))
  for (i <- 1 until FtqRedirectAheadNum) {
    ftq_redirect_mem.io.ren.get(i + IfuRedirectNum) := ftqIdxAhead(i).valid
    ftq_redirect_mem.io.raddr(i + IfuRedirectNum)   := ftqIdxAhead(i).bits.value
    ftb_entry_mem.io.ren.get(i + IfuRedirectNum)    := ftqIdxAhead(i).valid
    ftb_entry_mem.io.raddr(i + IfuRedirectNum)      := ftqIdxAhead(i).bits.value

    ftq_pd_mem.io.ren.get(i) := ftqIdxAhead(i).valid
    ftq_pd_mem.io.raddr(i)   := ftqIdxAhead(i).bits.value
  }
  ftq_redirect_mem.io.ren.get(IfuRedirectNum) := Mux(aheadValid, ftqIdxAhead(0).valid, backendRedirect.valid)
  ftq_redirect_mem.io.raddr(IfuRedirectNum) := Mux(
    aheadValid,
    ftqIdxAhead(0).bits.value,
    backendRedirect.bits.ftqIdx.value
  )
  ftb_entry_mem.io.ren.get(IfuRedirectNum) := Mux(aheadValid, ftqIdxAhead(0).valid, backendRedirect.valid)
  ftb_entry_mem.io.raddr(IfuRedirectNum) := Mux(
    aheadValid,
    ftqIdxAhead(0).bits.value,
    backendRedirect.bits.ftqIdx.value
  )

  ftq_pd_mem.io.ren.get(0) := Mux(aheadValid, ftqIdxAhead(0).valid, backendRedirect.valid)
  ftq_pd_mem.io.raddr(0)   := Mux(aheadValid, ftqIdxAhead(0).bits.value, backendRedirect.bits.ftqIdx.value)

  for (i <- 0 until FtqRedirectAheadNum) {
    ftq_redirect_rdata(i) := ftq_redirect_mem.io.rdata(i + IfuRedirectNum)
    ftb_redirect_rdata(i) := ftb_entry_mem.io.rdata(i + IfuRedirectNum)

    ftq_pd_rdata(i) := ftq_pd_mem.io.rdata(i)
  }
  val stage3CfiInfo =
    Mux(realAhdValid, Mux1H(ftqIdxSelOH, ftq_redirect_rdata), ftq_redirect_mem.io.rdata(IfuRedirectNum))
  val stage3PdInfo       = Mux(realAhdValid, Mux1H(ftqIdxSelOH, ftq_pd_rdata), ftq_pd_mem.io.rdata(0))
  val backendRedirectCfi = fromBackendRedirect.bits.cfiUpdate
  backendRedirectCfi.fromFtqRedirectSram(stage3CfiInfo)
  backendRedirectCfi.pd := stage3PdInfo.toPd(fromBackendRedirect.bits.ftqOffset)

  val r_ftb_entry = Mux(realAhdValid, Mux1H(ftqIdxSelOH, ftb_redirect_rdata), ftb_entry_mem.io.rdata(IfuRedirectNum))
  val r_ftqOffset = fromBackendRedirect.bits.ftqOffset

  backendRedirectCfi.br_hit := r_ftb_entry.brIsSaved(r_ftqOffset)
  backendRedirectCfi.jr_hit := r_ftb_entry.isJalr && r_ftb_entry.tailSlot.offset === r_ftqOffset
  // FIXME: not portable
  val sc_disagree = stage3CfiInfo.sc_disagree.getOrElse(VecInit(Seq.fill(numBr)(false.B)))
  backendRedirectCfi.sc_hit := backendRedirectCfi.br_hit && Mux(
    r_ftb_entry.brSlots(0).offset === r_ftqOffset,
    sc_disagree(0),
    sc_disagree(1)
  )

  when(entry_hit_status(fromBackendRedirect.bits.ftqIdx.value) === h_hit) {
    backendRedirectCfi.shift := PopCount(r_ftb_entry.getBrMaskByOffset(r_ftqOffset)) +&
      (backendRedirectCfi.pd.isBr && !r_ftb_entry.brIsSaved(r_ftqOffset) &&
        !r_ftb_entry.newBrCanNotInsert(r_ftqOffset))

    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr && (r_ftb_entry.brIsSaved(r_ftqOffset) ||
      !r_ftb_entry.newBrCanNotInsert(r_ftqOffset))
  }.otherwise {
    backendRedirectCfi.shift       := (backendRedirectCfi.pd.isBr && backendRedirectCfi.taken).asUInt
    backendRedirectCfi.addIntoHist := backendRedirectCfi.pd.isBr.asUInt
  }

  // ***************************************************************************
  // **************************** redirect from ifu ****************************
  // ***************************************************************************
  val fromIfuRedirect = WireInit(0.U.asTypeOf(Valid(new BranchPredictionRedirect)))
  fromIfuRedirect.valid              := pdWb.valid && pdWb.bits.misOffset.valid && !backendFlush
  fromIfuRedirect.bits.ftqIdx        := pdWb.bits.ftqIdx
  fromIfuRedirect.bits.ftqOffset     := pdWb.bits.misOffset.bits
  fromIfuRedirect.bits.level         := RedirectLevel.flushAfter
  fromIfuRedirect.bits.BTBMissBubble := true.B
  fromIfuRedirect.bits.debugIsMemVio := false.B
  fromIfuRedirect.bits.debugIsCtrl   := false.B

  val ifuRedirectCfiUpdate = fromIfuRedirect.bits.cfiUpdate
  ifuRedirectCfiUpdate.pc        := pdWb.bits.pc(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.pd        := pdWb.bits.pd(pdWb.bits.misOffset.bits)
  ifuRedirectCfiUpdate.predTaken := cfiIndex_vec(pdWb.bits.ftqIdx.value).valid
  ifuRedirectCfiUpdate.target    := pdWb.bits.target
  ifuRedirectCfiUpdate.taken     := pdWb.bits.cfiOffset.valid
  ifuRedirectCfiUpdate.isMisPred := pdWb.bits.misOffset.valid

  val ifuRedirectReg   = RegNextWithEnable(fromIfuRedirect, hasInit = true)
  val ifuRedirectToBpu = WireInit(ifuRedirectReg)
  ifuFlush := fromIfuRedirect.valid || ifuRedirectToBpu.valid

  ftq_redirect_mem.io.ren.get.head := fromIfuRedirect.valid
  ftq_redirect_mem.io.raddr.head   := fromIfuRedirect.bits.ftqIdx.value

  val toBpuCfi = ifuRedirectToBpu.bits.cfiUpdate
  toBpuCfi.fromFtqRedirectSram(ftq_redirect_mem.io.rdata.head)
  when(ifuRedirectReg.bits.cfiUpdate.pd.isRet && ifuRedirectReg.bits.cfiUpdate.pd.valid) {
    toBpuCfi.target := toBpuCfi.topAddr
  }

  when(ifuRedirectReg.valid) {
    ifuRedirected(ifuRedirectReg.bits.ftqIdx.value) := true.B
  }.elsewhen(RegNext(pdWb.valid)) {
    // if pdWb and no redirect, set to false
    ifuRedirected(last_cycle_bpu_in_ptr.value) := false.B
  }

  // **********************************************************************
  // ***************************** to backend *****************************
  // **********************************************************************
  // to backend pc mem / target
  io.toBackend.pc_mem_wen   := RegNext(last_cycle_bpu_in)
  io.toBackend.pc_mem_waddr := RegEnable(last_cycle_bpu_in_idx, last_cycle_bpu_in)
  io.toBackend.pc_mem_wdata := RegEnable(bpu_in_bypass_buf_for_ifu, last_cycle_bpu_in)

  // num cycle is fixed
  val newest_entry_en: Bool = RegNext(last_cycle_bpu_in || backendRedirect.valid || ifuRedirectToBpu.valid)
  io.toBackend.newest_entry_en     := RegNext(newest_entry_en)
  io.toBackend.newest_entry_ptr    := RegEnable(newest_entry_ptr, newest_entry_en)
  io.toBackend.newest_entry_target := RegEnable(newest_entry_target, newest_entry_en)

  // *********************************************************************
  // **************************** wb from exu ****************************
  // *********************************************************************

  backendRedirect.valid := io.fromBackend.redirect.valid
  backendRedirect.bits.connectRedirect(io.fromBackend.redirect.bits)
  backendRedirect.bits.BTBMissBubble := false.B

  def extractRedirectInfo(wb: Valid[Redirect]) = {
    val ftqPtr    = wb.bits.ftqIdx
    val ftqOffset = wb.bits.ftqOffset
    val taken     = wb.bits.cfiUpdate.taken
    val mispred   = wb.bits.cfiUpdate.isMisPred
    (wb.valid, ftqPtr, ftqOffset, taken, mispred)
  }

  // fix mispredict entry
  val lastIsMispredict = RegNext(
    backendRedirect.valid && backendRedirect.bits.level === RedirectLevel.flushAfter,
    init = false.B
  )

  def updateCfiInfo(redirect: Valid[Redirect], isBackend: Boolean = true) = {
    val (r_valid, r_ptr, r_offset, r_taken, r_mispred) = extractRedirectInfo(redirect)
    val r_idx                                          = r_ptr.value
    val cfiIndex_bits_wen                              = r_valid && r_taken && r_offset < cfiIndex_vec(r_idx).bits
    val cfiIndex_valid_wen                             = r_valid && r_offset === cfiIndex_vec(r_idx).bits
    when(cfiIndex_bits_wen || cfiIndex_valid_wen) {
      cfiIndex_vec(r_idx).valid := cfiIndex_bits_wen || cfiIndex_valid_wen && r_taken
    }.elsewhen(r_valid && !r_taken && r_offset =/= cfiIndex_vec(r_idx).bits) {
      cfiIndex_vec(r_idx).valid := false.B
    }
    when(cfiIndex_bits_wen) {
      cfiIndex_vec(r_idx).bits := r_offset
    }
    newest_entry_target_modified := true.B
    newest_entry_target          := redirect.bits.cfiUpdate.target
    newest_entry_ptr_modified    := true.B
    newest_entry_ptr             := r_ptr

    update_target(r_idx) := redirect.bits.cfiUpdate.target // TODO: remove this
    if (isBackend) {
      mispredict_vec(r_idx)(r_offset) := r_mispred && redirect.bits.level === RedirectLevel.flushAfter
    }
  }

  when(fromBackendRedirect.valid) {
    updateCfiInfo(fromBackendRedirect)
  }.elsewhen(ifuRedirectToBpu.valid) {
    updateCfiInfo(ifuRedirectToBpu, isBackend = false)
  }

  when(fromBackendRedirect.valid) {
    when(fromBackendRedirect.bits.ControlRedirectBubble) {
      when(fromBackendRedirect.bits.ControlBTBMissBubble) {
        topdown_stage.reasons(TopDownCounters.BTBMissBubble.id)                  := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.TAGEMissBubble) {
        topdown_stage.reasons(TopDownCounters.TAGEMissBubble.id)                  := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.SCMissBubble) {
        topdown_stage.reasons(TopDownCounters.SCMissBubble.id)                  := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.SCMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.ITTAGEMissBubble) {
        topdown_stage.reasons(TopDownCounters.ITTAGEMissBubble.id)                  := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
      }.elsewhen(fromBackendRedirect.bits.RASMissBubble) {
        topdown_stage.reasons(TopDownCounters.RASMissBubble.id)                  := true.B
        io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.RASMissBubble.id) := true.B
      }

    }.elsewhen(backendRedirect.bits.MemVioRedirectBubble) {
      topdown_stage.reasons(TopDownCounters.MemVioRedirectBubble.id)                  := true.B
      io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
    }.otherwise {
      topdown_stage.reasons(TopDownCounters.OtherRedirectBubble.id)                  := true.B
      io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
    }
  }.elsewhen(ifuRedirectReg.valid) {
    topdown_stage.reasons(TopDownCounters.BTBMissBubble.id)                  := true.B
    io.toIfu.req.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
  }

  io.ControlBTBMissBubble := fromBackendRedirect.bits.ControlBTBMissBubble
  io.TAGEMissBubble       := fromBackendRedirect.bits.TAGEMissBubble
  io.SCMissBubble         := fromBackendRedirect.bits.SCMissBubble
  io.ITTAGEMissBubble     := fromBackendRedirect.bits.ITTAGEMissBubble
  io.RASMissBubble        := fromBackendRedirect.bits.RASMissBubble

  // ***********************************************************************************
  // **************************** flush ptr and state queue ****************************
  // ***********************************************************************************

  val redirectVec = VecInit(backendRedirect, fromIfuRedirect)

  // when redirect, we should reset ptrs and status queues
  io.icacheFlush := redirectVec.map(r => r.valid).reduce(_ || _)
  XSPerfAccumulate("icacheFlushFromBackend", backendRedirect.valid)
  XSPerfAccumulate("icacheFlushFromIFU", fromIfuRedirect.valid)
  when(redirectVec.map(r => r.valid).reduce(_ || _)) {
    val r                          = PriorityMux(redirectVec.map(r => r.valid -> r.bits))
    val notIfu                     = redirectVec.dropRight(1).map(r => r.valid).reduce(_ || _)
    val (idx, offset, flushItSelf) = (r.ftqIdx, r.ftqOffset, RedirectLevel.flushItself(r.level))
    val next                       = idx + 1.U
    bpuPtr := next
    copied_bpu_ptr.map(_ := next)
    ifuPtr_write      := next
    ifuWbPtr_write    := next
    ifuPtrPlus1_write := idx + 2.U
    ifuPtrPlus2_write := idx + 3.U
    pfPtr_write       := next
    pfPtrPlus1_write  := idx + 2.U
  }

  // only the valid bit is actually needed
  io.toIfu.redirect.bits    := backendRedirect.bits
  io.toIfu.redirect.valid   := stage2Flush
  io.toIfu.topdown_redirect := fromBackendRedirect

  // ****************************************************************
  // **************************** to bpu ****************************
  // ****************************************************************

  io.toBpu.redirctFromIFU := ifuRedirectToBpu.valid
  io.toBpu.redirect       := Mux(fromBackendRedirect.valid, fromBackendRedirect, ifuRedirectToBpu)
  val dummy_s1_pred_cycle_vec = VecInit(List.tabulate(FtqSize)(_ => 0.U(64.W)))
  val redirect_latency =
    GTimer() - pred_s1_cycle.getOrElse(dummy_s1_pred_cycle_vec)(io.toBpu.redirect.bits.ftqIdx.value) + 1.U
  XSPerfHistogram("backend_redirect_latency", redirect_latency, fromBackendRedirect.valid, 0, 60, 1)
  XSPerfHistogram(
    "ifu_redirect_latency",
    redirect_latency,
    !fromBackendRedirect.valid && ifuRedirectToBpu.valid,
    0,
    60,
    1
  )

  XSError(
    io.toBpu.redirect.valid && isBefore(io.toBpu.redirect.bits.ftqIdx, commitPtr),
    "Ftq received a redirect after its commit, check backend or replay"
  )

  val may_have_stall_from_bpu = Wire(Bool())
  val bpu_ftb_update_stall    = RegInit(0.U(2.W)) // 2-cycle stall, so we need 3 states
  may_have_stall_from_bpu := bpu_ftb_update_stall =/= 0.U

  // TODO: frontend does not need this many rob commit channels
  private val robCommitPtr: FtqPtr = WireInit(FtqPtr(false.B, 0.U))
  when(io.fromBackend.rob_commits.map(_.valid).reduce(_ | _)) {
    robCommitPtr := ParallelPriorityMux(
      io.fromBackend.rob_commits.map(_.valid).reverse,
      io.fromBackend.rob_commits.map(_.bits.ftqIdx).reverse
    )
  }
  backendCommit := commitPtr =/= ifuWbPtr && !may_have_stall_from_bpu && isAfter(robCommitPtr, commitPtr)
  when(backendCommit) {
    commitPtr_write      := commitPtrPlus1
    commitPtrPlus1_write := commitPtrPlus1 + 1.U
  }
  // frontend commit is one cycle later than backend commit because reading srams needs one cycle
  val s2_commitPtr     = RegEnable(commitPtr, backendCommit)
  val s2_backendCommit = RegNext(backendCommit, init = false.B)

  /**
    *************************************************************************************
    * MMIO instruction fetch is allowed only if MMIO is the oldest instruction.
    *************************************************************************************
    */
  val mmioReadPtr    = io.mmioCommitRead.mmioFtqPtr
  val mmioLastCommit = isAfter(commitPtr, mmioReadPtr) || commitPtr === mmioReadPtr && backendCommit
  io.mmioCommitRead.mmioLastCommit := RegNext(mmioLastCommit)

  // commit reads
  val s2_commitPcBundle = RegNext(ftq_pc_mem.io.commPtr_rdata)
  val s2_commitTarget =
    Mux(
      RegNext(commitPtr === newest_entry_ptr),
      RegEnable(newest_entry_target, newest_entry_target_modified),
      RegNext(ftq_pc_mem.io.commPtrPlus1_rdata.startAddr)
    )
  ftq_pd_mem.io.ren.get.last := backendCommit
  ftq_pd_mem.io.raddr.last   := commitPtr.value
  val s2_commitPd = ftq_pd_mem.io.rdata.last
  ftq_redirect_mem.io.ren.get.last := backendCommit
  ftq_redirect_mem.io.raddr.last   := commitPtr.value
  val s2_commitSpecMeta = ftq_redirect_mem.io.rdata.last
  ftq_meta_1r_sram.io.ren(0)   := backendCommit
  ftq_meta_1r_sram.io.raddr(0) := commitPtr.value
  val s2_commitMeta     = ftq_meta_1r_sram.io.rdata(0).meta
  val s2_commitFtbEntry = ftq_meta_1r_sram.io.rdata(0).ftb_entry

  val s1_commitCfi = WireInit(cfiIndex_vec(commitPtr.value))
  val s2_commitCfi = RegEnable(s1_commitCfi, backendCommit)

  val s2_commitMispredict: Vec[Bool] = RegEnable(mispredict_vec(commitPtr.value), backendCommit)
  val s1_commitHit   = entry_hit_status(commitPtr.value)
  val s2_commitHit   = RegEnable(s1_commitHit, backendCommit)
  val s2_commitStage = RegEnable(pred_stage(commitPtr.value), backendCommit)
  val s2_commitValid = s2_commitHit === h_hit || s2_commitCfi.valid // hit or taken

  val to_bpu_hit = s1_commitHit === h_hit || s1_commitHit === h_false_hit
  switch(bpu_ftb_update_stall) {
    is(0.U) {
      when(s1_commitCfi.valid && !to_bpu_hit && backendCommit) {
        bpu_ftb_update_stall := 2.U // 2-cycle stall
      }
    }
    is(2.U) {
      bpu_ftb_update_stall := 1.U
    }
    is(1.U) {
      bpu_ftb_update_stall := 0.U
    }
    is(3.U) {
      // XSError below
    }
  }
  XSError(bpu_ftb_update_stall === 3.U, "bpu_ftb_update_stall should be 0, 1 or 2")

  // update latency stats
  val update_latency = GTimer() - pred_s1_cycle.getOrElse(dummy_s1_pred_cycle_vec)(s2_commitPtr.value) + 1.U
  XSPerfHistogram("bpu_update_latency", update_latency, io.toBpu.update.valid, 0, 64, 2)

  io.toBpu.update       := DontCare
  io.toBpu.update.valid := s2_commitValid && s2_backendCommit
  val update = io.toBpu.update.bits
  update.false_hit   := s2_commitHit === h_false_hit
  update.pc          := s2_commitPcBundle.startAddr
  update.meta        := s2_commitMeta
  update.cfi_idx     := s2_commitCfi
  update.full_target := s2_commitTarget
  update.from_stage  := s2_commitStage
  update.spec_info   := s2_commitSpecMeta

  val s2_commitRealHit = s2_commitHit === h_hit
  val update_ftb_entry = update.ftb_entry

  val ftbEntryGen = Module(new FTBEntryGen).io
  ftbEntryGen.start_addr     := s2_commitPcBundle.startAddr
  ftbEntryGen.old_entry      := s2_commitFtbEntry
  ftbEntryGen.pd             := s2_commitPd
  ftbEntryGen.cfiIndex       := s2_commitCfi
  ftbEntryGen.target         := s2_commitTarget
  ftbEntryGen.hit            := s2_commitRealHit
  ftbEntryGen.mispredict_vec := s2_commitMispredict

  update_ftb_entry         := ftbEntryGen.new_entry
  update.new_br_insert_pos := ftbEntryGen.new_br_insert_pos
  update.mispred_mask      := ftbEntryGen.mispred_mask
  update.old_entry         := ftbEntryGen.is_old_entry
  update.pred_hit          := s2_commitHit === h_hit || s2_commitHit === h_false_hit
  update.br_taken_mask     := ftbEntryGen.taken_mask
  update.br_committed      := ftbEntryGen.new_entry.brValids
  update.jmp_taken         := ftbEntryGen.jmp_taken

  // ******************************************************************************
  // **************************** commit perf counters ****************************
  // ******************************************************************************

  val commit_mispred_mask     = s2_commitMispredict.asUInt
  val commit_not_mispred_mask = ~commit_mispred_mask

  val commit_br_mask = s2_commitPd.brMask.asUInt
  val commit_jmp_mask =
    UIntToOH(s2_commitPd.jmpOffset) & Fill(PredictWidth, s2_commitPd.jmpInfo.valid.asTypeOf(UInt(1.W)))
  val commit_cfi_mask = commit_br_mask | commit_jmp_mask

  val mbpInstrs = commit_cfi_mask

  val mbpRights = mbpInstrs & commit_not_mispred_mask
  val mbpWrongs = mbpInstrs & commit_mispred_mask

  io.bpuInfo.bpRight := PopCount(mbpRights)
  io.bpuInfo.bpWrong := PopCount(mbpWrongs)

  val hartId           = p(XSCoreParamsKey).HartId
  val isWriteFTQTable  = Constantin.createRecord(s"isWriteFTQTable$hartId")
  val ftqBranchTraceDB = ChiselDB.createTable(s"FTQTable$hartId", new FtqDebugBundle)
  // Cfi Info
  for (i <- 0 until PredictWidth) {
    val pc      = s2_commitPcBundle.startAddr + (i * instBytes).U
    val isBr    = s2_commitPd.brMask(i)
    val isJmp   = s2_commitPd.jmpInfo.valid && s2_commitPd.jmpOffset === i.U
    val isCfi   = isBr || isJmp
    val isTaken = s2_commitCfi.valid && s2_commitCfi.bits === i.U
    val misPred = s2_commitMispredict(i)
    // val ghist = commit_spec_meta.ghist.predHist
    val histPtr   = s2_commitSpecMeta.histPtr
    val predCycle = s2_commitMeta(63, 0)
    val target    = s2_commitTarget

    val brIdx = OHToUInt(Reverse(Cat(update_ftb_entry.brValids.zip(update_ftb_entry.brOffset).map { case (v, offset) =>
      v && offset === i.U
    })))
    val inFtbEntry = update_ftb_entry.brValids.zip(update_ftb_entry.brOffset).map { case (v, offset) =>
      v && offset === i.U
    }.reduce(_ || _)
    val addIntoHist =
      ((s2_commitHit === h_hit) && inFtbEntry) || (!(s2_commitHit === h_hit) && i.U === s2_commitCfi.bits && isBr && s2_commitCfi.valid)
    XSDebug(
      s2_backendCommit && isCfi,
      p"cfi_update: isBr(${isBr}) pc(${Hexadecimal(pc)}) " +
        p"taken(${isTaken}) mispred(${misPred}) cycle($predCycle) hist(${histPtr.value}) " +
        p"startAddr(${Hexadecimal(s2_commitPcBundle.startAddr)}) AddIntoHist(${addIntoHist}) " +
        p"brInEntry(${inFtbEntry}) brIdx(${brIdx}) target(${Hexadecimal(target)})\n"
    )

    val logbundle = Wire(new FtqDebugBundle)
    logbundle.pc        := pc
    logbundle.target    := target
    logbundle.isBr      := isBr
    logbundle.isJmp     := isJmp
    logbundle.isCall    := isJmp && s2_commitPd.hasCall
    logbundle.isRet     := isJmp && s2_commitPd.hasRet
    logbundle.misPred   := misPred
    logbundle.isTaken   := isTaken
    logbundle.predStage := s2_commitStage

    ftqBranchTraceDB.log(
      data = logbundle /* hardware of type T */,
      en = isWriteFTQTable.orR && s2_backendCommit && isCfi,
      site = "FTQ" + p(XSCoreParamsKey).HartId.toString,
      clock = clock,
      reset = reset
    )
  }

  val enq           = io.fromBpu.resp
  val perf_redirect = backendRedirect

  XSPerfAccumulate("entry", validEntries)
  XSPerfAccumulate("bpu_to_ftq_stall", enq.valid && !enq.ready)
  XSPerfAccumulate("mispredictRedirect", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level)
  XSPerfAccumulate("replayRedirect", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level))
  XSPerfAccumulate("predecodeRedirect", fromIfuRedirect.valid)

  XSPerfAccumulate("to_ifu_bubble", io.toIfu.req.ready && !io.toIfu.req.valid)

  XSPerfAccumulate("to_ifu_stall", io.toIfu.req.valid && !io.toIfu.req.ready)
  XSPerfAccumulate("from_bpu_real_bubble", !enq.valid && enq.ready && allowBpuIn)
  XSPerfAccumulate("bpu_to_ifu_bubble", bpuPtr === ifuPtr)
  XSPerfAccumulate(
    "bpu_to_ifu_bubble_when_ftq_full",
    (bpuPtr === ifuPtr) && isFull(bpuPtr, commitPtr) && io.toIfu.req.ready
  )

  XSPerfAccumulate("redirectAhead_ValidNum", ftqIdxAhead.map(_.valid).reduce(_ | _))
  XSPerfAccumulate("fromBackendRedirect_ValidNum", io.fromBackend.redirect.valid)
  XSPerfAccumulate("toBpuRedirect_ValidNum", io.toBpu.redirect.valid)

  val from_bpu = io.fromBpu.resp.bits
  val to_ifu   = io.toIfu.req.bits

  val commit_jal_mask  = UIntToOH(s2_commitPd.jmpOffset) & Fill(PredictWidth, s2_commitPd.hasJal.asTypeOf(UInt(1.W)))
  val commit_jalr_mask = UIntToOH(s2_commitPd.jmpOffset) & Fill(PredictWidth, s2_commitPd.hasJalr.asTypeOf(UInt(1.W)))
  val commit_call_mask = UIntToOH(s2_commitPd.jmpOffset) & Fill(PredictWidth, s2_commitPd.hasCall.asTypeOf(UInt(1.W)))
  val commit_ret_mask  = UIntToOH(s2_commitPd.jmpOffset) & Fill(PredictWidth, s2_commitPd.hasRet.asTypeOf(UInt(1.W)))

  val mbpBRights = mbpRights & commit_br_mask
  val mbpJRights = mbpRights & commit_jal_mask
  val mbpIRights = mbpRights & commit_jalr_mask
  val mbpCRights = mbpRights & commit_call_mask
  val mbpRRights = mbpRights & commit_ret_mask

  val mbpBWrongs = mbpWrongs & commit_br_mask
  val mbpJWrongs = mbpWrongs & commit_jal_mask
  val mbpIWrongs = mbpWrongs & commit_jalr_mask
  val mbpCWrongs = mbpWrongs & commit_call_mask
  val mbpRWrongs = mbpWrongs & commit_ret_mask

  val commit_pred_stage = RegNext(pred_stage(commitPtr.value))

  def pred_stage_map(src: UInt, name: String) =
    (0 until numBpStages).map(i =>
      f"${name}_stage_${i + 1}" -> PopCount(src.asBools.map(_ && commit_pred_stage === BP_STAGES(i)))
    ).foldLeft(Map[String, UInt]())(_ + _)

  val mispred_stage_map      = pred_stage_map(mbpWrongs, "mispredict")
  val br_mispred_stage_map   = pred_stage_map(mbpBWrongs, "br_mispredict")
  val jalr_mispred_stage_map = pred_stage_map(mbpIWrongs, "jalr_mispredict")
  val correct_stage_map      = pred_stage_map(mbpRights, "correct")
  val br_correct_stage_map   = pred_stage_map(mbpBRights, "br_correct")
  val jalr_correct_stage_map = pred_stage_map(mbpIRights, "jalr_correct")

  val update_valid = io.toBpu.update.valid
  def u(cond: Bool) = update_valid && cond
  val ftb_false_hit = u(update.false_hit)
  // assert(!ftb_false_hit)
  val ftb_hit = u(s2_commitHit === h_hit)

  val ftb_new_entry                = u(ftbEntryGen.is_init_entry)
  val ftb_new_entry_only_br        = ftb_new_entry && !update_ftb_entry.jmpValid
  val ftb_new_entry_only_jmp       = ftb_new_entry && !update_ftb_entry.brValids(0)
  val ftb_new_entry_has_br_and_jmp = ftb_new_entry && update_ftb_entry.brValids(0) && update_ftb_entry.jmpValid

  val ftb_old_entry = u(ftbEntryGen.is_old_entry)

  val ftb_modified_entry =
    u(ftbEntryGen.is_new_br || ftbEntryGen.is_jalr_target_modified || ftbEntryGen.is_strong_bias_modified)
  val ftb_modified_entry_new_br               = u(ftbEntryGen.is_new_br)
  val ftb_modified_entry_ifu_redirected       = u(ifuRedirected(s2_commitPtr.value))
  val ftb_modified_entry_jalr_target_modified = u(ftbEntryGen.is_jalr_target_modified)
  val ftb_modified_entry_br_full              = ftb_modified_entry && ftbEntryGen.is_br_full
  val ftb_modified_entry_strong_bias          = ftb_modified_entry && ftbEntryGen.is_strong_bias_modified

  def getFtbEntryLen(pc: UInt, entry: FTBEntry) = (entry.getFallThrough(pc) - pc) >> instOffsetBits
  val gen_ftb_entry_len = getFtbEntryLen(update.pc, ftbEntryGen.new_entry)
  XSPerfHistogram("ftb_init_entry_len", gen_ftb_entry_len, ftb_new_entry, 0, PredictWidth + 1, 1)
  XSPerfHistogram("ftb_modified_entry_len", gen_ftb_entry_len, ftb_modified_entry, 0, PredictWidth + 1, 1)
  val s3_ftb_entry_len = getFtbEntryLen(from_bpu.s3.pc(0), from_bpu.last_stage_ftb_entry)
  XSPerfHistogram("s3_ftb_entry_len", s3_ftb_entry_len, from_bpu.s3.valid(0), 0, PredictWidth + 1, 1)

  XSPerfHistogram("ftq_has_entry", validEntries, true.B, 0, FtqSize + 1, 1)

  val perfCountsMap = Map(
    "BpInstr"                        -> PopCount(mbpInstrs),
    "BpBInstr"                       -> PopCount(mbpBRights | mbpBWrongs),
    "BpRight"                        -> PopCount(mbpRights),
    "BpWrong"                        -> PopCount(mbpWrongs),
    "BpBRight"                       -> PopCount(mbpBRights),
    "BpBWrong"                       -> PopCount(mbpBWrongs),
    "BpJRight"                       -> PopCount(mbpJRights),
    "BpJWrong"                       -> PopCount(mbpJWrongs),
    "BpIRight"                       -> PopCount(mbpIRights),
    "BpIWrong"                       -> PopCount(mbpIWrongs),
    "BpCRight"                       -> PopCount(mbpCRights),
    "BpCWrong"                       -> PopCount(mbpCWrongs),
    "BpRRight"                       -> PopCount(mbpRRights),
    "BpRWrong"                       -> PopCount(mbpRWrongs),
    "ftb_false_hit"                  -> PopCount(ftb_false_hit),
    "ftb_hit"                        -> PopCount(ftb_hit),
    "ftb_new_entry"                  -> PopCount(ftb_new_entry),
    "ftb_new_entry_only_br"          -> PopCount(ftb_new_entry_only_br),
    "ftb_new_entry_only_jmp"         -> PopCount(ftb_new_entry_only_jmp),
    "ftb_new_entry_has_br_and_jmp"   -> PopCount(ftb_new_entry_has_br_and_jmp),
    "ftb_old_entry"                  -> PopCount(ftb_old_entry),
    "ftb_modified_entry"             -> PopCount(ftb_modified_entry),
    "ftb_modified_entry_new_br"      -> PopCount(ftb_modified_entry_new_br),
    "ftb_jalr_target_modified"       -> PopCount(ftb_modified_entry_jalr_target_modified),
    "ftb_modified_entry_br_full"     -> PopCount(ftb_modified_entry_br_full),
    "ftb_modified_entry_strong_bias" -> PopCount(ftb_modified_entry_strong_bias)
  ) ++ mispred_stage_map ++ br_mispred_stage_map ++ jalr_mispred_stage_map ++
    correct_stage_map ++ br_correct_stage_map ++ jalr_correct_stage_map

  for ((key, value) <- perfCountsMap) {
    XSPerfAccumulate(key, value)
  }

  // --------------------------- Debug --------------------------------
  // XSDebug(enq_fire, p"enq! " + io.fromBpu.resp.bits.toPrintable)
  XSDebug(io.toIfu.req.fire, p"fire to ifu " + io.toIfu.req.bits.toPrintable)
  XSDebug(s2_backendCommit, p"deq! [ptr] $s2_commitPtr\n")
  XSDebug(true.B, p"[bpuPtr] $bpuPtr, [ifuPtr] $ifuPtr, [ifuWbPtr] $ifuWbPtr [commPtr] $commitPtr\n")
  XSDebug(
    true.B,
    p"[in] v:${io.fromBpu.resp.valid} r:${io.fromBpu.resp.ready} " +
      p"[out] v:${io.toIfu.req.valid} r:${io.toIfu.req.ready}\n"
  )
  XSDebug(
    s2_backendCommit,
    p"[deq info] cfiIndex: $s2_commitCfi, $s2_commitPcBundle, target: ${Hexadecimal(s2_commitTarget)}\n"
  )

  //   def ubtbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ Mux(ans.hit.asBool,
  //           Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
  //           !taken),
  //         !taken),
  //       false.B)
  //     }
  //   }

  //   def btbCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ Mux(ans.hit.asBool,
  //           Mux(ans.taken.asBool, taken && ans.target === commitEntry.target,
  //           !taken),
  //         !taken),
  //       false.B)
  //     }
  //   }

  //   def tageCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isBr,
  //         isWrong ^ (ans.taken.asBool === taken),
  //       false.B)
  //     }
  //   }

  //   def loopCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && (pd.isBr) && ans.hit.asBool,
  //         isWrong ^ (!taken),
  //           false.B)
  //     }
  //   }

  //   def rasCheck(commit: FtqEntry, predAns: Seq[PredictorAnswer], isWrong: Bool) = {
  //     commit.valids.zip(commit.pd).zip(predAns).zip(commit.takens).map {
  //       case (((valid, pd), ans), taken) =>
  //       Mux(valid && pd.isRet.asBool /*&& taken*/ && ans.hit.asBool,
  //         isWrong ^ (ans.target === commitEntry.target),
  //           false.B)
  //     }
  //   }

  //   val ubtbRights = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), false.B)
  //   val ubtbWrongs = ubtbCheck(commitEntry, commitEntry.metas.map(_.ubtbAns), true.B)
  //   // btb and ubtb pred jal and jalr as well
  //   val btbRights = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), false.B)
  //   val btbWrongs = btbCheck(commitEntry, commitEntry.metas.map(_.btbAns), true.B)
  //   val tageRights = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), false.B)
  //   val tageWrongs = tageCheck(commitEntry, commitEntry.metas.map(_.tageAns), true.B)

  //   val loopRights = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), false.B)
  //   val loopWrongs = loopCheck(commitEntry, commitEntry.metas.map(_.loopAns), true.B)

  //   val rasRights = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), false.B)
  //   val rasWrongs = rasCheck(commitEntry, commitEntry.metas.map(_.rasAns), true.B)

  val perfEvents = Seq(
    ("bpu_s2_redirect        ", bpu_s2_redirect),
    ("bpu_s3_redirect        ", bpu_s3_redirect),
    ("bpu_to_ftq_stall       ", enq.valid && ~enq.ready),
    ("mispredictRedirect     ", perf_redirect.valid && RedirectLevel.flushAfter === perf_redirect.bits.level),
    ("replayRedirect         ", perf_redirect.valid && RedirectLevel.flushItself(perf_redirect.bits.level)),
    ("predecodeRedirect      ", fromIfuRedirect.valid),
    ("to_ifu_bubble          ", io.toIfu.req.ready && !io.toIfu.req.valid),
    ("from_bpu_real_bubble   ", !enq.valid && enq.ready && allowBpuIn),
    ("BpInstr                ", PopCount(mbpInstrs)),
    ("BpBInstr               ", PopCount(mbpBRights | mbpBWrongs)),
    ("BpRight                ", PopCount(mbpRights)),
    ("BpWrong                ", PopCount(mbpWrongs)),
    ("BpBRight               ", PopCount(mbpBRights)),
    ("BpBWrong               ", PopCount(mbpBWrongs)),
    ("BpJRight               ", PopCount(mbpJRights)),
    ("BpJWrong               ", PopCount(mbpJWrongs)),
    ("BpIRight               ", PopCount(mbpIRights)),
    ("BpIWrong               ", PopCount(mbpIWrongs)),
    ("BpCRight               ", PopCount(mbpCRights)),
    ("BpCWrong               ", PopCount(mbpCWrongs)),
    ("BpRRight               ", PopCount(mbpRRights)),
    ("BpRWrong               ", PopCount(mbpRWrongs)),
    ("ftb_false_hit          ", PopCount(ftb_false_hit)),
    ("ftb_hit                ", PopCount(ftb_hit))
  )
  generatePerfEvent()
}
