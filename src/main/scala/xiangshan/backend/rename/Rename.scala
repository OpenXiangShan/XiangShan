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
***************************************************************************************/

package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.backend.Bundles.{DecodedInst, DynInst}
import xiangshan.backend.decode.{FusionDecodeInfo, ImmUnion, Imm_I, Imm_LUI_LOAD, Imm_U}
import xiangshan.backend.fu.FuType
import xiangshan.backend.rename.freelist._
import xiangshan.backend.rob.{RobEnqIO, RobPtr}
import xiangshan.mem.mdp._
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.FuType._
import xiangshan.mem.{EewLog2, GenUSWholeEmul}
import xiangshan.mem.GenRealFlowNum

class Rename(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper with HasPerfEvents {

  // params alias
  private val numRegSrc = backendParams.numRegSrc
  private val numVecRegSrc = backendParams.numVecRegSrc
  private val numVecRatPorts = numVecRegSrc

  println(s"[Rename] numRegSrc: $numRegSrc")

  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val rabCommits = Input(new RabCommitIO)
    // from decode
    val in = Vec(RenameWidth, Flipped(DecoupledIO(new DecodedInst)))
    val fusionInfo = Vec(DecodeWidth - 1, Flipped(new FusionDecodeInfo))
    // ssit read result
    val ssit = Flipped(Vec(RenameWidth, Output(new SSITEntry)))
    // waittable read result
    val waittable = Flipped(Vec(RenameWidth, Output(Bool())))
    // to rename table
    val intReadPorts = Vec(RenameWidth, Vec(2, Input(UInt(PhyRegIdxWidth.W))))
    val fpReadPorts = Vec(RenameWidth, Vec(3, Input(UInt(PhyRegIdxWidth.W))))
    val vecReadPorts = Vec(RenameWidth, Vec(numVecRatPorts, Input(UInt(PhyRegIdxWidth.W))))
    val v0ReadPorts = Vec(RenameWidth, Vec(1, Input(UInt(PhyRegIdxWidth.W))))
    val vlReadPorts = Vec(RenameWidth, Vec(1, Input(UInt(PhyRegIdxWidth.W))))
    val intRenamePorts = Vec(RenameWidth, Output(new RatWritePort(log2Ceil(IntLogicRegs))))
    val fpRenamePorts = Vec(RenameWidth, Output(new RatWritePort(log2Ceil(FpLogicRegs))))
    val vecRenamePorts = Vec(RenameWidth, Output(new RatWritePort(log2Ceil(VecLogicRegs))))
    val v0RenamePorts = Vec(RenameWidth, Output(new RatWritePort(log2Ceil(V0LogicRegs))))
    val vlRenamePorts = Vec(RenameWidth, Output(new RatWritePort(log2Ceil(VlLogicRegs))))
    // from rename table
    val int_old_pdest = Vec(RabCommitWidth, Input(UInt(PhyRegIdxWidth.W)))
    val fp_old_pdest = Vec(RabCommitWidth, Input(UInt(PhyRegIdxWidth.W)))
    val vec_old_pdest = Vec(RabCommitWidth, Input(UInt(PhyRegIdxWidth.W)))
    val v0_old_pdest = Vec(RabCommitWidth, Input(UInt(PhyRegIdxWidth.W)))
    val vl_old_pdest = Vec(RabCommitWidth, Input(UInt(PhyRegIdxWidth.W)))
    val int_need_free = Vec(RabCommitWidth, Input(Bool()))
    // to dispatch1
    val out = Vec(RenameWidth, DecoupledIO(new DynInst))
    // for snapshots
    val snpt = Input(new SnapshotPort)
    val snptLastEnq = Flipped(ValidIO(new RobPtr))
    val snptIsFull= Input(Bool())
    // debug arch ports
    val debug_int_rat = if (backendParams.debugEn) Some(Vec(32, Input(UInt(PhyRegIdxWidth.W)))) else None
    val debug_fp_rat = if (backendParams.debugEn) Some(Vec(32, Input(UInt(PhyRegIdxWidth.W)))) else None
    val debug_vec_rat = if (backendParams.debugEn) Some(Vec(31, Input(UInt(PhyRegIdxWidth.W)))) else None
    val debug_v0_rat = if (backendParams.debugEn) Some(Vec(1, Input(UInt(PhyRegIdxWidth.W)))) else None
    val debug_vl_rat = if (backendParams.debugEn) Some(Vec(1, Input(UInt(PhyRegIdxWidth.W)))) else None
    // perf only
    val stallReason = new Bundle {
      val in = Flipped(new StallReasonIO(RenameWidth))
      val out = new StallReasonIO(RenameWidth)
    }
  })

  // io alias
  private val dispatchCanAcc = io.out.head.ready

  val compressUnit = Module(new CompressUnit())
  // create free list and rat
  val intFreeList = Module(new MEFreeList(IntPhyRegs))
  val fpFreeList = Module(new StdFreeList(FpPhyRegs - FpLogicRegs, FpLogicRegs, Reg_F))
  val vecFreeList = Module(new StdFreeList(VfPhyRegs - VecLogicRegs, VecLogicRegs, Reg_V, 31))
  val v0FreeList = Module(new StdFreeList(V0PhyRegs - V0LogicRegs, V0LogicRegs, Reg_V0, 1))
  val vlFreeList = Module(new StdFreeList(VlPhyRegs - VlLogicRegs, VlLogicRegs, Reg_Vl, 1))


  intFreeList.io.commit    <> io.rabCommits
  intFreeList.io.debug_rat.foreach(_ <> io.debug_int_rat.get)
  fpFreeList.io.commit     <> io.rabCommits
  fpFreeList.io.debug_rat.foreach(_ <> io.debug_fp_rat.get)
  vecFreeList.io.commit    <> io.rabCommits
  vecFreeList.io.debug_rat.foreach(_ <> io.debug_vec_rat.get)
  v0FreeList.io.commit <> io.rabCommits
  v0FreeList.io.debug_rat.foreach(_ <> io.debug_v0_rat.get)
  vlFreeList.io.commit <> io.rabCommits
  vlFreeList.io.debug_rat.foreach(_ <> io.debug_vl_rat.get)

  // decide if given instruction needs allocating a new physical register (CfCtrl: from decode; RobCommitInfo: from rob)
  def needDestReg[T <: DecodedInst](reg_t: RegType, x: T): Bool = reg_t match {
    case Reg_I => x.rfWen && x.ldest =/= 0.U
    case Reg_F => x.fpWen
    case Reg_V => x.vecWen
    case Reg_V0 => x.v0Wen
    case Reg_Vl => x.vlWen
  }
  def needDestRegCommit[T <: RabCommitInfo](reg_t: RegType, x: T): Bool = {
    reg_t match {
      case Reg_I => x.rfWen
      case Reg_F => x.fpWen
      case Reg_V => x.vecWen
      case Reg_V0 => x.v0Wen
      case Reg_Vl => x.vlWen
    }
  }
  def needDestRegWalk[T <: RabCommitInfo](reg_t: RegType, x: T): Bool = {
    reg_t match {
      case Reg_I => x.rfWen && x.ldest =/= 0.U
      case Reg_F => x.fpWen
      case Reg_V => x.vecWen
      case Reg_V0 => x.v0Wen
      case Reg_Vl => x.vlWen
    }
  }

  // connect [redirect + walk] ports for fp & vec & int free list
  Seq(fpFreeList, vecFreeList, intFreeList, v0FreeList, vlFreeList).foreach { case fl =>
    fl.io.redirect := io.redirect.valid
    fl.io.walk := io.rabCommits.isWalk
  }
  // only when all free list and dispatch1 has enough space can we do allocation
  // when isWalk, freelist can definitely allocate
  intFreeList.io.doAllocate := fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && dispatchCanAcc || io.rabCommits.isWalk
  fpFreeList.io.doAllocate := intFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && dispatchCanAcc || io.rabCommits.isWalk
  vecFreeList.io.doAllocate := intFreeList.io.canAllocate && fpFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && dispatchCanAcc || io.rabCommits.isWalk
  v0FreeList.io.doAllocate := intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && vlFreeList.io.canAllocate && dispatchCanAcc || io.rabCommits.isWalk
  vlFreeList.io.doAllocate := intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && dispatchCanAcc || io.rabCommits.isWalk

  //           dispatch1 ready ++ float point free list ready ++ int free list ready ++ vec free list ready     ++ not walk
  val canOut = dispatchCanAcc && fpFreeList.io.canAllocate && intFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !io.rabCommits.isWalk

  compressUnit.io.in.zip(io.in).foreach{ case(sink, source) =>
    sink.valid := source.valid
    sink.bits := source.bits
  }
  val needRobFlags = compressUnit.io.out.needRobFlags
  val instrSizesVec = compressUnit.io.out.instrSizes
  val compressMasksVec = compressUnit.io.out.masks

  // speculatively assign the instruction with an robIdx
  val validCount = PopCount(io.in.zip(needRobFlags).map{ case(in, needRobFlag) => in.valid && in.bits.lastUop && needRobFlag}) // number of instructions waiting to enter rob (from decode)
  val robIdxHead = RegInit(0.U.asTypeOf(new RobPtr))
  val lastCycleMisprediction = GatedValidRegNext(io.redirect.valid && !io.redirect.bits.flushItself())
  val robIdxHeadNext = Mux(io.redirect.valid, io.redirect.bits.robIdx, // redirect: move ptr to given rob index
         Mux(lastCycleMisprediction, robIdxHead + 1.U, // mis-predict: not flush robIdx itself
           Mux(canOut, robIdxHead + validCount, // instructions successfully entered next stage: increase robIdx
                      /* default */  robIdxHead))) // no instructions passed by this cycle: stick to old value
  robIdxHead := robIdxHeadNext

  /**
    * Rename: allocate free physical register and update rename table
    */
  val uops = Wire(Vec(RenameWidth, new DynInst))
  uops.foreach( uop => {
    uop.srcState      := DontCare
    uop.debugInfo     := DontCare
    uop.lqIdx         := DontCare
    uop.sqIdx         := DontCare
    uop.waitForRobIdx := DontCare
    uop.singleStep    := DontCare
    uop.snapshot      := DontCare
    uop.srcLoadDependency := DontCare
    uop.numLsElem       :=  DontCare
    uop.hasException  :=  DontCare
    uop.useRegCache   := DontCare
    uop.regCacheIdx   := DontCare
  })
  private val fuType       = uops.map(_.fuType)
  private val fuOpType     = uops.map(_.fuOpType)
  private val vtype        = uops.map(_.vpu.vtype)
  private val sew          = vtype.map(_.vsew)
  private val lmul         = vtype.map(_.vlmul)
  private val eew          = uops.map(_.vpu.veew)
  private val mop          = fuOpType.map(fuOpTypeItem => LSUOpType.getVecLSMop(fuOpTypeItem))
  private val isVlsType    = fuType.map(fuTypeItem => isVls(fuTypeItem))
  private val isSegment    = fuType.map(fuTypeItem => isVsegls(fuTypeItem))
  private val isUnitStride = fuOpType.map(fuOpTypeItem => LSUOpType.isAllUS(fuOpTypeItem))
  private val nf           = fuOpType.zip(uops.map(_.vpu.nf)).map { case (fuOpTypeItem, nfItem) => Mux(LSUOpType.isWhole(fuOpTypeItem), 0.U, nfItem) }
  private val mulBits      = 3 // dirty code
  private val emul         = fuOpType.zipWithIndex.map { case (fuOpTypeItem, index) =>
    Mux(
      LSUOpType.isWhole(fuOpTypeItem),
      GenUSWholeEmul(nf(index)),
      Mux(
        LSUOpType.isMasked(fuOpTypeItem),
        0.U(mulBits.W),
        EewLog2(eew(index)) - sew(index) + lmul(index)
      )
    )
  }
  private val isVecUnitType = isVlsType.zip(isUnitStride).map { case (isVlsTypeItme, isUnitStrideItem) =>
    isVlsTypeItme && isUnitStrideItem
  }
  private val instType = isSegment.zip(mop).map { case (isSegementItem, mopItem) => Cat(isSegementItem, mopItem) }
  // There is no way to calculate the 'flow' for 'unit-stride' exactly:
  //  Whether 'unit-stride' needs to be split can only be known after obtaining the address.
  // For scalar instructions, this is not handled here, and different assignments are done later according to the situation.
  private val numLsElem = instType.zipWithIndex.map { case (instTypeItem, index) =>
    Mux(
      isVecUnitType(index),
      VecMemUnitStrideMaxFlowNum.U,
      GenRealFlowNum(instTypeItem, emul(index), lmul(index), eew(index), sew(index))
    )
  }
  uops.zipWithIndex.map { case(u, i) =>
    u.numLsElem := Mux(io.in(i).valid & isVlsType(i), numLsElem(i), 0.U)
  }

  val needVecDest    = Wire(Vec(RenameWidth, Bool()))
  val needFpDest     = Wire(Vec(RenameWidth, Bool()))
  val needIntDest    = Wire(Vec(RenameWidth, Bool()))
  val needV0Dest     = Wire(Vec(RenameWidth, Bool()))
  val needVlDest     = Wire(Vec(RenameWidth, Bool()))
  val hasValid = Cat(io.in.map(_.valid)).orR
  private val inHeadValid = io.in.head.valid

  val isMove = Wire(Vec(RenameWidth, Bool()))
  isMove zip io.in.map(_.bits) foreach {
    case (move, in) => move := Mux(in.exceptionVec.asUInt.orR, false.B, in.isMove)
  }

  val walkNeedIntDest = WireDefault(VecInit(Seq.fill(RenameWidth)(false.B)))
  val walkNeedFpDest = WireDefault(VecInit(Seq.fill(RenameWidth)(false.B)))
  val walkNeedVecDest = WireDefault(VecInit(Seq.fill(RenameWidth)(false.B)))
  val walkNeedV0Dest = WireDefault(VecInit(Seq.fill(RenameWidth)(false.B)))
  val walkNeedVlDest = WireDefault(VecInit(Seq.fill(RenameWidth)(false.B)))
  val walkIsMove = WireDefault(VecInit(Seq.fill(RenameWidth)(false.B)))

  val intSpecWen = Wire(Vec(RenameWidth, Bool()))
  val fpSpecWen  = Wire(Vec(RenameWidth, Bool()))
  val vecSpecWen = Wire(Vec(RenameWidth, Bool()))
  val v0SpecWen = Wire(Vec(RenameWidth, Bool()))
  val vlSpecWen = Wire(Vec(RenameWidth, Bool()))

  val walkIntSpecWen = WireDefault(VecInit(Seq.fill(RenameWidth)(false.B)))

  val walkPdest = Wire(Vec(RenameWidth, UInt(PhyRegIdxWidth.W)))

  // uop calculation
  for (i <- 0 until RenameWidth) {
    (uops(i): Data).waiveAll :<= (io.in(i).bits: Data).waiveAll

    // update cf according to ssit result
    uops(i).storeSetHit := io.ssit(i).valid
    uops(i).loadWaitStrict := io.ssit(i).strict && io.ssit(i).valid
    uops(i).ssid := io.ssit(i).ssid

    // update cf according to waittable result
    uops(i).loadWaitBit := io.waittable(i)

    uops(i).replayInst := false.B // set by IQ or MemQ
    // alloc a new phy reg
    needV0Dest(i) := io.in(i).valid && needDestReg(Reg_V0, io.in(i).bits)
    needVlDest(i) := io.in(i).valid && needDestReg(Reg_Vl, io.in(i).bits)
    needVecDest(i) := io.in(i).valid && needDestReg(Reg_V, io.in(i).bits)
    needFpDest(i) := io.in(i).valid && needDestReg(Reg_F, io.in(i).bits)
    needIntDest(i) := io.in(i).valid && needDestReg(Reg_I, io.in(i).bits)
    if (i < RabCommitWidth) {
      walkNeedIntDest(i) := io.rabCommits.walkValid(i) && needDestRegWalk(Reg_I, io.rabCommits.info(i))
      walkNeedFpDest(i) := io.rabCommits.walkValid(i) && needDestRegWalk(Reg_F, io.rabCommits.info(i))
      walkNeedVecDest(i) := io.rabCommits.walkValid(i) && needDestRegWalk(Reg_V, io.rabCommits.info(i))
      walkNeedV0Dest(i) := io.rabCommits.walkValid(i) && needDestRegWalk(Reg_V0, io.rabCommits.info(i))
      walkNeedVlDest(i) := io.rabCommits.walkValid(i) && needDestRegWalk(Reg_Vl, io.rabCommits.info(i))
      walkIsMove(i) := io.rabCommits.info(i).isMove
    }
    fpFreeList.io.allocateReq(i) := needFpDest(i)
    fpFreeList.io.walkReq(i) := walkNeedFpDest(i)
    vecFreeList.io.allocateReq(i) := needVecDest(i)
    vecFreeList.io.walkReq(i) := walkNeedVecDest(i)
    v0FreeList.io.allocateReq(i) := needV0Dest(i)
    v0FreeList.io.walkReq(i) := walkNeedV0Dest(i)
    vlFreeList.io.allocateReq(i) := needVlDest(i)
    vlFreeList.io.walkReq(i) := walkNeedVlDest(i)
    intFreeList.io.allocateReq(i) := needIntDest(i) && !isMove(i)
    intFreeList.io.walkReq(i) := walkNeedIntDest(i) && !walkIsMove(i)

    // no valid instruction from decode stage || all resources (dispatch1 + both free lists) ready
    io.in(i).ready := canOut

    uops(i).robIdx := robIdxHead + PopCount(io.in.zip(needRobFlags).take(i).map{ case(in, needRobFlag) => in.valid && in.bits.lastUop && needRobFlag})
    uops(i).instrSize := instrSizesVec(i)
    val hasExceptionExceptFlushPipe = Cat(selectFrontend(uops(i).exceptionVec) :+ uops(i).exceptionVec(illegalInstr) :+ uops(i).exceptionVec(virtualInstr)).orR || uops(i).trigger.getFrontendCanFire
    when(isMove(i) || hasExceptionExceptFlushPipe) {
      uops(i).numUops := 0.U
      uops(i).numWB := 0.U
    }
    if (i > 0) {
      when(!needRobFlags(i - 1)) {
        uops(i).firstUop := false.B
        uops(i).ftqPtr := uops(i - 1).ftqPtr
        uops(i).ftqOffset := uops(i - 1).ftqOffset
        uops(i).numUops := instrSizesVec(i) - PopCount(compressMasksVec(i) & Cat(isMove.reverse))
        uops(i).numWB := instrSizesVec(i) - PopCount(compressMasksVec(i) & Cat(isMove.reverse))
      }
    }
    when(!needRobFlags(i)) {
      uops(i).lastUop := false.B
      uops(i).numUops := instrSizesVec(i) - PopCount(compressMasksVec(i) & Cat(isMove.reverse))
      uops(i).numWB := instrSizesVec(i) - PopCount(compressMasksVec(i) & Cat(isMove.reverse))
    }
    uops(i).wfflags := (compressMasksVec(i) & Cat(io.in.map(_.bits.wfflags).reverse)).orR
    uops(i).dirtyFs := (compressMasksVec(i) & Cat(io.in.map(_.bits.fpWen).reverse)).orR
    // vector instructions' uopSplitType cannot be UopSplitType.SCA_SIM
    uops(i).dirtyVs := (compressMasksVec(i) & Cat(io.in.map(_.bits.uopSplitType =/= UopSplitType.SCA_SIM).reverse)).orR
    // psrc0,psrc1,psrc2 don't require v0ReadPorts because their srcType can distinguish whether they are V0 or not
    uops(i).psrc(0) := Mux1H(uops(i).srcType(0)(2, 0), Seq(io.intReadPorts(i)(0), io.fpReadPorts(i)(0), io.vecReadPorts(i)(0)))
    uops(i).psrc(1) := Mux1H(uops(i).srcType(1)(2, 0), Seq(io.intReadPorts(i)(1), io.fpReadPorts(i)(1), io.vecReadPorts(i)(1)))
    uops(i).psrc(2) := Mux1H(uops(i).srcType(2)(2, 1), Seq(io.fpReadPorts(i)(2), io.vecReadPorts(i)(2)))
    uops(i).psrc(3) := io.v0ReadPorts(i)(0)
    uops(i).psrc(4) := io.vlReadPorts(i)(0)

    // int psrc2 should be bypassed from next instruction if it is fused
    if (i < RenameWidth - 1) {
      when (io.fusionInfo(i).rs2FromRs2 || io.fusionInfo(i).rs2FromRs1) {
        uops(i).psrc(1) := Mux(io.fusionInfo(i).rs2FromRs2, io.intReadPorts(i + 1)(1), io.intReadPorts(i + 1)(0))
      }.elsewhen(io.fusionInfo(i).rs2FromZero) {
        uops(i).psrc(1) := 0.U
      }
    }
    uops(i).eliminatedMove := isMove(i)

    // update pdest
    uops(i).pdest := MuxCase(0.U, Seq(
      needIntDest(i)    ->  intFreeList.io.allocatePhyReg(i),
      needFpDest(i)     ->  fpFreeList.io.allocatePhyReg(i),
      needVecDest(i)    ->  vecFreeList.io.allocatePhyReg(i),
      needV0Dest(i)    ->  v0FreeList.io.allocatePhyReg(i),
      needVlDest(i)    ->  vlFreeList.io.allocatePhyReg(i),
    ))

    // Assign performance counters
    uops(i).debugInfo.renameTime := GTimer()

    io.out(i).valid := io.in(i).valid && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !io.rabCommits.isWalk
    io.out(i).bits := uops(i)
    // Todo: move these shit in decode stage
    // dirty code for fence. The lsrc is passed by imm.
    when (io.out(i).bits.fuType === FuType.fence.U) {
      io.out(i).bits.imm := Cat(io.in(i).bits.lsrc(1), io.in(i).bits.lsrc(0))
    }

    // dirty code for SoftPrefetch (prefetch.r/prefetch.w)
//    when (io.in(i).bits.isSoftPrefetch) {
//      io.out(i).bits.fuType := FuType.ldu.U
//      io.out(i).bits.fuOpType := Mux(io.in(i).bits.lsrc(1) === 1.U, LSUOpType.prefetch_r, LSUOpType.prefetch_w)
//      io.out(i).bits.selImm := SelImm.IMM_S
//      io.out(i).bits.imm := Cat(io.in(i).bits.imm(io.in(i).bits.imm.getWidth - 1, 5), 0.U(5.W))
//    }

    // dirty code for lui+addi(w) fusion
    if (i < RenameWidth - 1) {
      val fused_lui32 = io.in(i).bits.selImm === SelImm.IMM_LUI32 && io.in(i).bits.fuType === FuType.alu.U
      when (fused_lui32) {
        val lui_imm = io.in(i).bits.imm(19, 0)
        val add_imm = io.in(i + 1).bits.imm(11, 0)
        require(io.out(i).bits.imm.getWidth >= lui_imm.getWidth + add_imm.getWidth)
        io.out(i).bits.imm := Cat(lui_imm, add_imm)
      }
    }

    // write speculative rename table
    // we update rat later inside commit code
    intSpecWen(i) := needIntDest(i) && intFreeList.io.canAllocate && intFreeList.io.doAllocate && !io.rabCommits.isWalk && !io.redirect.valid
    fpSpecWen(i)  := needFpDest(i)  && fpFreeList.io.canAllocate  && fpFreeList.io.doAllocate  && !io.rabCommits.isWalk && !io.redirect.valid
    vecSpecWen(i) := needVecDest(i) && vecFreeList.io.canAllocate && vecFreeList.io.doAllocate && !io.rabCommits.isWalk && !io.redirect.valid
    v0SpecWen(i) := needV0Dest(i) && v0FreeList.io.canAllocate && v0FreeList.io.doAllocate && !io.rabCommits.isWalk && !io.redirect.valid
    vlSpecWen(i) := needVlDest(i) && vlFreeList.io.canAllocate && vlFreeList.io.doAllocate && !io.rabCommits.isWalk && !io.redirect.valid


    if (i < RabCommitWidth) {
      walkIntSpecWen(i) := walkNeedIntDest(i) && !io.redirect.valid
      walkPdest(i) := io.rabCommits.info(i).pdest
    } else {
      walkPdest(i) := io.out(i).bits.pdest
    }
  }

  /**
    * How to set psrc:
    * - bypass the pdest to psrc if previous instructions write to the same ldest as lsrc
    * - default: psrc from RAT
    * How to set pdest:
    * - Mux(isMove, psrc, pdest_from_freelist).
    *
    * The critical path of rename lies here:
    * When move elimination is enabled, we need to update the rat with psrc.
    * However, psrc maybe comes from previous instructions' pdest, which comes from freelist.
    *
    * If we expand these logic for pdest(N):
    * pdest(N) = Mux(isMove(N), psrc(N), freelist_out(N))
    *          = Mux(isMove(N), Mux(bypass(N, N - 1), pdest(N - 1),
    *                           Mux(bypass(N, N - 2), pdest(N - 2),
    *                           ...
    *                           Mux(bypass(N, 0),     pdest(0),
    *                                                 rat_out(N))...)),
    *                           freelist_out(N))
    */
  // a simple functional model for now
  io.out(0).bits.pdest := Mux(isMove(0), uops(0).psrc.head, uops(0).pdest)

  // psrc(n) + pdest(1)
  val bypassCond: Vec[MixedVec[UInt]] = Wire(Vec(numRegSrc + 1, MixedVec(List.tabulate(RenameWidth-1)(i => UInt((i+1).W)))))
  require(io.in(0).bits.srcType.size == io.in(0).bits.numSrc)
  private val pdestLoc = io.in.head.bits.srcType.size // 2 vector src: v0, vl&vtype
  println(s"[Rename] idx of pdest in bypassCond $pdestLoc")
  for (i <- 1 until RenameWidth) {
    val v0Cond = io.in(i).bits.srcType.zipWithIndex.map{ case (s, i) =>
      if (i == 3) (s === SrcType.vp) || (s === SrcType.v0)
      else false.B
    } :+ needV0Dest(i)
    val vlCond = io.in(i).bits.srcType.zipWithIndex.map{ case (s, i) =>
      if (i == 4) s === SrcType.vp
      else false.B
    } :+ needVlDest(i)
    val vecCond = io.in(i).bits.srcType.map(_ === SrcType.vp) :+ needVecDest(i)
    val fpCond  = io.in(i).bits.srcType.map(_ === SrcType.fp) :+ needFpDest(i)
    val intCond = io.in(i).bits.srcType.map(_ === SrcType.xp) :+ needIntDest(i)
    val target = io.in(i).bits.lsrc :+ io.in(i).bits.ldest
    for ((((((cond1, (condV0, condVl)), cond2), cond3), t), j) <- vecCond.zip(v0Cond.zip(vlCond)).zip(fpCond).zip(intCond).zip(target).zipWithIndex) {
      val destToSrc = io.in.take(i).zipWithIndex.map { case (in, j) =>
        val indexMatch = in.bits.ldest === t
        val writeMatch =  cond3 && needIntDest(j) || cond2 && needFpDest(j) || cond1 && needVecDest(j)
        val v0vlMatch = condV0 && needV0Dest(j) || condVl && needVlDest(j)
        indexMatch && writeMatch || v0vlMatch
      }
      bypassCond(j)(i - 1) := VecInit(destToSrc).asUInt
    }
    io.out(i).bits.psrc(0) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(0)(i-1).asBools).foldLeft(uops(i).psrc(0)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.psrc(1) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(1)(i-1).asBools).foldLeft(uops(i).psrc(1)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.psrc(2) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(2)(i-1).asBools).foldLeft(uops(i).psrc(2)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.psrc(3) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(3)(i-1).asBools).foldLeft(uops(i).psrc(3)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.psrc(4) := io.out.take(i).map(_.bits.pdest).zip(bypassCond(4)(i-1).asBools).foldLeft(uops(i).psrc(4)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.out(i).bits.pdest := Mux(isMove(i), io.out(i).bits.psrc(0), uops(i).pdest)

    // Todo: better implementation for fields reuse
    // For fused-lui-load, load.src(0) is replaced by the imm.
    val last_is_lui = io.in(i - 1).bits.selImm === SelImm.IMM_U && io.in(i - 1).bits.srcType(0) =/= SrcType.pc
    val this_is_load = io.in(i).bits.fuType === FuType.ldu.U
    val lui_to_load = io.in(i - 1).valid && io.in(i - 1).bits.ldest === io.in(i).bits.lsrc(0)
    val fused_lui_load = last_is_lui && this_is_load && lui_to_load
    when (fused_lui_load) {
      // The first LOAD operand (base address) is replaced by LUI-imm and stored in imm
      val lui_imm = io.in(i - 1).bits.imm(ImmUnion.U.len - 1, 0)
      val ld_imm = io.in(i).bits.imm(ImmUnion.I.len - 1, 0)
      require(io.out(i).bits.imm.getWidth >= lui_imm.getWidth + ld_imm.getWidth)
      io.out(i).bits.srcType(0) := SrcType.imm
      io.out(i).bits.imm := Cat(lui_imm, ld_imm)
    }

  }

  val genSnapshot = Cat(io.out.map(out => out.fire && out.bits.snapshot)).orR
  val lastCycleCreateSnpt = RegInit(false.B)
  lastCycleCreateSnpt := genSnapshot && !io.snptIsFull
  val sameSnptDistance = (RobCommitWidth * 4).U
  // notInSameSnpt: 1.robidxHead - snapLastEnq >= sameSnptDistance 2.no snap
  val notInSameSnpt = GatedValidRegNext(distanceBetween(robIdxHeadNext, io.snptLastEnq.bits) >= sameSnptDistance || !io.snptLastEnq.valid)
  val allowSnpt = if (EnableRenameSnapshot) notInSameSnpt && !lastCycleCreateSnpt && io.in.head.bits.firstUop else false.B
  io.out.zip(io.in).foreach{ case (out, in) => out.bits.snapshot := allowSnpt && (!in.bits.preDecodeInfo.notCFI || FuType.isJump(in.bits.fuType)) && in.fire }
  io.out.map{ x =>
    x.bits.hasException := Cat(selectFrontend(x.bits.exceptionVec) :+ x.bits.exceptionVec(illegalInstr) :+ x.bits.exceptionVec(virtualInstr)).orR || x.bits.trigger.getFrontendCanFire
  }
  if(backendParams.debugEn){
    dontTouch(robIdxHeadNext)
    dontTouch(notInSameSnpt)
    dontTouch(genSnapshot)
  }
  intFreeList.io.snpt := io.snpt
  fpFreeList.io.snpt := io.snpt
  vecFreeList.io.snpt := io.snpt
  v0FreeList.io.snpt := io.snpt
  vlFreeList.io.snpt := io.snpt
  intFreeList.io.snpt.snptEnq := genSnapshot
  fpFreeList.io.snpt.snptEnq := genSnapshot
  vecFreeList.io.snpt.snptEnq := genSnapshot
  v0FreeList.io.snpt.snptEnq := genSnapshot
  vlFreeList.io.snpt.snptEnq := genSnapshot

  /**
    * Instructions commit: update freelist and rename table
    */
  for (i <- 0 until RabCommitWidth) {
    val commitValid = io.rabCommits.isCommit && io.rabCommits.commitValid(i)
    val walkValid = io.rabCommits.isWalk && io.rabCommits.walkValid(i)

    // I. RAT Update
    // When redirect happens (mis-prediction), don't update the rename table
    io.intRenamePorts(i).wen  := intSpecWen(i)
    io.intRenamePorts(i).addr := uops(i).ldest(log2Ceil(IntLogicRegs) - 1, 0)
    io.intRenamePorts(i).data := io.out(i).bits.pdest

    io.fpRenamePorts(i).wen  := fpSpecWen(i)
    io.fpRenamePorts(i).addr := uops(i).ldest(log2Ceil(FpLogicRegs) - 1, 0)
    io.fpRenamePorts(i).data := fpFreeList.io.allocatePhyReg(i)

    io.vecRenamePorts(i).wen := vecSpecWen(i)
    io.vecRenamePorts(i).addr := uops(i).ldest(log2Ceil(VecLogicRegs) - 1, 0)
    io.vecRenamePorts(i).data := vecFreeList.io.allocatePhyReg(i)

    io.v0RenamePorts(i).wen := v0SpecWen(i)
    io.v0RenamePorts(i).addr := uops(i).ldest(log2Ceil(V0LogicRegs) - 1, 0)
    io.v0RenamePorts(i).data := v0FreeList.io.allocatePhyReg(i)

    io.vlRenamePorts(i).wen := vlSpecWen(i)
    io.vlRenamePorts(i).addr := uops(i).ldest(log2Ceil(VlLogicRegs) - 1, 0)
    io.vlRenamePorts(i).data := vlFreeList.io.allocatePhyReg(i)

    // II. Free List Update
    intFreeList.io.freeReq(i) := io.int_need_free(i)
    intFreeList.io.freePhyReg(i) := RegNext(io.int_old_pdest(i))
    fpFreeList.io.freeReq(i)  := GatedValidRegNext(commitValid && needDestRegCommit(Reg_F, io.rabCommits.info(i)))
    fpFreeList.io.freePhyReg(i) := io.fp_old_pdest(i)
    vecFreeList.io.freeReq(i)  := GatedValidRegNext(commitValid && needDestRegCommit(Reg_V, io.rabCommits.info(i)))
    vecFreeList.io.freePhyReg(i) := io.vec_old_pdest(i)
    v0FreeList.io.freeReq(i) := GatedValidRegNext(commitValid && needDestRegCommit(Reg_V0, io.rabCommits.info(i)))
    v0FreeList.io.freePhyReg(i) := io.v0_old_pdest(i)
    vlFreeList.io.freeReq(i) := GatedValidRegNext(commitValid && needDestRegCommit(Reg_Vl, io.rabCommits.info(i)))
    vlFreeList.io.freePhyReg(i) := io.vl_old_pdest(i)
  }

  /*
  Debug and performance counters
   */
  def printRenameInfo(in: DecoupledIO[DecodedInst], out: DecoupledIO[DynInst]) = {
    XSInfo(out.fire, p"pc:${Hexadecimal(in.bits.pc)} in(${in.valid},${in.ready}) " +
      p"lsrc(0):${in.bits.lsrc(0)} -> psrc(0):${out.bits.psrc(0)} " +
      p"lsrc(1):${in.bits.lsrc(1)} -> psrc(1):${out.bits.psrc(1)} " +
      p"lsrc(2):${in.bits.lsrc(2)} -> psrc(2):${out.bits.psrc(2)} " +
      p"ldest:${in.bits.ldest} -> pdest:${out.bits.pdest}\n"
    )
  }

  for ((x,y) <- io.in.zip(io.out)) {
    printRenameInfo(x, y)
  }

  io.out.map { case x =>
    when(x.valid && x.bits.rfWen){
      assert(x.bits.ldest =/= 0.U, "rfWen cannot be 1 when Int regfile ldest is 0")
    }
  }
  val debugRedirect = RegEnable(io.redirect.bits, io.redirect.valid)
  // bad speculation
  val recStall = io.redirect.valid || io.rabCommits.isWalk
  val ctrlRecStall = Mux(io.redirect.valid, io.redirect.bits.debugIsCtrl, io.rabCommits.isWalk && debugRedirect.debugIsCtrl)
  val mvioRecStall = Mux(io.redirect.valid, io.redirect.bits.debugIsMemVio, io.rabCommits.isWalk && debugRedirect.debugIsMemVio)
  val otherRecStall = recStall && !(ctrlRecStall || mvioRecStall)
  XSPerfAccumulate("recovery_stall", recStall)
  XSPerfAccumulate("control_recovery_stall", ctrlRecStall)
  XSPerfAccumulate("mem_violation_recovery_stall", mvioRecStall)
  XSPerfAccumulate("other_recovery_stall", otherRecStall)
  // freelist stall
  val notRecStall = !io.out.head.valid && !recStall
  val intFlStall = notRecStall && inHeadValid && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !intFreeList.io.canAllocate
  val fpFlStall = notRecStall && inHeadValid && intFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !fpFreeList.io.canAllocate
  val vecFlStall = notRecStall && inHeadValid && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !vecFreeList.io.canAllocate
  val v0FlStall = notRecStall && inHeadValid && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && vlFreeList.io.canAllocate && !v0FreeList.io.canAllocate
  val vlFlStall = notRecStall && inHeadValid && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && !vlFreeList.io.canAllocate
  val multiFlStall = notRecStall && inHeadValid && (PopCount(Cat(
    !intFreeList.io.canAllocate,
    !fpFreeList.io.canAllocate,
    !vecFreeList.io.canAllocate,
    !v0FreeList.io.canAllocate,
    !vlFreeList.io.canAllocate,
  )) > 1.U)
  // other stall
  val otherStall = notRecStall && !intFlStall && !fpFlStall && !vecFlStall && !v0FlStall && !vlFlStall && !multiFlStall

  io.stallReason.in.backReason.valid := io.stallReason.out.backReason.valid || !io.in.head.ready
  io.stallReason.in.backReason.bits := Mux(io.stallReason.out.backReason.valid, io.stallReason.out.backReason.bits,
    MuxCase(TopDownCounters.OtherCoreStall.id.U, Seq(
      ctrlRecStall  -> TopDownCounters.ControlRecoveryStall.id.U,
      mvioRecStall  -> TopDownCounters.MemVioRecoveryStall.id.U,
      otherRecStall -> TopDownCounters.OtherRecoveryStall.id.U,
      intFlStall    -> TopDownCounters.IntFlStall.id.U,
      fpFlStall     -> TopDownCounters.FpFlStall.id.U,
      vecFlStall    -> TopDownCounters.VecFlStall.id.U,
      v0FlStall     -> TopDownCounters.V0FlStall.id.U,
      vlFlStall     -> TopDownCounters.VlFlStall.id.U,
      multiFlStall  -> TopDownCounters.MultiFlStall.id.U,
    )
  ))
  io.stallReason.out.reason.zip(io.stallReason.in.reason).zip(io.in.map(_.valid)).foreach { case ((out, in), valid) =>
    out := Mux(io.stallReason.in.backReason.valid, io.stallReason.in.backReason.bits, in)
  }

  XSDebug(io.rabCommits.isWalk, p"Walk Recovery Enabled\n")
  XSDebug(io.rabCommits.isWalk, p"validVec:${Binary(io.rabCommits.walkValid.asUInt)}\n")
  for (i <- 0 until RabCommitWidth) {
    val info = io.rabCommits.info(i)
    XSDebug(io.rabCommits.isWalk && io.rabCommits.walkValid(i), p"[#$i walk info] " +
      p"ldest:${info.ldest} rfWen:${info.rfWen} fpWen:${info.fpWen} vecWen:${info.vecWen} v0Wen:${info.v0Wen} vlWen:${info.vlWen}")
  }

  XSDebug(p"inValidVec: ${Binary(Cat(io.in.map(_.valid)))}\n")

  XSPerfAccumulate("in_valid_count", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire_count", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("in_valid_not_ready_count", PopCount(io.in.map(x => x.valid && !x.ready)))
  XSPerfAccumulate("wait_cycle", !io.in.head.valid && dispatchCanAcc)

  // These stall reasons could overlap each other, but we configure the priority as fellows.
  // walk stall > dispatch stall > int freelist stall > fp freelist stall
  private val inHeadStall = io.in.head match { case x => x.valid && !x.ready }
  private val stallForWalk      = inHeadValid &&  io.rabCommits.isWalk
  private val stallForDispatch  = inHeadValid && !io.rabCommits.isWalk && !dispatchCanAcc
  private val stallForIntFL     = inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !intFreeList.io.canAllocate
  private val stallForFpFL      = inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !fpFreeList.io.canAllocate
  private val stallForVecFL     = inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !vecFreeList.io.canAllocate
  private val stallForV0FL      = inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && vlFreeList.io.canAllocate && !v0FreeList.io.canAllocate
  private val stallForVlFL      = inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && !vlFreeList.io.canAllocate
  XSPerfAccumulate("stall_cycle",          inHeadStall)
  XSPerfAccumulate("stall_cycle_walk",     stallForWalk)
  XSPerfAccumulate("stall_cycle_dispatch", stallForDispatch)
  XSPerfAccumulate("stall_cycle_int",      stallForIntFL)
  XSPerfAccumulate("stall_cycle_fp",       stallForFpFL)
  XSPerfAccumulate("stall_cycle_vec",      stallForVecFL)
  XSPerfAccumulate("stall_cycle_vec",      stallForV0FL)
  XSPerfAccumulate("stall_cycle_vec",      stallForVlFL)

  XSPerfHistogram("in_valid_range",  PopCount(io.in.map(_.valid)),  true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("in_fire_range",   PopCount(io.in.map(_.fire)),   true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("out_valid_range", PopCount(io.out.map(_.valid)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("out_fire_range",  PopCount(io.out.map(_.fire)),  true.B, 0, DecodeWidth + 1, 1)

  XSPerfAccumulate("move_instr_count", PopCount(io.out.map(out => out.fire && out.bits.isMove)))
  val is_fused_lui_load = io.out.map(o => o.fire && o.bits.fuType === FuType.ldu.U && o.bits.srcType(0) === SrcType.imm)
  XSPerfAccumulate("fused_lui_load_instr_count", PopCount(is_fused_lui_load))

  val renamePerf = Seq(
    ("rename_in                  ", PopCount(io.in.map(_.valid & io.in(0).ready ))                                                               ),
    ("rename_waitinstr           ", PopCount((0 until RenameWidth).map(i => io.in(i).valid && !io.in(i).ready))                                  ),
    ("rename_stall               ", inHeadStall),
    ("rename_stall_cycle_walk    ", inHeadValid &&  io.rabCommits.isWalk),
    ("rename_stall_cycle_dispatch", inHeadValid && !io.rabCommits.isWalk && !dispatchCanAcc),
    ("rename_stall_cycle_int     ", inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !intFreeList.io.canAllocate),
    ("rename_stall_cycle_fp      ", inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !fpFreeList.io.canAllocate),
    ("rename_stall_cycle_vec     ", inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && v0FreeList.io.canAllocate && vlFreeList.io.canAllocate && !vecFreeList.io.canAllocate),
    ("rename_stall_cycle_v0      ", inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && vlFreeList.io.canAllocate && !v0FreeList.io.canAllocate),
    ("rename_stall_cycle_vl      ", inHeadValid && !io.rabCommits.isWalk && dispatchCanAcc && intFreeList.io.canAllocate && fpFreeList.io.canAllocate && vecFreeList.io.canAllocate && v0FreeList.io.canAllocate && !vlFreeList.io.canAllocate),
  )
  val intFlPerf = intFreeList.getPerfEvents
  val fpFlPerf = fpFreeList.getPerfEvents
  val vecFlPerf = vecFreeList.getPerfEvents
  val v0FlPerf = v0FreeList.getPerfEvents
  val vlFlPerf = vlFreeList.getPerfEvents
  val perfEvents = renamePerf ++ intFlPerf ++ fpFlPerf ++ vecFlPerf ++ v0FlPerf ++ vlFlPerf
  generatePerfEvent()
}
