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

package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.backend.rename.RatReadPort
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.fu.FuType
import yunsuan.VpermType

class DecodeStage(implicit p: Parameters) extends XSModule
  with HasPerfEvents
  with VectorConstants {

  // params alias
  private val numVecRegSrc = backendParams.numVecRegSrc
  private val numVecRatPorts = numVecRegSrc
  private val v0Idx = 0
  private val vlIdx = 0
  private val vconfigIdx = VCONFIG_IDX

  val io = IO(new Bundle() {
    val redirect = Input(Bool())
    val canAccept = Output(Bool())
    val vtypeRedirect = Input(Bool())
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new StaticInst)))
    // to Rename
    val out = Vec(DecodeWidth, DecoupledIO(new DecodedInst))
    // RAT read
    val intRat = Vec(RenameWidth, Vec(2, Flipped(new RatReadPort))) // Todo: make it configurable
    val fpRat = Vec(RenameWidth, Vec(3, Flipped(new RatReadPort)))
    val vecRat = Vec(RenameWidth, Vec(numVecRatPorts, Flipped(new RatReadPort)))
    val v0Rat = Vec(RenameWidth, Flipped(new RatReadPort))
    val vlRat = Vec(RenameWidth, Flipped(new RatReadPort))
    // csr control
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))
    // vtype update
    val isResumeVType = Input(Bool())
    val commitVType = new Bundle {
      val vtype = Flipped(Valid(new VType))
      val hasVsetvl = Input(Bool())
    }
    val walkVType = Flipped(Valid(new VType))
    val stallReason = new Bundle {
      val in = Flipped(new StallReasonIO(DecodeWidth))
      val out = new StallReasonIO(DecodeWidth)
    }
    val vsetvlVType = Input(VType())
    val lastSpecVType = (Valid(new VType))
    val specVtype = Output(new VType)
  })

  // io alias
  private val outReadys = io.out.map(_.ready)
  private val inValids = io.in.map(_.valid)
  private val inValid = VecInit(inValids).asUInt.orR
  private val outValids = io.out.map(_.valid)
  private val outValid = VecInit(outValids).asUInt.orR
  //readyFromRename Counter
  val readyCounter = PriorityMuxDefault(outReadys.map(x => !x).zip((0 until RenameWidth).map(_.U)), RenameWidth.U)

  val decoderComp = Module(new DecodeUnitComp)
  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))
  val vtypeGen = Module(new VTypeGen)

  val debug_globalCounter = RegInit(0.U(XLEN.W))

  val canAccept = Wire(Bool())

  //Simple 6
  decoders.zip(io.in).foreach { case (dst, src) => dst.io.enq.ctrlFlow := src.bits }
  decoders.foreach { case dst => dst.io.csrCtrl := io.csrCtrl }
  decoders.foreach { case dst => dst.io.enq.vtype := vtypeGen.io.vtype }
  val isComplexVec = VecInit(inValids.zip(decoders.map(_.io.deq.isComplex)).map { case (valid, isComplex) => valid && isComplex })
  val isSimpleVec = VecInit(inValids.zip(decoders.map(_.io.deq.isComplex)).map { case (valid, isComplex) => valid && !isComplex })
  val simpleDecodedInst = VecInit(decoders.map(_.io.deq.decodedInst))

  val complexNum = Wire(UInt(3.W))
  // (0, 1, 2, 3, 4, 5) + complexNum
  val complexNumAddLocation: Vec[UInt] = VecInit((0 until DecodeWidth).map(x => (x.U +& complexNum)))
  val noMoreThanRenameReady: Vec[Bool] = VecInit(complexNumAddLocation.map(x => x <= readyCounter))
  val complexValid = VecInit((isComplexVec zip noMoreThanRenameReady).map(x => x._1 & x._2)).asUInt.orR
  val complexInst = PriorityMuxDefault(isComplexVec.zip(decoders.map(_.io.deq.decodedInst)), 0.U.asTypeOf(new DecodedInst))
  val complexUopInfo = PriorityMuxDefault(isComplexVec.zip(decoders.map(_.io.deq.uopInfo)), 0.U.asTypeOf(new UopInfo))

  vtypeGen.io.insts.zipWithIndex.foreach { case (inst, i) =>
    inst.valid := io.in(i).valid
    inst.bits := io.in(i).bits.instr
  }
  vtypeGen.io.canUpdateVType := decoderComp.io.in.fire && decoderComp.io.in.bits.simpleDecodedInst.isVset
  vtypeGen.io.redirect := io.vtypeRedirect
  vtypeGen.io.commitVType := io.commitVType
  vtypeGen.io.walkVType := io.walkVType
  vtypeGen.io.vsetvlVType := io.vsetvlVType
  io.specVtype := vtypeGen.io.specVtype
  io.lastSpecVType := vtypeGen.io.lastSpecVType

  //Comp 1
  decoderComp.io.redirect := io.redirect
  decoderComp.io.csrCtrl := io.csrCtrl
  decoderComp.io.vtypeBypass := vtypeGen.io.vtype
  // The input inst of decoderComp is latched last cycle.
  // Set input empty, if there is no complex inst latched last cycle.
  decoderComp.io.in.valid := complexValid && !io.isResumeVType
  decoderComp.io.in.bits.simpleDecodedInst := complexInst
  decoderComp.io.in.bits.uopInfo := complexUopInfo
  decoderComp.io.out.complexDecodedInsts.zipWithIndex.foreach { case (out, i) => out.ready := io.out(i).ready }

  val complexDecodedInst = VecInit(decoderComp.io.out.complexDecodedInsts.map(_.bits))
  val complexDecodedInstValid = VecInit(decoderComp.io.out.complexDecodedInsts.map(_.valid))
  complexNum := decoderComp.io.complexNum

  // Vec(S,S,S,C,S,S) -> Vec(0,0,0,0,1,1)
  val simplePrefixVec = VecInit((0 until DecodeWidth).map(i => VecInit(isSimpleVec.take(i + 1)).asUInt.andR))
  // Vec(S,S,S,C,S,S) -> Vec(0,0,0,1,0,0)
  val firstComplexOH: Vec[Bool] = VecInit(PriorityEncoderOH(isComplexVec))

  // block vector inst when vtype is resuming
  val hasVectorInst = VecInit(decoders.map(x => FuType.FuTypeOrR(x.io.deq.decodedInst.fuType, FuType.vecArithOrMem ++ FuType.vecVSET))).asUInt.orR

  canAccept := !io.redirect && (io.out.head.ready || decoderComp.io.in.ready) && !io.isResumeVType

  io.canAccept := canAccept

  io.in.zipWithIndex.foreach { case (in, i) =>
    in.ready := !io.redirect && (
      simplePrefixVec(i) && (i.U +& complexNum) < readyCounter ||
      firstComplexOH(i) && (i.U +& complexNum) <= readyCounter && decoderComp.io.in.ready
    ) && !(hasVectorInst && io.isResumeVType)
  }

  val finalDecodedInst = Wire(Vec(DecodeWidth, new DecodedInst))
  val finalDecodedInstValid = Wire(Vec(DecodeWidth, Bool()))

  for (i <- 0 until DecodeWidth) {
    finalDecodedInst(i) := Mux(complexNum > i.U, complexDecodedInst(i), simpleDecodedInst(i.U - complexNum))
    finalDecodedInstValid(i) := Mux(complexNum > i.U, complexDecodedInstValid(i), simplePrefixVec(i.U - complexNum))
  }

  io.out.zipWithIndex.foreach { case (inst, i) =>
    inst.valid := finalDecodedInstValid(i) && !(hasVectorInst && io.isResumeVType)
    inst.bits := finalDecodedInst(i)
    inst.bits.lsrc(0) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).lsrc(1), finalDecodedInst(i).lsrc(0))
    inst.bits.lsrc(1) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).lsrc(0), finalDecodedInst(i).lsrc(1))
    inst.bits.srcType(0) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).srcType(1), finalDecodedInst(i).srcType(0))
    inst.bits.srcType(1) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).srcType(0), finalDecodedInst(i).srcType(1))
    inst.bits.v0Wen := finalDecodedInst(i).vecWen && finalDecodedInst(i).ldest === 0.U || finalDecodedInst(i).v0Wen
    // when src0/src1/src2 read V0, src3 read V0
    val srcType0123HasV0 = finalDecodedInst(i).srcType.zip(finalDecodedInst(i).lsrc).take(4).map { case (s, l) =>
      SrcType.isVp(s) && (l === 0.U)
    }.reduce(_ || _)
    inst.bits.srcType(3) := Mux(srcType0123HasV0, SrcType.v0, finalDecodedInst(i).srcType(3))
  }

  for (i <- 0 until DecodeWidth) {

    // We use the lsrc/ldest before fusion decoder to read RAT for better timing.
    io.intRat(i)(0).addr := io.out(i).bits.lsrc(0)
    io.intRat(i)(1).addr := io.out(i).bits.lsrc(1)
    io.intRat(i).foreach(_.hold := !io.out(i).ready)

    // Floating-point instructions can not be fused now.
    io.fpRat(i)(0).addr := io.out(i).bits.lsrc(0)
    io.fpRat(i)(1).addr := io.out(i).bits.lsrc(1)
    io.fpRat(i)(2).addr := io.out(i).bits.lsrc(2)
    io.fpRat(i).foreach(_.hold := !io.out(i).ready)

    // Vec instructions
    // TODO: vec uop dividers need change this
    io.vecRat(i)(0).addr := io.out(i).bits.lsrc(0) // vs1
    io.vecRat(i)(1).addr := io.out(i).bits.lsrc(1) // vs2
    io.vecRat(i)(2).addr := io.out(i).bits.lsrc(2) // old_vd
    io.vecRat(i).foreach(_.hold := !io.out(i).ready)

    io.v0Rat(i).addr := Mux(FuType.isVppu(io.out(i).bits.fuType) && (io.out(i).bits.fuOpType === VpermType.vcompress), io.out(i).bits.lsrc(3), v0Idx.U) // v0
    io.v0Rat(i).hold := !io.out(i).ready

    io.vlRat(i).addr := vlIdx.U // vl
    io.vlRat(i).hold := !io.out(i).ready
  }

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR

  debug_globalCounter := debug_globalCounter + PopCount(io.out.map(_.fire))

  io.stallReason.in.backReason := io.stallReason.out.backReason
  io.stallReason.out.reason.zip(io.stallReason.in.reason).zip(io.in.map(_.valid)).foreach { case ((out, in), valid) =>
    out := Mux(io.stallReason.out.backReason.valid,
               io.stallReason.out.backReason.bits,
               in)
  }

  XSPerfAccumulate("in_valid_count", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire_count", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("in_valid_not_ready_count", PopCount(io.in.map(x => x.valid && !x.ready)))
  XSPerfAccumulate("stall_cycle", io.in.head match { case x => x.valid && !x.ready})
  XSPerfAccumulate("wait_cycle", !io.in.head.valid && io.out.head.ready)

  XSPerfHistogram("in_valid_range", PopCount(io.in.map(_.valid)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("in_fire_range", PopCount(io.in.map(_.fire)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("out_valid_range", PopCount(io.out.map(_.valid)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("out_fire_range", PopCount(io.out.map(_.fire)), true.B, 0, DecodeWidth + 1, 1)

  val fusionValid = VecInit(io.fusion.map(x => GatedValidRegNext(x)))
  val inValidNotReady = io.in.map(in => GatedValidRegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)       ),
    ("decoder_waitInstr",   PopCount(inValidNotReady)            ),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
  )
  generatePerfEvent()

  // for more readable verilog
  dontTouch(isSimpleVec)
  dontTouch(isComplexVec)
  dontTouch(simplePrefixVec)
  dontTouch(complexValid)
  dontTouch(complexNum)
  dontTouch(readyCounter)
  dontTouch(firstComplexOH)
}
