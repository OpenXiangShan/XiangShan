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
import xiangshan.backend.fu.vector.Bundles.{VType, Vl}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.wrapper.CSRToDecode
import yunsuan.VpermType
import xiangshan.ExceptionNO.{illegalInstr, virtualInstr}
import xiangshan.frontend.FtqPtr

class DecodeStageIO(implicit p: Parameters) extends XSBundle {
  // params alias
  private val numIntRegSrc = backendParams.numIntRegSrc
  private val numIntRatPorts = numIntRegSrc
  private val numFpRegSrc = backendParams.numFpRegSrc
  private val numFpRatPorts = numFpRegSrc
  private val numVecRegSrc = backendParams.numVecRegSrc
  private val numVecRatPorts = numVecRegSrc

  val redirect = Input(Bool())
  val canAccept = Output(Bool())
  // from Ibuffer
  val in = Vec(DecodeWidth, Flipped(DecoupledIO(new StaticInst)))
  // to Rename
  val out = Vec(DecodeWidth, DecoupledIO(new DecodedInst))
  // RAT read
  val intRat = Vec(RenameWidth, Vec(numIntRatPorts, Flipped(new RatReadPort(IntLogicRegs))))
  val fpRat = Vec(RenameWidth, Vec(numFpRatPorts, Flipped(new RatReadPort(FpLogicRegs))))
  val vecRat = Vec(RenameWidth, Vec(numVecRatPorts, Flipped(new RatReadPort(VecLogicRegs))))
  val v0Rat = Vec(RenameWidth, Flipped(new RatReadPort(V0LogicRegs)))
  val vlRat = Vec(RenameWidth, Flipped(new RatReadPort(VlLogicRegs)))
  // csr control
  val csrCtrl = Input(new CustomCSRCtrlIO)
  val fromCSR = Input(new CSRToDecode)
  val fusion = Vec(DecodeWidth - 1, Input(Bool()))

  // vtype update
  val fromRob = new Bundle {
    val isResumeVType = Input(Bool())
    val walkToArchVType = Input(Bool())
    val commitVType = new Bundle {
      val vtype = Flipped(Valid(new VType))
      val hasVsetvl = Input(Bool())
    }
    val walkVType = Flipped(Valid(new VType))
  }
  val stallReason = new Bundle {
    val in = Flipped(new StallReasonIO(DecodeWidth))
    val out = new StallReasonIO(DecodeWidth)
  }
  val vsetvlVType = Input(VType())
  val vstart = Input(Vl())

  val toCSR = new Bundle {
    val trapInstInfo = ValidIO(new TrapInstInfo)
  }
}

class DecodeStage(implicit p: Parameters) extends XSModule
  with HasPerfEvents
  with VectorConstants {

  val io = IO(new DecodeStageIO)

  // io alias
  private val outReadys = io.out.map(_.ready)
  private val inValids = io.in.map(_.valid)
  private val inValid = VecInit(inValids).asUInt.orR
  private val outValids = io.out.map(_.valid)
  private val outValid = VecInit(outValids).asUInt.orR
  //readyFromRename Counter
  /** Assume number of ready channels be "RenameWidth" if the first output channel is ready. If not, assume that be 0 */
  val readyCounter = Mux(outReadys.head, RenameWidth.U, 0.U)

  /** complex decoder */
  val decoderComp = Module(new DecodeUnitComp)
  /** simple decoders in Seq of DecodeWidth */
  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))
  /** vtype generation module */
  val vtypeGen = Module(new VTypeGen)

  val debug_globalCounter = RegInit(0.U(XLEN.W))

  /** whether DecodeStage can accept new requests from frontend (CtrlBlock) */
  val canAccept = Wire(Bool())

  //Simple 6
  decoders.zip(io.in).foreach { case (dst, src) =>
    dst.io.enq.ctrlFlow := src.bits
    dst.io.csrCtrl := io.csrCtrl
    dst.io.fromCSR := io.fromCSR
    dst.io.enq.vtype := vtypeGen.io.vtype
    dst.io.enq.vstart := io.vstart
  }

  /** whether instructions decoded by simple decoders require complex decoding */
  val isComplexVec = VecInit(inValids.zip(decoders.map(_.io.deq.isComplex)).map { case (valid, isComplex) => valid && isComplex })
  /** whether instructions decoded by simple decoders don't require complex decoding */
  val isSimpleVec = VecInit(inValids.zip(decoders.map(_.io.deq.isComplex)).map { case (valid, isComplex) => valid && !isComplex })
  /** instructions decoded by simple decoders */
  val simpleDecodedInst = VecInit(decoders.map(_.io.deq.decodedInst))

  /** whether instructions decoded by simple decoders are illegal */
  val isIllegalInstVec = VecInit((outValids lazyZip outReadys lazyZip io.out.map(_.bits)).map {
    case (valid, ready, decodedInst) =>
      valid && ready && (decodedInst.exceptionVec(ExceptionNO.EX_II) || decodedInst.exceptionVec(ExceptionNO.EX_VI))
  })
  /** at least 1 instruction decoded by simple decoders is illegal */
  val hasIllegalInst = Cat(isIllegalInstVec).orR
  /** at least 1 instruction decoded by simple decoders is illegal */
  val illegalInst = PriorityMuxDefault(isIllegalInstVec.zip(io.out.map(_.bits)), 0.U.asTypeOf(new DecodedInst))

  /** number of instructions generated by complex decoder */
  val complexNum = Wire(UInt(3.W))
  // (0, 1, 2, 3, 4, 5) + complexNum
  /** Order of simple decoders' result (in output of DecodeStage) considering complex decoder's. Since complex decoder's
   * results will be arranged before simple decoders' */
  val complexNumAddLocation: Vec[UInt] = VecInit((0 until DecodeWidth).map(x => (x.U +& complexNum)))
  /** mask off decoded instructions that can not be accepted */
  val noMoreThanRenameReady: Vec[Bool] = VecInit(complexNumAddLocation.map(x => x <= readyCounter))
  /** existance of complex instructions among first few simple decoders' results, which needs decoding */
  val complexValid = VecInit((isComplexVec zip noMoreThanRenameReady).map(x => x._1 & x._2)).asUInt.orR
  /** selected complex instruction for complex decoder */
  val complexInst = PriorityMuxDefault(isComplexVec.zip(decoders.map(_.io.deq.decodedInst)), 0.U.asTypeOf(new DecodedInst))
  /** selected complex micro operation information for complex decoder */
  val complexUopInfo = PriorityMuxDefault(isComplexVec.zip(decoders.map(_.io.deq.uopInfo)), 0.U.asTypeOf(new UopInfo))

  vtypeGen.io.insts.zip(io.in).foreach { case (inst, in) =>
    inst.valid := in.valid
    inst.bits := in.bits.instr
  }
  // when io.redirect is True, never update vtype
  vtypeGen.io.canUpdateVType := decoderComp.io.in.fire && decoderComp.io.in.bits.simpleDecodedInst.isVset && !io.redirect
  vtypeGen.io.walkToArchVType := io.fromRob.walkToArchVType
  vtypeGen.io.commitVType := io.fromRob.commitVType
  vtypeGen.io.walkVType := io.fromRob.walkVType
  vtypeGen.io.vsetvlVType := io.vsetvlVType

  //Comp 1
  decoderComp.io.redirect := io.redirect
  decoderComp.io.csrCtrl := io.csrCtrl
  decoderComp.io.vtypeBypass := vtypeGen.io.vtype
  // The input inst of decoderComp is latched last cycle.
  // Set input empty, if there is no complex inst latched last cycle.
  decoderComp.io.in.valid := complexValid && !io.fromRob.isResumeVType
  decoderComp.io.in.bits.simpleDecodedInst := complexInst
  decoderComp.io.in.bits.uopInfo := complexUopInfo
  decoderComp.io.out.complexDecodedInsts.zipWithIndex.foreach { case (out, i) => out.ready := io.out(i).ready }

  /** instructions decoded by complex decoders */
  val complexDecodedInst = VecInit(decoderComp.io.out.complexDecodedInsts.map(_.bits))
  /** whether instructions decoded by complex decoders are valid */
  val complexDecodedInstValid = VecInit(decoderComp.io.out.complexDecodedInsts.map(_.valid))
  complexNum := decoderComp.io.complexNum

  // Vec(S,S,S,C,S,C) -> Vec(1,1,1,0,0,0)
  /** whether a complex instruction is before or at this position of simple decoders' result */
  val simplePrefixVec = VecInit((0 until DecodeWidth).map(i => VecInit(isSimpleVec.take(i + 1)).asUInt.andR))

  // Vec(S,S,S,C,S,S) -> Vec(0,0,0,1,0,0)
  /** one-hot representation of the first complex instruction */
  val firstComplexOH: Vec[Bool] = VecInit(PriorityEncoderOH(isComplexVec))

  // block vector inst when vtype is resuming
  val hasVectorInst = VecInit(decoders.map(x => FuType.FuTypeOrR(x.io.deq.decodedInst.fuType, FuType.vecArithOrMem ++ FuType.vecVSET))).asUInt.orR

  /** condition of acceptation: no redirection, ready from rename/complex decoder, no resumeVType */
  canAccept := !io.redirect && (io.out.head.ready || decoderComp.io.in.ready) && !io.fromRob.isResumeVType

  io.canAccept := canAccept

  /**
   * Assign ready signal for DecodeStage's input. Ready signal in i-th channel:
   *
   * It must hold that no redirection and no isResumeVType is caught.
   * One situation for set up ready signal is that first "i" instructions are all simple instructions, and these "i"
   * instructions can be passed down to rename together with complex decoder's result.
   * Another situation is that first "i-1" instructions are all simple instructions, and the "i-th" instructions needs
   * to be sent to complex decoder, with complex decoder ready for new input.
   */
  io.in.zipWithIndex.foreach { case (in, i) =>
    in.ready := !io.redirect && (
      simplePrefixVec(i) && (i.U +& complexNum) < readyCounter ||
      firstComplexOH(i) && (i.U +& complexNum) <= readyCounter && decoderComp.io.in.ready
    ) && !io.fromRob.isResumeVType
  }

  /** final instruction decoding result */
  val finalDecodedInst = Wire(Vec(DecodeWidth, new DecodedInst))
  /** valid signs of final instruction decoding result */
  val finalDecodedInstValid = Wire(Vec(DecodeWidth, Bool()))

  /**
   * Select final result of DecodeStage. Select all complex decoded insts results at the beginning of final result, and
   * use simple decoded insts to fill the rest space in DecodeWidth.
   */
  for (i <- 0 until DecodeWidth) {
    finalDecodedInst(i) := Mux(complexNum > i.U, complexDecodedInst(i), simpleDecodedInst(i.U - complexNum))
    finalDecodedInstValid(i) := Mux(complexNum > i.U, complexDecodedInstValid(i), simplePrefixVec(i.U - complexNum))
  }

  /**
   * Generate output of DecodeStage. Pass finalDecodedInst to output as decoded instructions.
   * Note that finalDecodedInst is generated in order.
   */
  io.out.zipWithIndex.foreach { case (inst, i) =>
    inst.valid := finalDecodedInstValid(i) && !io.fromRob.isResumeVType
    inst.bits := finalDecodedInst(i)
    inst.bits.lsrc(0) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).lsrc(1), finalDecodedInst(i).lsrc(0))
    inst.bits.lsrc(1) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).lsrc(0), finalDecodedInst(i).lsrc(1))
    inst.bits.srcType(0) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).srcType(1), finalDecodedInst(i).srcType(0))
    inst.bits.srcType(1) := Mux(finalDecodedInst(i).vpu.isReverse, finalDecodedInst(i).srcType(0), finalDecodedInst(i).srcType(1))
    inst.bits.v0Wen := finalDecodedInst(i).vecWen && finalDecodedInst(i).ldest === 0.U || finalDecodedInst(i).v0Wen
    inst.bits.vecWen := finalDecodedInst(i).vecWen && finalDecodedInst(i).ldest =/= 0.U
    // when src0/src1/src2 read V0, src3 read V0
    val srcType0123HasV0 = finalDecodedInst(i).srcType.zip(finalDecodedInst(i).lsrc).take(4).map { case (s, l) =>
      SrcType.isVp(s) && (l === 0.U)
    }.reduce(_ || _)
    inst.bits.srcType(3) := Mux(srcType0123HasV0, SrcType.v0, finalDecodedInst(i).srcType(3))
  }

  io.out.map(x =>
    when(x.valid){
      assert(PopCount(VecInit(x.bits.rfWen, x.bits.fpWen, x.bits.vecWen, x.bits.v0Wen, x.bits.vlWen)) < 2.U,
        "DecodeOut: can't wirte two regfile in one uop/instruction")
    }
  )

  /**
   * Prepare address and hold for output to Rat (Rename Alias Table)
   */
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

    io.v0Rat(i).addr := V0_IDX.U // v0
    io.v0Rat(i).hold := !io.out(i).ready

    io.vlRat(i).addr := Vl_IDX.U // vl
    io.vlRat(i).hold := !io.out(i).ready
  }

  /** whether valid input requests from frontend exists */
  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR

  debug_globalCounter := debug_globalCounter + PopCount(io.out.map(_.fire))

  io.stallReason.in.backReason := io.stallReason.out.backReason
  io.stallReason.out.reason.zip(io.stallReason.in.reason).zip(io.in.map(_.valid)).foreach { case ((out, in), valid) =>
    out := Mux(io.stallReason.out.backReason.valid,
               io.stallReason.out.backReason.bits,
               in)
  }

  io.toCSR.trapInstInfo.valid := hasIllegalInst && !io.redirect
  io.toCSR.trapInstInfo.bits.fromDecodedInst(illegalInst)

  val recoveryFlag = RegInit(false.B)
  when(io.redirect) {
    recoveryFlag := true.B
  }.elsewhen(io.in.map(_.fire).reduce(_ || _)) {
    recoveryFlag := false.B
  }

  XSPerfAccumulate("in_valid_count", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("in_fire_count", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("in_valid_not_ready_count", PopCount(io.in.map(x => x.valid && !x.ready)))
  XSPerfAccumulate("stall_cycle", io.in.head match { case x => x.valid && !x.ready})
  XSPerfAccumulate("wait_cycle", !io.in.head.valid && io.out.head.ready)
  XSPerfAccumulate("inst_spec", PopCount(io.in.map(_.fire)))
  XSPerfAccumulate("recovery_bubble", recoveryFlag)

  XSPerfHistogram("in_valid_range", PopCount(io.in.map(_.valid)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("in_fire_range", PopCount(io.in.map(_.fire)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("out_valid_range", PopCount(io.out.map(_.valid)), true.B, 0, DecodeWidth + 1, 1)
  XSPerfHistogram("out_fire_range", PopCount(io.out.map(_.fire)), true.B, 0, DecodeWidth + 1, 1)

  val fusionValid = VecInit(io.fusion.map(x => GatedValidRegNext(x)))
  val inValidNotReady = io.in.map(in => GatedValidRegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)       ),
    ("decoder_waitInstr",   PopCount(inValidNotReady)   ),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
    ("INST_SPEC",           PopCount(io.in.map(_.fire))),
    ("RECOVERY_BUBBLE",     recoveryFlag)
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
