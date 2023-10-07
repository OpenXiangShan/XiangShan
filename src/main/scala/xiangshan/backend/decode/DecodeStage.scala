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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.backend.rename.RatReadPort
import xiangshan.backend.Bundles._
import xiangshan.backend.fu.vector.Bundles.VType

class DecodeStage(implicit p: Parameters) extends XSModule
  with HasPerfEvents
  with VectorConstants {

  // params alias
  private val numVecRegSrc = backendParams.numVecRegSrc
  private val numVecRatPorts = numVecRegSrc + 1 // +1 dst

  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new StaticInst)))
    // to Rename
    val out = Vec(DecodeWidth, DecoupledIO(new DecodedInst))
    // RAT read
    val intRat = Vec(RenameWidth, Vec(3, Flipped(new RatReadPort))) // Todo: make it configurable
    val fpRat = Vec(RenameWidth, Vec(4, Flipped(new RatReadPort)))
    val vecRat = Vec(RenameWidth, Vec(numVecRatPorts, Flipped(new RatReadPort)))
    // csr control
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))
    // vtype update
    val isRedirect = Input(Bool())
    val commitVType = Flipped(Valid(new VType))
    val walkVType = Flipped(Valid(new VType))
    val stallReason = new Bundle {
      val in = Flipped(new StallReasonIO(DecodeWidth))
      val out = new StallReasonIO(DecodeWidth)
    }
  })

  private val v0Idx = 0
  private val vconfigIdx = VCONFIG_IDX

  val decoderComp = Module(new DecodeUnitComp)
  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))
  val vtypeGen = Module(new VTypeGen)
  val debug_globalCounter = RegInit(0.U(XLEN.W))

  val isComplex = Wire(Vec(DecodeWidth, Bool()))
  val uopComplex = Wire(Vec(DecodeWidth, new DecodedInst))
  val isFirstVset = Wire(Bool())
  val complexNum = Wire(UInt(3.W))

  val uopSimple = Wire(Vec(DecodeWidth, new DecodedInst))
  val isComplexValid = VecInit(isComplex.zipWithIndex.map{
    case(iscomplex,i) => iscomplex && io.in(i).valid && !io.in(i).ready && (if (i==0) true.B else io.out(i).ready)
  })
  val oldComplex = Wire(new DecodeUnitDeqIO)
  oldComplex := PriorityMuxDefault(isComplexValid.zip(decoders.map(_.io.deq)), 0.U.asTypeOf(oldComplex))
  val oldComplexReg = RegNext(oldComplex)
  //Comp 1
  decoderComp.io.simple := oldComplexReg
  decoderComp.io.csrCtrl := io.csrCtrl
  decoderComp.io.vtype := vtypeGen.io.vtype
  decoderComp.io.in0pc := io.in(0).bits.pc
  decoderComp.io.isComplex := isComplex
  decoderComp.io.validFromIBuf.zip(io.in).map { case (dst, src) => dst := src.valid }
  decoderComp.io.readyFromRename.zip(io.out).map { case (dst, src) => dst := src.ready }
  uopComplex := decoderComp.io.deq.decodedInsts
  io.out.zip(decoderComp.io.deq.validToRename).map { case (dst, src) => dst.valid := src }
  io.in.zip(decoderComp.io.deq.readyToIBuf).map { case (dst, src) => dst.ready := src }
  isFirstVset := decoderComp.io.deq.isVset
  complexNum := decoderComp.io.deq.complexNum

  //Simple 6
  decoders.zip(io.in).map { case (dst, src) => dst.io.enq.ctrlFlow := src.bits }
  decoders.map { case dst => dst.io.csrCtrl := io.csrCtrl }
  decoders.map { case dst => dst.io.enq.vtype := vtypeGen.io.vtype }
  isComplex.zip(decoders.map(_.io.deq.isComplex)).map { case (dst, src) => dst := src }
  uopSimple.zip(decoders.map(_.io.deq.decodedInst)).map { case (dst, src) => dst := src }

  vtypeGen.io.firstInstr.valid :=  io.in(0).valid
  vtypeGen.io.firstInstr.bits.instr := io.in(0).bits.instr
  vtypeGen.io.firstInstr.bits.isVset := decoderComp.io.deq.isVset
  vtypeGen.io.isRedirect := io.isRedirect
  vtypeGen.io.commitVType := io.commitVType
  vtypeGen.io.walkVType := io.walkVType

  io.out.zip(0 until RenameWidth).map { case (dst, i) =>
    val uopSimpleFix = Mux(complexNum.orR, uopSimple((i + 1).U - complexNum), uopSimple(i))
    dst.bits := Mux(complexNum > i.U, uopComplex(i), uopSimpleFix)
  }

  for (i <- 0 until DecodeWidth) {

    // We use the lsrc/ldest before fusion decoder to read RAT for better timing.
    io.intRat(i)(0).addr := io.out(i).bits.lsrc(0)
    io.intRat(i)(1).addr := io.out(i).bits.lsrc(1)
    io.intRat(i)(2).addr := io.out(i).bits.ldest
    io.intRat(i).foreach(_.hold := !io.out(i).ready)

    // Floating-point instructions can not be fused now.
    io.fpRat(i)(0).addr := io.out(i).bits.lsrc(0)
    io.fpRat(i)(1).addr := io.out(i).bits.lsrc(1)
    io.fpRat(i)(2).addr := io.out(i).bits.lsrc(2)
    io.fpRat(i)(3).addr := io.out(i).bits.ldest
    io.fpRat(i).foreach(_.hold := !io.out(i).ready)

    // Vec instructions
    // TODO: vec uop dividers need change this
    io.vecRat(i)(0).addr := io.out(i).bits.lsrc(0) // vs1
    io.vecRat(i)(1).addr := io.out(i).bits.lsrc(1) // vs2
    io.vecRat(i)(2).addr := io.out(i).bits.lsrc(2) // old_vd
    io.vecRat(i)(3).addr := v0Idx.U                // v0
    io.vecRat(i)(4).addr := vconfigIdx.U           // vtype
    io.vecRat(i)(5).addr := io.out(i).bits.ldest   // vd
    io.vecRat(i).foreach(_.hold := !io.out(i).ready)
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

  val fusionValid = RegNext(io.fusion)
  val inFire = io.in.map(in => RegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)       ),
    ("decoder_waitInstr",   PopCount(inFire)            ),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
  )
  generatePerfEvent()
}
