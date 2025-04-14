///***************************************************************************************
// * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// * Copyright (c) 2020-2021 Peng Cheng Laboratory
// *
// * XiangShan is licensed under Mulan PSL v2.
// * You can use this software according to the terms and conditions of the Mulan PSL v2.
// * You may obtain a copy of Mulan PSL v2 at:
// *          http://license.coscl.org.cn/MulanPSL2
// *
// * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
// *
// * See the Mulan PSL v2 for more details.
// ***************************************************************************************/
//
//package xiangshan.backend.decode
//
//import org.chipsalliance.cde.config.Parameters
//import chisel3._
//import chisel3.util._
//import freechips.rocketchip.rocket.Instructions
//import freechips.rocketchip.util.uintToBitPat
//import utils._
//import utility._
//import xiangshan.ExceptionNO.illegalInstr
//import xiangshan._
//import xiangshan.backend.fu.fpu.FPU
//import xiangshan.backend.fu.FuType
//import freechips.rocketchip.rocket.Instructions._
//import xiangshan.backend.Bundles.{DecodeOutUop}
//import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
//import xiangshan.backend.fu.vector.Bundles.{VSew, VType, VLmul, Vl}
//import yunsuan.VpermType
//import chisel3.util.experimental.decode.{QMCMinimizer, TruthTable, decoder}
//
//class indexedLSUopTable(uopIdx:Int) extends Module {
//  val src = IO(Input(UInt(4.W)))
//  val outOffsetVs2 = IO(Output(UInt(3.W)))
//  val outOffsetVd = IO(Output(UInt(3.W)))
//  def genCsBundle_VEC_INDEXED_LDST(lmul:Int, emul:Int, uopIdx:Int): (Int, Int) ={
//    // only consider non segment indexed load/store
//    if (lmul < emul) {    // lmul < emul, uop num is depend on emul * nf
//      var offset = 1 << (emul - lmul)
//      for (i <- 0 until (1 << emul)) {
//        if (uopIdx == i) {
//          return (i, i / offset)
//        }
//      }
//    } else {              // lmul > emul, uop num is depend on lmul * nf
//      var offset = 1 << (lmul - emul)
//      for (i <- 0 until (1 << lmul)) {
//        if (uopIdx == i) {
//          return (i / offset, i)
//        }
//      }
//    }
//    return (0, 0)
//  }
//  // strided load/store
//  var combVemulNf : Seq[(Int, Int, Int, Int)] = Seq()
//  for (emul <- 0 until 4) {
//    for (lmul <- 0 until 4) {
//      var offset = genCsBundle_VEC_INDEXED_LDST(lmul, emul, uopIdx)
//      var offsetVs2 = offset._1
//      var offsetVd = offset._2
//      combVemulNf :+= (emul, lmul, offsetVs2, offsetVd)
//    }
//  }
//  val out = decoder(QMCMinimizer, src, TruthTable(combVemulNf.map {
//    case (emul, lmul, offsetVs2, offsetVd) =>
//      (BitPat((emul << 2 | lmul).U(4.W)), BitPat((offsetVs2 << 3 | offsetVd).U(6.W)))
//  }, BitPat.N(6)))
//  outOffsetVs2 := out(5, 3)
//  outOffsetVd := out(2, 0)
//}
//
//trait VectorConstants {
//  val MAX_VLMUL = 8
//  val VECTOR_TMP_REG_LMUL = 32 // 32~46  ->  15
//  val VECTOR_COMPRESS = 1 // in v0 regfile
//  val MAX_INDEXED_LS_UOPNUM = 64
//}
//
//class DecodeUnitCompInput(implicit p: Parameters) extends XSBundle {
//  val simpleDecodedInst = new DecodeOutUop
//  val uopInfo = new UopInfo
//}
//
//class DecodeUnitCompOutput(implicit p: Parameters) extends XSBundle {
//  val complexDecodedInsts = Vec(RenameWidth, DecoupledIO(new DecodeOutUop))
//}
//
//class DecodeUnitCompIO(implicit p: Parameters) extends XSBundle {
//  val redirect = Input(Bool())
//  val csrCtrl = Input(new CustomCSRCtrlIO)
//  // When the first inst in decode vector is complex inst, pass it in
//  val in = Flipped(DecoupledIO(new DecodeUnitCompInput))
//  val out = new DecodeUnitCompOutput
//  val complexNum = Output(UInt((DecodeWidth.U.getWidth).W))
//}
//
///**
// * @author zly
// */
//class DecodeUnitComp()(implicit p : Parameters) extends XSModule with DecodeUnitConstants with VectorConstants {
//  val io = IO(new DecodeUnitCompIO)
//
//  // alias
//  private val inReady = io.in.ready
//  private val inValid = io.in.valid
//  private val inDecodedInst = WireInit(io.in.bits.simpleDecodedInst)
//  private val inInstFields = io.in.bits.simpleDecodedInst.instr.asTypeOf(new XSInstBitFields)
//  private val inUopInfo = io.in.bits.uopInfo
//  private val outValids = io.out.complexDecodedInsts.map(_.valid)
//  private val outReadys = io.out.complexDecodedInsts.map(_.ready)
//  private val outDecodedInsts = io.out.complexDecodedInsts.map(_.bits)
//  private val outComplexNum = io.complexNum
//
//  val maxUopSize = MaxUopSize
//  when (io.in.fire && io.in.bits.simpleDecodedInst.isVset) {
//    when(inInstFields.RD === 0.U && inInstFields.RS1 === 0.U) {
//      inDecodedInst.fuOpType := VSETOpType.keepVl(io.in.bits.simpleDecodedInst.fuOpType)
//    }.elsewhen(inInstFields.RS1 === 0.U) {
//      inDecodedInst.fuOpType := VSETOpType.setVlmax(io.in.bits.simpleDecodedInst.fuOpType)
//    }
//  }
//
//  val latchedInst = RegEnable(inDecodedInst, inValid && inReady)
//  val latchedUopInfo = RegEnable(inUopInfo, inValid && inReady)
//  //input bits
//  private val instFields: XSInstBitFields = latchedInst.instr.asTypeOf(new XSInstBitFields)
//
//  val src1 = Cat(0.U(1.W), instFields.RS1)
//  val src2 = Cat(0.U(1.W), instFields.RS2)
//  val dest = Cat(0.U(1.W), instFields.RD)
//
//  val nf    = instFields.NF
//  val width = instFields.WIDTH(1, 0)
//
//  //output of DecodeUnit
//  val numOfWB = Wire(UInt(log2Up(maxUopSize).W))
//  val lmul = Wire(UInt(4.W))
//  val isVsetSimple = Wire(Bool())
//
//  val indexedLSRegOffset = Seq.tabulate(MAX_VLMUL)(i => Module(new indexedLSUopTable(i)))
//  indexedLSRegOffset.map(_.src := 0.U)
//
//  //pre decode
//  lmul := latchedUopInfo.lmul
//  isVsetSimple := latchedInst.isVset
//  val vlmulReg = latchedInst.vpu.vlmul
//  val vsewReg = latchedInst.vpu.vsew
//  val vstartReg = latchedInst.vpu.vstart
//  val dependOldVdReg = latchedInst.vpu.isDependOldVd
//
//  //Type of uop Div
//  val typeOfSplit = latchedInst.uopSplitType
//  val src1Type = latchedInst.srcType(0)
//  val src1IsImm = src1Type === SrcType.imm
//  val src1IsFp = src1Type === SrcType.fp
//
//  val isVstore = FuType.isVStore(latchedInst.fuType)
//
//  // exception generator
//  val vecException = Module(new VecExceptionGen)
//  vecException.io.inst := latchedInst.instr
//  vecException.io.decodedInst := latchedInst
//  vecException.io.vtype := latchedInst.vpu.vtype
//  vecException.io.vstart := latchedInst.vpu.vstart
//  val illegalInst = vecException.io.illegalInst
//
//  numOfWB := latchedUopInfo.numOfWB
//
//  //uops dispatch
//  val s_idle :: s_active :: Nil = Enum(2)
//  val state = RegInit(s_idle)
//  val stateNext = WireDefault(state)
//  val numDecodedUop = RegInit(0.U(log2Up(maxUopSize).W))
//  val uopRes = RegInit(0.U(log2Up(maxUopSize).W))
//  val uopResNext = WireInit(uopRes)
//  val e64 = 3.U(2.W)
//  val isUsSegment = instFields.MOP === 0.U && ((nf =/= 0.U && instFields.LUMOP === 0.U) || instFields.LUMOP === "b10000".U)
//  val isIxSegment = instFields.MOP(0) === 1.U && nf =/= 0.U
//  val isSdSegment = instFields.MOP === "b10".U && nf =/= 0.U
//
//  //uop div up to maxUopSize
//  val csBundle = Wire(Vec(maxUopSize, new DecodeOutUop))
//  val fixedDecodedInst = Wire(Vec(maxUopSize, new DecodeOutUop))
//
//  csBundle.foreach { case dst =>
//    dst := latchedInst
//    dst.numWB := latchedUopInfo.numOfWB
//    dst.exceptionVec(ExceptionNO.EX_II) := latchedInst.exceptionVec(ExceptionNO.EX_II) || illegalInst
//    dst.firstUop := false.B
//    dst.lastUop := false.B
//    dst.vlsInstr := false.B
//  }
//
//  csBundle(0).firstUop := true.B
//  csBundle(numOfWB - 1.U).lastUop := true.B
//
//  // when vstart is not zero, the last uop will modify vstart to zero
//  // therefore, blockback and flush pipe
//  csBundle(numOfWB - 1.U).blockBackward := vstartReg =/= 0.U
//  csBundle(0.U).flushPipe := vstartReg =/= 0.U
//
//  switch(typeOfSplit) {
//    is(UopSplitType.AMO_CAS_W) {
//      csBundle(0).uopIdx := 0.U
//      csBundle(0).fuOpType := Cat(1.U(3.W), LSUOpType.amocas_w)
//      csBundle(0).lsrc(0) := 0.U
//      csBundle(0).lsrc(1) := src2
//      csBundle(0).rfWen := false.B
//      csBundle(0).waitForward := true.B
//      csBundle(0).blockBackward := false.B
//      csBundle(0).flushPipe := false.B
//
//      csBundle(1).uopIdx := 1.U
//      csBundle(1).fuOpType := Cat(0.U(3.W), LSUOpType.amocas_w)
//      csBundle(1).lsrc(0) := src1
//      csBundle(1).lsrc(1) := dest
//      csBundle(1).waitForward := false.B
//      csBundle(1).blockBackward := true.B
//    }
//    is(UopSplitType.AMO_CAS_D) {
//      csBundle(0).uopIdx := 0.U
//      csBundle(0).fuOpType := Cat(1.U(3.W), LSUOpType.amocas_d)
//      csBundle(0).lsrc(0) := 0.U
//      csBundle(0).lsrc(1) := src2
//      csBundle(0).rfWen := false.B
//      csBundle(0).waitForward := true.B
//      csBundle(0).blockBackward := false.B
//      csBundle(0).flushPipe := false.B
//
//      csBundle(1).uopIdx := 1.U
//      csBundle(1).fuOpType := Cat(0.U(3.W), LSUOpType.amocas_d)
//      csBundle(1).lsrc(0) := src1
//      csBundle(1).lsrc(1) := dest
//      csBundle(1).waitForward := false.B
//      csBundle(1).blockBackward := true.B
//    }
//    is(UopSplitType.AMO_CAS_Q) {
//      csBundle(0).uopIdx := 0.U
//      csBundle(0).fuOpType := Cat(1.U(3.W), LSUOpType.amocas_q)
//      csBundle(0).lsrc(0) := 0.U
//      csBundle(0).lsrc(1) := src2
//      csBundle(0).rfWen := false.B
//      csBundle(0).waitForward := true.B
//      csBundle(0).blockBackward := false.B
//      csBundle(0).flushPipe := false.B
//
//      csBundle(1).uopIdx := 1.U
//      csBundle(1).fuOpType := Cat(0.U(3.W), LSUOpType.amocas_q)
//      csBundle(1).lsrc(0) := src1
//      csBundle(1).lsrc(1) := dest
//      csBundle(1).waitForward := false.B
//      csBundle(1).blockBackward := false.B
//
//      csBundle(2).uopIdx := 2.U
//      csBundle(2).fuOpType := Cat(3.U(3.W), LSUOpType.amocas_q)
//      csBundle(2).lsrc(0) := 0.U
//      csBundle(2).lsrc(1) := Mux(src2 === 0.U, 0.U, src2 + 1.U)
//      csBundle(2).rfWen := false.B
//      csBundle(2).waitForward := false.B
//      csBundle(2).blockBackward := false.B
//
//      csBundle(3).uopIdx := 3.U
//      csBundle(3).fuOpType := Cat(2.U(3.W), LSUOpType.amocas_q)
//      csBundle(3).lsrc(0) := 0.U
//      csBundle(3).lsrc(1) := Mux(dest === 0.U, 0.U, dest + 1.U)
//      csBundle(3).ldest := Mux(dest === 0.U, 0.U, dest + 1.U)
//      csBundle(3).waitForward := false.B
//      csBundle(3).blockBackward := true.B
//    }
//    is(UopSplitType.VSET) {
//      // In simple decoder, rfWen and vecWen are not set
//      when(isVsetSimple) {
//        // Default
//        // uop0 set rd, never flushPipe
//        csBundle(0).fuType := FuType.vsetiwi.U
//        csBundle(0).flushPipe := Mux(VSETOpType.isVsetvl(latchedInst.fuOpType), true.B, vstartReg =/= 0.U)
//        csBundle(0).blockBackward := false.B
//        csBundle(0).rfWen := true.B
//        // uop1 set vl, vsetvl will flushPipe
//        csBundle(1).ldest := Vl_IDX.U
//        csBundle(1).vecWen := false.B
//        csBundle(1).vlWen := true.B
//        csBundle(1).flushPipe := false.B
//        csBundle(1).blockBackward := Mux(VSETOpType.isVsetvl(latchedInst.fuOpType), true.B, vstartReg =/= 0.U)
//        when(VSETOpType.isVsetvli(latchedInst.fuOpType) && dest === 0.U && src1 === 0.U) {
//          // write nothing, uop0 is a nop instruction
//          csBundle(0).rfWen := false.B
//          csBundle(0).fpWen := false.B
//          csBundle(0).vecWen := false.B
//          csBundle(0).vlWen := false.B
//          csBundle(1).fuType := FuType.vsetfwf.U
//          csBundle(1).srcType(0) := SrcType.no
//          csBundle(1).srcType(2) := SrcType.no
//          csBundle(1).vlRen := true.B
//        }.elsewhen(VSETOpType.isVsetvl(latchedInst.fuOpType) && dest === 0.U && src1 === 0.U) {
//          // uop0: mv vtype gpr to vector region
//          csBundle(0).srcType(0) := SrcType.xp
//          csBundle(0).srcType(1) := SrcType.no
//          csBundle(0).lsrc(0) := src2
//          csBundle(0).lsrc(1) := 0.U
//          csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//          csBundle(0).fuType := FuType.i2v.U
//          csBundle(0).fuOpType := Cat(IF2VectorType.i2Vec(2, 0), e64)
//          csBundle(0).rfWen := false.B
//          csBundle(0).fpWen := false.B
//          csBundle(0).vecWen := true.B
//          csBundle(0).vlWen := false.B
//          // uop1: uvsetvcfg_vv
//          csBundle(1).fuType := FuType.vsetfwf.U
//          // vl
//          csBundle(1).srcType(0) := SrcType.no
//          csBundle(1).srcType(2) := SrcType.no
//          csBundle(1).vlRen := true.B
//          // vtype
//          csBundle(1).srcType(1) := SrcType.vp
//          csBundle(1).lsrc(1) := VECTOR_TMP_REG_LMUL.U
//          csBundle(1).vecWen := false.B
//          csBundle(1).vlWen := true.B
//          csBundle(1).ldest := Vl_IDX.U
//        }.elsewhen(dest === 0.U) {
//          // write nothing, uop0 is a nop instruction
//          csBundle(0).rfWen := false.B
//          csBundle(0).fpWen := false.B
//          csBundle(0).vecWen := false.B
//          csBundle(0).vlWen := false.B
//        }.elsewhen(VSETOpType.isVsetvl(latchedInst.fuOpType)) {
//          // because vsetvl may modified src2 when src2 == rd,
//          // we need to modify vd in second uop to avoid dependency
//          // uop0 set vl
//          csBundle(0).fuType := FuType.vsetiwf.U
//          csBundle(0).ldest := Vl_IDX.U
//          csBundle(0).rfWen := false.B
//          csBundle(0).vlWen := true.B
//          // uop1 set rd
//          csBundle(1).fuType := FuType.vsetiwi.U
//          csBundle(1).ldest := dest
//          csBundle(1).rfWen := true.B
//          csBundle(1).vlWen := false.B
//        }
//      }
//
//    }
//    is(UopSplitType.VEC_VVV) {
//      for (i <- 0 until MAX_VLMUL) {
//        csBundle(i).lsrc(0) := src1 + i.U
//        csBundle(i).lsrc(1) := src2 + i.U
//        csBundle(i).lsrc(2) := dest + i.U
//        csBundle(i).ldest := dest + i.U
//        csBundle(i).uopIdx := i.U
//      }
//    }
//    is(UopSplitType.VEC_VFV) {
//      /*
//      f to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.fp
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.f2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
//      csBundle(0).vecWen := true.B
//      csBundle(0).vpu.isReverse := false.B
//      /*
//      LMUL
//       */
//      for (i <- 0 until MAX_VLMUL) {
//        csBundle(i + 1).srcType(0) := SrcType.vp
//        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(i + 1).lsrc(1) := src2 + i.U
//        csBundle(i + 1).lsrc(2) := dest + i.U
//        csBundle(i + 1).ldest := dest + i.U
//        csBundle(i + 1).uopIdx := i.U
//      }
//    }
//    is(UopSplitType.VEC_EXT2) {
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i).lsrc(1) := src2 + i.U
//        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
//        csBundle(2 * i).ldest := dest + (2 * i).U
//        csBundle(2 * i).uopIdx := (2 * i).U
//        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
//        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
//        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_EXT4) {
//      for (i <- 0 until MAX_VLMUL / 4) {
//        csBundle(4 * i).lsrc(1) := src2 + i.U
//        csBundle(4 * i).lsrc(2) := dest + (4 * i).U
//        csBundle(4 * i).ldest := dest + (4 * i).U
//        csBundle(4 * i).uopIdx := (4 * i).U
//        csBundle(4 * i + 1).lsrc(1) := src2 + i.U
//        csBundle(4 * i + 1).lsrc(2) := dest + (4 * i + 1).U
//        csBundle(4 * i + 1).ldest := dest + (4 * i + 1).U
//        csBundle(4 * i + 1).uopIdx := (4 * i + 1).U
//        csBundle(4 * i + 2).lsrc(1) := src2 + i.U
//        csBundle(4 * i + 2).lsrc(2) := dest + (4 * i + 2).U
//        csBundle(4 * i + 2).ldest := dest + (4 * i + 2).U
//        csBundle(4 * i + 2).uopIdx := (4 * i + 2).U
//        csBundle(4 * i + 3).lsrc(1) := src2 + i.U
//        csBundle(4 * i + 3).lsrc(2) := dest + (4 * i + 3).U
//        csBundle(4 * i + 3).ldest := dest + (4 * i + 3).U
//        csBundle(4 * i + 3).uopIdx := (4 * i + 3).U
//      }
//    }
//    is(UopSplitType.VEC_EXT8) {
//      for (i <- 0 until MAX_VLMUL) {
//        csBundle(i).lsrc(1) := src2
//        csBundle(i).lsrc(2) := dest + i.U
//        csBundle(i).ldest := dest + i.U
//        csBundle(i).uopIdx := i.U
//      }
//    }
//    is(UopSplitType.VEC_0XV) {
//      /*
//      i/f to vector move
//       */
//      csBundle(0).srcType(0) := Mux(src1IsFp, SrcType.fp, SrcType.reg)
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := Mux(src1IsFp, FuType.f2v.U, FuType.i2v.U)
//      csBundle(0).fuOpType := Cat(Mux(src1IsFp, IF2VectorType.fDup2Vec(2, 0), IF2VectorType.i2Vec(2, 0)), vsewReg)
//      csBundle(0).rfWen := false.B
//      csBundle(0).fpWen := false.B
//      csBundle(0).vecWen := true.B
//      /*
//      vmv.s.x
//       */
//      csBundle(1).srcType(0) := SrcType.vp
//      csBundle(1).srcType(1) := SrcType.imm
//      csBundle(1).srcType(2) := SrcType.vp
//      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//      csBundle(1).lsrc(1) := 0.U
//      csBundle(1).lsrc(2) := dest
//      csBundle(1).ldest := dest
//      csBundle(1).rfWen := false.B
//      csBundle(1).fpWen := false.B
//      csBundle(1).vecWen := true.B
//      csBundle(1).uopIdx := 0.U
//    }
//    is(UopSplitType.VEC_VXV) {
//      /*
//      i to vector move
//       */
//      csBundle(0).srcType(0) := Mux(src1IsImm, SrcType.imm, SrcType.reg)
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.i2v.U
//      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.immDup2Vec(2, 0), IF2VectorType.iDup2Vec(2, 0)), vsewReg)
//      csBundle(0).vecWen := true.B
//      csBundle(0).vpu.isReverse := false.B
//      /*
//      LMUL
//       */
//      for (i <- 0 until MAX_VLMUL) {
//        csBundle(i + 1).srcType(0) := SrcType.vp
//        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(i + 1).lsrc(1) := src2 + i.U
//        csBundle(i + 1).lsrc(2) := dest + i.U
//        csBundle(i + 1).ldest := dest + i.U
//        csBundle(i + 1).uopIdx := i.U
//      }
//    }
//    is(UopSplitType.VEC_VVW) {
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i).lsrc(0) := src1 + i.U
//        csBundle(2 * i).lsrc(1) := src2 + i.U
//        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
//        csBundle(2 * i).ldest := dest + (2 * i).U
//        csBundle(2 * i).uopIdx := (2 * i).U
//        csBundle(2 * i + 1).lsrc(0) := src1 + i.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
//        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
//        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_VFW) {
//      /*
//      f to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.fp
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.f2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
//      csBundle(0).rfWen := false.B
//      csBundle(0).fpWen := false.B
//      csBundle(0).vecWen := true.B
//
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i + 1).srcType(0) := SrcType.vp
//        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
//        csBundle(2 * i + 1).ldest := dest + (2 * i).U
//        csBundle(2 * i + 1).uopIdx := (2 * i).U
//        csBundle(2 * i + 2).srcType(0) := SrcType.vp
//        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 2).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_WVW) {
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i).lsrc(0) := src1 + i.U
//        csBundle(2 * i).lsrc(1) := src2 + (2 * i).U
//        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
//        csBundle(2 * i).ldest := dest + (2 * i).U
//        csBundle(2 * i).uopIdx := (2 * i).U
//        csBundle(2 * i + 1).lsrc(0) := src1 + i.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i + 1).U
//        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
//        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
//        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_VXW) {
//      /*
//      i to vector move
//       */
//      csBundle(0).srcType(0) := Mux(src1IsImm, SrcType.imm, SrcType.reg)
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.i2v.U
//      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.immDup2Vec(2, 0), IF2VectorType.iDup2Vec(2, 0)), vsewReg)
//      csBundle(0).vecWen := true.B
//
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i + 1).srcType(0) := SrcType.vp
//        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
//        csBundle(2 * i + 1).ldest := dest + (2 * i).U
//        csBundle(2 * i + 1).uopIdx := (2 * i).U
//        csBundle(2 * i + 2).srcType(0) := SrcType.vp
//        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 2).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_WXW) {
//      /*
//      i to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.reg
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.i2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.iDup2Vec(2, 0), vsewReg)
//      csBundle(0).vecWen := true.B
//
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i + 1).srcType(0) := SrcType.vp
//        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
//        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
//        csBundle(2 * i + 1).ldest := dest + (2 * i).U
//        csBundle(2 * i + 1).uopIdx := (2 * i).U
//        csBundle(2 * i + 2).srcType(0) := SrcType.vp
//        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 2).lsrc(1) := src2 + (2 * i + 1).U
//        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_WVV) {
//      for (i <- 0 until MAX_VLMUL / 2) {
//
//        csBundle(2 * i).lsrc(0) := src1 + i.U
//        csBundle(2 * i).lsrc(1) := src2 + (2 * i).U
//        csBundle(2 * i).lsrc(2) := dest + i.U
//        csBundle(2 * i).ldest := dest + i.U
//        csBundle(2 * i).uopIdx := (2 * i).U
//        csBundle(2 * i + 1).lsrc(0) := src1 + i.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i + 1).U
//        csBundle(2 * i + 1).lsrc(2) := dest + i.U
//        csBundle(2 * i + 1).ldest := dest + i.U
//        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_WFW) {
//      /*
//      f to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.fp
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.f2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
//      csBundle(0).rfWen := false.B
//      csBundle(0).fpWen := false.B
//      csBundle(0).vecWen := true.B
//
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i + 1).srcType(0) := SrcType.vp
//        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
//        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
//        csBundle(2 * i + 1).ldest := dest + (2 * i).U
//        csBundle(2 * i + 1).uopIdx := (2 * i).U
//        csBundle(2 * i + 2).srcType(0) := SrcType.vp
//        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 2).lsrc(1) := src2 + (2 * i + 1).U
//        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
//        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_WXV) {
//      /*
//      i to vector move
//       */
//      csBundle(0).srcType(0) := Mux(src1IsImm, SrcType.imm, SrcType.reg)
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.i2v.U
//      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.immDup2Vec(2, 0), IF2VectorType.iDup2Vec(2, 0)), vsewReg)
//      csBundle(0).vecWen := true.B
//
//      for (i <- 0 until MAX_VLMUL / 2) {
//        csBundle(2 * i + 1).srcType(0) := SrcType.vp
//        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
//        csBundle(2 * i + 1).lsrc(2) := dest + i.U
//        csBundle(2 * i + 1).ldest := dest + i.U
//        csBundle(2 * i + 1).uopIdx := (2 * i).U
//        csBundle(2 * i + 2).srcType(0) := SrcType.vp
//        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2 * i + 2).lsrc(1) := src2 + (2 * i + 1).U
//        csBundle(2 * i + 2).lsrc(2) := dest + i.U
//        csBundle(2 * i + 2).ldest := dest + i.U
//        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
//      }
//    }
//    is(UopSplitType.VEC_VVM) {
//      csBundle(0).lsrc(2) := dest
//      csBundle(0).ldest := dest
//      csBundle(0).uopIdx := 0.U
//      for (i <- 1 until MAX_VLMUL) {
//        csBundle(i).lsrc(0) := src1 + i.U
//        csBundle(i).lsrc(1) := src2 + i.U
//        csBundle(i).lsrc(2) := dest
//        csBundle(i).ldest := dest
//        csBundle(i).uopIdx := i.U
//      }
//    }
//    is(UopSplitType.VEC_VFM) {
//      /*
//      f to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.fp
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.f2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
//      csBundle(0).rfWen := false.B
//      csBundle(0).fpWen := false.B
//      csBundle(0).vecWen := true.B
//      //LMUL
//      csBundle(1).srcType(0) := SrcType.vp
//      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//      csBundle(1).lsrc(2) := dest
//      csBundle(1).ldest := dest
//      csBundle(1).uopIdx := 0.U
//      for (i <- 1 until MAX_VLMUL) {
//        csBundle(i + 1).srcType(0) := SrcType.vp
//        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(i + 1).lsrc(1) := src2 + i.U
//        csBundle(i + 1).lsrc(2) := dest
//        csBundle(i + 1).ldest := dest
//        csBundle(i + 1).uopIdx := i.U
//      }
//      csBundle(numOfWB - 1.U).ldest := dest
//    }
//    is(UopSplitType.VEC_VXM) {
//      /*
//      i to vector move
//       */
//      csBundle(0).srcType(0) := Mux(src1IsImm, SrcType.imm, SrcType.reg)
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.i2v.U
//      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.immDup2Vec(2, 0), IF2VectorType.iDup2Vec(2, 0)), vsewReg)
//      csBundle(0).vecWen := true.B
//      //LMUL
//      csBundle(1).srcType(0) := SrcType.vp
//      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//      csBundle(1).lsrc(2) := dest
//      csBundle(1).ldest := dest
//      csBundle(1).uopIdx := 0.U
//      for (i <- 1 until MAX_VLMUL) {
//        csBundle(i + 1).srcType(0) := SrcType.vp
//        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//        csBundle(i + 1).lsrc(1) := src2 + i.U
//        csBundle(i + 1).lsrc(2) := dest
//        csBundle(i + 1).ldest := dest
//        csBundle(i + 1).uopIdx := i.U
//      }
//      csBundle(numOfWB - 1.U).ldest := dest
//    }
//    is(UopSplitType.VEC_SLIDE1UP) {
//      /*
//      i to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.reg
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.i2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.iDup2Vec(2, 0), vsewReg)
//      csBundle(0).vecWen := true.B
//      //LMUL
//      csBundle(1).srcType(0) := SrcType.vp
//      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//      csBundle(1).lsrc(2) := dest
//      csBundle(1).ldest := dest
//      csBundle(1).uopIdx := 0.U
//      for (i <- 1 until MAX_VLMUL) {
//        csBundle(i + 1).srcType(0) := SrcType.vp
//        csBundle(i + 1).lsrc(0) := src2 + (i - 1).U
//        csBundle(i + 1).lsrc(1) := src2 + i.U
//        csBundle(i + 1).lsrc(2) := dest + i.U
//        csBundle(i + 1).ldest := dest + i.U
//        csBundle(i + 1).uopIdx := i.U
//      }
//    }
//    is(UopSplitType.VEC_FSLIDE1UP) {
//      /*
//      f to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.fp
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.f2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
//      csBundle(0).rfWen := false.B
//      csBundle(0).fpWen := false.B
//      csBundle(0).vecWen := true.B
//      //LMUL
//      csBundle(1).srcType(0) := SrcType.vp
//      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//      csBundle(1).lsrc(1) := src2
//      csBundle(1).lsrc(2) := dest
//      csBundle(1).ldest := dest
//      csBundle(1).uopIdx := 0.U
//      for (i <- 1 until MAX_VLMUL) {
//        csBundle(i + 1).srcType(0) := SrcType.vp
//        csBundle(i + 1).lsrc(0) := src2 + (i - 1).U
//        csBundle(i + 1).lsrc(1) := src2 + i.U
//        csBundle(i + 1).lsrc(2) := dest + i.U
//        csBundle(i + 1).ldest := dest + i.U
//        csBundle(i + 1).uopIdx := i.U
//      }
//    }
//    is(UopSplitType.VEC_SLIDE1DOWN) { // lmul+lmul = 16
//      /*
//      i to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.reg
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.i2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.iDup2Vec(2, 0), vsewReg)
//      csBundle(0).vecWen := true.B
//      //LMUL
//      for (i <- 0 until MAX_VLMUL) {
//        csBundle(2 * i + 1).srcType(0) := SrcType.vp
//        csBundle(2 * i + 1).srcType(1) := SrcType.vp
//        csBundle(2 * i + 1).lsrc(0) := src2 + (i + 1).U
//        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 1).lsrc(2) := dest + i.U
//        csBundle(2 * i + 1).ldest := VECTOR_TMP_REG_LMUL.U + 1.U
//        csBundle(2 * i + 1).uopIdx := (2 * i).U
//        if (2 * i + 2 < MAX_VLMUL * 2) {
//          csBundle(2 * i + 2).srcType(0) := SrcType.vp
//          csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//          // csBundle(2 * i + 2).lsrc(1) := src2 + i.U         // DontCare
//          csBundle(2 * i + 2).lsrc(2) := VECTOR_TMP_REG_LMUL.U + 1.U
//          csBundle(2 * i + 2).ldest := dest + i.U
//          csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
//        }
//      }
//      csBundle(numOfWB - 1.U).srcType(0) := SrcType.vp
//      csBundle(numOfWB - 1.U).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//      csBundle(numOfWB - 1.U).ldest := dest + lmul - 1.U
//    }
//    is(UopSplitType.VEC_FSLIDE1DOWN) {
//      /*
//      f to vector move
//       */
//      csBundle(0).srcType(0) := SrcType.fp
//      csBundle(0).srcType(1) := SrcType.imm
//      csBundle(0).srcType(2) := SrcType.imm
//      csBundle(0).lsrc(1) := 0.U
//      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//      csBundle(0).fuType := FuType.f2v.U
//      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
//      csBundle(0).rfWen := false.B
//      csBundle(0).fpWen := false.B
//      csBundle(0).vecWen := true.B
//      //LMUL
//      for (i <- 0 until MAX_VLMUL) {
//        csBundle(2 * i + 1).srcType(0) := SrcType.vp
//        csBundle(2 * i + 1).srcType(1) := SrcType.vp
//        csBundle(2 * i + 1).lsrc(0) := src2 + (i + 1).U
//        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
//        csBundle(2 * i + 1).lsrc(2) := dest + i.U
//        csBundle(2 * i + 1).ldest := VECTOR_TMP_REG_LMUL.U + 1.U
//        csBundle(2 * i + 1).uopIdx := (2 * i).U
//        if (2 * i + 2 < MAX_VLMUL * 2) {
//          csBundle(2 * i + 2).srcType(0) := SrcType.vp
//          csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//          // csBundle(2 * i + 2).lsrc(1) := src2 + i.U         // DontCare
//          csBundle(2 * i + 2).lsrc(2) := VECTOR_TMP_REG_LMUL.U + 1.U
//          csBundle(2 * i + 2).ldest := dest + i.U
//          csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
//        }
//      }
//      csBundle(numOfWB - 1.U).srcType(0) := SrcType.vp
//      csBundle(numOfWB - 1.U).lsrc(0) := VECTOR_TMP_REG_LMUL.U
//      csBundle(numOfWB - 1.U).ldest := dest + lmul - 1.U
//    }
//    is(UopSplitType.VEC_VRED) {
//      when(vlmulReg === "b001".U) {
//        csBundle(0).srcType(2) := SrcType.DC
//        csBundle(0).lsrc(0) := src2 + 1.U
//        csBundle(0).lsrc(1) := src2
//        csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//        csBundle(0).uopIdx := 0.U
//      }
//      when(vlmulReg === "b010".U) {
//        csBundle(0).srcType(2) := SrcType.DC
//        csBundle(0).lsrc(0) := src2 + 1.U
//        csBundle(0).lsrc(1) := src2
//        csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
//        csBundle(0).uopIdx := 0.U
//
//        csBundle(1).srcType(2) := SrcType.DC
//        csBundle(1).lsrc(0) := src2 + 3.U
//        csBundle(1).lsrc(1) := src2 + 2.U
//        csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
//        csBundle(1).uopIdx := 1.U
//
//        csBundle(2).srcType(2) := SrcType.DC
//        csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
//        csBundle(2).lsrc(1) := VECTOR_TMP_REG_LMUL.U
//        csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
//        csBundle(2).uopIdx := 2.U
//      }
//      when(vlmulReg === "b011".U) {
//        for (i <- 0 until MAX_VLMUL) {
//          if (i < MAX_VLMUL - MAX_VLMUL / 2) {
//            csBundle(i).lsrc(0) := src2 + (i * 2 + 1).U
//            csBundle(i).lsrc(1) := src2 + (i * 2).U
//            csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
//          } else if (i < MAX_VLMUL - MAX_VLMUL / 4) {
//            csBundle(i).lsrc(0) := (VECTOR_TMP_REG_LMUL + (i - MAX_VLMUL / 2) * 2 + 1).U
//            csBundle(i).lsrc(1) := (VECTOR_TMP_REG_LMUL + (i - MAX_VLMUL / 2) * 2).U
//            csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
//          } else if (i < MAX_VLMUL - MAX_VLMUL / 8) {
//            csBundle(6).lsrc(0) := (VECTOR_TMP_REG_LMUL + 5).U
//            csBundle(6).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
//            csBundle(6).ldest := (VECTOR_TMP_REG_LMUL + 6).U
//          }
//          csBundle(i).srcType(2) := SrcType.DC
//          csBundle(i).uopIdx := i.U
//        }
//      }
//      when(vlmulReg(2) === 0.U && vlmulReg(1, 0).orR) {
//        /*
//         * 2 <= vlmul <= 8
//         */
//        csBundle(numOfWB - 1.U).srcType(2) := SrcType.vp
//        csBundle(numOfWB - 1.U).lsrc(0) := src1
//        csBundle(numOfWB - 1.U).lsrc(1) := VECTOR_TMP_REG_LMUL.U + numOfWB - 2.U
//        csBundle(numOfWB - 1.U).lsrc(2) := dest
//        csBundle(numOfWB - 1.U).ldest := dest
//        csBundle(numOfWB - 1.U).uopIdx := numOfWB - 1.U
//      }
//    }
//    is(UopSplitType.VEC_VFRED) {
//      val vlmul = vlmulReg
//      val vsew = vsewReg
//      when(vlmul === VLmul.m8){
//        for (i <- 0 until 4) {
//          csBundle(i).lsrc(0) := src2 + (i * 2 + 1).U
//          csBundle(i).lsrc(1) := src2 + (i * 2).U
//          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
//          csBundle(i).uopIdx := i.U
//        }
//        for (i <- 4 until 6) {
//          csBundle(i).lsrc(0) := (VECTOR_TMP_REG_LMUL + (i - 4) * 2 + 1).U
//          csBundle(i).lsrc(1) := (VECTOR_TMP_REG_LMUL + (i - 4) * 2).U
//          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
//          csBundle(i).uopIdx := i.U
//        }
//        csBundle(6).lsrc(0) := (VECTOR_TMP_REG_LMUL + 5).U
//        csBundle(6).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
//        csBundle(6).ldest := (VECTOR_TMP_REG_LMUL + 6).U
//        csBundle(6).uopIdx := 6.U
//        when(vsew === VSew.e64) {
//          csBundle(7).lsrc(0) := (VECTOR_TMP_REG_LMUL + 6).U
//          csBundle(7).lsrc(1) := (VECTOR_TMP_REG_LMUL + 6).U
//          csBundle(7).ldest := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(7).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(7).uopIdx := 7.U
//          csBundle(8).lsrc(0) := src1
//          csBundle(8).lsrc(1) := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(8).ldest := dest
//          csBundle(8).uopIdx := 8.U
//        }
//        when(vsew === VSew.e32) {
//          csBundle(7).lsrc(0) := (VECTOR_TMP_REG_LMUL + 6).U
//          csBundle(7).lsrc(1) := (VECTOR_TMP_REG_LMUL + 6).U
//          csBundle(7).ldest := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(7).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(7).uopIdx := 7.U
//          csBundle(8).lsrc(0) := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(8).lsrc(1) := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(8).ldest := (VECTOR_TMP_REG_LMUL + 8).U
//          csBundle(8).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(8).uopIdx := 8.U
//          csBundle(9).lsrc(0) := src1
//          csBundle(9).lsrc(1) := (VECTOR_TMP_REG_LMUL + 8).U
//          csBundle(9).ldest := dest
//          csBundle(9).uopIdx := 9.U
//        }
//        when(vsew === VSew.e16) {
//          csBundle(7).lsrc(0) := (VECTOR_TMP_REG_LMUL + 6).U
//          csBundle(7).lsrc(1) := (VECTOR_TMP_REG_LMUL + 6).U
//          csBundle(7).ldest := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(7).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(7).uopIdx := 7.U
//          csBundle(8).lsrc(0) := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(8).lsrc(1) := (VECTOR_TMP_REG_LMUL + 7).U
//          csBundle(8).ldest := (VECTOR_TMP_REG_LMUL + 8).U
//          csBundle(8).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(8).uopIdx := 8.U
//          csBundle(9).lsrc(0) := (VECTOR_TMP_REG_LMUL + 8).U
//          csBundle(9).lsrc(1) := (VECTOR_TMP_REG_LMUL + 8).U
//          csBundle(9).ldest := (VECTOR_TMP_REG_LMUL + 9).U
//          csBundle(9).vpu.fpu.isFoldTo1_8 := true.B
//          csBundle(9).uopIdx := 9.U
//          csBundle(10).lsrc(0) := src1
//          csBundle(10).lsrc(1) := (VECTOR_TMP_REG_LMUL + 9).U
//          csBundle(10).ldest := dest
//          csBundle(10).uopIdx := 10.U
//        }
//      }
//      when(vlmul === VLmul.m4) {
//        for (i <- 0 until 2) {
//          csBundle(i).lsrc(0) := src2 + (i * 2 + 1).U
//          csBundle(i).lsrc(1) := src2 + (i * 2).U
//          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
//          csBundle(i).uopIdx := i.U
//        }
//        csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
//        csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//        csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
//        csBundle(2).uopIdx := 2.U
//        when(vsew === VSew.e64) {
//          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(3).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(3).uopIdx := 3.U
//          csBundle(4).lsrc(0) := src1
//          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(4).ldest := dest
//          csBundle(4).uopIdx := 4.U
//        }
//        when(vsew === VSew.e32) {
//          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(3).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(3).uopIdx := 3.U
//          csBundle(4).lsrc(0) := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(4).ldest := (VECTOR_TMP_REG_LMUL + 4).U
//          csBundle(4).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(4).uopIdx := 4.U
//          csBundle(5).lsrc(0) := src1
//          csBundle(5).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
//          csBundle(5).ldest := dest
//          csBundle(5).uopIdx := 5.U
//        }
//        when(vsew === VSew.e16) {
//          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(3).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(3).uopIdx := 3.U
//          csBundle(4).lsrc(0) := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(4).ldest := (VECTOR_TMP_REG_LMUL + 4).U
//          csBundle(4).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(4).uopIdx := 4.U
//          csBundle(5).lsrc(0) := (VECTOR_TMP_REG_LMUL + 4).U
//          csBundle(5).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
//          csBundle(5).ldest := (VECTOR_TMP_REG_LMUL + 5).U
//          csBundle(5).vpu.fpu.isFoldTo1_8 := true.B
//          csBundle(5).uopIdx := 5.U
//          csBundle(6).lsrc(0) := src1
//          csBundle(6).lsrc(1) := (VECTOR_TMP_REG_LMUL + 5).U
//          csBundle(6).ldest := dest
//          csBundle(6).uopIdx := 6.U
//        }
//      }
//      when(vlmul === VLmul.m2) {
//        csBundle(0).lsrc(0) := src2 + 1.U
//        csBundle(0).lsrc(1) := src2 + 0.U
//        csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
//        csBundle(0).uopIdx := 0.U
//        when(vsew === VSew.e64) {
//          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(1).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(1).uopIdx := 1.U
//          csBundle(2).lsrc(0) := src1
//          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).ldest := dest
//          csBundle(2).uopIdx := 2.U
//        }
//        when(vsew === VSew.e32) {
//          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(1).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(1).uopIdx := 1.U
//          csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(2).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(2).uopIdx := 2.U
//          csBundle(3).lsrc(0) := src1
//          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).ldest := dest
//          csBundle(3).uopIdx := 3.U
//        }
//        when(vsew === VSew.e16) {
//          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(1).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(1).uopIdx := 1.U
//          csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(2).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(2).uopIdx := 2.U
//          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(3).vpu.fpu.isFoldTo1_8 := true.B
//          csBundle(3).uopIdx := 3.U
//          csBundle(4).lsrc(0) := src1
//          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
//          csBundle(4).ldest := dest
//          csBundle(4).uopIdx := 4.U
//        }
//      }
//      when(vlmul === VLmul.m1) {
//        when(vsew === VSew.e64) {
//          csBundle(0).lsrc(0) := src2
//          csBundle(0).lsrc(1) := src2
//          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(0).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(0).uopIdx := 0.U
//          csBundle(1).lsrc(0) := src1
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := dest
//          csBundle(1).uopIdx := 1.U
//        }
//        when(vsew === VSew.e32) {
//          csBundle(0).lsrc(0) := src2
//          csBundle(0).lsrc(1) := src2
//          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(0).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(0).uopIdx := 0.U
//          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(1).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(1).uopIdx := 1.U
//          csBundle(2).lsrc(0) := src1
//          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).ldest := dest
//          csBundle(2).uopIdx := 2.U
//        }
//        when(vsew === VSew.e16) {
//          csBundle(0).lsrc(0) := src2
//          csBundle(0).lsrc(1) := src2
//          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(0).vpu.fpu.isFoldTo1_2 := true.B
//          csBundle(0).uopIdx := 0.U
//          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(1).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(1).uopIdx := 1.U
//          csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(2).vpu.fpu.isFoldTo1_8 := true.B
//          csBundle(2).uopIdx := 2.U
//          csBundle(3).lsrc(0) := src1
//          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
//          csBundle(3).ldest := dest
//          csBundle(3).uopIdx := 3.U
//        }
//      }
//      when(vlmul === VLmul.mf2) {
//        when(vsew === VSew.e32) {
//          csBundle(0).lsrc(0) := src2
//          csBundle(0).lsrc(1) := src2
//          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(0).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(0).uopIdx := 0.U
//          csBundle(1).lsrc(0) := src1
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := dest
//          csBundle(1).uopIdx := 1.U
//        }
//        when(vsew === VSew.e16) {
//          csBundle(0).lsrc(0) := src2
//          csBundle(0).lsrc(1) := src2
//          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(0).vpu.fpu.isFoldTo1_4 := true.B
//          csBundle(0).uopIdx := 0.U
//          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(1).vpu.fpu.isFoldTo1_8 := true.B
//          csBundle(1).uopIdx := 1.U
//          csBundle(2).lsrc(0) := src1
//          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
//          csBundle(2).ldest := dest
//          csBundle(2).uopIdx := 2.U
//        }
//      }
//      when(vlmul === VLmul.mf4) {
//        when(vsew === VSew.e16) {
//          csBundle(0).lsrc(0) := src2
//          csBundle(0).lsrc(1) := src2
//          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(0).vpu.fpu.isFoldTo1_8 := true.B
//          csBundle(0).uopIdx := 0.U
//          csBundle(1).lsrc(0) := src1
//          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
//          csBundle(1).ldest := dest
//          csBundle(1).uopIdx := 1.U
//        }
//      }
//    }
//
//    is(UopSplitType.VEC_VFREDOSUM) {
//      import yunsuan.VfaluType
//      val vlmul = vlmulReg
//      val vsew = vsewReg
//      val isWiden = latchedInst.fuOpType === VfaluType.vfwredosum
//      when(vlmul === VLmul.m8) {
//        when(vsew === VSew.e64) {
//          val vlmax = 16
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e32) {
//          val vlmax = 32
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e16) {
//          val vlmax = 64
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//      }
//      when(vlmul === VLmul.m4) {
//        when(vsew === VSew.e64) {
//          val vlmax = 8
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e32) {
//          val vlmax = 16
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e16) {
//          val vlmax = 32
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//      }
//      when(vlmul === VLmul.m2) {
//        when(vsew === VSew.e64) {
//          val vlmax = 4
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e32) {
//          val vlmax = 8
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e16) {
//          val vlmax = 16
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//      }
//      when(vlmul === VLmul.m1) {
//        when(vsew === VSew.e64) {
//          val vlmax = 2
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e32) {
//          val vlmax = 4
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e16) {
//          val vlmax = 8
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//      }
//      when(vlmul === VLmul.mf2) {
//        when(vsew === VSew.e32) {
//          val vlmax = 2
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//        when(vsew === VSew.e16) {
//          val vlmax = 4
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//      }
//      when(vlmul === VLmul.mf4) {
//        when(vsew === VSew.e16) {
//          val vlmax = 2
//          for (i <- 0 until vlmax) {
//            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
//            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
//            csBundle(i).uopIdx := i.U
//          }
//        }
//      }
//    }
//
//  complexNum := Mux(io.validFromIBuf(0) && readyCounter.orR ,
//    Mux(uopRes0 > readyCounter, readyCounter, uopRes0),
//    0.U)
//  validToRename.zipWithIndex.foreach{
//    case(dst, i) =>
//      val validFix = Mux(complexNum.orR, validSimple((i+1).U - complexNum), validSimple(i))
//      dst := MuxCase(false.B, Seq(
//        (io.validFromIBuf(0) && readyCounter.orR && uopRes0 > readyCounter) -> Mux(readyCounter > i.U, true.B, false.B),
//        (io.validFromIBuf(0) && readyCounter.orR && !(uopRes0 > readyCounter)) -> Mux(complexNum > i.U, true.B, validFix && notInfVec(i.U - complexNum) && io.readyFromRename(i)),
//      ).toSeq)
//  }
//
//  readyToIBuf.zipWithIndex.foreach {
//    case (dst, i) =>
//      val readyToIBuf0 = Mux(io.isComplex(0), io.in0pc === io.simple.decodedInst.pc, true.B)
//      dst := MuxCase(true.B, Seq(
//        (io.validFromIBuf(0) && uopRes0 > readyCounter || !readyCounter.orR) -> false.B,
//        (io.validFromIBuf(0) && !(uopRes0 > readyCounter) && readyCounter.orR) -> (if (i==0) readyToIBuf0 else Mux(RenameWidth.U - complexNum >= i.U, notInfVec(i) && validSimple(i) && io.readyFromRename(i), false.B))
//      ).toSeq)
//  }
//
//  io.deq.decodedInsts := decodedInsts
//  io.deq.complexNum := complexNum
//  io.deq.validToRename := validToRename
//  io.deq.readyToIBuf := readyToIBuf
}
