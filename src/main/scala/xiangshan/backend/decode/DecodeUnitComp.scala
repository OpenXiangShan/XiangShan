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
import freechips.rocketchip.rocket.Instructions
import freechips.rocketchip.util.uintToBitPat
import utils._
import utility._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.fu.FuType
import freechips.rocketchip.rocket.Instructions._
import xiangshan.backend.Bundles.{DecodedInst, StaticInst}
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.vector.Bundles.{VSew, VType, VLmul}
import yunsuan.VpermType
import scala.collection.Seq
import chisel3.util.experimental.decode.{QMCMinimizer, TruthTable, decoder}

class indexedLSUopTable(uopIdx:Int) extends Module {
  val src = IO(Input(UInt(7.W)))
  val outOffsetVs2 = IO(Output(UInt(3.W)))
  val outOffsetVd = IO(Output(UInt(3.W)))
  val outIsFirstUopInVd = IO(Output(Bool()))
  def genCsBundle_VEC_INDEXED_LDST(lmul:Int, emul:Int, nfields:Int, uopIdx:Int): (Int, Int, Int) ={
    if (lmul * nfields <= 8) {
      for (k <-0 until nfields) {
        if (lmul < emul) {    // lmul < emul, uop num is depend on emul * nf
          var offset = 1 << (emul - lmul)
          for (i <- 0 until (1 << emul)) {
            if (uopIdx == k * (1 << emul) + i) {
              return (i, i / offset + k * (1 << lmul), if (i % offset == 0) 1 else 0)
            }
          }
        } else {              // lmul > emul, uop num is depend on lmul * nf
          var offset = 1 << (lmul - emul)
          for (i <- 0 until (1 << lmul)) {
            if (uopIdx == k * (1 << lmul) + i) {
              return (i / offset, i + k * (1 << lmul), 1)
            }
          }
        }
      }
    }
    return (0, 0, 1)
  }
  // strided load/store
  var combVemulNf : Seq[(Int, Int, Int, Int, Int, Int)] = Seq()
  for (emul <- 0 until 4) {
    for (lmul <- 0 until 4) {
      for (nf <- 0 until 8) {
        var offset = genCsBundle_VEC_INDEXED_LDST(lmul, emul, nf+1, uopIdx)
        var offsetVs2 = offset._1
        var offsetVd = offset._2
        var isFirstUopInVd = offset._3
        combVemulNf :+= (emul, lmul, nf, isFirstUopInVd, offsetVs2, offsetVd)
      }
    }
  }
  val out = decoder(QMCMinimizer, src, TruthTable(combVemulNf.map {
    case (emul, lmul, nf, isFirstUopInVd, offsetVs2, offsetVd) =>
      (BitPat((emul << 5 | lmul << 3 | nf).U(7.W)), BitPat((isFirstUopInVd << 6 | offsetVs2 << 3 | offsetVd).U(7.W)))
  }, BitPat.N(7)))
  outOffsetVs2 := out(5, 3)
  outOffsetVd := out(2, 0)
  outIsFirstUopInVd := out(6).asBool
}

trait VectorConstants {
  val MAX_VLMUL = 8
  val FP_TMP_REG_MV = 32
  val VECTOR_TMP_REG_LMUL = 33 // 33~47  ->  15
  val MAX_INDEXED_LS_UOPNUM = 64
}

class DecodeUnitCompIO(implicit p: Parameters) extends XSBundle {
  val redirect = Input(Bool())
  val csrCtrl = Input(new CustomCSRCtrlIO)
  val vtypeBypass = Input(new VType)
  // When the first inst in decode vector is complex inst, pass it in
  val in = Flipped(DecoupledIO(new Bundle {
    val simpleDecodedInst = new DecodedInst
    val uopInfo = new UopInfo
  }))
  val out = new Bundle {
    val complexDecodedInsts = Vec(RenameWidth, DecoupledIO(new DecodedInst))
  }
  val complexNum = Output(UInt(3.W))
}

/**
  * @author zly
  */
class DecodeUnitComp()(implicit p : Parameters) extends XSModule with DecodeUnitConstants with VectorConstants {
  val io = IO(new DecodeUnitCompIO)

  // alias
  private val inReady = io.in.ready
  private val inValid = io.in.valid
  private val inDecodedInst = WireInit(io.in.bits.simpleDecodedInst)
  private val inInstFields = io.in.bits.simpleDecodedInst.instr.asTypeOf(new XSInstBitFields)
  private val inUopInfo = io.in.bits.uopInfo
  private val outValids = io.out.complexDecodedInsts.map(_.valid)
  private val outReadys = io.out.complexDecodedInsts.map(_.ready)
  private val outDecodedInsts = io.out.complexDecodedInsts.map(_.bits)
  private val outComplexNum = io.complexNum

  val maxUopSize = MaxUopSize
  when (io.in.fire && io.in.bits.simpleDecodedInst.isVset) {
    when(inInstFields.RD === 0.U && inInstFields.RS1 === 0.U) {
      inDecodedInst.fuOpType := VSETOpType.keepVl(io.in.bits.simpleDecodedInst.fuOpType)
    }.elsewhen(inInstFields.RS1 === 0.U) {
      inDecodedInst.fuOpType := VSETOpType.setVlmax(io.in.bits.simpleDecodedInst.fuOpType)
    }
  }

  val latchedInst = RegEnable(inDecodedInst, inValid && inReady)
  val latchedUopInfo = RegEnable(inUopInfo, inValid && inReady)
  //input bits
  private val instFields: XSInstBitFields = latchedInst.instr.asTypeOf(new XSInstBitFields)

  val src1 = Cat(0.U(1.W), instFields.RS1)
  val src2 = Cat(0.U(1.W), instFields.RS2)
  val dest = Cat(0.U(1.W), instFields.RD)

  val nf    = instFields.NF
  val width = instFields.WIDTH(1, 0)

  //output of DecodeUnit
  val numOfUop = Wire(UInt(log2Up(maxUopSize).W))
  val numOfWB = Wire(UInt(log2Up(maxUopSize).W))
  val lmul = Wire(UInt(4.W))
  val isVsetSimple = Wire(Bool())

  val indexedLSRegOffset = Seq.tabulate(MAX_INDEXED_LS_UOPNUM)(i => Module(new indexedLSUopTable(i)))
  indexedLSRegOffset.map(_.src := 0.U)

  //pre decode
  lmul := latchedUopInfo.lmul
  isVsetSimple := latchedInst.isVset
  val vlmulReg = latchedInst.vpu.vlmul
  val vsewReg = latchedInst.vpu.vsew

  //Type of uop Div
  val typeOfSplit = latchedInst.uopSplitType
  val src1Type = latchedInst.srcType(0)
  val src1IsImm = src1Type === SrcType.imm
  val src1IsFp = src1Type === SrcType.fp

  numOfUop := latchedUopInfo.numOfUop
  numOfWB := latchedUopInfo.numOfWB

  //uops dispatch
  val s_idle :: s_active :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val stateNext = WireDefault(state)
  val numDecodedUop = RegInit(0.U(log2Up(maxUopSize).W))
  val uopRes = RegInit(0.U(log2Up(maxUopSize).W))
  val uopResNext = WireInit(uopRes)
  val e64 = 3.U(2.W)

  //uop div up to maxUopSize
  val csBundle = Wire(Vec(maxUopSize, new DecodedInst))
  csBundle.foreach { case dst =>
    dst := latchedInst
    dst.numUops := latchedUopInfo.numOfUop
    dst.numWB := latchedUopInfo.numOfWB
    dst.firstUop := false.B
    dst.lastUop := false.B
    dst.vlsInstr := false.B
  }

  csBundle(0).firstUop := true.B
  csBundle(numOfUop - 1.U).lastUop := true.B

  switch(typeOfSplit) {
    is(UopSplitType.VSET) {
      // In simple decoder, rfWen and vecWen are not set
      when(isVsetSimple) {
        // Default
        // uop0 set rd, never flushPipe
        csBundle(0).fuType := FuType.vsetiwi.U
        csBundle(0).flushPipe := false.B
        csBundle(0).rfWen := true.B
        // uop1 set vl, vsetvl will flushPipe
        csBundle(1).ldest := VCONFIG_IDX.U
        csBundle(1).vecWen := true.B
        when(VSETOpType.isVsetvli(latchedInst.fuOpType) && dest === 0.U && src1 === 0.U) {
          csBundle(1).fuType := FuType.vsetfwf.U
          csBundle(1).srcType(0) := SrcType.vp
          csBundle(1).lsrc(0) := VCONFIG_IDX.U
        }.elsewhen(VSETOpType.isVsetvl(latchedInst.fuOpType) && dest === 0.U && src1 === 0.U) {
          // uop0: mv vtype gpr to vector region
          csBundle(0).srcType(0) := SrcType.xp
          csBundle(0).srcType(1) := SrcType.no
          csBundle(0).lsrc(1) := 0.U
          csBundle(0).ldest := FP_TMP_REG_MV.U
          csBundle(0).fuType := FuType.i2v.U
          csBundle(0).fuOpType := Cat(IF2VectorType.i2Vec(2, 0), e64)
          csBundle(0).rfWen := false.B
          csBundle(0).fpWen := true.B
          csBundle(0).vecWen := false.B
          csBundle(0).flushPipe := false.B
          // uop1: uvsetvcfg_vv
          csBundle(1).fuType := FuType.vsetfwf.U
          // vl
          csBundle(1).srcType(0) := SrcType.vp
          csBundle(1).lsrc(0) := VCONFIG_IDX.U
          // vtype
          csBundle(1).srcType(1) := SrcType.fp
          csBundle(1).lsrc(1) := FP_TMP_REG_MV.U
          csBundle(1).vecWen := true.B
          csBundle(1).ldest := VCONFIG_IDX.U
        }
        // use bypass vtype from vtypeGen
        csBundle(0).vpu.connectVType(io.vtypeBypass)
        csBundle(1).vpu.connectVType(io.vtypeBypass)
      }
    }
    is(UopSplitType.VEC_VVV) {
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i).lsrc(0) := src1 + i.U
        csBundle(i).lsrc(1) := src2 + i.U
        csBundle(i).lsrc(2) := dest + i.U
        csBundle(i).ldest := dest + i.U
        csBundle(i).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_VFV) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.fp
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.f2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
      csBundle(0).vecWen := true.B
      csBundle(0).vpu.isReverse := false.B
      /*
      LMUL
       */
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.vp
        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).lsrc(2) := dest + i.U
        csBundle(i + 1).ldest := dest + i.U
        csBundle(i + 1).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_EXT2) {
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).lsrc(1) := src2 + i.U
        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ldest := dest + (2 * i).U
        csBundle(2 * i).uopIdx := (2 * i).U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_EXT4) {
      for (i <- 0 until MAX_VLMUL / 4) {
        csBundle(4 * i).lsrc(1) := src2 + i.U
        csBundle(4 * i).lsrc(2) := dest + (4 * i).U
        csBundle(4 * i).ldest := dest + (4 * i).U
        csBundle(4 * i).uopIdx := (4 * i).U
        csBundle(4 * i + 1).lsrc(1) := src2 + i.U
        csBundle(4 * i + 1).lsrc(2) := dest + (4 * i + 1).U
        csBundle(4 * i + 1).ldest := dest + (4 * i + 1).U
        csBundle(4 * i + 1).uopIdx := (4 * i + 1).U
        csBundle(4 * i + 2).lsrc(1) := src2 + i.U
        csBundle(4 * i + 2).lsrc(2) := dest + (4 * i + 2).U
        csBundle(4 * i + 2).ldest := dest + (4 * i + 2).U
        csBundle(4 * i + 2).uopIdx := (4 * i + 2).U
        csBundle(4 * i + 3).lsrc(1) := src2 + i.U
        csBundle(4 * i + 3).lsrc(2) := dest + (4 * i + 3).U
        csBundle(4 * i + 3).ldest := dest + (4 * i + 3).U
        csBundle(4 * i + 3).uopIdx := (4 * i + 3).U
      }
    }
    is(UopSplitType.VEC_EXT8) {
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i).lsrc(1) := src2
        csBundle(i).lsrc(2) := dest + i.U
        csBundle(i).ldest := dest + i.U
        csBundle(i).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_0XV) {
      /*
      i/f to vector move
       */
      csBundle(0).srcType(0) := Mux(src1IsFp, SrcType.fp, SrcType.reg)
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := Mux(src1IsFp, FuType.f2v.U, FuType.i2v.U)
      csBundle(0).fuOpType := Cat(Mux(src1IsFp, IF2VectorType.fDup2Vec(2, 0), IF2VectorType.i2Vec(2, 0)), vsewReg)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := true.B
      /*
      vmv.s.x
       */
      csBundle(1).srcType(0) := SrcType.vp
      csBundle(1).srcType(1) := SrcType.imm
      csBundle(1).srcType(2) := SrcType.vp
      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
      csBundle(1).lsrc(1) := 0.U
      csBundle(1).lsrc(2) := dest
      csBundle(1).ldest := dest
      csBundle(1).rfWen := false.B
      csBundle(1).fpWen := false.B
      csBundle(1).vecWen := true.B
      csBundle(1).uopIdx := 0.U
    }
    is(UopSplitType.VEC_VXV) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.immDup2Vec(2, 0), IF2VectorType.iDup2Vec(2, 0)), vsewReg)
      csBundle(0).vecWen := true.B
      csBundle(0).vpu.isReverse := false.B
      /*
      LMUL
       */
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.vp
        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).lsrc(2) := dest + i.U
        csBundle(i + 1).ldest := dest + i.U
        csBundle(i + 1).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_VVW) {
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).lsrc(0) := src1 + i.U
        csBundle(2 * i).lsrc(1) := src2 + i.U
        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ldest := dest + (2 * i).U
        csBundle(2 * i).uopIdx := (2 * i).U
        csBundle(2 * i + 1).lsrc(0) := src1 + i.U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_VFW) {
      /*
      f to vector move
       */
      csBundle(0).srcType(0) := SrcType.fp
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.f2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := true.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ldest := dest + (2 * i).U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.vp
        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 2).lsrc(1) := src2 + i.U
        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_WVW) {
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).lsrc(0) := src1 + i.U
        csBundle(2 * i).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ldest := dest + (2 * i).U
        csBundle(2 * i).uopIdx := (2 * i).U
        csBundle(2 * i + 1).lsrc(0) := src1 + i.U
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_VXW) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.iDup2Vec(2, 0), vsewReg)
      csBundle(0).vecWen := true.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ldest := dest + (2 * i).U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.vp
        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 2).lsrc(1) := src2 + i.U
        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_WXW) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.iDup2Vec(2, 0), vsewReg)
      csBundle(0).vecWen := true.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ldest := dest + (2 * i).U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.vp
        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 2).lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_WVV) {
      for (i <- 0 until MAX_VLMUL / 2) {

        csBundle(2 * i).lsrc(0) := src1 + i.U
        csBundle(2 * i).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i).lsrc(2) := dest + i.U
        csBundle(2 * i).ldest := dest + i.U
        csBundle(2 * i).uopIdx := (2 * i).U
        csBundle(2 * i + 1).lsrc(0) := src1 + i.U
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 1).lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ldest := dest + i.U
        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_WFW) {
      /*
      f to vector move
       */
      csBundle(0).srcType(0) := SrcType.fp
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.f2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := true.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ldest := dest + (2 * i).U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.vp
        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 2).lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_WXV) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.immDup2Vec(2, 0), IF2VectorType.iDup2Vec(2, 0)), vsewReg)
      csBundle(0).vecWen := true.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i + 1).lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ldest := dest + i.U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.vp
        csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 2).lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 2).lsrc(2) := dest + i.U
        csBundle(2 * i + 2).ldest := dest + i.U
        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_VVM) {
      csBundle(0).lsrc(2) := dest
      csBundle(0).ldest := dest
      csBundle(0).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i).lsrc(0) := src1 + i.U
        csBundle(i).lsrc(1) := src2 + i.U
        csBundle(i).lsrc(2) := dest
        csBundle(i).ldest := dest
        csBundle(i).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_VFM) {
      /*
      f to vector move
       */
      csBundle(0).srcType(0) := SrcType.fp
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.f2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := true.B
      //LMUL
      csBundle(1).srcType(0) := SrcType.vp
      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
      csBundle(1).lsrc(2) := dest
      csBundle(1).ldest := dest
      csBundle(1).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.vp
        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).lsrc(2) := dest
        csBundle(i + 1).ldest := dest
        csBundle(i + 1).uopIdx := i.U
      }
      csBundle(numOfUop - 1.U).ldest := dest
    }
    is(UopSplitType.VEC_VXM) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.immDup2Vec(2, 0), IF2VectorType.iDup2Vec(2, 0)), vsewReg)
      csBundle(0).vecWen := true.B
      //LMUL
      csBundle(1).srcType(0) := SrcType.vp
      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
      csBundle(1).lsrc(2) := dest
      csBundle(1).ldest := dest
      csBundle(1).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.vp
        csBundle(i + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).lsrc(2) := dest
        csBundle(i + 1).ldest := dest
        csBundle(i + 1).uopIdx := i.U
      }
      csBundle(numOfUop - 1.U).ldest := dest
    }
    is(UopSplitType.VEC_SLIDE1UP) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.iDup2Vec(2, 0), vsewReg)
      csBundle(0).vecWen := true.B
      //LMUL
      csBundle(1).srcType(0) := SrcType.vp
      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
      csBundle(1).lsrc(2) := dest
      csBundle(1).ldest := dest
      csBundle(1).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.vp
        csBundle(i + 1).lsrc(0) := src2 + (i - 1).U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).lsrc(2) := dest + i.U
        csBundle(i + 1).ldest := dest + i.U
        csBundle(i + 1).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_FSLIDE1UP) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.fp
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.f2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := true.B
      //LMUL
      csBundle(1).srcType(0) := SrcType.vp
      csBundle(1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
      csBundle(1).lsrc(1) := src2
      csBundle(1).lsrc(2) := dest
      csBundle(1).ldest := dest
      csBundle(1).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.vp
        csBundle(i + 1).lsrc(0) := src2 + (i - 1).U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).lsrc(2) := dest + i.U
        csBundle(i + 1).ldest := dest + i.U
        csBundle(i + 1).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_SLIDE1DOWN) { // lmul+lmul = 16
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.iDup2Vec(2, 0), vsewReg)
      csBundle(0).vecWen := true.B
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).srcType(1) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := src2 + (i + 1).U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ldest := VECTOR_TMP_REG_LMUL.U + 1.U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        if (2 * i + 2 < MAX_VLMUL * 2) {
          csBundle(2 * i + 2).srcType(0) := SrcType.vp
          csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
          // csBundle(2 * i + 2).lsrc(1) := src2 + i.U         // DontCare
          csBundle(2 * i + 2).lsrc(2) := VECTOR_TMP_REG_LMUL.U + 1.U
          csBundle(2 * i + 2).ldest := dest + i.U
          csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
        }
      }
      csBundle(numOfUop - 1.U).srcType(0) := SrcType.vp
      csBundle(numOfUop - 1.U).lsrc(0) := VECTOR_TMP_REG_LMUL.U
      csBundle(numOfUop - 1.U).ldest := dest + lmul - 1.U
    }
    is(UopSplitType.VEC_FSLIDE1DOWN) {
      /*
      i to vector move
       */
      csBundle(0).srcType(0) := SrcType.fp
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.f2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.fDup2Vec(2, 0), vsewReg)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := true.B
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).srcType(1) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := src2 + (i + 1).U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ldest := VECTOR_TMP_REG_LMUL.U + 1.U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        if (2 * i + 2 < MAX_VLMUL * 2) {
          csBundle(2 * i + 2).srcType(0) := SrcType.vp
          csBundle(2 * i + 2).lsrc(0) := VECTOR_TMP_REG_LMUL.U
          // csBundle(2 * i + 2).lsrc(1) := src2 + i.U         // DontCare
          csBundle(2 * i + 2).lsrc(2) := VECTOR_TMP_REG_LMUL.U + 1.U
          csBundle(2 * i + 2).ldest := dest + i.U
          csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
        }
      }
      csBundle(numOfUop - 1.U).srcType(0) := SrcType.vp
      csBundle(numOfUop - 1.U).lsrc(0) := VECTOR_TMP_REG_LMUL.U
      csBundle(numOfUop - 1.U).ldest := dest + lmul - 1.U
    }
    is(UopSplitType.VEC_VRED) {
      when(vlmulReg === "b001".U) {
        csBundle(0).srcType(2) := SrcType.DC
        csBundle(0).lsrc(0) := src2 + 1.U
        csBundle(0).lsrc(1) := src2
        csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(0).uopIdx := 0.U
      }
      when(vlmulReg === "b010".U) {
        csBundle(0).srcType(2) := SrcType.DC
        csBundle(0).lsrc(0) := src2 + 1.U
        csBundle(0).lsrc(1) := src2
        csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(0).uopIdx := 0.U

        csBundle(1).srcType(2) := SrcType.DC
        csBundle(1).lsrc(0) := src2 + 3.U
        csBundle(1).lsrc(1) := src2 + 2.U
        csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
        csBundle(1).uopIdx := 1.U

        csBundle(2).srcType(2) := SrcType.DC
        csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
        csBundle(2).lsrc(1) := VECTOR_TMP_REG_LMUL.U
        csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
        csBundle(2).uopIdx := 2.U
      }
      when(vlmulReg === "b011".U) {
        for (i <- 0 until MAX_VLMUL) {
          if (i < MAX_VLMUL - MAX_VLMUL / 2) {
            csBundle(i).lsrc(0) := src2 + (i * 2 + 1).U
            csBundle(i).lsrc(1) := src2 + (i * 2).U
            csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
          } else if (i < MAX_VLMUL - MAX_VLMUL / 4) {
            csBundle(i).lsrc(0) := (VECTOR_TMP_REG_LMUL + (i - MAX_VLMUL / 2) * 2 + 1).U
            csBundle(i).lsrc(1) := (VECTOR_TMP_REG_LMUL + (i - MAX_VLMUL / 2) * 2).U
            csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
          } else if (i < MAX_VLMUL - MAX_VLMUL / 8) {
            csBundle(6).lsrc(0) := (VECTOR_TMP_REG_LMUL + 5).U
            csBundle(6).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
            csBundle(6).ldest := (VECTOR_TMP_REG_LMUL + 6).U
          }
          csBundle(i).srcType(2) := SrcType.DC
          csBundle(i).uopIdx := i.U
        }
      }
      when(vlmulReg(2) === 0.U && vlmulReg(1, 0).orR) {
        /*
         * 2 <= vlmul <= 8
         */
        csBundle(numOfUop - 1.U).srcType(2) := SrcType.vp
        csBundle(numOfUop - 1.U).lsrc(0) := src1
        csBundle(numOfUop - 1.U).lsrc(1) := VECTOR_TMP_REG_LMUL.U + numOfUop - 2.U
        csBundle(numOfUop - 1.U).lsrc(2) := dest
        csBundle(numOfUop - 1.U).ldest := dest
        csBundle(numOfUop - 1.U).uopIdx := numOfUop - 1.U
      }
    }
    is(UopSplitType.VEC_VFRED) {
      val vlmul = vlmulReg
      val vsew = vsewReg
      when(vlmul === VLmul.m8){
        for (i <- 0 until 4) {
          csBundle(i).lsrc(0) := src2 + (i * 2 + 1).U
          csBundle(i).lsrc(1) := src2 + (i * 2).U
          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
          csBundle(i).uopIdx := i.U
        }
        for (i <- 4 until 6) {
          csBundle(i).lsrc(0) := (VECTOR_TMP_REG_LMUL + (i - 4) * 2 + 1).U
          csBundle(i).lsrc(1) := (VECTOR_TMP_REG_LMUL + (i - 4) * 2).U
          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
          csBundle(i).uopIdx := i.U
        }
        csBundle(6).lsrc(0) := (VECTOR_TMP_REG_LMUL + 5).U
        csBundle(6).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
        csBundle(6).ldest := (VECTOR_TMP_REG_LMUL + 6).U
        csBundle(6).uopIdx := 6.U
        when(vsew === VSew.e64) {
          csBundle(7).lsrc(0) := (VECTOR_TMP_REG_LMUL + 6).U
          csBundle(7).lsrc(1) := (VECTOR_TMP_REG_LMUL + 6).U
          csBundle(7).ldest := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(7).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(7).uopIdx := 7.U
          csBundle(8).lsrc(0) := src1
          csBundle(8).lsrc(1) := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(8).ldest := dest
          csBundle(8).uopIdx := 8.U
        }
        when(vsew === VSew.e32) {
          csBundle(7).lsrc(0) := (VECTOR_TMP_REG_LMUL + 6).U
          csBundle(7).lsrc(1) := (VECTOR_TMP_REG_LMUL + 6).U
          csBundle(7).ldest := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(7).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(7).uopIdx := 7.U
          csBundle(8).lsrc(0) := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(8).lsrc(1) := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(8).ldest := (VECTOR_TMP_REG_LMUL + 8).U
          csBundle(8).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(8).uopIdx := 8.U
          csBundle(9).lsrc(0) := src1
          csBundle(9).lsrc(1) := (VECTOR_TMP_REG_LMUL + 8).U
          csBundle(9).ldest := dest
          csBundle(9).uopIdx := 9.U
        }
        when(vsew === VSew.e16) {
          csBundle(7).lsrc(0) := (VECTOR_TMP_REG_LMUL + 6).U
          csBundle(7).lsrc(1) := (VECTOR_TMP_REG_LMUL + 6).U
          csBundle(7).ldest := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(7).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(7).uopIdx := 7.U
          csBundle(8).lsrc(0) := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(8).lsrc(1) := (VECTOR_TMP_REG_LMUL + 7).U
          csBundle(8).ldest := (VECTOR_TMP_REG_LMUL + 8).U
          csBundle(8).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(8).uopIdx := 8.U
          csBundle(9).lsrc(0) := (VECTOR_TMP_REG_LMUL + 8).U
          csBundle(9).lsrc(1) := (VECTOR_TMP_REG_LMUL + 8).U
          csBundle(9).ldest := (VECTOR_TMP_REG_LMUL + 9).U
          csBundle(9).vpu.fpu.isFoldTo1_8 := true.B
          csBundle(9).uopIdx := 9.U
          csBundle(10).lsrc(0) := src1
          csBundle(10).lsrc(1) := (VECTOR_TMP_REG_LMUL + 9).U
          csBundle(10).ldest := dest
          csBundle(10).uopIdx := 10.U
        }
      }
      when(vlmul === VLmul.m4) {
        for (i <- 0 until 2) {
          csBundle(i).lsrc(0) := src2 + (i * 2 + 1).U
          csBundle(i).lsrc(1) := src2 + (i * 2).U
          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
          csBundle(i).uopIdx := i.U
        }
        csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
        csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
        csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
        csBundle(2).uopIdx := 2.U
        when(vsew === VSew.e64) {
          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(3).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(3).uopIdx := 3.U
          csBundle(4).lsrc(0) := src1
          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(4).ldest := dest
          csBundle(4).uopIdx := 4.U
        }
        when(vsew === VSew.e32) {
          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(3).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(3).uopIdx := 3.U
          csBundle(4).lsrc(0) := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(4).ldest := (VECTOR_TMP_REG_LMUL + 4).U
          csBundle(4).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(4).uopIdx := 4.U
          csBundle(5).lsrc(0) := src1
          csBundle(5).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
          csBundle(5).ldest := dest
          csBundle(5).uopIdx := 5.U
        }
        when(vsew === VSew.e16) {
          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(3).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(3).uopIdx := 3.U
          csBundle(4).lsrc(0) := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(4).ldest := (VECTOR_TMP_REG_LMUL + 4).U
          csBundle(4).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(4).uopIdx := 4.U
          csBundle(5).lsrc(0) := (VECTOR_TMP_REG_LMUL + 4).U
          csBundle(5).lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
          csBundle(5).ldest := (VECTOR_TMP_REG_LMUL + 5).U
          csBundle(5).vpu.fpu.isFoldTo1_8 := true.B
          csBundle(5).uopIdx := 5.U
          csBundle(6).lsrc(0) := src1
          csBundle(6).lsrc(1) := (VECTOR_TMP_REG_LMUL + 5).U
          csBundle(6).ldest := dest
          csBundle(6).uopIdx := 6.U
        }
      }
      when(vlmul === VLmul.m2) {
        csBundle(0).lsrc(0) := src2 + 1.U
        csBundle(0).lsrc(1) := src2 + 0.U
        csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
        csBundle(0).uopIdx := 0.U
        when(vsew === VSew.e64) {
          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(1).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(1).uopIdx := 1.U
          csBundle(2).lsrc(0) := src1
          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).ldest := dest
          csBundle(2).uopIdx := 2.U
        }
        when(vsew === VSew.e32) {
          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(1).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(1).uopIdx := 1.U
          csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(2).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(2).uopIdx := 2.U
          csBundle(3).lsrc(0) := src1
          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).ldest := dest
          csBundle(3).uopIdx := 3.U
        }
        when(vsew === VSew.e16) {
          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(1).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(1).uopIdx := 1.U
          csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(2).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(2).uopIdx := 2.U
          csBundle(3).lsrc(0) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).ldest := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(3).vpu.fpu.isFoldTo1_8 := true.B
          csBundle(3).uopIdx := 3.U
          csBundle(4).lsrc(0) := src1
          csBundle(4).lsrc(1) := (VECTOR_TMP_REG_LMUL + 3).U
          csBundle(4).ldest := dest
          csBundle(4).uopIdx := 4.U
        }
      }
      when(vlmul === VLmul.m1) {
        when(vsew === VSew.e64) {
          csBundle(0).lsrc(0) := src2
          csBundle(0).lsrc(1) := src2
          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(0).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(0).uopIdx := 0.U
          csBundle(1).lsrc(0) := src1
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := dest
          csBundle(1).uopIdx := 1.U
        }
        when(vsew === VSew.e32) {
          csBundle(0).lsrc(0) := src2
          csBundle(0).lsrc(1) := src2
          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(0).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(0).uopIdx := 0.U
          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(1).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(1).uopIdx := 1.U
          csBundle(2).lsrc(0) := src1
          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).ldest := dest
          csBundle(2).uopIdx := 2.U
        }
        when(vsew === VSew.e16) {
          csBundle(0).lsrc(0) := src2
          csBundle(0).lsrc(1) := src2
          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(0).vpu.fpu.isFoldTo1_2 := true.B
          csBundle(0).uopIdx := 0.U
          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(1).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(1).uopIdx := 1.U
          csBundle(2).lsrc(0) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).ldest := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(2).vpu.fpu.isFoldTo1_8 := true.B
          csBundle(2).uopIdx := 2.U
          csBundle(3).lsrc(0) := src1
          csBundle(3).lsrc(1) := (VECTOR_TMP_REG_LMUL + 2).U
          csBundle(3).ldest := dest
          csBundle(3).uopIdx := 3.U
        }
      }
      when(vlmul === VLmul.mf2) {
        when(vsew === VSew.e32) {
          csBundle(0).lsrc(0) := src2
          csBundle(0).lsrc(1) := src2
          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(0).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(0).uopIdx := 0.U
          csBundle(1).lsrc(0) := src1
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := dest
          csBundle(1).uopIdx := 1.U
        }
        when(vsew === VSew.e16) {
          csBundle(0).lsrc(0) := src2
          csBundle(0).lsrc(1) := src2
          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(0).vpu.fpu.isFoldTo1_4 := true.B
          csBundle(0).uopIdx := 0.U
          csBundle(1).lsrc(0) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(1).vpu.fpu.isFoldTo1_8 := true.B
          csBundle(1).uopIdx := 1.U
          csBundle(2).lsrc(0) := src1
          csBundle(2).lsrc(1) := (VECTOR_TMP_REG_LMUL + 1).U
          csBundle(2).ldest := dest
          csBundle(2).uopIdx := 2.U
        }
      }
      when(vlmul === VLmul.mf4) {
        when(vsew === VSew.e16) {
          csBundle(0).lsrc(0) := src2
          csBundle(0).lsrc(1) := src2
          csBundle(0).ldest := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(0).vpu.fpu.isFoldTo1_8 := true.B
          csBundle(0).uopIdx := 0.U
          csBundle(1).lsrc(0) := src1
          csBundle(1).lsrc(1) := (VECTOR_TMP_REG_LMUL + 0).U
          csBundle(1).ldest := dest
          csBundle(1).uopIdx := 1.U
        }
      }
    }

    is(UopSplitType.VEC_VFREDOSUM) {
      import yunsuan.VfaluType
      val vlmul = vlmulReg
      val vsew = vsewReg
      val isWiden = latchedInst.fuOpType === VfaluType.vfwredosum
      when(vlmul === VLmul.m8) {
        when(vsew === VSew.e64) {
          val vlmax = 16
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e32) {
          val vlmax = 32
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_4 := (if (i % 4 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e16) {
          val vlmax = 64
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_8 := (if (i % 8 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
      }
      when(vlmul === VLmul.m4) {
        when(vsew === VSew.e64) {
          val vlmax = 8
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e32) {
          val vlmax = 16
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e16) {
          val vlmax = 32
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
      }
      when(vlmul === VLmul.m2) {
        when(vsew === VSew.e64) {
          val vlmax = 4
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e32) {
          val vlmax = 8
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e16) {
          val vlmax = 16
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
      }
      when(vlmul === VLmul.m1) {
        when(vsew === VSew.e64) {
          val vlmax = 2
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 2 == 0) src2 + (i/2).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 2 == 0) src2 + (i/2).U else if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := (if (i % 2 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e32) {
          val vlmax = 4
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e16) {
          val vlmax = 8
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
      }
      when(vlmul === VLmul.mf2) {
        when(vsew === VSew.e32) {
          val vlmax = 2
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 4 == 0) src2 + (i/4).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 4 == 0) src2 + (i/4).U else if (i == vlmax - 1) dest else if (i % 4 == 1) Mux(isWiden, src2 + (i/4).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_2 := isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_4 := !isWiden && (if (i % 4 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
        when(vsew === VSew.e16) {
          val vlmax = 4
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
      }
      when(vlmul === VLmul.mf4) {
        when(vsew === VSew.e16) {
          val vlmax = 2
          for (i <- 0 until vlmax) {
            csBundle(i).lsrc(0) := (if (i == 0) src1 else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(1) := (if (i % 8 == 0) src2 + (i/8).U else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).lsrc(2) := (if (i % 8 == 0) src2 + (i/8).U else if (i == vlmax - 1) dest else if (i % 8 == 1) Mux(isWiden, src2 + (i/8).U, VECTOR_TMP_REG_LMUL.U) else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).ldest := (if (i == vlmax - 1) dest else VECTOR_TMP_REG_LMUL.U)
            csBundle(i).vpu.fpu.isFoldTo1_4 := isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).vpu.fpu.isFoldTo1_8 := !isWiden && (if (i % 8 == 0) false.B else true.B)
            csBundle(i).uopIdx := i.U
          }
        }
      }
    }

    is(UopSplitType.VEC_SLIDEUP) {
      // i to vector move
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.imm2Vec(2, 0), IF2VectorType.i2Vec(2, 0)), vsewReg)
      csBundle(0).vecWen := true.B
      // LMUL
      for (i <- 0 until MAX_VLMUL)
        for (j <- 0 to i) {
          val old_vd = if (j == 0) {
            dest + i.U
          } else (VECTOR_TMP_REG_LMUL + j).U
          val vd = if (j == i) {
            dest + i.U
          } else (VECTOR_TMP_REG_LMUL + j + 1).U
          csBundle(i * (i + 1) / 2 + j + 1).srcType(0) := SrcType.vp
          csBundle(i * (i + 1) / 2 + j + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
          csBundle(i * (i + 1) / 2 + j + 1).lsrc(1) := src2 + j.U
          csBundle(i * (i + 1) / 2 + j + 1).lsrc(2) := old_vd
          csBundle(i * (i + 1) / 2 + j + 1).ldest := vd
          csBundle(i * (i + 1) / 2 + j + 1).uopIdx := (i * (i + 1) / 2 + j).U
        }
    }

    is(UopSplitType.VEC_SLIDEDOWN) {
      // i to vector move
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.imm2Vec(2, 0), IF2VectorType.i2Vec(2, 0)), vsewReg)
      csBundle(0).vecWen := true.B
      // LMUL
      for (i <- 0 until MAX_VLMUL)
        for (j <- (0 to i).reverse) {
          when(i.U < lmul) {
            val old_vd = if (j == 0) {
              dest + lmul - 1.U - i.U
            } else (VECTOR_TMP_REG_LMUL + j).U
            val vd = if (j == i) {
              dest + lmul - 1.U - i.U
            } else (VECTOR_TMP_REG_LMUL + j + 1).U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).srcType(0) := SrcType.vp
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(0) := VECTOR_TMP_REG_LMUL.U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(1) := src2 + lmul - 1.U - j.U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(2) := old_vd
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).ldest := vd
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).uopIdx := numOfUop - (i * (i + 1) / 2 + i - j + 2).U
          }
        }
    }

    is(UopSplitType.VEC_M0X) {
      // LMUL
      for (i <- 0 until MAX_VLMUL) {
        val srcType0 = if (i == 0) SrcType.DC else SrcType.vp
        val ldest = (VECTOR_TMP_REG_LMUL + i).U
        csBundle(i).srcType(0) := srcType0
        csBundle(i).srcType(1) := SrcType.vp
        csBundle(i).rfWen := false.B
        csBundle(i).fpWen := false.B
        csBundle(i).vecWen := true.B
        csBundle(i).lsrc(0) := (VECTOR_TMP_REG_LMUL + i - 1).U
        csBundle(i).lsrc(1) := src2
        // csBundle(i).lsrc(2) := dest + i.U  DontCare
        csBundle(i).ldest := ldest
        csBundle(i).uopIdx := i.U
      }
      csBundle(lmul - 1.U).rfWen := true.B
      csBundle(lmul - 1.U).fpWen := false.B
      csBundle(lmul - 1.U).vecWen := false.B
      csBundle(lmul - 1.U).ldest := dest
    }

    is(UopSplitType.VEC_MVV) {
      // LMUL
      for (i <- 0 until MAX_VLMUL) {
        val srcType0 = if (i == 0) SrcType.DC else SrcType.vp
        csBundle(i * 2 + 0).srcType(0) := srcType0
        csBundle(i * 2 + 0).srcType(1) := SrcType.vp
        csBundle(i * 2 + 0).lsrc(0) := (VECTOR_TMP_REG_LMUL + i - 1).U
        csBundle(i * 2 + 0).lsrc(1) := src2
        csBundle(i * 2 + 0).lsrc(2) := dest + i.U
        csBundle(i * 2 + 0).ldest := dest + i.U
        csBundle(i * 2 + 0).uopIdx := (i * 2 + 0).U

        csBundle(i * 2 + 1).srcType(0) := srcType0
        csBundle(i * 2 + 1).srcType(1) := SrcType.vp
        csBundle(i * 2 + 1).lsrc(0) := (VECTOR_TMP_REG_LMUL + i - 1).U
        csBundle(i * 2 + 1).lsrc(1) := src2
        // csBundle(i).lsrc(2) := dest + i.U  DontCare
        csBundle(i * 2 + 1).ldest := (VECTOR_TMP_REG_LMUL + i).U
        csBundle(i * 2 + 1).uopIdx := (i * 2 + 1).U
      }
    }

    is(UopSplitType.VEC_M0X_VFIRST) {
      // LMUL
      csBundle(0).rfWen := true.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := false.B
      csBundle(0).ldest := dest
    }
    is(UopSplitType.VEC_VWW) {
      for (i <- 0 until MAX_VLMUL*2) {
        when(i.U < lmul){
          csBundle(i).srcType(2) := SrcType.DC
          csBundle(i).lsrc(0) := src2 + i.U
          csBundle(i).lsrc(1) := src2 + i.U
          // csBundle(i).lsrc(2) := dest + (2 * i).U
          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
          csBundle(i).uopIdx :=  i.U
        } otherwise {
          csBundle(i).srcType(2) := SrcType.DC
          csBundle(i).lsrc(0) := VECTOR_TMP_REG_LMUL.U + Cat((i.U-lmul),0.U(1.W)) + 1.U
          csBundle(i).lsrc(1) := VECTOR_TMP_REG_LMUL.U + Cat((i.U-lmul),0.U(1.W))
          // csBundle(i).lsrc(2) := dest + (2 * i).U
          csBundle(i).ldest := (VECTOR_TMP_REG_LMUL + i).U
          csBundle(i).uopIdx := i.U
        }
        csBundle(numOfUop-1.U).srcType(2) := SrcType.vp
        csBundle(numOfUop-1.U).lsrc(0) := src1
        csBundle(numOfUop-1.U).lsrc(2) := dest
        csBundle(numOfUop-1.U).ldest := dest
      }
    }
    is(UopSplitType.VEC_RGATHER) {
      def genCsBundle_VEC_RGATHER(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            // csBundle(i * len + j).srcType(0) := SrcType.vp // SrcType.imm
            // csBundle(i * len + j).srcType(1) := SrcType.vp
            // csBundle(i * len + j).srcType(2) := SrcType.vp
            csBundle(i * len + j).lsrc(0) := src1 + i.U
            csBundle(i * len + j).lsrc(1) := src2 + j.U
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j - 1).U
            csBundle(i * len + j).lsrc(2) := vd_old
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j).ldest := vd
            csBundle(i * len + j).uopIdx := (i * len + j).U
          }
      }
      switch(vlmulReg) {
        is("b001".U ){
          genCsBundle_VEC_RGATHER(2)
        }
        is("b010".U ){
          genCsBundle_VEC_RGATHER(4)
        }
        is("b011".U ){
          genCsBundle_VEC_RGATHER(8)
        }
      }
    }
    is(UopSplitType.VEC_RGATHER_VX) {
      def genCsBundle_RGATHER_VX(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            csBundle(i * len + j + 1).srcType(0) := SrcType.vp
            // csBundle(i * len + j + 1).srcType(1) := SrcType.vp
            // csBundle(i * len + j + 1).srcType(2) := SrcType.vp
            csBundle(i * len + j + 1).lsrc(0) := VECTOR_TMP_REG_LMUL.U
            csBundle(i * len + j + 1).lsrc(1) := src2 + j.U
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j + 1).lsrc(2) := vd_old
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j + 1).U
            csBundle(i * len + j + 1).ldest := vd
            csBundle(i * len + j + 1).uopIdx := (i * len + j).U
          }
      }
      // i to vector move
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(Mux(src1IsImm, IF2VectorType.imm2Vec(2, 0), IF2VectorType.i2Vec(2, 0)), vsewReg)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := false.B
      csBundle(0).vecWen := true.B
      genCsBundle_RGATHER_VX(1)
      switch(vlmulReg) {
        is("b001".U ){
          genCsBundle_RGATHER_VX(2)
        }
        is("b010".U ){
          genCsBundle_RGATHER_VX(4)
        }
        is("b011".U ){
          genCsBundle_RGATHER_VX(8)
        }
      }
    }
    is(UopSplitType.VEC_RGATHEREI16) {
      def genCsBundle_VEC_RGATHEREI16_SEW8(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            val vd_old0 = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j*2-1).U
            val vd0 = (VECTOR_TMP_REG_LMUL + j*2 ).U
            csBundle((i * len + j)*2+0).lsrc(0) := src1 + (i*2+0).U
            csBundle((i * len + j)*2+0).lsrc(1) := src2 + j.U
            csBundle((i * len + j)*2+0).lsrc(2) := vd_old0
            csBundle((i * len + j)*2+0).ldest := vd0
            csBundle((i * len + j)*2+0).uopIdx := ((i * len + j)*2+0).U
            val vd_old1 = (VECTOR_TMP_REG_LMUL + j*2).U
            val vd1 = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j*2+1 ).U
            csBundle((i * len + j)*2+1).lsrc(0) := src1 + (i*2+1).U
            csBundle((i * len + j)*2+1).lsrc(1) := src2 + j.U
            csBundle((i * len + j)*2+1).lsrc(2) := vd_old1
            csBundle((i * len + j)*2+1).ldest := vd1
            csBundle((i * len + j)*2+1).uopIdx := ((i * len + j)*2+1).U
          }
      }
      def genCsBundle_VEC_RGATHEREI16(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j-1).U
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j).lsrc(0) := src1 + i.U
            csBundle(i * len + j).lsrc(1) := src2 + j.U
            csBundle(i * len + j).lsrc(2) := vd_old
            csBundle(i * len + j).ldest := vd
            csBundle(i * len + j).uopIdx := (i * len + j).U
          }
      }
      def genCsBundle_VEC_RGATHEREI16_SEW32(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j-1).U
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j).lsrc(0) := src1 + (i / 2).U
            csBundle(i * len + j).lsrc(1) := src2 + j.U
            csBundle(i * len + j).lsrc(2) := vd_old
            csBundle(i * len + j).ldest := vd
            csBundle(i * len + j).uopIdx := (i * len + j).U
          }
      }
      def genCsBundle_VEC_RGATHEREI16_SEW64(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j-1).U
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j).lsrc(0) := src1 + (i / 4).U
            csBundle(i * len + j).lsrc(1) := src2 + j.U
            csBundle(i * len + j).lsrc(2) := vd_old
            csBundle(i * len + j).ldest := vd
            csBundle(i * len + j).uopIdx := (i * len + j).U
          }
      }
      when(!vsewReg.orR){
        genCsBundle_VEC_RGATHEREI16_SEW8(1)
      }.elsewhen(vsewReg === VSew.e32){
        genCsBundle_VEC_RGATHEREI16_SEW32(1)
      }.elsewhen(vsewReg === VSew.e64){
        genCsBundle_VEC_RGATHEREI16_SEW64(1)
      }.otherwise{
        genCsBundle_VEC_RGATHEREI16(1)
      }
      switch(vlmulReg) {
        is("b001".U) {
          when(!vsewReg.orR) {
            genCsBundle_VEC_RGATHEREI16_SEW8(2)
          }.elsewhen(vsewReg === VSew.e32){
            genCsBundle_VEC_RGATHEREI16_SEW32(2)
          }.elsewhen(vsewReg === VSew.e64){
            genCsBundle_VEC_RGATHEREI16_SEW64(2)
          }.otherwise{
            genCsBundle_VEC_RGATHEREI16(2)
          }
        }
        is("b010".U) {
          when(!vsewReg.orR) {
            genCsBundle_VEC_RGATHEREI16_SEW8(4)
          }.elsewhen(vsewReg === VSew.e32){
            genCsBundle_VEC_RGATHEREI16_SEW32(4)
          }.elsewhen(vsewReg === VSew.e64){
            genCsBundle_VEC_RGATHEREI16_SEW64(4)
          }.otherwise{
            genCsBundle_VEC_RGATHEREI16(4)
          }
        }
        is("b011".U) {
          when(vsewReg === VSew.e32){
            genCsBundle_VEC_RGATHEREI16_SEW32(8)
          }.elsewhen(vsewReg === VSew.e64){
            genCsBundle_VEC_RGATHEREI16_SEW64(8)
          }.otherwise{
            genCsBundle_VEC_RGATHEREI16(8)
          }
        }
      }
    }
    is(UopSplitType.VEC_COMPRESS) {
      def genCsBundle_VEC_COMPRESS(len:Int): Unit = {
        for (i <- 0 until len) {
          val jlen = if (i == len-1) i+1 else i+2
          for (j <- 0 until jlen) {
            val vd_old = if(i==j) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j + 1).U
            val vd = if(i==len-1) (dest + j.U) else {
              if (j == i+1) VECTOR_TMP_REG_LMUL.U else (VECTOR_TMP_REG_LMUL + j + 1).U
            }
            val src13Type = if (j == i+1) DontCare else SrcType.vp
            csBundle(i*(i+3)/2 + j).srcType(0) := src13Type
            csBundle(i*(i+3)/2 + j).srcType(1) := SrcType.vp
            csBundle(i*(i+3)/2 + j).srcType(2) := src13Type
            csBundle(i*(i+3)/2 + j).srcType(3) := SrcType.vp
            csBundle(i*(i+3)/2 + j).lsrc(0) := src1
            csBundle(i*(i+3)/2 + j).lsrc(1) := src2 + i.U
            csBundle(i*(i+3)/2 + j).lsrc(2) := vd_old
            csBundle(i*(i+3)/2 + j).lsrc(3) := VECTOR_TMP_REG_LMUL.U
            csBundle(i*(i+3)/2 + j).ldest := vd
            csBundle(i*(i+3)/2 + j).uopIdx := (i*(i+3)/2 + j).U
          }
        }
      }
      switch(vlmulReg) {
        is("b001".U ){
          genCsBundle_VEC_COMPRESS(2)
        }
        is("b010".U ){
          genCsBundle_VEC_COMPRESS(4)
        }
        is("b011".U ){
          genCsBundle_VEC_COMPRESS(8)
        }
      }
    }
    is(UopSplitType.VEC_MVNR) {
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i).lsrc(0) := src1 + i.U
        csBundle(i).lsrc(1) := src2 + i.U
        csBundle(i).lsrc(2) := dest + i.U
        csBundle(i).ldest := dest + i.U
        csBundle(i).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_US_LDST) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.i2Vec(2, 0), e64)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).vlsInstr := true.B
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.fp
        csBundle(i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 1).lsrc(2) := dest + i.U // old vd
        csBundle(i + 1).ldest := dest + i.U
        csBundle(i + 1).uopIdx := i.U
        csBundle(i + 1).vlsInstr := true.B
      }
    }
    is(UopSplitType.VEC_S_LDST) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.i2Vec(2, 0), e64)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).vlsInstr := true.B

      csBundle(1).srcType(0) := SrcType.reg
      csBundle(1).srcType(1) := SrcType.imm
      csBundle(1).lsrc(0) := latchedInst.lsrc(1)
      csBundle(1).lsrc(1) := 0.U
      csBundle(1).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(1).fuType := FuType.i2v.U
      csBundle(1).fuOpType := Cat(IF2VectorType.i2Vec(2, 0), e64)
      csBundle(1).rfWen := false.B
      csBundle(1).fpWen := true.B
      csBundle(1).vecWen := false.B
      csBundle(1).vlsInstr := true.B

      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 2).srcType(0) := SrcType.fp
        csBundle(i + 2).srcType(1) := SrcType.fp
        csBundle(i + 2).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 2).lsrc(1) := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 2).lsrc(2) := dest + i.U // old vd
        csBundle(i + 2).ldest := dest + i.U
        csBundle(i + 2).uopIdx := i.U
        csBundle(i + 2).vlsInstr := true.B
      }
    }
    is(UopSplitType.VEC_I_LDST) {
    /*
      FMV.D.X
       */
      val vlmul = vlmulReg
      val vsew = Cat(0.U(1.W), vsewReg)
      val veew = Cat(0.U(1.W), width)
      val vemul: UInt = veew.asUInt + 1.U + vlmul.asUInt + ~vsew.asUInt
      val simple_lmul = MuxLookup(vlmul, 0.U(2.W))(Array(
        "b001".U -> 1.U,
        "b010".U -> 2.U,
        "b011".U -> 3.U
      ))
      val simple_emul = MuxLookup(vemul, 0.U(2.W))(Array(
        "b001".U -> 1.U,
        "b010".U -> 2.U,
        "b011".U -> 3.U
      ))
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2v.U
      csBundle(0).fuOpType := Cat(IF2VectorType.i2Vec(2, 0), e64)
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).vlsInstr := true.B

      //LMUL
      for (i <- 0 until MAX_INDEXED_LS_UOPNUM) {
        indexedLSRegOffset(i).src := Cat(simple_emul, simple_lmul, nf)
        val offsetVs2 = indexedLSRegOffset(i).outOffsetVs2
        val offsetVd = indexedLSRegOffset(i).outOffsetVd
        val isFirstUopInVd = indexedLSRegOffset(i).outIsFirstUopInVd
        csBundle(i + 1).srcType(0) := SrcType.fp
        csBundle(i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 1).lsrc(1) := Mux1H(UIntToOH(offsetVs2, MAX_VLMUL), (0 until MAX_VLMUL).map(j => src2 + j.U))
        /**
          * For indexed instructions, VLSU will concatenate all the uops that write the same logic vd register and
          * writeback only once for all these uops. However, these uops share the same lsrc(2)/old vd and the same
          * ldest/vd that is equal to old vd, which leads to data dependence between the uops. Therefore there will be
          * deadlock for indexed instructions with emul > lmul.
          *
          * Assume N = emul/lmul. To break the deadlock, only the first uop will read old vd as lsrc(2), and the rest
          * N-1 uops will read temporary vector register.
          */
        // csBundle(i + 1).lsrc(2) := Mux1H(UIntToOH(offsetVd, MAX_VLMUL), (0 until MAX_VLMUL).map(j => dest + j.U))
        csBundle(i + 1).lsrc(2) := Mux(
          isFirstUopInVd,
          Mux1H(UIntToOH(offsetVd, MAX_VLMUL), (0 until MAX_VLMUL).map(j => dest + j.U)),
          VECTOR_TMP_REG_LMUL.U
        )
        csBundle(i + 1).ldest := Mux1H(UIntToOH(offsetVd, MAX_VLMUL), (0 until MAX_VLMUL).map(j => dest + j.U))
        csBundle(i + 1).uopIdx := i.U
        csBundle(i + 1).vlsInstr := true.B
      }
    }
  }

  //readyFromRename Counter
  val readyCounter = PriorityMuxDefault(outReadys.map(x => !x).zip((0 until RenameWidth).map(_.U)), RenameWidth.U)

  // The left uops of the complex inst in ComplexDecoder can be send out this cycle
  val thisAllOut = uopRes <= readyCounter

  switch(state) {
    is(s_idle) {
      when (inValid) {
        stateNext := s_active
        uopResNext := inUopInfo.numOfUop
      }
    }
    is(s_active) {
      when (thisAllOut) {
        when (inValid) {
          stateNext := s_active
          uopResNext := inUopInfo.numOfUop
        }.otherwise {
          stateNext := s_idle
          uopResNext := 0.U
        }
      }.otherwise {
        stateNext := s_active
        uopResNext := uopRes - readyCounter
      }
    }
  }

  state := Mux(io.redirect, s_idle, stateNext)
  uopRes := Mux(io.redirect, 0.U, uopResNext)

  val complexNum = Mux(uopRes > readyCounter, readyCounter, uopRes)

  for(i <- 0 until RenameWidth) {
    outValids(i) := complexNum > i.U
    outDecodedInsts(i) := Mux((i.U + numOfUop - uopRes) < maxUopSize.U, csBundle(i.U + numOfUop - uopRes), csBundle(maxUopSize - 1))
  }

  outComplexNum := Mux(state === s_active, complexNum, 0.U)
  inReady := state === s_idle || state === s_active && thisAllOut

//  val validSimple = Wire(Vec(DecodeWidth, Bool()))
//  validSimple.zip(io.validFromIBuf.zip(io.isComplex)).map{ case (dst, (src1, src2)) => dst := src1 && !src2 }
//  val notInf = Wire(Vec(DecodeWidth, Bool()))
//  notInf.drop(1).zip(io.validFromIBuf.drop(1).zip(validSimple.drop(1))).map{ case (dst, (src1, src2)) => dst := !src1 || src2 }
//  notInf(0) := !io.validFromIBuf(0) || validSimple(0) || (io.isComplex(0) && io.in0pc === io.simple.decodedInst.pc)
//  val notInfVec = Wire(Vec(DecodeWidth, Bool()))
//  notInfVec.zipWithIndex.map{ case (dst, i) => dst := Cat(notInf.take(i + 1)).andR}
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
