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

trait VectorConstants {
  val MAX_VLMUL = 8
  val FP_TMP_REG_MV = 32
  val VECTOR_TMP_REG_LMUL = 33 // 33~47  ->  15
}

class DecodeUnitCompIO(implicit p: Parameters) extends XSBundle {
  val simple = new Bundle {
    val decodedInst = Input(new DecodedInst)
    val isComplex = Input(Bool())
    val uopInfo = Input(new UopInfo)
  }
  val vtype = Input(new VType)
  val in0pc = Input(UInt(VAddrBits.W))
  val isComplex = Input(Vec(DecodeWidth, Bool()))
  val validFromIBuf = Input(Vec(DecodeWidth, Bool()))
  val readyFromRename = Input(Vec(RenameWidth, Bool()))
  val deq = new Bundle {
    val decodedInsts = Output(Vec(RenameWidth, new DecodedInst))
    val isVset = Output(Bool())
    val readyToIBuf = Output(Vec(DecodeWidth, Bool()))
    val validToRename = Output(Vec(RenameWidth, Bool()))
    val complexNum = Output(UInt(3.W))
  }
  val csrCtrl = Input(new CustomCSRCtrlIO)
}

/**
  * @author zly
  */
class DecodeUnitComp()(implicit p : Parameters) extends XSModule with DecodeUnitConstants with VectorConstants {
  val io = IO(new DecodeUnitCompIO)

  val maxUopSize = MaxUopSize
  //input bits
  private val inst: XSInstBitFields = io.simple.decodedInst.instr.asTypeOf(new XSInstBitFields)

  val src1 = Cat(0.U(1.W), inst.RS1)
  val src2 = Cat(0.U(1.W), inst.RS2)
  val dest = Cat(0.U(1.W), inst.RD)

  val nf    = inst.NF
  val width = inst.WIDTH(1, 0)

  //output bits
  val decodedInsts = Wire(Vec(RenameWidth, new DecodedInst))
  val validToRename = Wire(Vec(RenameWidth, Bool()))
  val readyToIBuf = Wire(Vec(DecodeWidth, Bool()))
  val complexNum = Wire(UInt(3.W))

  //output of DecodeUnit
  val decodedInstsSimple = Wire(new DecodedInst)
  val numOfUop = Wire(UInt(log2Up(maxUopSize+1).W))
  val lmul = Wire(UInt(4.W))
  val isVsetSimple = Wire(Bool())

  //pre decode
  decodedInstsSimple := io.simple.decodedInst
  lmul := io.simple.uopInfo.lmul
  isVsetSimple := io.simple.decodedInst.isVset
  val vlmulReg = io.simple.decodedInst.vpu.vlmul
  val vsewReg = io.simple.decodedInst.vpu.vsew
  when(isVsetSimple) {
    when(dest === 0.U && src1 === 0.U) {
      decodedInstsSimple.fuOpType := VSETOpType.keepVl(io.simple.decodedInst.fuOpType)
    }.elsewhen(src1 === 0.U) {
      decodedInstsSimple.fuOpType := VSETOpType.setVlmax(io.simple.decodedInst.fuOpType)
    }
    when(io.vtype.illegal){
      decodedInstsSimple.flushPipe := true.B
    }
  }
  //Type of uop Div
  val typeOfSplit = decodedInstsSimple.uopSplitType

  when(typeOfSplit === UopSplitType.DIR) {
    numOfUop := Mux(dest =/= 0.U, 2.U,
      Mux(src1 =/= 0.U, 1.U,
        Mux(VSETOpType.isVsetvl(decodedInstsSimple.fuOpType), 2.U, 1.U)))
  } .otherwise {
    numOfUop := io.simple.uopInfo.numOfUop
  }


  //uop div up to maxUopSize
  val csBundle = Wire(Vec(maxUopSize, new DecodedInst))
  csBundle.map { case dst =>
    dst := decodedInstsSimple
    dst.firstUop := false.B
    dst.lastUop := false.B
  }

  csBundle(0).numUops := numOfUop
  csBundle(0).firstUop := true.B
  csBundle(numOfUop - 1.U).lastUop := true.B

  switch(typeOfSplit) {
    is(UopSplitType.DIR) {
      when(isVsetSimple) {
        when(dest =/= 0.U) {
          csBundle(0).fuType := FuType.vsetiwi.U
          csBundle(0).fuOpType := VSETOpType.switchDest(decodedInstsSimple.fuOpType)
          csBundle(0).flushPipe := false.B
          csBundle(0).rfWen := true.B
          csBundle(0).vecWen := false.B
          csBundle(1).ldest := VCONFIG_IDX.U
          csBundle(1).rfWen := false.B
          csBundle(1).vecWen := true.B
        }.elsewhen(src1 =/= 0.U) {
          csBundle(0).ldest := VCONFIG_IDX.U
        }.elsewhen(VSETOpType.isVsetvli(decodedInstsSimple.fuOpType)) {
          csBundle(0).fuType := FuType.vsetfwf.U
          csBundle(0).srcType(0) := SrcType.vp
          csBundle(0).lsrc(0) := VCONFIG_IDX.U
        }.elsewhen(VSETOpType.isVsetvl(decodedInstsSimple.fuOpType)) {
          csBundle(0).srcType(0) := SrcType.reg
          csBundle(0).srcType(1) := SrcType.imm
          csBundle(0).lsrc(1) := 0.U
          csBundle(0).ldest := FP_TMP_REG_MV.U
          csBundle(0).fuType := FuType.i2f.U
          csBundle(0).rfWen := false.B
          csBundle(0).fpWen := true.B
          csBundle(0).vecWen := false.B
          csBundle(0).fpu.isAddSub := false.B
          csBundle(0).fpu.typeTagIn := FPU.D
          csBundle(0).fpu.typeTagOut := FPU.D
          csBundle(0).fpu.fromInt := true.B
          csBundle(0).fpu.wflags := false.B
          csBundle(0).fpu.fpWen := true.B
          csBundle(0).fpu.div := false.B
          csBundle(0).fpu.sqrt := false.B
          csBundle(0).fpu.fcvt := false.B
          csBundle(0).flushPipe := false.B
          csBundle(1).fuType := FuType.vsetfwf.U
          csBundle(1).srcType(0) := SrcType.vp
          csBundle(1).lsrc(0) := VCONFIG_IDX.U
          csBundle(1).srcType(1) := SrcType.fp
          csBundle(1).lsrc(1) := FP_TMP_REG_MV.U
          csBundle(1).ldest := VCONFIG_IDX.U
        }
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
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i).lsrc(1) := src2 + i.U
        csBundle(i).lsrc(2) := dest + i.U
        csBundle(i).ldest := dest + i.U
        csBundle(i).uopIdx := i.U
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
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      /*
      vfmv.s.f
       */
      csBundle(1).srcType(0) := SrcType.fp
      csBundle(1).srcType(1) := SrcType.vp
      csBundle(1).srcType(2) := SrcType.vp
      csBundle(1).lsrc(0) := FP_TMP_REG_MV.U
      csBundle(1).lsrc(1) := 0.U
      csBundle(1).lsrc(2) := dest
      csBundle(1).ldest := dest
      csBundle(1).fuType := FuType.vppu.U
      csBundle(1).fuOpType := VpermType.dummy
      csBundle(1).rfWen := false.B
      csBundle(1).fpWen := false.B
      csBundle(1).vecWen := true.B
    }
    is(UopSplitType.VEC_VXV) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      /*
      LMUL
       */
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.fp
        csBundle(i + 1).lsrc(0) := FP_TMP_REG_MV.U
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
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).lsrc(0) := src1
        csBundle(2 * i).lsrc(1) := src2 + i.U
        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ldest := dest + (2 * i).U
        csBundle(2 * i).uopIdx := (2 * i).U
        csBundle(2 * i + 1).lsrc(0) := src1
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
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
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.fp
        csBundle(2 * i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ldest := dest + (2 * i).U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.fp
        csBundle(2 * i + 2).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 2).lsrc(1) := src2 + i.U
        csBundle(2 * i + 2).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_WXW) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.fp
        csBundle(2 * i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ldest := dest + (2 * i).U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.fp
        csBundle(2 * i + 2).lsrc(0) := FP_TMP_REG_MV.U
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
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).lsrc(0) := src1
        csBundle(2 * i).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i).lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ldest := dest + (2 * i).U
        csBundle(2 * i).uopIdx := (2 * i).U
        csBundle(2 * i + 1).lsrc(0) := src1
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 1).lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
      }
    }
    is(UopSplitType.VEC_WXV) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).srcType(0) := SrcType.fp
        csBundle(2 * i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 1).lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i + 1).lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ldest := dest + i.U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        csBundle(2 * i + 2).srcType(0) := SrcType.fp
        csBundle(2 * i + 2).lsrc(0) := FP_TMP_REG_MV.U
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
      csBundle(numOfUop - 1.U).ldest := dest
    }
    is(UopSplitType.VEC_VFM) {
      csBundle(0).lsrc(2) := dest
      csBundle(0).ldest := dest
      csBundle(0).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i).lsrc(0) := src1
        csBundle(i).lsrc(1) := src2 + i.U
        csBundle(i).lsrc(2) := dest
        csBundle(i).ldest := dest
        csBundle(i).uopIdx := i.U
      }
      csBundle(numOfUop - 1.U).ldest := dest
    }
    is(UopSplitType.VEC_VXM) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      //LMUL
      csBundle(1).srcType(0) := SrcType.fp
      csBundle(1).lsrc(0) := FP_TMP_REG_MV.U
      csBundle(1).lsrc(2) := dest
      csBundle(1).ldest := dest
      csBundle(1).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.fp
        csBundle(i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).lsrc(2) := dest
        csBundle(i + 1).ldest := dest
        csBundle(i + 1).uopIdx := i.U
      }
      csBundle(numOfUop - 1.U).ldest := dest
    }
    is(UopSplitType.VEC_SLIDE1UP) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      //LMUL
      csBundle(1).srcType(0) := SrcType.fp
      csBundle(1).lsrc(0) := FP_TMP_REG_MV.U
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
      //LMUL
      csBundle(0).srcType(0) := SrcType.fp
      csBundle(0).lsrc(0) := src1
      csBundle(0).lsrc(1) := src2
      csBundle(0).lsrc(2) := dest
      csBundle(0).ldest := dest
      csBundle(0).uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i).srcType(0) := SrcType.vp
        csBundle(i).lsrc(0) := src2 + (i - 1).U
        csBundle(i).lsrc(1) := src2 + i.U
        csBundle(i).lsrc(2) := dest + i.U
        csBundle(i).ldest := dest + i.U
        csBundle(i).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_SLIDE1DOWN) { // lmul+lmul = 16
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(2 * i + 1).srcType(0) := SrcType.vp
        csBundle(2 * i + 1).srcType(1) := SrcType.vp
        csBundle(2 * i + 1).lsrc(0) := src2 + (i + 1).U
        csBundle(2 * i + 1).lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).uopIdx := (2 * i).U
        if (2 * i + 2 < MAX_VLMUL * 2) {
          csBundle(2 * i + 2).srcType(0) := SrcType.fp
          csBundle(2 * i + 2).lsrc(0) := FP_TMP_REG_MV.U
          // csBundle(2 * i + 2).lsrc(1) := src2 + i.U         // DontCare
          csBundle(2 * i + 2).lsrc(2) := VECTOR_TMP_REG_LMUL.U
          csBundle(2 * i + 2).ldest := dest + i.U
          csBundle(2 * i + 2).uopIdx := (2 * i + 1).U
        }
      }
      csBundle(numOfUop - 1.U).srcType(0) := SrcType.fp
      csBundle(numOfUop - 1.U).lsrc(0) := FP_TMP_REG_MV.U
      csBundle(numOfUop - 1.U).ldest := dest + lmul - 1.U
    }
    is(UopSplitType.VEC_FSLIDE1DOWN) {
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(2 * i).srcType(0) := SrcType.vp
        csBundle(2 * i).srcType(1) := SrcType.vp
        csBundle(2 * i).lsrc(0) := src2 + (i + 1).U
        csBundle(2 * i).lsrc(1) := src2 + i.U
        csBundle(2 * i).lsrc(2) := dest + i.U
        csBundle(2 * i).ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i).uopIdx := (2 * i).U
        csBundle(2 * i + 1).srcType(0) := SrcType.fp
        csBundle(2 * i + 1).lsrc(0) := src1
        csBundle(2 * i + 1).lsrc(2) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).ldest := dest + i.U
        csBundle(2 * i + 1).uopIdx := (2 * i + 1).U
      }
      csBundle(numOfUop - 1.U).srcType(0) := SrcType.fp
      csBundle(numOfUop - 1.U).lsrc(0) := src1
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
      when(vlmulReg.orR) {
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
      val isWiden = decodedInstsSimple.fuOpType === VfaluType.vfwredosum
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
      // FMV.D.X
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      // LMUL
      for (i <- 0 until MAX_VLMUL)
        for (j <- 0 to i) {
          val old_vd = if (j == 0) {
            dest + i.U
          } else (VECTOR_TMP_REG_LMUL + j - 1).U
          val vd = if (j == i) {
            dest + i.U
          } else (VECTOR_TMP_REG_LMUL + j).U
          csBundle(i * (i + 1) / 2 + j + 1).srcType(0) := SrcType.fp
          csBundle(i * (i + 1) / 2 + j + 1).lsrc(0) := FP_TMP_REG_MV.U
          csBundle(i * (i + 1) / 2 + j + 1).lsrc(1) := src2 + j.U
          csBundle(i * (i + 1) / 2 + j + 1).lsrc(2) := old_vd
          csBundle(i * (i + 1) / 2 + j + 1).ldest := vd
          csBundle(i * (i + 1) / 2 + j + 1).uopIdx := (i * (i + 1) / 2 + j).U
        }
    }

    is(UopSplitType.VEC_ISLIDEUP) {
      // LMUL
      for (i <- 0 until MAX_VLMUL)
        for (j <- 0 to i) {
          val old_vd = if (j == 0) {
            dest + i.U
          } else (VECTOR_TMP_REG_LMUL + j - 1).U
          val vd = if (j == i) {
            dest + i.U
          } else (VECTOR_TMP_REG_LMUL + j).U
          csBundle(i * (i + 1) / 2 + j).lsrc(1) := src2 + j.U
          csBundle(i * (i + 1) / 2 + j).lsrc(2) := old_vd
          csBundle(i * (i + 1) / 2 + j).ldest := vd
          csBundle(i * (i + 1) / 2 + j).uopIdx := (i * (i + 1) / 2 + j).U
        }
    }

    is(UopSplitType.VEC_SLIDEDOWN) {
      // FMV.D.X
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      // LMUL
      for (i <- 0 until MAX_VLMUL)
        for (j <- (0 to i).reverse) {
          when(i.U < lmul) {
            val old_vd = if (j == 0) {
              dest + lmul - 1.U - i.U
            } else (VECTOR_TMP_REG_LMUL + j - 1).U
            val vd = if (j == i) {
              dest + lmul - 1.U - i.U
            } else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).srcType(0) := SrcType.fp
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(0) := FP_TMP_REG_MV.U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(1) := src2 + lmul - 1.U - j.U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(2) := old_vd
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).ldest := vd
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).uopIdx := numOfUop - (i * (i + 1) / 2 + i - j + 2).U
          }
        }
    }

    is(UopSplitType.VEC_ISLIDEDOWN) {
      // LMUL
      for (i <- 0 until MAX_VLMUL)
        for (j <- (0 to i).reverse) {
          when(i.U < lmul) {
            val old_vd = if (j == 0) {
              dest + lmul - 1.U - i.U
            } else (VECTOR_TMP_REG_LMUL + j - 1).U
            val vd = if (j == i) {
              dest + lmul - 1.U - i.U
            } else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(1) := src2 + lmul - 1.U - j.U
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).lsrc(2) := old_vd
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).ldest := vd
            csBundle(numOfUop - (i * (i + 1) / 2 + i - j + 1).U).uopIdx := numOfUop - (i * (i + 1) / 2 + i - j + 1).U
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
        csBundle(i).vecWen := true.B
        csBundle(i).lsrc(0) := (VECTOR_TMP_REG_LMUL + i - 1).U
        csBundle(i).lsrc(1) := src2
        // csBundle(i).lsrc(2) := dest + i.U  DontCare
        csBundle(i).ldest := ldest
        csBundle(i).uopIdx := i.U
      }
      csBundle(lmul - 1.U).vecWen := false.B
      csBundle(lmul - 1.U).fpWen := true.B
      csBundle(lmul - 1.U).ldest := FP_TMP_REG_MV.U
      // FMV_X_D
      csBundle(lmul).srcType(0) := SrcType.fp
      csBundle(lmul).srcType(1) := SrcType.imm
      csBundle(lmul).lsrc(0) := FP_TMP_REG_MV.U
      csBundle(lmul).lsrc(1) := 0.U
      csBundle(lmul).ldest := dest
      csBundle(lmul).fuType := FuType.fmisc.U
      csBundle(lmul).rfWen := true.B
      csBundle(lmul).fpWen := false.B
      csBundle(lmul).vecWen := false.B
      csBundle(lmul).fpu.isAddSub := false.B
      csBundle(lmul).fpu.typeTagIn := FPU.D
      csBundle(lmul).fpu.typeTagOut := FPU.D
      csBundle(lmul).fpu.fromInt := false.B
      csBundle(lmul).fpu.wflags := false.B
      csBundle(lmul).fpu.fpWen := false.B
      csBundle(lmul).fpu.div := false.B
      csBundle(lmul).fpu.sqrt := false.B
      csBundle(lmul).fpu.fcvt := false.B
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
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).ldest := FP_TMP_REG_MV.U
      // FMV_X_D
      csBundle(1).srcType(0) := SrcType.fp
      csBundle(1).srcType(1) := SrcType.imm
      csBundle(1).lsrc(0) := FP_TMP_REG_MV.U
      csBundle(1).lsrc(1) := 0.U
      csBundle(1).ldest := dest
      csBundle(1).fuType := FuType.fmisc.U
      csBundle(1).rfWen := true.B
      csBundle(1).fpWen := false.B
      csBundle(1).vecWen := false.B
      csBundle(1).fpu.isAddSub := false.B
      csBundle(1).fpu.typeTagIn := FPU.D
      csBundle(1).fpu.typeTagOut := FPU.D
      csBundle(1).fpu.fromInt := false.B
      csBundle(1).fpu.wflags := false.B
      csBundle(1).fpu.fpWen := false.B
      csBundle(1).fpu.div := false.B
      csBundle(1).fpu.sqrt := false.B
      csBundle(1).fpu.fcvt := false.B
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
            csBundle(i * len + j + 1).srcType(0) := SrcType.fp
            // csBundle(i * len + j + 1).srcType(1) := SrcType.vp
            // csBundle(i * len + j + 1).srcType(2) := SrcType.vp
            csBundle(i * len + j + 1).lsrc(0) := FP_TMP_REG_MV.U
            csBundle(i * len + j + 1).lsrc(1) := src2 + j.U
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j - 1).U
            csBundle(i * len + j + 1).lsrc(2) := vd_old
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j + 1).ldest := vd
            csBundle(i * len + j + 1).uopIdx := (i * len + j).U
          }
      }
      // FMV.D.X
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      switch(vlmulReg) {
        is("b000".U ){
          genCsBundle_RGATHER_VX(1)
        }
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
            // csBundle(i * len + j).srcType(0) := SrcType.vp // SrcType.imm
            // csBundle(i * len + j).srcType(1) := SrcType.vp
            // csBundle(i * len + j).srcType(2) := SrcType.vp
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
            // csBundle(i * len + j).srcType(0) := SrcType.vp // SrcType.imm
            // csBundle(i * len + j).srcType(1) := SrcType.vp
            // csBundle(i * len + j).srcType(2) := SrcType.vp
            csBundle(i * len + j).lsrc(0) := src1 + i.U
            csBundle(i * len + j).lsrc(1) := src2 + j.U
            csBundle(i * len + j).lsrc(2) := vd_old
            csBundle(i * len + j).ldest := vd
            csBundle(i * len + j).uopIdx := (i * len + j).U
          }
      }
      switch(vlmulReg) {
        is("b000".U ){
          when(!vsewReg.orR){
            genCsBundle_VEC_RGATHEREI16_SEW8(1)
          } .otherwise{
            genCsBundle_VEC_RGATHEREI16(1)
          }
        }
        is("b001".U) {
          when(!vsewReg.orR) {
            genCsBundle_VEC_RGATHEREI16_SEW8(2)
          }.otherwise {
            genCsBundle_VEC_RGATHEREI16(2)
          }
        }
        is("b010".U) {
          when(!vsewReg.orR) {
            genCsBundle_VEC_RGATHEREI16_SEW8(4)
          }.otherwise {
            genCsBundle_VEC_RGATHEREI16(4)
          }
        }
        is("b011".U) {
          genCsBundle_VEC_RGATHEREI16(8)
        }
      }
    }
    is(UopSplitType.VEC_COMPRESS) {
      def genCsBundle_VEC_COMPRESS(len:Int): Unit ={
        for (i <- 0 until len){
          val jlen = if (i == len-1) i+1 else i+2
          for (j <- 0 until jlen) {
            val vd_old = if(i==j) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j + 1).U
            val vd = if(i==len-1) (dest + j.U) else{
              if (j == i+1) VECTOR_TMP_REG_LMUL.U else (VECTOR_TMP_REG_LMUL + j + 1).U
            }
            val src23Type = if (j == i+1) DontCare else SrcType.vp
            csBundle(i*(i+3)/2 + j).srcType(0) := SrcType.vp
            csBundle(i*(i+3)/2 + j).srcType(1) := src23Type
            csBundle(i*(i+3)/2 + j).srcType(2) := src23Type
            csBundle(i*(i+3)/2 + j).lsrc(0) := src1
            csBundle(i*(i+3)/2 + j).lsrc(1) := src2 + i.U
            csBundle(i*(i+3)/2 + j).lsrc(2) := vd_old
            // csBundle(i*(i+3)/2 + j).lsrc(3) := VECTOR_TMP_REG_LMUL.U
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
    is(UopSplitType.VEC_US_LDST) {
      /*
      FMV.D.X
       */
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.fp
        csBundle(i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 1).ldest := dest + i.U
        csBundle(i + 1).uopIdx := i.U
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
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B

      csBundle(1).srcType(0) := SrcType.imm
      csBundle(1).srcType(1) := SrcType.reg
      csBundle(1).lsrc(0) := 0.U
      csBundle(1).ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(1).fuType := FuType.i2f.U
      csBundle(1).rfWen := false.B
      csBundle(1).fpWen := true.B
      csBundle(1).vecWen := false.B
      csBundle(1).fpu.isAddSub := false.B
      csBundle(1).fpu.typeTagIn := FPU.D
      csBundle(1).fpu.typeTagOut := FPU.D
      csBundle(1).fpu.fromInt := true.B
      csBundle(1).fpu.wflags := false.B
      csBundle(1).fpu.fpWen := true.B
      csBundle(1).fpu.div := false.B
      csBundle(1).fpu.sqrt := false.B
      csBundle(1).fpu.fcvt := false.B

      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 2).srcType(0) := SrcType.fp
        csBundle(i + 2).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 2).lsrc(1) := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 2).ldest := dest + i.U
        csBundle(i + 2).uopIdx := i.U
      }
    }
    is(UopSplitType.VEC_I_LDST) {
    /*
      FMV.D.X
       */
      val vlmul = vlmulReg
      val vsew = vsewReg
      val veew = Cat(0.U(1.W), width)
      val vemul: UInt = veew.asUInt + 1.U + vlmul.asUInt + ~vsew.asUInt
      csBundle(0).srcType(0) := SrcType.reg
      csBundle(0).srcType(1) := SrcType.imm
      csBundle(0).lsrc(1) := 0.U
      csBundle(0).ldest := FP_TMP_REG_MV.U
      csBundle(0).fuType := FuType.i2f.U
      csBundle(0).rfWen := false.B
      csBundle(0).fpWen := true.B
      csBundle(0).vecWen := false.B
      csBundle(0).fpu.isAddSub := false.B
      csBundle(0).fpu.typeTagIn := FPU.D
      csBundle(0).fpu.typeTagOut := FPU.D
      csBundle(0).fpu.fromInt := true.B
      csBundle(0).fpu.wflags := false.B
      csBundle(0).fpu.fpWen := true.B
      csBundle(0).fpu.div := false.B
      csBundle(0).fpu.sqrt := false.B
      csBundle(0).fpu.fcvt := false.B

      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 1).srcType(0) := SrcType.fp
        csBundle(i + 1).lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 1).lsrc(1) := src2 + i.U
        csBundle(i + 1).ldest := dest + i.U
        csBundle(i + 1).uopIdx := i.U
      }
    }
  }

  //uops dispatch
  val s_normal :: s_ext :: Nil = Enum(2)
  val state = RegInit(s_normal)
  val state_next = WireDefault(state)
  val uopRes = RegInit(0.U)

  //readyFromRename Counter
  val readyCounter = PriorityMuxDefault(io.readyFromRename.map(x => !x).zip((0 to (RenameWidth - 1)).map(_.U)), RenameWidth.U)

  switch(state) {
    is(s_normal) {
      state_next := Mux(io.validFromIBuf(0) && (numOfUop > readyCounter) && (readyCounter =/= 0.U), s_ext, s_normal)
    }
    is(s_ext) {
      state_next := Mux(io.validFromIBuf(0) && (uopRes > readyCounter), s_ext, s_normal)
    }
  }

  state := state_next

  val uopRes0 = Mux(state === s_normal, numOfUop, uopRes)
  val uopResJudge = Mux(state === s_normal,
    io.validFromIBuf(0) && (readyCounter =/= 0.U) && (uopRes0 > readyCounter),
    io.validFromIBuf(0) && (uopRes0 > readyCounter))
  uopRes := Mux(uopResJudge, uopRes0 - readyCounter, 0.U)

  for(i <- 0 until RenameWidth) {
    decodedInsts(i) := MuxCase(csBundle(i), Seq(
      (state === s_normal) -> csBundle(i),
      (state === s_ext) -> Mux((i.U + numOfUop -uopRes) < maxUopSize.U, csBundle(i.U + numOfUop - uopRes), csBundle(maxUopSize - 1))
    ).toSeq)
  }

  val validSimple = Wire(Vec(DecodeWidth, Bool()))
  validSimple.zip(io.validFromIBuf.zip(io.isComplex)).map{ case (dst, (src1, src2)) => dst := src1 && !src2 }
  val notInf = Wire(Vec(DecodeWidth, Bool()))
  notInf.drop(1).zip(io.validFromIBuf.drop(1).zip(validSimple.drop(1))).map{ case (dst, (src1, src2)) => dst := !src1 || src2 }
  notInf(0) := !io.validFromIBuf(0) || validSimple(0) || (io.isComplex(0) && io.in0pc === io.simple.decodedInst.pc)
  val notInfVec = Wire(Vec(DecodeWidth, Bool()))
  notInfVec.zipWithIndex.map{ case (dst, i) => dst := Cat(notInf.take(i + 1)).andR}

  complexNum := Mux(io.validFromIBuf(0) && readyCounter.orR ,
    Mux(uopRes0 > readyCounter, readyCounter, uopRes0),
    0.U)
  validToRename.zipWithIndex.foreach{
    case(dst, i) =>
      val validFix = Mux(complexNum.orR, validSimple((i+1).U - complexNum), validSimple(i))
      dst := MuxCase(false.B, Seq(
        (io.validFromIBuf(0) && readyCounter.orR && uopRes0 > readyCounter) -> Mux(readyCounter > i.U, true.B, false.B),
        (io.validFromIBuf(0) && readyCounter.orR && !(uopRes0 > readyCounter)) -> Mux(complexNum > i.U, true.B, validFix && notInfVec(i.U - complexNum) && io.readyFromRename(i)),
      ).toSeq)
  }

  readyToIBuf.zipWithIndex.foreach {
    case (dst, i) =>
      val readyToIBuf0 = Mux(io.isComplex(0), io.in0pc === io.simple.decodedInst.pc, true.B)
      dst := MuxCase(true.B, Seq(
        (io.validFromIBuf(0) && uopRes0 > readyCounter || !readyCounter.orR) -> false.B,
        (io.validFromIBuf(0) && !(uopRes0 > readyCounter) && readyCounter.orR) -> (if (i==0) readyToIBuf0 else Mux(RenameWidth.U - complexNum >= i.U, notInfVec(i) && validSimple(i) && io.readyFromRename(i), false.B))
      ).toSeq)
  }

  io.deq.decodedInsts := decodedInsts
  io.deq.isVset := isVsetSimple
  io.deq.complexNum := complexNum
  io.deq.validToRename := validToRename
  io.deq.readyToIBuf := readyToIBuf

}
