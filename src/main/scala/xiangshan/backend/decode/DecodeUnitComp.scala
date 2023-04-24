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
import freechips.rocketchip.rocket.Instructions
import freechips.rocketchip.util.uintToBitPat
import utils._
import utility._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import xiangshan.backend.fu.fpu.FPU
import freechips.rocketchip.rocket.Instructions._
import yunsuan.VpermType
import scala.collection.Seq

trait VectorConstants {
  val MAX_VLMUL = 8
  val INT_VCONFIG = 32
  val FP_TMP_REG_MV = 32
  val VECTOR_TMP_REG_LMUL = 32 // 32~46  ->  15
}

class DecodeUnitCompIO(implicit p: Parameters) extends XSBundle {
  val enq = new Bundle { val ctrl_flow = Input(new CtrlFlow) }
  val vconfig = Input(new VConfig)
  val isComplex = Input(Vec(DecodeWidth - 1, Bool()))
  val validFromIBuf = Input(Vec(DecodeWidth, Bool()))
  val readyFromRename = Input(Vec(RenameWidth, Bool()))
  val deq = new Bundle {
    val cf_ctrl = Output(Vec(RenameWidth, new CfCtrl))
    val isVset = Output(Bool())
    val readyToIBuf = Output(Vec(DecodeWidth, Bool()))
    val validToRename = Output(Vec(RenameWidth, Bool()))
    val complexNum = Output(UInt(3.W))
  }
  val csrCtrl = Input(new CustomCSRCtrlIO)
}

class DecodeUnitComp(maxNumOfUop : Int)(implicit p : Parameters) extends XSModule with DecodeUnitConstants with VectorConstants {
  val io = IO(new DecodeUnitCompIO)
  //input bits
  val ctrl_flow = Wire(new CtrlFlow)
  ctrl_flow := io.enq.ctrl_flow
  //output bits
  val cf_ctrl = Wire(Vec(RenameWidth, new CfCtrl()))
  val validToRename = Wire(Vec(RenameWidth, Bool()))
  val readyToIBuf = Wire(Vec(DecodeWidth, Bool()))
  val complexNum = Wire(UInt(3.W))

  //output of DecodeUnit
  val cf_ctrl_u = Wire(new CfCtrl)
  val isVset_u = Wire(Bool())

  //pre decode
  val simple = Module(new DecodeUnit)
  simple.io.enq.ctrl_flow := ctrl_flow
  simple.io.vconfig := io.vconfig
  simple.io.csrCtrl := io.csrCtrl
  cf_ctrl_u := simple.io.deq.cf_ctrl
  isVset_u := simple.io.deq.isVset

  //Type of uop Div
  val typeOfDiv = cf_ctrl_u.ctrl.uopDivType

  //LMUL
  val lmul = MuxLookup(simple.io.vconfig.vtype.vlmul, 1.U(4.W), Array(
    "b001".U -> 2.U,
    "b010".U -> 4.U,
    "b011".U -> 8.U
  ))
  val numOfUopVslide = MuxLookup(simple.io.vconfig.vtype.vlmul, 1.U(log2Up(maxNumOfUop+1).W), Array(
    "b001".U -> 3.U,
    "b010".U -> 10.U,
    "b011".U -> 36.U
  ))
  val numOfUopVrgather = MuxLookup(simple.io.vconfig.vtype.vlmul, 1.U(log2Up(maxNumOfUop+1).W), Array(
    "b001".U -> 4.U,
    "b010".U -> 16.U,
    "b011".U -> 64.U
  ))
  val numOfUopVrgatherei16 = Mux((!simple.io.vconfig.vtype.vsew.orR()) && (simple.io.vconfig.vtype.vlmul =/= "b011".U), 
                                  Cat(numOfUopVrgather, 0.U(1.W)), 
                                  numOfUopVrgather
                                )
  //number of uop
  val numOfUop = MuxLookup(typeOfDiv, 1.U(log2Up(maxNumOfUop+1).W), Array(
    UopDivType.VEC_0XV         -> 2.U,
    UopDivType.DIR             -> 2.U,
    UopDivType.VEC_VVV         -> lmul,
    UopDivType.VEC_EXT2        -> lmul,
    UopDivType.VEC_EXT4        -> lmul,
    UopDivType.VEC_EXT8        -> lmul,
    UopDivType.VEC_VVM         -> lmul,
    UopDivType.VEC_VXM         -> (lmul +& 1.U),
    UopDivType.VEC_VXV         -> (lmul +& 1.U),
    UopDivType.VEC_VVW         -> Cat(lmul, 0.U(1.W)),     // lmul <= 4
    UopDivType.VEC_WVW         -> Cat(lmul, 0.U(1.W)),     // lmul <= 4
    UopDivType.VEC_VXW         -> Cat(lmul, 1.U(1.W)),     // lmul <= 4
    UopDivType.VEC_WXW         -> Cat(lmul, 1.U(1.W)),     // lmul <= 4
    UopDivType.VEC_WVV         -> Cat(lmul, 0.U(1.W)),     // lmul <= 4
    UopDivType.VEC_WXV         -> Cat(lmul, 1.U(1.W)),     // lmul <= 4
    UopDivType.VEC_SLIDE1UP    -> (lmul +& 1.U),
    UopDivType.VEC_FSLIDE1UP   -> lmul,
    UopDivType.VEC_SLIDE1DOWN  -> Cat(lmul, 0.U(1.W)),
    UopDivType.VEC_FSLIDE1DOWN -> (Cat(lmul, 0.U(1.W)) -1.U),
    UopDivType.VEC_VRED        -> lmul,
    UopDivType.VEC_SLIDEUP     -> (numOfUopVslide + 1.U),
    UopDivType.VEC_ISLIDEUP    -> numOfUopVslide,
    UopDivType.VEC_SLIDEDOWN   -> (numOfUopVslide + 1.U),
    UopDivType.VEC_ISLIDEDOWN  -> numOfUopVslide,
    UopDivType.VEC_M0X         -> (lmul +& 1.U),
    UopDivType.VEC_MVV         -> (Cat(lmul, 0.U(1.W)) -1.U),
    UopDivType.VEC_M0X_VFIRST  -> 2.U,
    UopDivType.VEC_VWW         -> Cat(lmul, 0.U(1.W)),
    UopDivType.VEC_RGATHER     -> numOfUopVrgather,
    UopDivType.VEC_RGATHER_VX  -> (numOfUopVrgather +& 1.U),
    UopDivType.VEC_RGATHEREI16 -> numOfUopVrgatherei16,
  ))

  val src1 = Cat(0.U(1.W), ctrl_flow.instr(19, 15))
  val src2 = Cat(0.U(1.W), ctrl_flow.instr(24, 20))
  val dest = Cat(0.U(1.W), ctrl_flow.instr(11, 7 ))

  //uop div up to maxNumOfUop
  val csBundle = Wire(Vec(maxNumOfUop, new CfCtrl))
  csBundle.map { case dst =>
    dst := cf_ctrl_u
    dst.ctrl.firstUop := false.B
    dst.ctrl.lastUop := false.B
  }

  csBundle(0).ctrl.firstUop := true.B
  csBundle(numOfUop - 1.U).ctrl.lastUop := true.B

  switch(typeOfDiv) {
    is(UopDivType.DIR) {
      when(isVset_u) {
        csBundle(0).ctrl.flushPipe := ALUOpType.isVsetvli(cf_ctrl_u.ctrl.fuOpType) && cf_ctrl_u.ctrl.lsrc(0).orR || ALUOpType.isVsetvl(cf_ctrl_u.ctrl.fuOpType)
        csBundle(0).ctrl.fuOpType := ALUOpType.vsetExchange(cf_ctrl_u.ctrl.fuOpType)
        csBundle(1).ctrl.ldest := INT_VCONFIG.U
        csBundle(1).ctrl.flushPipe := false.B
      }
    }
    is(UopDivType.VEC_VVV) {
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i).ctrl.lsrc(0) := src1 + i.U
        csBundle(i).ctrl.lsrc(1) := src2 + i.U
        csBundle(i).ctrl.lsrc(2) := dest + i.U
        csBundle(i).ctrl.ldest := dest + i.U
        csBundle(i).ctrl.uopIdx := i.U
      }
    }
    is(UopDivType.VEC_EXT2) {
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i).ctrl.lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ctrl.ldest := dest + (2 * i).U
        csBundle(2 * i).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).ctrl.lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i + 1).U
      }
    }
    is(UopDivType.VEC_EXT4) {
      for (i <- 0 until MAX_VLMUL / 4) {
        csBundle(4 * i).ctrl.lsrc(1) := src2 + i.U
        csBundle(4 * i).ctrl.lsrc(2) := dest + (4 * i).U
        csBundle(4 * i).ctrl.ldest := dest + (4 * i).U
        csBundle(4 * i).ctrl.uopIdx := (4 * i).U
        csBundle(4 * i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(4 * i + 1).ctrl.lsrc(2) := dest + (4 * i + 1).U
        csBundle(4 * i + 1).ctrl.ldest := dest + (4 * i + 1).U
        csBundle(4 * i + 1).ctrl.uopIdx := (4 * i + 1).U
        csBundle(4 * i + 2).ctrl.lsrc(1) := src2 + i.U
        csBundle(4 * i + 2).ctrl.lsrc(2) := dest + (4 * i + 2).U
        csBundle(4 * i + 2).ctrl.ldest := dest + (4 * i + 2).U
        csBundle(4 * i + 2).ctrl.uopIdx := (4 * i + 2).U
        csBundle(4 * i + 3).ctrl.lsrc(1) := src2 + i.U
        csBundle(4 * i + 3).ctrl.lsrc(2) := dest + (4 * i + 3).U
        csBundle(4 * i + 3).ctrl.ldest := dest + (4 * i + 3).U
        csBundle(4 * i + 3).ctrl.uopIdx := (4 * i + 3).U
      }
    }
    is(UopDivType.VEC_EXT8) {
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i).ctrl.lsrc(1) := src2
        csBundle(i).ctrl.lsrc(2) := dest + i.U
        csBundle(i).ctrl.ldest := dest + i.U
        csBundle(i).ctrl.uopIdx := i.U
      }
    }
    is(UopDivType.VEC_0XV) {
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B
      /*
      vfmv.s.f
       */
      csBundle(1).ctrl.srcType(0) := SrcType.fp
      csBundle(1).ctrl.srcType(1) := SrcType.vp
      csBundle(1).ctrl.srcType(2) := SrcType.vp
      csBundle(1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
      csBundle(1).ctrl.lsrc(1) := 0.U
      csBundle(1).ctrl.lsrc(2) := dest
      csBundle(1).ctrl.ldest := dest
      csBundle(1).ctrl.fuType := FuType.vppu
      csBundle(1).ctrl.fuOpType := VpermType.vfmv_s_f
      csBundle(1).ctrl.rfWen := false.B
      csBundle(1).ctrl.fpWen := false.B
      csBundle(1).ctrl.vecWen := true.B
    }
    is(UopDivType.VEC_VXV) {
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B
      /*
      LMUL
       */
      for (i <- 0 until MAX_VLMUL) {
        csBundle(i + 1).ctrl.srcType(0) := SrcType.fp
        csBundle(i + 1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(i + 1).ctrl.lsrc(2) := dest + i.U
        csBundle(i + 1).ctrl.ldest := dest + i.U
        csBundle(i + 1).ctrl.uopIdx := i.U
      }
    }
    is(UopDivType.VEC_VVW) {
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).ctrl.lsrc(0) := src1 + i.U
        csBundle(2 * i).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i).ctrl.lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ctrl.ldest := dest + (2 * i).U
        csBundle(2 * i).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 1).ctrl.lsrc(0) := src1 + i.U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).ctrl.lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i + 1).U
      }
    }
    is(UopDivType.VEC_WVW) {
      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i).ctrl.lsrc(0) := src1 + i.U
        csBundle(2 * i).ctrl.lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i).ctrl.lsrc(2) := dest + (2 * i).U
        csBundle(2 * i).ctrl.ldest := dest + (2 * i).U
        csBundle(2 * i).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 1).ctrl.lsrc(0) := src1 + i.U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i + 1).U
      }
    }
    is(UopDivType.VEC_VXW) {
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).ctrl.srcType(0) := SrcType.fp
        csBundle(2 * i + 1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).ctrl.lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ctrl.ldest := dest + (2 * i).U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 2).ctrl.srcType(0) := SrcType.fp
        csBundle(2 * i + 2).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 2).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i + 2).ctrl.lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ctrl.ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ctrl.uopIdx := (2 * i + 1).U
      }
    }
    is(UopDivType.VEC_WXW) {
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).ctrl.srcType(0) := SrcType.fp
        csBundle(2 * i + 1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i + 1).ctrl.lsrc(2) := dest + (2 * i).U
        csBundle(2 * i + 1).ctrl.ldest := dest + (2 * i).U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 2).ctrl.srcType(0) := SrcType.fp
        csBundle(2 * i + 2).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 2).ctrl.lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 2).ctrl.lsrc(2) := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ctrl.ldest := dest + (2 * i + 1).U
        csBundle(2 * i + 2).ctrl.uopIdx := (2 * i + 1).U
      }
    }
    is(UopDivType.VEC_WVV) {
      for (i <- 0 until MAX_VLMUL / 2) {

        csBundle(2 * i).ctrl.lsrc(0) := src1 + i.U
        csBundle(2 * i).ctrl.lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i).ctrl.lsrc(2) := dest + i.U
        csBundle(2 * i).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 1).ctrl.lsrc(0) := src1 + i.U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 1).ctrl.lsrc(2) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).ctrl.ldest := dest + i.U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i + 1).U
      }
    }
    is(UopDivType.VEC_WXV) {
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B

      for (i <- 0 until MAX_VLMUL / 2) {
        csBundle(2 * i + 1).ctrl.srcType(0) := SrcType.fp
        csBundle(2 * i + 1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + (2 * i).U
        csBundle(2 * i + 1).ctrl.lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 2).ctrl.srcType(0) := SrcType.fp
        csBundle(2 * i + 2).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(2 * i + 2).ctrl.lsrc(1) := src2 + (2 * i + 1).U
        csBundle(2 * i + 2).ctrl.lsrc(2) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 2).ctrl.ldest := dest + i.U
        csBundle(2 * i + 2).ctrl.uopIdx := (2 * i + 1).U
      }
    }
    is(UopDivType.VEC_VVM) {
      csBundle(0).ctrl.lsrc(2) := dest
      csBundle(0).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(0).ctrl.uopIdx := 0.U
      for(i <- 1 until MAX_VLMUL) {
        csBundle(i).ctrl.lsrc(0) := src1 + i.U
        csBundle(i).ctrl.lsrc(1) := src2 + i.U
        csBundle(i).ctrl.lsrc(2) := VECTOR_TMP_REG_LMUL.U
        csBundle(i).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(i).ctrl.uopIdx := i.U
      }
      csBundle(numOfUop - 1.U).ctrl.ldest := dest
    }
    is(UopDivType.VEC_VXM) {
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B
      //LMUL
      csBundle(1).ctrl.srcType(0) := SrcType.fp
      csBundle(1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
      csBundle(1).ctrl.lsrc(2) := dest
      csBundle(1).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
      csBundle(1).ctrl.uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i + 1).ctrl.srcType(0) := SrcType.fp
        csBundle(i + 1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
        csBundle(i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(i + 1).ctrl.lsrc(2) := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 1).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(i + 1).ctrl.uopIdx := i.U
      }
      csBundle(numOfUop - 1.U).ctrl.ldest := dest
    }
    is(UopDivType.VEC_SLIDE1UP) {
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B
      //LMUL
      csBundle(1).ctrl.srcType(0) := SrcType.fp
      csBundle(1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
      csBundle(1).ctrl.lsrc(2) := dest
      csBundle(1).ctrl.ldest := dest
      csBundle(1).ctrl.uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i + 1).ctrl.srcType(0) := SrcType.vp
        csBundle(i + 1).ctrl.lsrc(0) := src2 + (i - 1).U
        csBundle(i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(i + 1).ctrl.lsrc(2) := dest + i.U
        csBundle(i + 1).ctrl.ldest := dest + i.U
        csBundle(i + 1).ctrl.uopIdx := i.U
      }
    }
    is(UopDivType.VEC_FSLIDE1UP) {
      //LMUL
      csBundle(0).ctrl.srcType(0) := SrcType.fp
      csBundle(0).ctrl.lsrc(0) := src1
      csBundle(0).ctrl.lsrc(1) := src2
      csBundle(0).ctrl.lsrc(2) := dest
      csBundle(0).ctrl.ldest := dest
      csBundle(0).ctrl.uopIdx := 0.U
      for (i <- 1 until MAX_VLMUL) {
        csBundle(i).ctrl.srcType(0) := SrcType.vp
        csBundle(i).ctrl.lsrc(0) := src2 + (i - 1).U
        csBundle(i).ctrl.lsrc(1) := src2 + i.U
        csBundle(i).ctrl.lsrc(2) := dest + i.U
        csBundle(i).ctrl.ldest := dest + i.U
        csBundle(i).ctrl.uopIdx := i.U
      }
    }
    is(UopDivType.VEC_SLIDE1DOWN) { // lmul+lmul = 16
      /*
      FMV.D.X
       */
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(2 * i + 1).ctrl.srcType(0) := SrcType.vp
        csBundle(2 * i + 1).ctrl.srcType(1) := SrcType.vp
        csBundle(2 * i + 1).ctrl.lsrc(0) := src2 + (i+1).U
        csBundle(2 * i + 1).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i + 1).ctrl.lsrc(2) := dest + i.U
        csBundle(2 * i + 1).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i).U
        if (2 * i + 2 < MAX_VLMUL * 2 ){
          csBundle(2 * i + 2).ctrl.srcType(0) := SrcType.fp
          csBundle(2 * i + 2).ctrl.lsrc(0) := FP_TMP_REG_MV.U
          // csBundle(2 * i + 2).ctrl.lsrc(1) := src2 + i.U         // DontCare
          csBundle(2 * i + 2).ctrl.lsrc(2) := VECTOR_TMP_REG_LMUL.U
          csBundle(2 * i + 2).ctrl.ldest := dest + i.U
          csBundle(2 * i + 2).ctrl.uopIdx := (2 * i + 1).U
        }
      }
      csBundle(numOfUop - 1.U).ctrl.srcType(0) := SrcType.fp
      csBundle(numOfUop - 1.U).ctrl.lsrc(0) := FP_TMP_REG_MV.U
      csBundle(numOfUop - 1.U).ctrl.ldest := dest + lmul - 1.U
    }
    is(UopDivType.VEC_FSLIDE1DOWN) {
      //LMUL
      for (i <- 0 until MAX_VLMUL) {
        csBundle(2 * i).ctrl.srcType(0) := SrcType.vp
        csBundle(2 * i).ctrl.srcType(1) := SrcType.vp
        csBundle(2 * i).ctrl.lsrc(0) := src2 + (i+1).U
        csBundle(2 * i).ctrl.lsrc(1) := src2 + i.U
        csBundle(2 * i).ctrl.lsrc(2) := dest + i.U
        csBundle(2 * i).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i).ctrl.uopIdx := (2 * i).U
        csBundle(2 * i + 1).ctrl.srcType(0) := SrcType.fp
        csBundle(2 * i + 1).ctrl.lsrc(0) := src1
        csBundle(2 * i + 1).ctrl.lsrc(2) := VECTOR_TMP_REG_LMUL.U
        csBundle(2 * i + 1).ctrl.ldest := dest + i.U
        csBundle(2 * i + 1).ctrl.uopIdx := (2 * i + 1).U
      }
      csBundle(numOfUop - 1.U).ctrl.srcType(0) := SrcType.fp
      csBundle(numOfUop - 1.U).ctrl.lsrc(0) := src1
      csBundle(numOfUop - 1.U).ctrl.ldest := dest + lmul - 1.U
    }
    is(UopDivType.VEC_VRED) {
      when(simple.io.vconfig.vtype.vlmul === "b001".U){
          csBundle(0).ctrl.srcType(2) := SrcType.DC
          csBundle(0).ctrl.lsrc(0) := src2 + 1.U
          csBundle(0).ctrl.lsrc(1) := src2
          csBundle(0).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
          csBundle(0).ctrl.uopIdx := 0.U
      }
      when(simple.io.vconfig.vtype.vlmul === "b010".U) {
        csBundle(0).ctrl.srcType(2) := SrcType.DC
        csBundle(0).ctrl.lsrc(0) := src2 + 1.U
        csBundle(0).ctrl.lsrc(1) := src2
        csBundle(0).ctrl.ldest := VECTOR_TMP_REG_LMUL.U
        csBundle(0).ctrl.uopIdx := 0.U

        csBundle(1).ctrl.srcType(2) := SrcType.DC
        csBundle(1).ctrl.lsrc(0) := src2 + 3.U
        csBundle(1).ctrl.lsrc(1) := src2 + 2.U
        csBundle(1).ctrl.ldest := (VECTOR_TMP_REG_LMUL+1).U
        csBundle(1).ctrl.uopIdx := 1.U

        csBundle(2).ctrl.srcType(2) := SrcType.DC
        csBundle(2).ctrl.lsrc(0) := (VECTOR_TMP_REG_LMUL+1).U
        csBundle(2).ctrl.lsrc(1) := VECTOR_TMP_REG_LMUL.U
        csBundle(2).ctrl.ldest := (VECTOR_TMP_REG_LMUL+2).U
        csBundle(2).ctrl.uopIdx := 2.U
      }
      when(simple.io.vconfig.vtype.vlmul === "b011".U) {
        for(i <- 0 until MAX_VLMUL){
          if(i < MAX_VLMUL - MAX_VLMUL/2){
            csBundle(i).ctrl.lsrc(0) := src2 + (i * 2 + 1).U
            csBundle(i).ctrl.lsrc(1) := src2 + (i * 2).U
            csBundle(i).ctrl.ldest := (VECTOR_TMP_REG_LMUL + i).U
          } else if (i < MAX_VLMUL - MAX_VLMUL/4) {
            csBundle(i).ctrl.lsrc(0) := (VECTOR_TMP_REG_LMUL + (i - MAX_VLMUL/2)*2 + 1).U
            csBundle(i).ctrl.lsrc(1) := (VECTOR_TMP_REG_LMUL + (i - MAX_VLMUL/2)*2).U
            csBundle(i).ctrl.ldest := (VECTOR_TMP_REG_LMUL + i).U
          }else if (i < MAX_VLMUL - MAX_VLMUL/8) {
            csBundle(6).ctrl.lsrc(0) := (VECTOR_TMP_REG_LMUL + 5).U
            csBundle(6).ctrl.lsrc(1) := (VECTOR_TMP_REG_LMUL + 4).U
            csBundle(6).ctrl.ldest := (VECTOR_TMP_REG_LMUL + 6).U
          }
          csBundle(i).ctrl.srcType(2) := SrcType.DC
          csBundle(i).ctrl.uopIdx := i.U
        }
      }
      when (simple.io.vconfig.vtype.vlmul.orR()){
        csBundle(numOfUop - 1.U).ctrl.srcType(2) := SrcType.vp
        csBundle(numOfUop - 1.U).ctrl.lsrc(0) := src1
        csBundle(numOfUop - 1.U).ctrl.lsrc(1) := VECTOR_TMP_REG_LMUL.U + numOfUop - 2.U
        csBundle(numOfUop - 1.U).ctrl.lsrc(2) := dest
        csBundle(numOfUop - 1.U).ctrl.ldest := dest
        csBundle(numOfUop - 1.U).ctrl.uopIdx := numOfUop - 1.U
      }
    }

    is(UopDivType.VEC_SLIDEUP) {
      // FMV.D.X
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B
      // LMUL
      for(i <- 0 until MAX_VLMUL)
        for(j <- 0 to i){
          val old_vd = if (j==0) {dest + i.U} else (VECTOR_TMP_REG_LMUL+j-1).U
          val vd = if (j==i) {dest + i.U} else (VECTOR_TMP_REG_LMUL+j).U
          csBundle(i*(i+1)/2+j+1).ctrl.srcType(0) := SrcType.fp
          csBundle(i*(i+1)/2+j+1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
          csBundle(i*(i+1)/2+j+1).ctrl.lsrc(1) := src2 + j.U
          csBundle(i*(i+1)/2+j+1).ctrl.lsrc(2) := old_vd
          csBundle(i*(i+1)/2+j+1).ctrl.ldest := vd
          csBundle(i*(i+1)/2+j+1).ctrl.uopIdx := (i*(i+1)/2+j).U
        }
    }

    is(UopDivType.VEC_ISLIDEUP) {
      // LMUL
      for(i <- 0 until MAX_VLMUL)
        for(j <- 0 to i){
          val old_vd = if (j==0) {dest + i.U} else (VECTOR_TMP_REG_LMUL+j-1).U
          val vd = if (j==i) {dest + i.U} else (VECTOR_TMP_REG_LMUL+j).U
          csBundle(i*(i+1)/2+j).ctrl.lsrc(1) := src2 + j.U
          csBundle(i*(i+1)/2+j).ctrl.lsrc(2) := old_vd
          csBundle(i*(i+1)/2+j).ctrl.ldest := vd
          csBundle(i*(i+1)/2+j).ctrl.uopIdx := (i*(i+1)/2+j).U
        }
    }

    is(UopDivType.VEC_SLIDEDOWN) {
      // FMV.D.X
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B
      // LMUL
      for(i <- 0 until MAX_VLMUL)
        for(j <- (0 to i).reverse){
          when(i.U < lmul){
            val old_vd = if (j==0) {dest + lmul -1.U - i.U} else (VECTOR_TMP_REG_LMUL+j-1).U
            val vd = if (j==i) {dest + lmul - 1.U - i.U} else (VECTOR_TMP_REG_LMUL+j).U
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.srcType(0) := SrcType.fp
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.lsrc(0) := FP_TMP_REG_MV.U
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.lsrc(1) := src2 + lmul - 1.U - j.U
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.lsrc(2) := old_vd
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.ldest := vd
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.uopIdx := numOfUop-(i*(i+1)/2+i-j+2).U
          }
        }
    }

    is(UopDivType.VEC_ISLIDEDOWN) {
      // LMUL
      for(i <- 0 until MAX_VLMUL)
        for(j <- (0 to i).reverse){
          when(i.U < lmul){
            val old_vd = if (j==0) {dest + lmul -1.U - i.U} else (VECTOR_TMP_REG_LMUL+j-1).U
            val vd = if (j==i) {dest + lmul - 1.U - i.U} else (VECTOR_TMP_REG_LMUL+j).U
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.lsrc(1) := src2 + lmul - 1.U - j.U
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.lsrc(2) := old_vd
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.ldest := vd
            csBundle(numOfUop-(i*(i+1)/2+i-j+1).U).ctrl.uopIdx := numOfUop-(i*(i+1)/2+i-j+1).U
          }
        }
    }

    is(UopDivType.VEC_M0X) {
      // LMUL
      for (i <- 0 until MAX_VLMUL) {
        val srcType0 = if (i==0) SrcType.DC else SrcType.vp
        val ldest = (VECTOR_TMP_REG_LMUL + i).U
        csBundle(i).ctrl.srcType(0) := srcType0
        csBundle(i).ctrl.srcType(1) := SrcType.vp
        csBundle(i).ctrl.rfWen := false.B
        csBundle(i).ctrl.vecWen := true.B
        csBundle(i).ctrl.lsrc(0) := (VECTOR_TMP_REG_LMUL + i - 1).U
        csBundle(i).ctrl.lsrc(1) := src2
        // csBundle(i).ctrl.lsrc(2) := dest + i.U  DontCare
        csBundle(i).ctrl.ldest := ldest
        csBundle(i).ctrl.uopIdx := i.U
      }
      csBundle(lmul-1.U).ctrl.vecWen := false.B
      csBundle(lmul-1.U).ctrl.fpWen := true.B
      csBundle(lmul-1.U).ctrl.ldest := FP_TMP_REG_MV.U
      // FMV_X_D
      csBundle(lmul).ctrl.srcType(0) := SrcType.fp
      csBundle(lmul).ctrl.srcType(1) := SrcType.imm
      csBundle(lmul).ctrl.lsrc(0) := FP_TMP_REG_MV.U
      csBundle(lmul).ctrl.lsrc(1) := 0.U
      csBundle(lmul).ctrl.ldest := dest
      csBundle(lmul).ctrl.fuType := FuType.fmisc
      csBundle(lmul).ctrl.rfWen := true.B
      csBundle(lmul).ctrl.fpWen := false.B
      csBundle(lmul).ctrl.vecWen := false.B
      csBundle(lmul).ctrl.fpu.isAddSub := false.B
      csBundle(lmul).ctrl.fpu.typeTagIn := FPU.D
      csBundle(lmul).ctrl.fpu.typeTagOut := FPU.D
      csBundle(lmul).ctrl.fpu.fromInt := false.B
      csBundle(lmul).ctrl.fpu.wflags := false.B
      csBundle(lmul).ctrl.fpu.fpWen := false.B
      csBundle(lmul).ctrl.fpu.div := false.B
      csBundle(lmul).ctrl.fpu.sqrt := false.B
      csBundle(lmul).ctrl.fpu.fcvt := false.B
    }

    is(UopDivType.VEC_MVV) {
      // LMUL
      for (i <- 0 until MAX_VLMUL) {
        val srcType0 = if (i==0) SrcType.DC else SrcType.vp
        csBundle(i*2+0).ctrl.srcType(0) := srcType0
        csBundle(i*2+0).ctrl.srcType(1) := SrcType.vp
        csBundle(i*2+0).ctrl.lsrc(0) := (VECTOR_TMP_REG_LMUL + i - 1).U
        csBundle(i*2+0).ctrl.lsrc(1) := src2
        csBundle(i*2+0).ctrl.lsrc(2) := dest + i.U
        csBundle(i*2+0).ctrl.ldest := dest + i.U
        csBundle(i*2+0).ctrl.uopIdx := (i*2+0).U

        csBundle(i*2+1).ctrl.srcType(0) := srcType0
        csBundle(i*2+1).ctrl.srcType(1) := SrcType.vp
        csBundle(i*2+1).ctrl.lsrc(0) := (VECTOR_TMP_REG_LMUL + i - 1).U
        csBundle(i*2+1).ctrl.lsrc(1) := src2
        // csBundle(i).ctrl.lsrc(2) := dest + i.U  DontCare
        csBundle(i*2+1).ctrl.ldest := (VECTOR_TMP_REG_LMUL + i).U
        csBundle(i*2+1).ctrl.uopIdx := (i*2+1).U
      }
    }

    is(UopDivType.VEC_M0X_VFIRST) {
      // LMUL
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      // FMV_X_D
      csBundle(1).ctrl.srcType(0) := SrcType.fp
      csBundle(1).ctrl.srcType(1) := SrcType.imm
      csBundle(1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
      csBundle(1).ctrl.lsrc(1) := 0.U
      csBundle(1).ctrl.ldest := dest
      csBundle(1).ctrl.fuType := FuType.fmisc
      csBundle(1).ctrl.rfWen := true.B
      csBundle(1).ctrl.fpWen := false.B
      csBundle(1).ctrl.vecWen := false.B
      csBundle(1).ctrl.fpu.isAddSub := false.B
      csBundle(1).ctrl.fpu.typeTagIn := FPU.D
      csBundle(1).ctrl.fpu.typeTagOut := FPU.D
      csBundle(1).ctrl.fpu.fromInt := false.B
      csBundle(1).ctrl.fpu.wflags := false.B
      csBundle(1).ctrl.fpu.fpWen := false.B
      csBundle(1).ctrl.fpu.div := false.B
      csBundle(1).ctrl.fpu.sqrt := false.B
      csBundle(1).ctrl.fpu.fcvt := false.B
    }

    is(UopDivType.VEC_VWW) {
      for (i <- 0 until MAX_VLMUL*2) {
        when(i.U < lmul){
          csBundle(i).ctrl.srcType(2) := SrcType.DC
          csBundle(i).ctrl.lsrc(0) := src2 + i.U
          csBundle(i).ctrl.lsrc(1) := src2 + i.U
          // csBundle(i).ctrl.lsrc(2) := dest + (2 * i).U
          csBundle(i).ctrl.ldest := (VECTOR_TMP_REG_LMUL + i).U
          csBundle(i).ctrl.uopIdx :=  i.U
        } otherwise {
          csBundle(i).ctrl.srcType(2) := SrcType.DC
          csBundle(i).ctrl.lsrc(0) := VECTOR_TMP_REG_LMUL.U + Cat((i.U-lmul),0.U(1.W)) + 1.U
          csBundle(i).ctrl.lsrc(1) := VECTOR_TMP_REG_LMUL.U + Cat((i.U-lmul),0.U(1.W))
          // csBundle(i).ctrl.lsrc(2) := dest + (2 * i).U
          csBundle(i).ctrl.ldest := (VECTOR_TMP_REG_LMUL + i).U
          csBundle(i).ctrl.uopIdx := i.U
        }
        csBundle(numOfUop-1.U).ctrl.srcType(2) := SrcType.vp
        csBundle(numOfUop-1.U).ctrl.lsrc(0) := src1
        csBundle(numOfUop-1.U).ctrl.lsrc(2) := dest
        csBundle(numOfUop-1.U).ctrl.ldest := dest
      }
    }

    is(UopDivType.VEC_RGATHER) {
      def genCsBundle_VEC_RGATHER(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            // csBundle(i * len + j).ctrl.srcType(0) := SrcType.vp // SrcType.imm
            // csBundle(i * len + j).ctrl.srcType(1) := SrcType.vp
            // csBundle(i * len + j).ctrl.srcType(2) := SrcType.vp
            csBundle(i * len + j).ctrl.lsrc(0) := src1 + i.U
            csBundle(i * len + j).ctrl.lsrc(1) := src2 + j.U
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j - 1).U
            csBundle(i * len + j).ctrl.lsrc(2) := vd_old
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j).ctrl.ldest := vd
            csBundle(i * len + j).ctrl.uopIdx := (i * len + j).U
          }
      }
      switch(simple.io.vconfig.vtype.vlmul) {
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

    is(UopDivType.VEC_RGATHER_VX) {
      def genCsBundle_RGATHER_VX(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            csBundle(i * len + j + 1).ctrl.srcType(0) := SrcType.fp
            // csBundle(i * len + j + 1).ctrl.srcType(1) := SrcType.vp
            // csBundle(i * len + j + 1).ctrl.srcType(2) := SrcType.vp
            csBundle(i * len + j + 1).ctrl.lsrc(0) := FP_TMP_REG_MV.U
            csBundle(i * len + j + 1).ctrl.lsrc(1) := src2 + j.U
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j - 1).U
            csBundle(i * len + j + 1).ctrl.lsrc(2) := vd_old
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            csBundle(i * len + j + 1).ctrl.ldest := vd
            csBundle(i * len + j + 1).ctrl.uopIdx := (i * len + j).U
          }
      }

      // FMV.D.X
      csBundle(0).ctrl.srcType(0) := SrcType.reg
      csBundle(0).ctrl.srcType(1) := SrcType.imm
      csBundle(0).ctrl.lsrc(1) := 0.U
      csBundle(0).ctrl.ldest := FP_TMP_REG_MV.U
      csBundle(0).ctrl.fuType := FuType.i2f
      csBundle(0).ctrl.rfWen := false.B
      csBundle(0).ctrl.fpWen := true.B
      csBundle(0).ctrl.vecWen := false.B
      csBundle(0).ctrl.fpu.isAddSub := false.B
      csBundle(0).ctrl.fpu.typeTagIn := FPU.D
      csBundle(0).ctrl.fpu.typeTagOut := FPU.D
      csBundle(0).ctrl.fpu.fromInt := true.B
      csBundle(0).ctrl.fpu.wflags := false.B
      csBundle(0).ctrl.fpu.fpWen := true.B
      csBundle(0).ctrl.fpu.div := false.B
      csBundle(0).ctrl.fpu.sqrt := false.B
      csBundle(0).ctrl.fpu.fcvt := false.B

      switch(simple.io.vconfig.vtype.vlmul) {
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

    is(UopDivType.VEC_RGATHEREI16) {
      def genCsBundle_VEC_RGATHEREI16_SEW8(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            val vd_old0 = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j*2-1).U
            val vd0 = (VECTOR_TMP_REG_LMUL + j*2 ).U
            // csBundle(i * len + j).ctrl.srcType(0) := SrcType.vp // SrcType.imm
            // csBundle(i * len + j).ctrl.srcType(1) := SrcType.vp
            // csBundle(i * len + j).ctrl.srcType(2) := SrcType.vp
            csBundle((i * len + j)*2+0).ctrl.lsrc(0) := src1 + (i*2+0).U
            csBundle((i * len + j)*2+0).ctrl.lsrc(1) := src2 + j.U
            csBundle((i * len + j)*2+0).ctrl.lsrc(2) := vd_old0
            csBundle((i * len + j)*2+0).ctrl.ldest := vd0
            csBundle((i * len + j)*2+0).ctrl.uopIdx := ((i * len + j)*2+0).U

            val vd_old1 = (VECTOR_TMP_REG_LMUL + j*2).U
            val vd1 = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j*2+1 ).U
            csBundle((i * len + j)*2+1).ctrl.lsrc(0) := src1 + (i*2+1).U
            csBundle((i * len + j)*2+1).ctrl.lsrc(1) := src2 + j.U
            csBundle((i * len + j)*2+1).ctrl.lsrc(2) := vd_old1
            csBundle((i * len + j)*2+1).ctrl.ldest := vd1
            csBundle((i * len + j)*2+1).ctrl.uopIdx := ((i * len + j)*2+1).U
          }
      }
      def genCsBundle_VEC_RGATHEREI16(len:Int): Unit ={
        for (i <- 0 until len)
          for (j <- 0 until len) {
            val vd_old = if(j==0) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j-1).U
            val vd = if(j==len-1) (dest + i.U) else (VECTOR_TMP_REG_LMUL + j).U
            // csBundle(i * len + j).ctrl.srcType(0) := SrcType.vp // SrcType.imm
            // csBundle(i * len + j).ctrl.srcType(1) := SrcType.vp
            // csBundle(i * len + j).ctrl.srcType(2) := SrcType.vp
            csBundle(i * len + j).ctrl.lsrc(0) := src1 + i.U
            csBundle(i * len + j).ctrl.lsrc(1) := src2 + j.U
            csBundle(i * len + j).ctrl.lsrc(2) := vd_old
            csBundle(i * len + j).ctrl.ldest := vd
            csBundle(i * len + j).ctrl.uopIdx := (i * len + j).U
          }
      }
      switch(simple.io.vconfig.vtype.vlmul) {
        is("b000".U ){
          when(!simple.io.vconfig.vtype.vsew.orR()){
            genCsBundle_VEC_RGATHEREI16_SEW8(1)
          } .otherwise{
            genCsBundle_VEC_RGATHEREI16(1)
          }
        }

        is("b001".U) {
          when(!simple.io.vconfig.vtype.vsew.orR()) {
            genCsBundle_VEC_RGATHEREI16_SEW8(2)
          }.otherwise {
            genCsBundle_VEC_RGATHEREI16(2)
          }
        }

        is("b010".U) {
          when(!simple.io.vconfig.vtype.vsew.orR()) {
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
  }

  //uops dispatch
  val normal :: ext :: Nil = Enum(2)
  val stateReg = RegInit(normal)
  val uopRes = RegInit(0.U)

  //readyFromRename Counter
  val readyCounter = PriorityMuxDefault(io.readyFromRename.map(x => !x).zip((0 to (RenameWidth - 1)).map(_.U)), RenameWidth.U)

  switch(stateReg) {
    is(normal) {
      stateReg := Mux(io.validFromIBuf(0) && (numOfUop > readyCounter) && (readyCounter =/= 0.U), ext, normal)
    }
    is(ext) {
      stateReg := Mux(io.validFromIBuf(0) && (uopRes > readyCounter), ext, normal)
    }
  }

  val uopRes0 = Mux(stateReg === normal, numOfUop, uopRes)
  val uopResJudge = Mux(stateReg === normal,
                        io.validFromIBuf(0) && (readyCounter =/= 0.U) && (uopRes0 > readyCounter),
                        io.validFromIBuf(0) && (uopRes0 > readyCounter))
  uopRes := Mux(uopResJudge, uopRes0 - readyCounter, 0.U)

  for(i <- 0 until RenameWidth) {
    cf_ctrl(i) := MuxCase(csBundle(i), Seq(
      (stateReg === normal) -> csBundle(i),
      (stateReg === ext) -> Mux((i.U + numOfUop -uopRes) < maxNumOfUop.U, csBundle(i.U + numOfUop - uopRes), csBundle(maxNumOfUop - 1))
    ))
  }


  val validSimple = Wire(Vec(DecodeWidth - 1, Bool()))
  validSimple.zip(io.validFromIBuf.drop(1).zip(io.isComplex)).map{ case (dst, (src1, src2)) => dst := src1 && !src2 }
  val notInf = Wire(Vec(DecodeWidth - 1, Bool()))
  notInf.zip(io.validFromIBuf.drop(1).zip(validSimple)).map{ case (dst, (src1, src2)) => dst := !src1 || src2 }
  val notInfVec = Wire(Vec(DecodeWidth, Bool()))
  notInfVec.drop(1).zip(0 until DecodeWidth - 1).map{ case (dst, i) => dst := Cat(notInf.take(i + 1)).andR}
  notInfVec(0) := true.B

  complexNum := Mux(io.validFromIBuf(0) && readyCounter.orR , 
                    Mux(uopRes0 > readyCounter, readyCounter, uopRes0),
                    1.U)
  validToRename.zipWithIndex.foreach{
    case(dst, i) =>
      dst := MuxCase(false.B, Seq(
        (io.validFromIBuf(0) && uopRes0 > readyCounter   ) -> Mux(readyCounter > i.U, true.B, false.B),
        (io.validFromIBuf(0) && !(uopRes0 > readyCounter)) -> Mux(complexNum > i.U, true.B, validSimple(i.U - complexNum) && notInfVec(i.U - complexNum) && io.readyFromRename(i)),
      ))
  }

  readyToIBuf.zipWithIndex.foreach {
    case (dst, i) =>
      dst := MuxCase(true.B, Seq(
        (io.validFromIBuf(0) && uopRes0 > readyCounter) -> false.B,
        (io.validFromIBuf(0) && !(uopRes0 > readyCounter)) -> (if (i==0) true.B else Mux(RenameWidth.U - complexNum >= i.U, notInfVec(i - 1) && validSimple(i - 1) && io.readyFromRename(i), false.B)),
      ))
  }

  io.deq.cf_ctrl := cf_ctrl
  io.deq.isVset := isVset_u
  io.deq.complexNum := complexNum
  io.deq.validToRename := validToRename
  io.deq.readyToIBuf := readyToIBuf

}

