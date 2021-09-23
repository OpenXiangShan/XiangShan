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

package xiangshan.backend.fu.fpu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import fudian.{FCMA, FCMA_ADD, FMUL, FMULToFADD}
import xiangshan._
import utils._


class MulToAddIO(val ftypes: Seq[FPU.FType])(implicit val p: Parameters) extends Bundle {
  val mul_out = MixedVec(ftypes.map(t => new FMULToFADD(t.expWidth, t.precision)))
  val addend = UInt(ftypes.map(_.len).max.W)
  val uop = new MicroOp

  def getFloat = mul_out.head
  def getDouble = mul_out.last
}

class FMUL_pipe(val mulLat: Int = 2)(implicit p: Parameters)
  extends FPUPipelineModule
{
  override def latency: Int = mulLat
  override val dataModule: FPUDataModule = null

  val toAdd = IO(Output(new MulToAddIO(FPU.ftypes)))

  val uopIn = uopVec(0)
  val fpCtrl = uopIn.ctrl.fpu
  val typeTagIn = fpCtrl.typeTagIn

  val src1 = FPU.unbox(io.in.bits.src(0), typeTagIn)
  val src2 = FPU.unbox(io.in.bits.src(1), typeTagIn)

  val s_mul :: d_mul :: Nil = FPU.ftypes.zipWithIndex.map{ case (ftype, i) =>
    val mul = Module(new FMUL(ftype.expWidth, ftype.precision))
    val in1 = src1
    val in2 = Mux(fpCtrl.fmaCmd(1), invert_sign(src2, ftype.len), src2)
    mul.io.a := in1
    mul.io.b := in2
    mul.io.rm := rm
    mul
  }
  val muls = Seq(s_mul, d_mul)
  val singleOut = typeTagIn === FPU.S
  val result = Mux(singleOut,
    FPU.box(Cat(0.U(32.W), s_mul.io.result), FPU.S),
    FPU.box(d_mul.io.result, FPU.D)
  )
  val exc = Mux(singleOut,
    s_mul.io.fflags,
    d_mul.io.fflags
  )
  val stages = Wire(Vec(latency, new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
    val toAdd = new MulToAddIO(FPU.ftypes)
  }))

  for((s, i) <- stages.zipWithIndex){
    if(i == 0){
      val en = regEnable(i+1)
      s.data := RegEnable(result, en)
      s.exc := RegEnable(exc, en)
      s.toAdd.addend := RegEnable(io.in.bits.src(2), en)
      for(i <- FPU.ftypes.indices){
        s.toAdd.mul_out(i) := RegEnable(muls(i).io.to_fadd, en)
      }
      // we already save it in pipeline regs
      s.toAdd.uop := DontCare
    } else {
      s := RegEnable(stages(i - 1), regEnable(i+1))
    }
  }
  toAdd := stages.last.toAdd
  toAdd.uop := uopVec.last
  io.out.bits.data := stages.last.data
  fflags := stages.last.exc
}

class FADD_pipe(val addLat: Int = 2)(implicit p: Parameters) extends FPUPipelineModule {
  override val dataModule: FPUDataModule = null
  override def latency: Int = addLat

  val mulToAdd = IO(Input(new MulToAddIO(FPU.ftypes)))
  val isFMA = IO(Input(Bool()))

  val uopIn = Mux(isFMA, mulToAdd.uop, io.in.bits.uop)
  val fpCtrl = uopIn.ctrl.fpu
  val typeTagIn = fpCtrl.typeTagIn

  val src1 = FPU.unbox(io.in.bits.src(0), typeTagIn)
  val src2 = FPU.unbox(
    Mux(isFMA, mulToAdd.addend, io.in.bits.src(1)), typeTagIn
  )

  // TODO: reuse hardware
  val s_adder :: d_adder :: Nil = FPU.ftypes.zipWithIndex.map { case (ftype,i) =>
    val fadder = Module(new FCMA_ADD(
      ftype.expWidth, 2*ftype.precision, ftype.precision
    ))
    val w = ftype.len
    val in1 = Mux(isFMA,
      mulToAdd.mul_out(i).fp_prod.asUInt(),
      Cat(src1(ftype.len - 1, 0), 0.U(ftype.precision.W))
    )
    val in2 = Cat(
      Mux(fpCtrl.fmaCmd(0), invert_sign(src2, ftype.len), src2(ftype.len - 1, 0)),
      0.U(ftype.precision.W)
    )
    fadder.io.a := in1
    fadder.io.b := in2
    fadder.io.b_inter_valid := isFMA
    fadder.io.b_inter_flags := Mux(isFMA,
      mulToAdd.mul_out(i).inter_flags,
      0.U.asTypeOf(fadder.io.b_inter_flags)
    )
    fadder.io.rm := rm
    fadder
  }

  val singleOut = typeTagIn === FPU.S
  val result = Mux(singleOut,
    FPU.box(Cat(0.U(32.W), s_adder.io.result), FPU.S),
    FPU.box(d_adder.io.result, FPU.D)
  )
  val exc = Mux(singleOut,
    s_adder.io.fflags,
    d_adder.io.fflags
  )
  val stages = Wire(Vec(latency, new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  }))

  for((s, i) <- stages.zipWithIndex){
    if(i == 0){
      s.data := RegEnable(result, regEnable(i+1))
      s.exc := RegEnable(exc, regEnable(i+1))
    } else {
      s := RegEnable(stages(i - 1), regEnable(i+1))
    }
  }

  io.out.bits.data := stages.last.data
  fflags := stages.last.exc
}

class FMAMidResult extends FMULToFADD(FPU.ftypes.last.expWidth, FPU.ftypes.last.precision) {

  def toFloat: FMULToFADD = {
    val floatMidResult = Wire(new FMULToFADD(FPU.ftypes.head.expWidth, FPU.ftypes.head.precision))
    floatMidResult.fp_prod.sign := fp_prod.sign
    floatMidResult.fp_prod.exp := fp_prod.exp
    floatMidResult.fp_prod.sig := fp_prod.sig
    floatMidResult.inter_flags := inter_flags
    floatMidResult
  }

  def fromFloat(float: FMULToFADD): FMULToFADD = {
    fp_prod.sign := float.fp_prod.sign
    fp_prod.exp := float.fp_prod.exp
    fp_prod.sig := float.fp_prod.sig
    inter_flags := float.inter_flags
    this
  }
}

class FMAMidResultIO extends Bundle {
  val in = Flipped(ValidIO(new FMAMidResult))
  val out = ValidIO(new FMAMidResult)
  val waitForAdd = Input(Bool())
}

class FMA(implicit p: Parameters) extends FPUSubModule {
  val midResult = IO(new FMAMidResultIO)

  override val dataModule = null
  val mul_pipe = Module(new FMUL_pipe())
  val add_pipe = Module(new FADD_pipe())


  mul_pipe.io.redirectIn := io.redirectIn
  mul_pipe.io.flushIn := io.flushIn
  mul_pipe.rm := rm

  add_pipe.io.redirectIn := io.redirectIn
  add_pipe.io.flushIn := io.flushIn
  add_pipe.rm := rm

  val fpCtrl = io.in.bits.uop.ctrl.fpu
  mul_pipe.io.in <> io.in
  mul_pipe.io.in.valid := io.in.valid && !fpCtrl.isAddSub && !midResult.in.valid

  // For better timing, we let out.valid be true even if it's flushed.
  val waitAddOperand = RegEnable(midResult.waitForAdd, !mul_pipe.io.out.valid || mul_pipe.io.out.ready)
  val isFMA = mul_pipe.io.out.valid && mul_pipe.io.out.bits.uop.ctrl.fpu.ren3 && !waitAddOperand
  // However, when sending instructions to add_pipe, we need to determine whether it's flushed.
  val mulFlushed = mul_pipe.io.out.bits.uop.roqIdx.needFlush(io.redirectIn, io.flushIn)
  val isFMAReg = RegNext(isFMA && !mulFlushed)

  add_pipe.mulToAdd <> mul_pipe.toAdd
  midResult.out.valid := RegNext(mul_pipe.io.out.valid && waitAddOperand && !mulFlushed)
  midResult.out.bits := mul_pipe.toAdd.getDouble
  when (RegNext(mul_pipe.io.out.bits.uop.ctrl.fpu.typeTagIn === FPU.S)) {
    midResult.out.bits.fromFloat(mul_pipe.toAdd.getFloat)
  }
  when (midResult.in.valid && !isFMAReg) {
    add_pipe.mulToAdd.getDouble := midResult.in.bits
    add_pipe.mulToAdd.getFloat := midResult.in.bits.toFloat
    add_pipe.mulToAdd.addend := io.in.bits.src(2)
    add_pipe.mulToAdd.uop := io.in.bits.uop
  }

  // For FADD, it accepts instructions from io.in and FMUL.
  // When FMUL gives an FMA, FADD accepts this instead of io.in.
  // Since FADD gets FMUL data from add_pipe.mulToAdd, only uop needs Mux.
  add_pipe.io.in.valid := io.in.valid && (fpCtrl.isAddSub || midResult.in.valid) || isFMAReg
  add_pipe.io.in.bits.src := io.in.bits.src
  add_pipe.io.in.bits.uop := Mux(isFMAReg, add_pipe.mulToAdd.uop, io.in.bits.uop)
  add_pipe.isFMA := io.in.valid && midResult.in.valid || isFMAReg

  // When the in uop is Add/Sub, we check FADD, otherwise fmul is checked.
  io.in.ready := Mux(fpCtrl.isAddSub || midResult.in.valid,
    !isFMAReg && add_pipe.io.in.ready,
    mul_pipe.io.in.ready
  )

  // For FMUL:
  // (1) It always accept FMA from FADD (if an FMA wants FMUL, it's never blocked).
  // (2) It has lower writeback arbitration priority than FADD (and may be blocked when FMUL.out.valid).
  XSError(isFMA && !add_pipe.io.in.ready, "FMA should not be blocked\n")
  mul_pipe.io.out.ready := isFMA || (io.out.ready && !add_pipe.io.out.valid) || waitAddOperand
  add_pipe.io.out.ready := io.out.ready

  io.out.bits.uop := Mux(add_pipe.io.out.valid,
    add_pipe.io.out.bits.uop,
    mul_pipe.io.out.bits.uop
  )
  io.out.bits.data := Mux(RegNext(add_pipe.io.out.valid),
    add_pipe.io.out.bits.data,
    mul_pipe.io.out.bits.data
  )
  fflags := Mux(RegNext(add_pipe.io.out.valid),
    add_pipe.fflags,
    mul_pipe.fflags
  )
  io.out.valid := add_pipe.io.out.valid || (mul_pipe.io.out.valid && !isFMA && !waitAddOperand)

  XSPerfAccumulate("fma_partial_issue_fire", io.in.fire && midResult.waitForAdd)
  XSPerfAccumulate("fma_mid_result_in_fire", io.in.fire && midResult.in.valid)
}
