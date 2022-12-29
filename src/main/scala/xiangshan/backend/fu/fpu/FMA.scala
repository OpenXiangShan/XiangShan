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
import fudian.utils.Multiplier
import fudian.{FCMA, FCMA_ADD, FCMA_ADD_s1, FCMA_ADD_s2, FMUL, FMULToFADD, FMUL_s1, FMUL_s2, FMUL_s3, RawFloat}
import xiangshan._
import utils._
import utility._


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

  val typeSel = VecInit(FPU.ftypes.zipWithIndex.map(_._2.U === typeTagIn))

  val src1 = FPU.unbox(io.in.bits.src(0), typeTagIn)
  val src2 = FPU.unbox(io.in.bits.src(1), typeTagIn)

  val multiplier = Module(new Multiplier(FPU.ftypes.last.precision+1, pipeAt = Seq(1)))

  val stages = FPU.ftypes.map{ t =>
    // s1 -> s2 -> s3
    val s1 = Module(new FMUL_s1(t.expWidth, t.precision))
    val s2 = Module(new FMUL_s2(t.expWidth, t.precision))
    val s3 = Module(new FMUL_s3(t.expWidth, t.precision))

    val in1 = src1
    val in2 = Mux(fpCtrl.fmaCmd(1), invert_sign(src2, t.len), src2)
    s1.io.a := in1
    s1.io.b := in2
    s1.io.rm := rm

    s2.io.in := S1Reg(s1.io.out)
    s2.io.prod := multiplier.io.result
    s3.io.in := S2Reg(s2.io.out)
    (s1, s2, s3)
  }

  val (s1, s2, s3) = stages.unzip3
  val (mul_a_sel, mul_b_sel) = s1.zipWithIndex.map{
    case (s, i) =>
      val raw_a = RawFloat.fromUInt(s.io.a, s.expWidth, s.precision)
      val raw_b = RawFloat.fromUInt(s.io.b, s.expWidth, s.precision)
      (
        (typeTagIn === i.U) -> raw_a.sig,
        (typeTagIn === i.U) -> raw_b.sig
      )
  }.unzip
  multiplier.io.a := Mux1H(mul_a_sel)
  multiplier.io.b := Mux1H(mul_b_sel)
  multiplier.io.regEnables(0) := regEnable(1)

  val outSel = S2Reg(S1Reg(typeSel))

  val s_mul :: d_mul :: Nil = FPU.ftypes.zipWithIndex.map{ case (ftype, i) =>
    val mul = Module(new FMUL(ftype.expWidth, ftype.precision))
    val in1 = src1
    val in2 = Mux(fpCtrl.fmaCmd(1), invert_sign(src2, ftype.len), src2)
    mul.io.a := in1
    mul.io.b := in2
    mul.io.rm := rm
    mul
  }

  toAdd.addend := S2Reg(S1Reg(io.in.bits.src(2)))
  toAdd.mul_out.zip(s3.map(_.io.to_fadd)).foreach(x => x._1 := x._2)
  toAdd.uop := uopVec.last
  io.out.bits.data := Mux1H(outSel, s3.zip(FPU.ftypes).map{
    case (mod, t) => FPU.box(mod.io.result, t)
  })
  fflags := Mux1H(outSel, s3.map(_.io.fflags))
}

class FADD_pipe(val addLat: Int = 2)(implicit p: Parameters) extends FPUPipelineModule {
  override val dataModule: FPUDataModule = null
  override def latency: Int = addLat

  val mulToAdd = IO(Input(new MulToAddIO(FPU.ftypes)))
  val isFMA = IO(Input(Bool()))

  val src1 = S1Reg(FPU.unbox(io.in.bits.src(0), io.in.bits.uop.ctrl.fpu.typeTagIn))
  val src2 = S1Reg(FPU.unbox(
    Mux(isFMA, mulToAdd.addend, io.in.bits.src(1)), io.in.bits.uop.ctrl.fpu.typeTagIn
  ))

  val uopIn = S1Reg(Mux(isFMA, mulToAdd.uop, io.in.bits.uop))
  val fpCtrl = uopIn.ctrl.fpu
  val typeTagIn = fpCtrl.typeTagIn

  val fma = S1Reg(isFMA)
  val mulProd = S1Reg(mulToAdd.mul_out)

  val stages = FPU.ftypes.zipWithIndex.map{
    case (t, i) =>
      val s1 = Module(new FCMA_ADD_s1(t.expWidth, 2*t.precision, t.precision))
      val s2 = Module(new FCMA_ADD_s2(t.expWidth, t.precision))
      val in1 = Mux(fma,
        mulProd(i).fp_prod.asUInt,
        Cat(src1(t.len - 1, 0), 0.U(t.precision.W))
      )
      val in2 = Cat(
        Mux(fpCtrl.fmaCmd(0), invert_sign(src2, t.len), src2(t.len - 1, 0)),
        0.U(t.precision.W)
      )
      s1.io.a := in1
      s1.io.b := in2
      s1.io.b_inter_valid := fma
      s1.io.b_inter_flags := Mux(fma,
        mulProd(i).inter_flags,
        0.U.asTypeOf(s1.io.b_inter_flags)
      )
      s1.io.rm := S1Reg(rm)
      s2.io.in := S2Reg(s1.io.out)
      (s1, s2)
  }

  val (s1, s2) = stages.unzip

  val outSel = S2Reg(VecInit(FPU.ftypes.zipWithIndex.map(_._2.U === typeTagIn)))
  io.out.bits.data := Mux1H(outSel, s2.zip(FPU.ftypes).map{
    case (mod, t) => FPU.box(mod.io.result, t)
  })
  fflags := Mux1H(outSel, s2.map(_.io.fflags))
}

class FMA(implicit p: Parameters) extends FPUSubModule {

  override val dataModule = null
  val mul_pipe = Module(new FMUL_pipe())
  val add_pipe = Module(new FADD_pipe())


  mul_pipe.io.redirectIn := io.redirectIn
  mul_pipe.rm := rm

  add_pipe.io.redirectIn := io.redirectIn
  add_pipe.rm := rm

  val fpCtrl = io.in.bits.uop.ctrl.fpu
  mul_pipe.io.in <> io.in
  mul_pipe.io.in.valid := io.in.valid && !fpCtrl.isAddSub

  // For better timing, we let out.valid be true even if it's flushed.
  val isFMA = mul_pipe.io.out.valid && mul_pipe.io.out.bits.uop.ctrl.fpu.ren3
  // However, when sending instructions to add_pipe, we need to determine whether it's flushed.
  val mulFlushed = mul_pipe.io.out.bits.uop.robIdx.needFlush(io.redirectIn)
  val isFMAReg = RegNext(isFMA && !mulFlushed)

  add_pipe.mulToAdd <> mul_pipe.toAdd

  // For FADD, it accepts instructions from io.in and FMUL.
  // When FMUL gives an FMA, FADD accepts this instead of io.in.
  // Since FADD gets FMUL data from add_pipe.mulToAdd, only uop needs Mux.
  add_pipe.io.in.valid := io.in.valid && fpCtrl.isAddSub || isFMAReg
  add_pipe.io.in.bits.src := io.in.bits.src
  add_pipe.io.in.bits.uop := Mux(isFMAReg, add_pipe.mulToAdd.uop, io.in.bits.uop)
  add_pipe.isFMA := isFMAReg

  // When the in uop is Add/Sub, we check FADD, otherwise fmul is checked.
  io.in.ready := Mux(fpCtrl.isAddSub,
    !isFMAReg && add_pipe.io.in.ready,
    mul_pipe.io.in.ready
  )

  // For FMUL:
  // (1) It always accept FMA from FADD (if an FMA wants FMUL, it's never blocked).
  // (2) It has lower writeback arbitration priority than FADD (and may be blocked when FMUL.out.valid).
  XSError(isFMA && !add_pipe.io.in.ready, "FMA should not be blocked\n")
  mul_pipe.io.out.ready := isFMA || (io.out.ready && !add_pipe.io.out.valid)
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
  io.out.valid := add_pipe.io.out.valid || (mul_pipe.io.out.valid && !isFMA)
}
