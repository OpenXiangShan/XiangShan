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

package xiangshan.backend.fu.vector

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{Mux1H, _}
import xiangshan.backend.fu.FunctionUnit
import xiangshan.{SelImm, SrcType}
import utility._

abstract class VPUDataModule(len: Int = 128)(implicit p: Parameters) extends FunctionUnit(len: Int)
{
  val rm = IO(Input(UInt(3.W)))
  val fflags = IO(Output(UInt(5.W)))
  val vstart = IO(Input(UInt(XLEN.W)))
  val vxrm = IO(Input(UInt(2.W)))
  val vxsat = IO(Output(UInt(1.W)))
  val needReverse = Wire(Bool())
  val needClearMask = Wire(Bool())
  val src1Sew = Wire(UInt(2.W))

  // rename signal
  val in = io.in.bits
  val ctrl = in.uop.ctrl
  val vtype = ctrl.vconfig.vtype

  // for generate src1 and src2
  src1Sew := vtype.vsew(1,0)
  private val imm = VecInit(Seq.fill(VLEN/XLEN)(VecImmExtractor(ctrl.selImm, src1Sew, ctrl.imm))).asUInt
  private val src1 = VecExtractor(src1Sew, io.in.bits.src(0))
  val _vs1 = Mux(SrcType.isImm(ctrl.srcType(0)), imm,
             Mux(in.uop.ctrl.srcType(0) === SrcType.vp, io.in.bits.src(0), src1))
  val _vs2 = in.src(1)
  // generate src1 and src2
  val vs1 = Mux(needReverse, _vs2, _vs1)
  val vs2 = Mux(needReverse, _vs1, _vs2)
  val mask = Mux(needClearMask, 0.U, in.src(3))

  // connect io
  io.out.bits.uop := DontCare
  io.in.ready := DontCare
  fflags := DontCare
  vxsat := DontCare

}


abstract class VPUSubModule(len: Int = 128)(implicit p: Parameters) extends FunctionUnit(len: Int)
{
  val rm = IO(Input(UInt(3.W)))
  val fflags = IO(Output(UInt(5.W)))
  val vstart = IO(Input(UInt(XLEN.W)))
  val vxrm = IO(Input(UInt(2.W)))
  val vxsat = IO(Output(UInt(1.W)))

  val dataModule: Seq[VPUDataModule]
  val select : Seq[Bool]

  def connectDataModule = {
  // def some signal
    val dataReg = Reg(io.out.bits.data.cloneType)
    val dataWire = Wire(dataReg.cloneType)
    val s_idle :: s_compute :: s_finish :: Nil = Enum(3)
    val state = RegInit(s_idle)

    val outValid = dataModule.map(_.io.out.valid).reduce(_||_)
    val outFire = dataModule.map(_.io.out.fire()).reduce(_||_)
  // reg input signal
    val s0_uopReg = Reg(io.in.bits.uop.cloneType)
    val s0_selectReg = Reg(VecInit(select).asUInt().cloneType)
    val inHs = io.in.fire()
    when(inHs && state === s_idle){
        s0_uopReg := io.in.bits.uop
        s0_selectReg := VecInit(select).asUInt()
      }
    dataReg := Mux(outValid, dataWire, dataReg)

  // fsm
    switch (state) {
      is (s_idle) {
        state := Mux(inHs, s_compute, s_idle)
      }
      is (s_compute) {
        state := Mux(outValid, Mux(outFire, s_idle, s_finish),
                              s_compute)
      }
      is (s_finish) {
        state := Mux(io.out.fire(), s_idle, s_finish)
      }
    }

  // connect
    dataWire := Mux1H(s0_selectReg, dataModule.map(_.io.out.bits.data))
    dataModule.zipWithIndex.foreach{ case(l, i) =>
      l.io.in.bits <> io.in.bits
      l.io.redirectIn := DontCare
      l.rm := rm
      l.vxrm := vxrm
      l.vstart := vstart
      l.io.in.valid := io.in.valid && state === s_idle && select(i)
      l.io.out.ready := io.out.ready
    }
    vxsat := Mux1H(s0_selectReg, dataModule.map(_.vxsat))
    fflags := Mux1H(s0_selectReg, dataModule.map(_.fflags))

    io.out.bits.data :=  Mux(state === s_compute && outFire, dataWire, dataReg)
    io.out.bits.uop := s0_uopReg
    io.out.valid := state === s_compute && outValid || state === s_finish
    io.in.ready := state === s_idle
  }
}


object VecImmExtractor {
  def Imm_OPIVIS(imm: UInt): UInt = {
    SignExt(imm(4,0), 8)
  }
  def Imm_OPIVIU(imm: UInt): UInt = {
    ZeroExt(imm(4,0), 8)
  }

  def imm_sew(sew: UInt, imm: UInt): UInt = {
    val _imm = SignExt(imm(7,0), 64)
    LookupTree(sew(1,0), List(
      "b00".U -> VecInit(Seq.fill(8)(_imm(7,0))).asUInt,
      "b01".U -> VecInit(Seq.fill(4)(_imm(15,0))).asUInt,
      "b10".U -> VecInit(Seq.fill(2)(_imm(31,0))).asUInt,
      "b11".U -> _imm(63,0),
    ))
  }

  def apply(immType: UInt, sew: UInt, imm: UInt): UInt = {
    val _imm = Mux(immType === SelImm.IMM_OPIVIS, Imm_OPIVIS(imm), Imm_OPIVIU(imm))
    imm_sew(sew, _imm(7,0))
  }
}

object VecExtractor{
  def xf2v_sew(sew: UInt, xf:UInt): UInt = {
    LookupTree(sew(1, 0), List(
      "b00".U -> VecInit(Seq.fill(16)(xf(7, 0))).asUInt,
      "b01".U -> VecInit(Seq.fill(8)(xf(15, 0))).asUInt,
      "b10".U -> VecInit(Seq.fill(4)(xf(31, 0))).asUInt,
      "b11".U -> VecInit(Seq.fill(2)(xf(63, 0))).asUInt,
    ))
  }

  def apply(sew: UInt, xf: UInt): UInt = {
    xf2v_sew(sew, xf)
  }
}