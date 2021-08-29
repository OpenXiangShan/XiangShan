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

// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{SignExt, ZeroExt}

class IntToFPDataModule(implicit p: Parameters) extends FPUDataModule {

  val in_valid, out_ready = IO(Input(Bool()))
  val in_ready, out_valid = IO(Output(Bool()))
  val kill_w, kill_r = IO(Input(Bool()))

  val s_idle :: s_cvt :: s_finish :: Nil = Enum(3)
  val state = RegInit(s_idle)


  val in_fire = in_valid && in_ready
  val out_fire = out_valid && out_ready
  in_ready := state === s_idle
  out_valid := state === s_finish

  val src1 = RegEnable(io.in.src(0), in_fire)
  val rmReg = RegEnable(rm, in_fire)
  val ctrl = RegEnable(io.in.fpCtrl, in_fire)

  switch(state){
    is(s_idle){
      when(in_fire && !kill_w){
        state := s_cvt
      }
    }
    is(s_cvt){
      state := s_finish
    }
    is(s_finish){
      when(out_fire){
        state := s_idle
      }
    }
  }
  when(state =/= s_idle && kill_r){
    state := s_idle
  }

  /*
      s_cvt
   */
  val tag = ctrl.typeTagOut
  val typ = ctrl.typ
  val wflags = ctrl.wflags

  val mux = Wire(new Bundle() {
    val data = UInt(XLEN.W)
    val exc = UInt(5.W)
  })

  // fmv
  mux.data := src1
  mux.exc := 0.U

  val intValue = Mux(typ(1),
    Mux(typ(0), ZeroExt(src1, XLEN), SignExt(src1, XLEN)),
    Mux(typ(0), ZeroExt(src1(31, 0), XLEN), SignExt(src1(31, 0), XLEN))
  )

  when(wflags){
    val i2fResults = for(t <- FPU.ftypes) yield {
      val i2f = Module(new fudian.IntToFP(t.expWidth, t.precision))
      i2f.io.sign := ~typ(0)
      i2f.io.long := typ(1)
      i2f.io.int := intValue
      i2f.io.rm := rmReg
      (i2f.io.result, i2f.io.fflags)
    }
    val (data, exc) = i2fResults.unzip
    mux.data := VecInit(data)(tag)
    mux.exc := VecInit(exc)(tag)
  }

  val muxReg = Reg(mux.cloneType)
  when(state === s_cvt){
    muxReg.data := FPU.box(mux.data, ctrl.typeTagOut)
    muxReg.exc := mux.exc
  }

  fflags := muxReg.exc
  io.out.data := muxReg.data
}

class IntToFP(implicit p: Parameters) extends FPUSubModule {
  override val dataModule = Module(new IntToFPDataModule)
  dataModule.in_valid := io.in.valid
  dataModule.out_ready := io.out.ready
  connectDataModule
  val uopReg = RegEnable(io.in.bits.uop, io.in.fire())
  dataModule.kill_w := io.in.bits.uop.roqIdx.needFlush(io.redirectIn, io.flushIn)
  dataModule.kill_r := uopReg.roqIdx.needFlush(io.redirectIn, io.flushIn)
  io.in.ready := dataModule.in_ready
  io.out.valid := dataModule.out_valid
  io.out.bits.uop := uopReg
}
