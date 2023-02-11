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

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utility.ParallelMux

class VsetModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val lsrc0NotZero = Input(Bool())
    val ldest = Input(UInt(6.W))
    val src0  = Input(UInt(XLEN.W))
    val src1  = Input(UInt(XLEN.W))
    val func  = Input(FuOpType())
    val vconfig = Input(new VConfig)

    val res   = Output(UInt(XLEN.W))
  })

  val vtype = io.src1(7, 0)
  val vlmul = vtype(2, 0)
  val vsew = vtype(5, 3)

  val avlImm = Cat(0.U(3.W), io.src1(14, 10))
  val vlLast = io.vconfig.vl

  val rd = io.ldest
  val lsrc0NotZero = io.lsrc0NotZero
  val vl = WireInit(0.U(XLEN.W))
  val vconfig = WireInit(0.U(XLEN.W))

  // vlen =  128
  val vlmaxVec = (0 to 7).map(i => if(i < 4) (16 << i).U(8.W) else (16 >> (8 - i)).U(8.W))
  val shamt = vlmul + (~vsew).asUInt + 1.U
  val vlmax = ParallelMux((0 to 7).map(_.U).map(_ === shamt), vlmaxVec)

  val isVsetivli = io.func === ALUOpType.vsetivli2 || io.func === ALUOpType.vsetivli1
  val vlWhenRs1Not0 = Mux(isVsetivli, Mux(avlImm > vlmax, vlmax, avlImm),
                                      Mux(io.src0 > vlmax, vlmax, io.src0))
  vl := Mux(isVsetivli, Mux(avlImm > vlmax, vlmax, avlImm),
        Mux(lsrc0NotZero, Mux(io.src0 > vlmax, vlmax, io.src0),
        Mux(rd === 0.U, Cat(0.U(56.W), vlLast), vlmax)))

  vconfig := Cat(0.U(48.W), vl(7, 0), vtype)

  io.res := Mux(io.func === ALUOpType.vsetvli2 || io.func === ALUOpType.vsetvl2 || io.func === ALUOpType.vsetivli2, vl, vconfig)
}

class Vset(implicit p: Parameters) extends FUWithRedirect {

  val uop = io.in.bits.uop

  // vset

  val isVset = ALUOpType.isVset(io.in.bits.uop.ctrl.fuOpType)
  val dataModule = Module(new VsetModule)

  dataModule.io.lsrc0NotZero := uop.ctrl.imm(15) // lsrc(0) Not Zero
  dataModule.io.ldest := uop.ctrl.ldest
  dataModule.io.src0 := io.in.bits.src(0)
  dataModule.io.src1 := io.in.bits.src(1)
  dataModule.io.func := io.in.bits.uop.ctrl.fuOpType
  dataModule.io.vconfig := uop.ctrl.vconfig

  redirectOutValid := false.B
  redirectOut := DontCare

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid && isVset
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := dataModule.io.res
}