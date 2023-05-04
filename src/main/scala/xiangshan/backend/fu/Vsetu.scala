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
import utility.ZeroExt
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType, Vl}

class VsetModuleIO(implicit p: Parameters) extends XSBundle {
  private val vlWidth = p(XSCoreParamsKey).vlWidth

  val in = Input(new Bundle {
    val avl   : UInt = Vl()
    val vtype : VType = VType()
    val func  : UInt = FuOpType()
    val oldVl : UInt = Vl() // Todo: check if it can be optimized
  })

  val out = Output(new Bundle {
    val vconfig: VConfig = VConfig()
  })

  // test bundle for internal state
  val testOut = Output(new Bundle {
    val log2Vlmax : UInt = UInt(3.W)
    val vlmax     : UInt = UInt(vlWidth.W)
  })
}

class VsetModule(implicit p: Parameters) extends XSModule {
  val io = IO(new VsetModuleIO)

  private val avl   = io.in.avl
  private val oldVL = io.in.oldVl
  private val func  = io.in.func
  private val vtype = io.in.vtype

  private val outVConfig = io.out.vconfig

  private val vlWidth = p(XSCoreParamsKey).vlWidth

  private val isKeepVl   = VSETOpType.isKeepVl(func)
  private val isSetVlmax = VSETOpType.isSetVlmax(func)
  private val isVsetivli = VSETOpType.isVsetivli(func)

  private val vlmul: UInt = vtype.vlmul
  private val vsew : UInt = vtype.vsew


  private val vl = WireInit(0.U(XLEN.W))

  // VLMAX = VLEN * LMUL / SEW
  //       = VLEN << (Cat(~vlmul(2), vlmul(1,0)) >> (vsew + 1.U)
  //       = VLEN >> shamt
  // shamt = (vsew + 1.U) - (Cat(~vlmul(2), vlmul(1,0))

  // vlen =  128
  private val log2Vlen = log2Up(VLEN)
  println(s"[VsetModule] log2Vlen: $log2Vlen")
  println(s"[VsetModule] vlWidth: $vlWidth")
  // mf8-->b001, m1-->b100, m8-->b111
  private val ilmul = Cat(!vlmul(2), vlmul(1, 0))

  // vlen = 128, lmul = 8, sew = 8, log2Vlen = 7,
  // vlmul = b011, ilmul - 4 = b111 - 4 = 3, vsew = 0, vsew + 3 = 3, 7 + (7 - 4) - (0 + 3) = 7
  // vlen = 128, lmul = 2, sew = 16
  // vlmul = b001, ilmul - 4 = b101 - 4 = 3, vsew = 1, 7 + (5 - 4) - (1 + 3) = 4
  private val log2Vlmax: UInt = log2Vlen.U(3.W) + (ilmul - "b100".U) - (vsew + "b011".U)
  private val vlmax = (1.U(vlWidth.W) << (log2Vlmax - 1.U)).asUInt

//  private val vlmaxVec: Seq[UInt] = (0 to 7).map(i => if(i < 4) (16 << i).U(8.W) else (16 >> (8 - i)).U(8.W))
//  private val shamt = vlmul + (~vsew).asUInt + 1.U
//  private val vlmax = ParallelMux((0 to 7).map(_.U === shamt), vlmaxVec)

  private val normalVL = Mux(avl > vlmax, vlmax, avl)

  vl := MuxCase(normalVL, Seq(
    isVsetivli -> normalVL,
    isKeepVl   -> ZeroExt(oldVL, XLEN),
    isSetVlmax -> vlmax,
  ))

  outVConfig.vl := vl
  outVConfig.vtype.illegal := false.B // Todo
  outVConfig.vtype.vta := vtype.vta
  outVConfig.vtype.vma := vtype.vma
  outVConfig.vtype.vlmul := vtype.vlmul
  outVConfig.vtype.vsew := vtype.vsew

  io.testOut.vlmax := vlmax
  io.testOut.log2Vlmax := log2Vlmax
}
