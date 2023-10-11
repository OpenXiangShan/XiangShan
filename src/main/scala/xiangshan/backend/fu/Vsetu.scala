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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.vector.Bundles.{VConfig, VType, Vl}

class VsetModuleIO(implicit p: Parameters) extends XSBundle {
  private val vlWidth = p(XSCoreParamsKey).vlWidth

  val in = Input(new Bundle {
    val avl   : UInt = UInt(XLEN.W)
    val vtype : VType = VType()
    val func  : UInt = FuOpType()
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
  private val func  = io.in.func
  private val vtype = io.in.vtype

  private val outVConfig = io.out.vconfig

  private val vlWidth = p(XSCoreParamsKey).vlWidth

  private val isSetVlmax = VSETOpType.isSetVlmax(func)
  private val isVsetivli = VSETOpType.isVsetivli(func)

  private val vlmul: UInt = vtype.vlmul
  private val vsew : UInt = vtype.vsew

  private val vl = WireInit(0.U(XLEN.W))

  // EncodedLMUL = log(LMUL)
  // EncodedSEW  = log(SEW) - 3
  //        VLMAX  = VLEN * LMUL / SEW
  // => log(VLMAX) = log(VLEN * LMUL / SEW)
  // => log(VLMAX) = log(VLEN) + log(LMUL) - log(SEW)
  // =>     VLMAX  = 1 << log(VLMAX)
  //               = 1 << (log(VLEN) + log(LMUL) - log(SEW))

  // vlen =  128
  private val log2Vlen = log2Up(VLEN)
  println(s"[VsetModule] log2Vlen: $log2Vlen")
  println(s"[VsetModule] vlWidth: $vlWidth")

  private val log2Vlmul = vlmul
  private val log2Vsew = vsew +& "b011".U

  // vlen = 128, lmul = 8, sew = 8, log2Vlen = 7,
  // vlmul = b011, vsew = 0, 7 + 3 - (0 + 3) = 7
  // vlen = 128, lmul = 2, sew = 16
  // vlmul = b001, vsew = 1, 7 + 1 - (1 + 3) = 4
  private val log2Vlmax: UInt = log2Vlen.U(3.W) + log2Vlmul - log2Vsew
  private val vlmax = (1.U(vlWidth.W) << log2Vlmax).asUInt

  private val normalVL = Mux(avl > vlmax, vlmax, avl)

  vl := Mux(isSetVlmax, vlmax, normalVL)

  private val log2Elen = log2Up(ELEN)
  private val log2VsewMax = Mux(log2Vlmul(2), log2Elen.U + log2Vlmul, log2Elen.U)

  private val sewIllegal = log2Vsew > log2VsewMax
  private val lmulIllegal = vlmul === "b100".U

  private val illegal = lmulIllegal | sewIllegal | vtype.illegal

  outVConfig.vl := Mux(illegal, 0.U, vl)
  outVConfig.vtype.illegal := illegal
  outVConfig.vtype.vta := Mux(illegal, 0.U, vtype.vta)
  outVConfig.vtype.vma := Mux(illegal, 0.U, vtype.vma)
  outVConfig.vtype.vlmul := Mux(illegal, 0.U, vtype.vlmul)
  outVConfig.vtype.vsew := Mux(illegal, 0.U, vtype.vsew)

  io.testOut.vlmax := vlmax
  io.testOut.log2Vlmax := log2Vlmax
}
