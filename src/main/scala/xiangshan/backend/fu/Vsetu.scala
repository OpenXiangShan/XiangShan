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
import utility.{ParallelMux, ZeroExt}
import xiangshan._
class VsetModule(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val src0  = Input(UInt(XLEN.W))
    val src1  = Input(UInt(XLEN.W))
    val func  = Input(FuOpType())
    val oldVConfig = Input(UInt(16.W))

    val vconfig = Output(UInt(XLEN.W))
    val vl      = Output(UInt(XLEN.W))
  })

  private val vtypeWidth = 8
  private val vlWidth = 8

  private val setOldVLFlag = VSETOpType.oldvlFlag(io.func)
  private val setVLMAXFlag = VSETOpType.vlmaxFlag(io.func)
  private val isVsetivli = VSETOpType.isVsetivli(io.func)

  private val vtype = io.src1(7, 0)
  private val vlmul = vtype(2, 0)
  private val vsew = vtype(5, 3)

  private val avlImm = ZeroExt(io.src1(14, 10), XLEN)
  private val avl = Mux(VSETOpType.isVsetivli(io.func), avlImm, io.src0)
  private val oldVL = io.oldVConfig(vtypeWidth + vlWidth - 1, vtypeWidth)

  private val vl = WireInit(0.U(XLEN.W))

  // vlen =  128
  private val vlmaxVec = (0 to 7).map(i => if(i < 4) (16 << i).U(8.W) else (16 >> (8 - i)).U(8.W))
  private val shamt = vlmul + (~vsew).asUInt + 1.U
  private val vlmax = ParallelMux((0 to 7).map(_.U === shamt), vlmaxVec)

  private val normalVL = Mux(avl > vlmax, vlmax, avl)

  vl := Mux(isVsetivli, normalVL,
    Mux(setOldVLFlag, ZeroExt(oldVL, XLEN),
      Mux(setVLMAXFlag, vlmax, normalVL)))
  io.vl := vl
  io.vconfig := ZeroExt(Cat(vl(7, 0), vtype), XLEN)
}
