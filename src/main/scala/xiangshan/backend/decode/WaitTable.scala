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
import xiangshan._
import utils._

// 21264-like wait table
class WaitTable(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val raddr = Vec(DecodeWidth, Input(UInt(MemPredPCWidth.W))) // decode pc(VaddrBits-1, 1)
    val rdata = Vec(DecodeWidth, Output(Bool())) // loadWaitBit
    val update = Vec(StorePipelineWidth, Input(new MemPredUpdateReq)) // RegNext should be added outside
    val csrCtrl = Input(new CustomCSRCtrlIO)
  })

  val data = RegInit(VecInit(Seq.fill(WaitTableSize)(0.U(2.W))))
  val resetCounter = RegInit(0.U(ResetTimeMax2Pow.W))
  resetCounter := resetCounter + 1.U

  // read ports
  for (i <- 0 until DecodeWidth) {
    io.rdata(i) := (data(io.raddr(i))(LWTUse2BitCounter.B.asUInt) || io.csrCtrl.no_spec_load) && !io.csrCtrl.lvpred_disable
  }

  // write ports (with priority)
  (0 until StorePipelineWidth).map(i => {
    when(io.update(i).valid){
      data(io.update(i).waddr) := Cat(data(io.update(i).waddr)(0), true.B)
    }
  })


  // reset period: ResetTimeMax2Pow
  when(resetCounter(ResetTimeMax2Pow-1, ResetTimeMin2Pow)(RegNext(io.csrCtrl.lvpred_timeout))) {
    for (j <- 0 until WaitTableSize) {
      data(j) := 0.U
    }
    resetCounter:= 0.U
  }

  // debug
  for (i <- 0 until StorePipelineWidth) {
    when (io.update(i).valid) {
      XSDebug("%d: waittable update: pc %x data: %x\n", GTimer(), io.update(i).waddr, io.update(i).wdata)
    }
  }

  XSPerfAccumulate("wait_table_bit_set", PopCount(data.map(d => d(1))))
}
