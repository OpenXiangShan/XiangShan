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

package xiangshan.backend.dispatch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.rename.BusyTableReadIO

class Dispatch2Int(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.IntDqDeqWidth, DecoupledIO(new MicroOp)))
    val readRf = Vec(NRIntReadPorts - NRMemReadPorts, Output(UInt(PhyRegIdxWidth.W)))
    val readState = Vec(NRIntReadPorts - NRMemReadPorts, Flipped(new BusyTableReadIO))
    val enqIQCtrl = Vec(exuParameters.AluCnt, DecoupledIO(new MicroOp))
  })

  require(exuParameters.AluCnt <= dpParams.IntDqDeqWidth)

  io.enqIQCtrl <> DontCare
  for (i <- 0 until exuParameters.AluCnt) {
    io.enqIQCtrl(i) <> io.fromDq(i)
    if (i > 0) {
      io.enqIQCtrl(i).valid := io.fromDq(i).valid && !FuType.jmpCanAccept(io.fromDq(i).bits.ctrl.fuType)
      io.fromDq(i).ready := io.enqIQCtrl(i).ready && !FuType.jmpCanAccept(io.fromDq(i).bits.ctrl.fuType)
    }
    io.readRf(2*i) := io.enqIQCtrl(i).bits.psrc(0)
    io.readRf(2*i + 1) := io.enqIQCtrl(i).bits.psrc(1)

    io.readState(2*i  ).req := io.fromDq(i).bits.psrc(0)
    io.readState(2*i+1).req := io.fromDq(i).bits.psrc(1)

    io.enqIQCtrl(i).bits.srcState(0) := io.readState(i * 2).resp
    io.enqIQCtrl(i).bits.srcState(1) := io.readState(i * 2 + 1).resp
  }

  for (i <- exuParameters.AluCnt until dpParams.IntDqDeqWidth) {
    io.fromDq(i).ready := false.B
  }

}
