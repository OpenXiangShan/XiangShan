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

    val jmpCanAccept = FuType.jmpCanAccept(io.fromDq(i).bits.ctrl.fuType)
    val mduCanAccept = FuType.mduCanAccept(io.fromDq(i).bits.ctrl.fuType)
    if (i >= exuParameters.MduCnt) {
      io.enqIQCtrl(i).valid := io.fromDq(i).valid && !jmpCanAccept && !mduCanAccept
      io.fromDq(i).ready := io.enqIQCtrl(i).ready && !jmpCanAccept && !mduCanAccept
    }
    else if (i >= exuParameters.JmpCnt) {
      io.enqIQCtrl(i).valid := io.fromDq(i).valid && !jmpCanAccept
      io.fromDq(i).ready := io.enqIQCtrl(i).ready && !jmpCanAccept
    }

    io.readRf(2*i) := io.enqIQCtrl(i).bits.psrc(0)
    io.readRf(2*i + 1) := io.enqIQCtrl(i).bits.psrc(1)

    io.readState(2*i  ).req := io.fromDq(i).bits.psrc(0)
    io.readState(2*i+1).req := io.fromDq(i).bits.psrc(1)

    io.enqIQCtrl(i).bits.srcState(0) := io.readState(i * 2).resp
    io.enqIQCtrl(i).bits.srcState(1) := io.readState(i * 2 + 1).resp

    val numReady = io.enqIQCtrl(i).fire + PopCount(io.enqIQCtrl(i).bits.srcState.take(2).map(_ === SrcState.rdy))
    XSPerfHistogram(s"deq_num_src_ready_$i", numReady, true.B, 0, 3, 1)
  }
  val enqFire = io.enqIQCtrl.map(_.fire)
  val numRegSources = io.enqIQCtrl.map(enq => PopCount(enq.bits.ctrl.srcType.take(2).map(_ === SrcType.reg)))
  val numRegfilePorts = enqFire.zip(numRegSources).map(enq => Mux(enq._1, enq._2, 0.U)).reduce(_ + _)
  XSPerfHistogram("regfile_ports", numRegfilePorts, true.B, 0, 8, 1)

  for (i <- exuParameters.AluCnt until dpParams.IntDqDeqWidth) {
    io.fromDq(i).ready := false.B
  }

}
