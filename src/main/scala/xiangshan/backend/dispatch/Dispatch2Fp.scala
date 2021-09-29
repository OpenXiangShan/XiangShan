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

class Dispatch2Fp(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val fromDq = Flipped(Vec(dpParams.FpDqDeqWidth, DecoupledIO(new MicroOp)))
    val readRf = Vec(NRFpReadPorts - exuParameters.StuCnt, Output(UInt(PhyRegIdxWidth.W)))
    val readState = Vec(NRFpReadPorts - exuParameters.StuCnt, Flipped(new BusyTableReadIO))
    val enqIQCtrl = Vec(exuParameters.FmacCnt, DecoupledIO(new MicroOp))
  })

  require(exuParameters.FmacCnt <= dpParams.FpDqDeqWidth)

  io.enqIQCtrl <> DontCare
  for (i <- 0 until exuParameters.FmacCnt) {
    io.enqIQCtrl(i) <> io.fromDq(i)

    val fmiscCanAccept = FuType.fmiscCanAccept(io.fromDq(i).bits.ctrl.fuType)
    if (i >= exuParameters.FmiscCnt) {
      io.enqIQCtrl(i).valid := io.fromDq(i).valid && !fmiscCanAccept
      io.fromDq(i).ready := io.enqIQCtrl(i).ready && !fmiscCanAccept
    }

    io.readRf(3*i) := io.enqIQCtrl(i).bits.psrc(0)
    io.readRf(3*i+1) := io.enqIQCtrl(i).bits.psrc(1)
    io.readRf(3*i+2) := io.enqIQCtrl(i).bits.psrc(2)

    io.readState(3*i  ).req := io.fromDq(i).bits.psrc(0)
    io.readState(3*i+1).req := io.fromDq(i).bits.psrc(1)
    io.readState(3*i+2).req := io.fromDq(i).bits.psrc(2)
    io.enqIQCtrl(i).bits.srcState(0) := io.readState(3*i).resp
    io.enqIQCtrl(i).bits.srcState(1) := io.readState(3*i+1).resp
    io.enqIQCtrl(i).bits.srcState(2) := io.readState(3*i+2).resp

    val numReady = io.enqIQCtrl(i).fire + PopCount(io.enqIQCtrl(i).bits.srcState.map(_ === SrcState.rdy))
    XSPerfHistogram(s"deq_num_src_ready_$i", numReady, true.B, 0, 4, 1)
  }
  val enqFire = io.enqIQCtrl.map(_.fire)
  val numRegSources = io.enqIQCtrl.map(enq => PopCount(enq.bits.ctrl.srcType.map(_ === SrcType.fp)))
  val numRegfilePorts = enqFire.zip(numRegSources).map(enq => Mux(enq._1, enq._2, 0.U)).reduce(_ + _)
  XSPerfHistogram("regfile_ports", numRegfilePorts, true.B, 0, 12, 1)

  for (i <- exuParameters.FmacCnt until dpParams.FpDqDeqWidth) {
    io.fromDq(i).ready := false.B
  }

  XSError(PopCount(io.fromDq.map(_.fire())) =/= PopCount(io.enqIQCtrl.map(_.fire())), "deq =/= enq\n")
}
