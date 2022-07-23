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

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import device.RAMHelper

class FakeDCache()(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new DCacheIO)

  io := DontCare
  // to LoadUnit
  for (i <- 0 until LoadPipelineWidth) {
    val fakeRAM = Module(new RAMHelper(64L * 1024 * 1024 * 1024))
    fakeRAM.clk   := clock
    fakeRAM.en    := io.lsu.load(i).resp.valid && !reset.asBool
    fakeRAM.rIdx  := RegNext((io.lsu.load(i).s1_paddr_dup_dcache - "h80000000".U) >> 3)
    fakeRAM.wIdx  := 0.U
    fakeRAM.wdata := 0.U
    fakeRAM.wmask := 0.U
    fakeRAM.wen   := false.B

    io.lsu.load(i).req.ready := true.B
    io.lsu.load(i).resp.valid := RegNext(RegNext(io.lsu.load(i).req.valid) && !io.lsu.load(i).s1_kill)
    io.lsu.load(i).resp.bits.data := fakeRAM.rdata
    io.lsu.load(i).resp.bits.miss := false.B
    io.lsu.load(i).resp.bits.replay := false.B
    io.lsu.load(i).resp.bits.id := DontCare
    io.lsu.load(i).s2_hit := true.B
    io.lsu.load(i).s1_disable_fast_wakeup := false.B
  }
  // to LSQ
  io.lsu.lsq.valid := false.B
  io.lsu.lsq.bits := DontCare
  // to Store Buffer
  io.lsu.store.req.ready := true.B
  io.lsu.store.main_pipe_hit_resp := DontCare
  io.lsu.store.refill_hit_resp := DontCare
  io.lsu.store.replay_resp := DontCare
  io.lsu.store.main_pipe_hit_resp.valid := RegNext(io.lsu.store.req.valid)
  io.lsu.store.main_pipe_hit_resp.bits.id := RegNext(io.lsu.store.req.bits.id)
  // to atomics
  val amoHelper = Module(new AMOHelper)
  amoHelper.clock := clock
  amoHelper.enable := io.lsu.atomics.req.valid && !reset.asBool
  amoHelper.cmd := io.lsu.atomics.req.bits.cmd
  amoHelper.addr := io.lsu.atomics.req.bits.addr
  amoHelper.wdata := io.lsu.atomics.req.bits.data
  amoHelper.mask := io.lsu.atomics.req.bits.mask
  io.lsu.atomics.req.ready := true.B
  io.lsu.atomics.resp.valid := RegNext(io.lsu.atomics.req.valid)
  assert(!io.lsu.atomics.resp.valid || io.lsu.atomics.resp.ready)
  io.lsu.atomics.resp.bits.data := amoHelper.rdata
  io.lsu.atomics.resp.bits.replay := false.B
  io.lsu.atomics.resp.bits.id := 1.U
}