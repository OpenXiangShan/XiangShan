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

package device

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4MasterParameters, AXI4MasterPortParameters, AXI4Parameters}
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._

class AXI4BurstMaster
(
  startAddr: Long = 0,
  nOp: Int = 1,
  beatBytes: Int = 8,
  burstLen: Int = 16,
  idRange: IdRange = IdRange(0, 1)
)(implicit p: Parameters) extends LazyModule {

  val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    Seq(AXI4MasterParameters("burst master", idRange))
  )))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){

    val io = IO(new Bundle{
      val finished = Output(Bool())
    })

    val (out, edge) = node.out.head
    // do not let dma AXI signals optimized out
    chisel3.dontTouch(out)
    val cnt = RegInit(nOp.U)
    val addr = RegInit(startAddr.U)
    val s_idle :: s_addr :: s_data :: Nil = Enum(3)
    val state = RegInit(s_idle)
    val ar = out.ar
    val r = out.r
    switch(state){
      is(s_idle){
        when(cnt =/= 0.U){
          state := s_addr
        }
      }
      is(s_addr){
        when(ar.ready){
          state := s_data
        }
      }
      is(s_data){
        when(r.valid){
          addr := addr + beatBytes.U
        }
        when(r.valid && r.bits.last){
          state := s_idle
          cnt := cnt - 1.U
        }
      }
    }

    io.finished := cnt === 0.U

    ar.valid := state === s_addr
    ar.bits.addr := addr
    ar.bits.size := log2Up(beatBytes).U
    ar.bits.len := (burstLen-1).U
    ar.bits.burst := AXI4Parameters.BURST_INCR

    r.ready := state === s_data
  }
}
