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

import org.chipsalliance.cde.config._
import chisel3._
import chisel3.util._
import chiseltest._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

class TLBurstMaster
(
  startAddr: Long = 0,
  nOp: Int = 1,
  beatBytes: Int = 8,
  burstLen: Int = 16,
  idRange: IdRange = IdRange(0, 1)
)(implicit p: Parameters) extends LazyModule {

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1("TLMaster", idRange))
  )))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    val io = IO(new Bundle() {
      val finished = Output(Bool())
    })

    val (out , edge) = node.out.head
    val cnt = RegInit(nOp.U)
    val addr = RegInit(startAddr.U)
    val s_idle :: s_addr :: s_data :: Nil = Enum(3)
    val state = RegInit(s_idle)

    switch(state){
      is(s_idle){
        when(cnt =/= 0.U){
          state := s_addr
        }
      }
      is(s_addr){
        when(out.a.fire){
          state := s_data
        }
      }
      is(s_data){
        when(out.d.fire){
          addr := addr + beatBytes.U
        }
        when(edge.done(out.d)){
          state := s_idle
          cnt := cnt - 1.U
        }
      }
    }

    io.finished := cnt===0.U

    val a = out.a
    val d = out.d

    a.valid := state === s_addr
    val (_, bundleA) = edge.Get(idRange.start.U, addr, log2Up(beatBytes*burstLen).U)
    a.bits := bundleA

    d.ready := state === s_data
  }
}
