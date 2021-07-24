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

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import device.RAMHelper
import xiangshan._

trait HasICacheConst { this: XSModule =>
  // 4-byte align * FetchWidth-inst
  val groupAlign = log2Up(FetchWidth * 4 * 2)
  def groupPC(pc: UInt): UInt = Cat(pc(VAddrBits-1, groupAlign), 0.U(groupAlign.W))
  def mask(pc: UInt): UInt = (Fill(PredictWidth * 2, 1.U(1.W)) >> pc(groupAlign - 1, 1))(PredictWidth - 1, 0)
}

class FakeIcacheReq(implicit p: Parameters) extends XSBundle {
  val addr = UInt(VAddrBits.W)
}

class FakeIcacheResp(implicit p: Parameters) extends XSBundle {
  val data = UInt(64.W)
  val finish = Bool()

}

class FakeCache(implicit p: Parameters) extends XSModule with HasICacheConst {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new FakeIcacheReq))
    val out = DecoupledIO(new FakeIcacheResp)
  })

  val memByte = 128 * 1024 * 1024

  val ramHelpers = Module(new RAMHelper(memByte)).io
  ramHelpers.clk := clock

  //fake instruction fetch pipeline
  //----------------
  //  ICache Stage1
  //----------------
  val gpc = io.in.bits.addr //use fetch pc
  io.in.ready := true.B

  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
  def index(addr: UInt): UInt = ((addr & offsetMask.U) >> log2Ceil(DataBytes)).asUInt()
  def inRange(idx: UInt): Bool = idx < (memByte / 8).U

  val s_idle :: s_mem_read :: Nil = Enum(2)
  val state = RegInit(s_idle)
  val beatCounter = RegInit(0.U(3.W))
  
  io.out.bits.finish := false.B
  switch(state){
    is(s_idle) {when(io.in.fire){state := s_mem_read}}
    is(s_mem_read){
      beatCounter := beatCounter + 1.U
      when(beatCounter === 7.U){
        state := s_idle
        beatCounter := 0.U
        io.out.bits.finish := true.B
      }
    }
  }

  val rIdx = index(gpc) + beatCounter
  ramHelpers.rIdx := rIdx
  Seq(
      ramHelpers.wmask,
      ramHelpers.wdata,
      ramHelpers.wen,
      ramHelpers.wIdx
  ).foreach(_ := 0.U)

  io.out.valid := (state === s_mem_read)
  io.out.bits.data := ramHelpers.rdata
}
