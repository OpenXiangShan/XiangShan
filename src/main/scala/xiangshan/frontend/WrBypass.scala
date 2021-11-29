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
import xiangshan._
import utils._
import chisel3.experimental.chiselName

class WrBypass[T <: Data](gen: T, val numEntries: Int, val idxWidth: Int,
  val numWays: Int = 1, val tagWidth: Int = 0)(implicit p: Parameters) extends XSModule {
  require(numEntries >= 0)
  require(idxWidth > 0)
  require(numWays >= 1)
  require(tagWidth >= 0)
  def hasTag = tagWidth > 0
  def multipleWays = numWays > 1
  val io = IO(new Bundle {
    val wen = Input(Bool())
    val write_idx = Input(UInt(idxWidth.W))
    val write_tag = if (hasTag) Some(Input(UInt(tagWidth.W))) else None
    val write_data = Input(Vec(numWays, gen))
    val write_way_mask = if (multipleWays) Some(Input(Vec(numWays, Bool()))) else None

    val hit = Output(Bool())
    val hit_data = Vec(numWays, Valid(gen))
  })

  class WrBypassPtr extends CircularQueuePtr[WrBypassPtr](numEntries){
    override def cloneType = (new WrBypassPtr).asInstanceOf[this.type]
  }



  val tags = RegInit(0.U.asTypeOf((Vec(numEntries, UInt(tagWidth.W)))))
  val idxes = RegInit(0.U.asTypeOf((Vec(numEntries, UInt(idxWidth.W)))))
  val datas = RegInit(0.U.asTypeOf(Vec(numEntries, Vec(numWays, gen))))
  val valids = RegInit(0.U.asTypeOf(Vec(numEntries, Vec(numWays, Bool()))))

  val enq_ptr = RegInit(0.U.asTypeOf(new WrBypassPtr))
  val enq_idx = enq_ptr.value

  val hits = VecInit((0 until numEntries).map {i =>
    idxes(i) === io.write_idx &&
    tags(i) === io.write_tag.getOrElse(0.U)
  })
  val hit = hits.reduce(_||_)
  val hit_idx = ParallelPriorityEncoder(hits)

  io.hit := hit
  for (i <- 0 until numWays) {
    io.hit_data(i).valid := valids(hit_idx)(i)
    io.hit_data(i).bits  := datas(hit_idx)(i)
  }

  for (i <- 0 until numWays) {
    when (io.wen) {
      val full_mask = Fill(numWays, 1.U(1.W)).asTypeOf(Vec(numWays, Bool()))
      val update_this_way = io.write_way_mask.getOrElse(full_mask)(i)
      when (hit) {
        when (update_this_way) {
          datas(hit_idx)(i) := io.write_data(i)
          valids(hit_idx)(i) := true.B
        }
      }.otherwise {
        valids(enq_idx)(i) := false.B
        when (update_this_way) {
          valids(enq_idx)(i) := true.B
          datas(enq_idx)(i) := io.write_data(i)
        }
      }
    }
    
  }

  when (io.wen && !hit) {
    idxes(enq_idx) := io.write_idx
    tags(enq_idx) := io.write_tag.getOrElse(0.U)
    enq_ptr := enq_ptr + 1.U
  }

  XSPerfAccumulate("wrbypass_hit",  io.wen &&  hit)
  XSPerfAccumulate("wrbypass_miss", io.wen && !hit)

  XSDebug(io.wen && hit,  p"wrbypass hit entry #${hit_idx}, idx ${io.write_idx}" +
    p"tag ${io.write_tag.getOrElse(0.U)}data ${io.write_data}\n")
  XSDebug(io.wen && !hit, p"wrbypass enq entry #${enq_idx}, idx ${io.write_idx}" +
    p"tag ${io.write_tag.getOrElse(0.U)}data ${io.write_data}\n")
}