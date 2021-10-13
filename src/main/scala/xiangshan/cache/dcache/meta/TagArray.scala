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
import utils.SRAMTemplate

class TagReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class TagWriteReq(implicit p: Parameters) extends TagReadReq {
  val tag = UInt(tagBits.W)
}

class TagArray(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq))
    val resp = Output(Vec(nWays, UInt(tagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })
  // TODO: reset is unnecessary?
  val rst_cnt = RegInit(0.U(log2Up(nSets + 1).W))
  val rst = rst_cnt < nSets.U
  val rstVal = 0.U
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.tag)
  val wmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.write.bits.way_en.asSInt).asBools
  val rmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.read.bits.way_en.asSInt).asBools
  when (rst) {
    rst_cnt := rst_cnt + 1.U
  }

  val tag_array = Module(new SRAMTemplate(UInt(tagBits.W), set = nSets, way = nWays,
    shouldReset = false, holdRead = false, singlePort = true))

  // tag write
  val wen = rst || io.write.valid
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt()
  )

  // tag read
  val ren = io.read.fire()
  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx = io.read.bits.idx)
  io.resp := tag_array.io.r.resp.data

  io.write.ready := !rst
  io.read.ready := !wen
}

class DuplicatedTagArray(readPorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(tagBits.W))))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })

  val array = Seq.fill(readPorts) { Module(new TagArray) }

  for (i <- 0 until readPorts) {
    array(i).io.write.valid := io.write.valid
    array(i).io.write.bits := io.write.bits
    array(i).io.read <> io.read(i)
    io.resp(i) <> array(i).io.resp
  }
  io.write.ready := true.B
}
