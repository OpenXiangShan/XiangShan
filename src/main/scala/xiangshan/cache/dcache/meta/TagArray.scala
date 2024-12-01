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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SRAMTemplate, XSPerfAccumulate, ClockGate}
import xiangshan.cache.CacheInstrucion._

class TagReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class TagWriteReq(implicit p: Parameters) extends TagReadReq {
  val vaddr = UInt(vtagBits.W)
  val tag = UInt(tagBits.W)
  val ecc = UInt(tagECCBits.W)

  def asECCTag() = {
    if (EnableTagEcc) {
      Cat(ecc, tag)
    } else {
      tag
    }
  }
}

class TagEccWriteReq(implicit p: Parameters) extends TagReadReq {
  val ecc = UInt(tagECCBits.W)
}

case object HasTagEccParam

abstract class AbstractTagArray(implicit p: Parameters) extends DCacheModule {
  val TagEccParam = if(EnableTagEcc) Some(HasTagEccParam) else None
}

class TagArray(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq))
    val resp = Output(Vec(nWays, UInt(encTagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })
  // TODO: reset is unnecessary?
  val rst_cnt = RegInit(0.U(log2Up(nSets + 1).W))
  val rst = rst_cnt < nSets.U
  val rstVal = 0.U
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.asECCTag())
  val wmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.write.bits.way_en.asSInt).asBools
  val rmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.read.bits.way_en.asSInt).asBools
  when (rst) {
    rst_cnt := rst_cnt + 1.U
  }

  val tag_array = Module(new SRAMTemplate(UInt(encTagBits.W), set = nSets, way = nWays,
    shouldReset = false, holdRead = false, singlePort = true))

  val wen = rst || io.write.valid
  io.write.ready := !rst
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt
  )

  // tag read
  val ren = io.read.fire
  io.read.ready := !wen
  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx = io.read.bits.idx)
  tag_array.clock := ClockGate(false.B, ren | wen, clock)
  io.resp := tag_array.io.r.resp.data

  XSPerfAccumulate("part_tag_read_counter", tag_array.io.r.req.valid)
}

class DuplicatedTagArray(readPorts: Int)(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(encTagBits.W))))
    val write = Flipped(DecoupledIO(new TagWriteReq))
    // customized cache op port
    val cacheOp = Flipped(new L1CacheInnerOpIO)
    val cacheOp_req_dup = Vec(DCacheDupNum, Flipped(Valid(new CacheCtrlReqInfo)))
    val cacheOp_req_bits_opCode_dup = Input(Vec(DCacheDupNum, UInt(XLEN.W)))
  })

  val array = Seq.fill(readPorts) { Module(new TagArray) }

  def getECCFromEncTag(encTag: UInt) = {
    if (EnableDataEcc) {
      require(encTag.getWidth == encTagBits, s"encTag=$encTag != encTagBits=$encTagBits!")
      encTag(encTagBits - 1, tagBits)
    } else {
      0.U
    }
  }

  val tag_read_oh = WireInit(VecInit(Seq.fill(readPorts)(0.U(XLEN.W))))
  for (i <- 0 until readPorts) {
    // normal read / write
    array(i).io.write.valid := io.write.valid
    array(i).io.write.bits.idx := io.write.bits.idx
    array(i).io.write.bits.way_en := io.write.bits.way_en
    array(i).io.write.bits.vaddr := io.write.bits.vaddr
    array(i).io.write.bits.tag := io.write.bits.tag
    array(i).io.write.bits.ecc := getECCFromEncTag(cacheParams.tagCode.encode(io.write.bits.tag))
    io.write.ready := true.B

    array(i).io.read <> io.read(i)
    io.read(i).ready := array(i).io.read.ready
    io.resp(i) := array(i).io.resp
    tag_read_oh(i) := PopCount(array(i).io.read.fire)
  }
  io.cacheOp.resp.valid := false.B
  io.cacheOp.resp.bits  := DontCare

  XSPerfAccumulate("tag_read_counter", tag_read_oh.reduce(_ + _))
}
