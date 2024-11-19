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
}

class TagEccWriteReq(implicit p: Parameters) extends TagReadReq {
  val ecc = UInt(eccTagBits.W)
}

case object HasTagEccParam

abstract class AbstractTagArray(implicit p: Parameters) extends DCacheModule {
  val TagEccParam = if(EnableTagEcc) Some(HasTagEccParam) else None
}

class TagArray(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq))
    val resp = Output(Vec(nWays, UInt(tagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq))
    // ecc
    val ecc_read = Flipped(DecoupledIO(new TagReadReq))
    val ecc_resp = Output(Vec(nWays, UInt(eccTagBits.W)))
    val ecc_write = Flipped(DecoupledIO(new TagEccWriteReq))
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

  val ecc_array = TagEccParam.map {
    case _ =>
      val ecc = Module(new SRAMTemplate(UInt(eccTagBits.W), set = nSets, way = nWays,
      shouldReset = false, holdRead = false, singlePort = true))
    ecc
  }

  val wen = rst || io.write.valid
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt
  )

  val ecc_wen = rst || io.ecc_write.valid
  val ecc_waddr = Mux(rst, rst_cnt, io.ecc_write.bits.idx)
  val ecc_wdata = Mux(rst, rstVal, io.ecc_write.bits.ecc)
  val ecc_wmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.ecc_write.bits.way_en.asSInt).asBools
  ecc_array match {
    case Some(ecc) =>
      ecc.io.w.req.valid := ecc_wen
      ecc.io.w.req.bits.apply(
        setIdx = ecc_waddr,
        data = ecc_wdata,
        waymask = VecInit(ecc_wmask).asUInt
      )
    case None =>
  }

  // tag read
  val ren = io.read.fire

  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx = io.read.bits.idx)
  tag_array.clock := ClockGate(false.B, ren | wen, clock)
  io.resp := tag_array.io.r.resp.data
  XSPerfAccumulate("part_tag_read_counter", tag_array.io.r.req.valid)

  val ecc_ren = io.ecc_read.fire
  ecc_array match {
    case Some(ecc) =>
      ecc.io.r.req.valid := ecc_ren
      ecc.io.r.req.bits.apply(setIdx = io.ecc_read.bits.idx)
      io.ecc_resp := ecc.io.r.resp.data
      ecc.clock := ClockGate(false.B, ecc_ren | ecc_wen, clock)
    case None =>
      io.ecc_resp := 0.U.asTypeOf(io.ecc_resp)
  }

  io.write.ready := !rst
  io.read.ready := !wen
  ecc_array match {
    case Some(ecc) =>
      io.ecc_write.ready := !rst
      io.ecc_read.ready := !ecc_wen
    case None =>
      io.ecc_write.ready := true.B
      io.ecc_read.ready := true.B
  }
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
    require(encTag.getWidth == encTagBits)
    encTag(encTagBits - 1, tagBits)
  }

  val tag_read_oh = WireInit(VecInit(Seq.fill(readPorts)(0.U(XLEN.W))))
  for (i <- 0 until readPorts) {
    // normal read / write
    array(i).io.write.valid := io.write.valid
    array(i).io.write.bits := io.write.bits
    array(i).io.ecc_write.valid := io.write.valid
    array(i).io.ecc_write.bits.idx := io.write.bits.idx
    array(i).io.ecc_write.bits.way_en := io.write.bits.way_en
    val ecc = getECCFromEncTag(cacheParams.tagCode.encode(io.write.bits.tag))
    array(i).io.ecc_write.bits.ecc := ecc

    array(i).io.read <> io.read(i)
    array(i).io.ecc_read.valid := io.read(i).valid
    array(i).io.ecc_read.bits := io.read(i).bits
    io.resp(i) := (array(i).io.ecc_resp zip array(i).io.resp).map { case (e, r) => Cat(e, r) }
    // extra ports for cache op
//    array(i).io.ecc_write.valid := false.B
//    array(i).io.ecc_write.bits := DontCare
    io.read(i).ready := array(i).io.read.ready && array(i).io.ecc_read.ready
    tag_read_oh(i) := PopCount(array(i).io.read.fire)
  }
  XSPerfAccumulate("tag_read_counter", tag_read_oh.reduce(_ + _))
  io.write.ready := true.B

  io.cacheOp.resp.valid := false.B
  io.cacheOp.resp.bits  := DontCare
}
