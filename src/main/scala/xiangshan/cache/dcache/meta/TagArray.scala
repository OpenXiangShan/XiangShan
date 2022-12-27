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
import utility.SRAMTemplate
import xiangshan.cache.CacheInstrucion._

class TagReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class TagWriteReq(implicit p: Parameters) extends TagReadReq {
  val tag = UInt(tagBits.W)
}

class TagEccWriteReq(implicit p: Parameters) extends TagReadReq {
  val ecc = UInt(eccTagBits.W)
}

class TagArray(implicit p: Parameters) extends DCacheModule {
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

  val ecc_array = Module(new SRAMTemplate(UInt(eccTagBits.W), set = nSets, way = nWays,
    shouldReset = false, holdRead = false, singlePort = true))

  val wen = rst || io.write.valid
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt()
  )

  val ecc_wen = rst || io.ecc_write.valid
  val ecc_waddr = Mux(rst, rst_cnt, io.ecc_write.bits.idx)
  val ecc_wdata = Mux(rst, rstVal, io.ecc_write.bits.ecc)
  val ecc_wmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.ecc_write.bits.way_en.asSInt).asBools
  ecc_array.io.w.req.valid := ecc_wen
  ecc_array.io.w.req.bits.apply(
    setIdx = ecc_waddr,
    data = ecc_wdata,
    waymask = VecInit(ecc_wmask).asUInt()
  )

  // tag read
  val ren = io.read.fire()

  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx = io.read.bits.idx)
  io.resp := tag_array.io.r.resp.data

  val ecc_ren = io.ecc_read.fire()
  ecc_array.io.r.req.valid := ecc_ren
  ecc_array.io.r.req.bits.apply(setIdx = io.ecc_read.bits.idx)
  io.ecc_resp := ecc_array.io.r.resp.data

  io.write.ready := !rst
  io.read.ready := !wen
  io.ecc_write.ready := !rst
  io.ecc_read.ready := !ecc_wen
}

class DuplicatedTagArray(readPorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(encTagBits.W))))
    val write = Flipped(DecoupledIO(new TagWriteReq))
    // customized cache op port
    val cacheOp = Flipped(new L1CacheInnerOpIO)
    val cacheOp_req_dup = Vec(11, Flipped(Valid(new CacheCtrlReqInfo)))
    val cacheOp_req_bits_opCode_dup = Input(Vec(11, UInt(XLEN.W)))
  })

  val array = Seq.fill(readPorts) { Module(new TagArray) }

  def getECCFromEncTag(encTag: UInt) = {
    require(encTag.getWidth == encTagBits)
    encTag(encTagBits - 1, tagBits)
  }

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
  }
  io.write.ready := true.B

  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B) 

  when (io.cacheOp.req.valid && isReadTag(io.cacheOp.req.bits.opCode)){
    for (i <- 0 until (readPorts / 3)) {
      array(i).io.read.valid := true.B
      array(i).io.read.bits.idx := io.cacheOp.req.bits.index
      array(i).io.read.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(0).valid && isReadTagECC(io.cacheOp_req_bits_opCode_dup(0))) {
    for (i <- 0 until (readPorts / 3)) {
      array(i).io.ecc_read.valid := true.B
      array(i).io.ecc_read.bits.idx := io.cacheOp.req.bits.index
      array(i).io.ecc_read.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(1).valid && isWriteTag(io.cacheOp_req_bits_opCode_dup(1))){
    for (i <- 0 until (readPorts / 3)) {
      array(i).io.write.valid := true.B
      array(i).io.write.bits.idx := io.cacheOp.req.bits.index
      array(i).io.write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      array(i).io.write.bits.tag := io.cacheOp.req.bits.write_tag_low
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(2).valid && isWriteTagECC(io.cacheOp_req_bits_opCode_dup(2))){
    for (i <- 0 until (readPorts / 3)) {
      array(i).io.ecc_write.valid := true.B
      array(i).io.ecc_write.bits.idx := io.cacheOp.req.bits.index
      array(i).io.ecc_write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      array(i).io.ecc_write.bits.ecc := io.cacheOp.req.bits.write_tag_ecc
    }
    cacheOpShouldResp := true.B
  }
  

  when (io.cacheOp_req_dup(3).valid && isReadTag(io.cacheOp_req_bits_opCode_dup(3))){
    for (i <- (readPorts / 3) until ((readPorts / 3) * 2)) {
      array(i).io.read.valid := true.B
      array(i).io.read.bits.idx := io.cacheOp.req.bits.index
      array(i).io.read.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(4).valid && isReadTagECC(io.cacheOp_req_bits_opCode_dup(4))) {
    for (i <- (readPorts / 3) until ((readPorts / 3) * 2)) {
      array(i).io.ecc_read.valid := true.B
      array(i).io.ecc_read.bits.idx := io.cacheOp.req.bits.index
      array(i).io.ecc_read.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(5).valid && isWriteTag(io.cacheOp_req_bits_opCode_dup(5))){
    for (i <- (readPorts / 3) until ((readPorts / 3) * 2)) {
      array(i).io.write.valid := true.B
      array(i).io.write.bits.idx := io.cacheOp.req.bits.index
      array(i).io.write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      array(i).io.write.bits.tag := io.cacheOp.req.bits.write_tag_low
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(6).valid && isWriteTagECC(io.cacheOp_req_bits_opCode_dup(6))){
    for (i <- (readPorts / 3) until ((readPorts / 3) * 2)) {
      array(i).io.ecc_write.valid := true.B
      array(i).io.ecc_write.bits.idx := io.cacheOp.req.bits.index
      array(i).io.ecc_write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      array(i).io.ecc_write.bits.ecc := io.cacheOp.req.bits.write_tag_ecc
    }
    cacheOpShouldResp := true.B
  }

  when (io.cacheOp_req_dup(7).valid && isReadTag(io.cacheOp_req_bits_opCode_dup(7))){
    for (i <- ((readPorts / 3) * 2) until readPorts) {
      array(i).io.read.valid := true.B
      array(i).io.read.bits.idx := io.cacheOp.req.bits.index
      array(i).io.read.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(8).valid && isReadTagECC(io.cacheOp_req_bits_opCode_dup(8))) {
    for (i <- ((readPorts / 3) * 2) until readPorts) {
      array(i).io.ecc_read.valid := true.B
      array(i).io.ecc_read.bits.idx := io.cacheOp.req.bits.index
      array(i).io.ecc_read.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
    }
    cacheOpShouldResp := true.B
  }
  when (io.cacheOp_req_dup(9).valid && isWriteTag(io.cacheOp_req_bits_opCode_dup(9))){
    for (i <- ((readPorts / 3) * 2) until readPorts) {
      array(i).io.write.valid := true.B
      array(i).io.write.bits.idx := io.cacheOp.req.bits.index
      array(i).io.write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      array(i).io.write.bits.tag := io.cacheOp.req.bits.write_tag_low
    }
    cacheOpShouldResp := true.B
  }
  when(io.cacheOp_req_dup(10).valid && isWriteTagECC(io.cacheOp_req_bits_opCode_dup(10))){
    for (i <- ((readPorts / 3) * 2) until readPorts) {
      array(i).io.ecc_write.valid := true.B
      array(i).io.ecc_write.bits.idx := io.cacheOp.req.bits.index
      array(i).io.ecc_write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      array(i).io.ecc_write.bits.ecc := io.cacheOp.req.bits.write_tag_ecc
    }
    cacheOpShouldResp := true.B
  }

  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  io.cacheOp.resp.bits.read_tag_low := Mux(io.cacheOp.resp.valid, array(0).io.resp(RegNext(io.cacheOp.req.bits.wayNum)), 0.U)
  io.cacheOp.resp.bits.read_tag_ecc := Mux(io.cacheOp.resp.valid, array(0).io.ecc_resp(RegNext(io.cacheOp.req.bits.wayNum)), 0.U)
  // TODO: deal with duplicated array
}
