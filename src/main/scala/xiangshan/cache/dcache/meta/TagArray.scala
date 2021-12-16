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

class TagEccWriteReq(implicit p: Parameters) extends TagReadReq {
  val ecc = UInt(eccTagBits.W)
}

class TagArray(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq))
    val resp = Output(Vec(nWays, UInt(tagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq))
//    val ecc_write = Flipped(DecoupledIO(new TagEccWriteReq))
//    val ecc_resp = Output(Vec(nWays, UInt(eccTagBits.W)))
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

//  val ecc_array = Module(new SRAMTemplate(UInt(eccTagBits.W), set = nSets, way = nWays,
//    shouldReset = false, holdRead = false, singlePort = true))

  // tag write
  def getECCFromEncTag(encTag: UInt) = {
    require(encTag.getWidth == encTagBits)
    encTag(encTagBits - 1, tagBits)
  }

  val wen = rst || io.write.valid
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt()
  )
//  ecc_array.io.w.req.valid := wen
//  ecc_array.io.w.req.bits.apply(
//    setIdx = waddr,
//    data = getECCFromEncTag(cacheParams.tagCode.encode(wdata)),
//    waymask = VecInit(wmask).asUInt()
//  )

  // tag read
  val ren = io.read.fire()

  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx = io.read.bits.idx)
  io.resp := tag_array.io.r.resp.data

//  ecc_array.io.r.req.valid := ren
//  ecc_array.io.r.req.bits.apply(setIdx = io.read.bits.idx)
//  io.ecc_resp := ecc_array.io.r.resp.data
//
//  // cache op tag ecc write
//  val ecc_force_wen = io.ecc_write.valid
//  ecc_array.io.w.req.valid := ecc_force_wen
//  when(ecc_force_wen){
//    ecc_array.io.w.req.bits.apply(
//      setIdx = io.ecc_write.bits.idx,
//      data = io.ecc_write.bits.ecc,
//      waymask = io.ecc_write.bits.way_en
//    )
//  }

  io.write.ready := !rst
//  io.ecc_write.ready := !rst
  io.read.ready := !wen
}

class DuplicatedTagArray(readPorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(tagBits.W))))
    val write = Flipped(DecoupledIO(new TagWriteReq))
    // customized cache op port
    val cacheOp = Flipped(new DCacheInnerOpIO)
  })

  val array = Seq.fill(readPorts) { Module(new TagArray) }

  for (i <- 0 until readPorts) {
    // normal read / write
    array(i).io.write.valid := io.write.valid
    array(i).io.write.bits := io.write.bits
    array(i).io.read <> io.read(i)
    io.resp(i) <> array(i).io.resp
    // extra ports for cache op
//    array(i).io.ecc_write.valid := false.B
//    array(i).io.ecc_write.bits := DontCare
  }
  io.write.ready := true.B

  // deal with customized cache op
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B) 
  when(io.cacheOp.req.valid){
    when(
      CacheInstrucion.isReadTag(io.cacheOp.req.bits.opCode)/* ||
      CacheInstrucion.isReadTagECC(io.cacheOp.req.bits.opCode)*/
    ){
      for (i <- 0 until readPorts) {
        array(i).io.read.valid := true.B
        array(i).io.read.bits.idx := io.cacheOp.req.bits.index
        array(i).io.read.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
      }
      cacheOpShouldResp := true.B
    }
    when(CacheInstrucion.isWriteTag(io.cacheOp.req.bits.opCode)){
      for (i <- 0 until readPorts) {
        array(i).io.write.valid := true.B
        array(i).io.write.bits.idx := io.cacheOp.req.bits.index
        array(i).io.write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
        array(i).io.write.bits.tag := io.cacheOp.req.bits.write_tag_low
      }
      cacheOpShouldResp := true.B
    }
//    when(CacheInstrucion.isWriteTagECC(io.cacheOp.req.bits.opCode)){
//      for (i <- 0 until readPorts) {
//        array(i).io.ecc_write.valid := true.B
//        array(i).io.ecc_write.bits.idx := io.cacheOp.req.bits.index
//        array(i).io.ecc_write.bits.way_en := UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
//        array(i).io.ecc_write.bits.ecc := io.cacheOp.req.bits.write_tag_ecc
//      }
//      cacheOpShouldResp := true.B
//    }
  }
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  io.cacheOp.resp.bits.read_tag_low := Mux(io.cacheOp.resp.valid, array(0).io.resp(io.cacheOp.req.bits.wayNum), 0.U)
//  io.cacheOp.resp.bits.read_tag_ecc := Mux(io.cacheOp.resp.valid, array(0).io.ecc_resp(io.cacheOp.req.bits.wayNum), 0.U)
  // TODO: deal with duplicated array
}
