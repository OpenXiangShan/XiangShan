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
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utils.{Code, ParallelOR, ReplacementPolicy, SRAMTemplate, XSDebug}
import xiangshan.L1CacheErrorInfo

// basic building blocks for L1 DCache
class L1Metadata(implicit p: Parameters) extends DCacheBundle {
  val coh = new ClientMetadata
  val tag = UInt(tagBits.W)
}

object L1Metadata {
  def apply(tag: Bits, coh: ClientMetadata, paddr: UInt)(implicit p: Parameters) = {
    val meta = Wire(new L1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class L1MetaReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
  val tag = UInt(tagBits.W)
}

class L1MetaWriteReq(implicit p: Parameters) extends L1MetaReadReq {
  val data = new L1Metadata
}


class L1MetadataArray(onReset: () => L1Metadata)(implicit p: Parameters) extends DCacheModule {
  val rstVal = onReset()
  val metaBits = rstVal.getWidth
  val encMetaBits = cacheParams.tagCode.width(metaBits)

  val io = IO(new Bundle {
    val read = Flipped(Decoupled(new L1MetaReadReq))
    val write = Flipped(Decoupled(new L1MetaWriteReq))
    val resp = Output(Vec(nWays, UInt(encMetaBits.W)))
    val error = Output(new L1CacheErrorInfo)
  })
  val rst_cnt = RegInit(0.U(log2Up(nSets + 1).W))
  val rst = rst_cnt < nSets.U
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.data).asUInt
  val wmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.write.bits.way_en.asSInt).asBools
  val rmask = Mux(rst || (nWays == 1).B, (-1).asSInt, io.read.bits.way_en.asSInt).asBools
  when(rst) {
    rst_cnt := rst_cnt + 1.U
  }

  val tag_array = Module(new SRAMTemplate(UInt(encMetaBits.W), set = nSets, way = nWays,
    shouldReset = false, holdRead = false, singlePort = true))

  // tag write
  val wen = rst || io.write.valid
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx = waddr,
    data = cacheParams.tagCode.encode(wdata),
    waymask = VecInit(wmask).asUInt)

  // tag read
  val ren = io.read.fire()
  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx = io.read.bits.idx)
  io.resp := tag_array.io.r.resp.data
  val ecc_errors = tag_array.io.r.resp.data.zipWithIndex.map({ case (d, w) =>
    cacheParams.tagCode.decode(d).error && RegNext(io.read.bits.way_en(w))
  })
  io.error.report_to_beu := RegNext(io.read.fire()) && Cat(ecc_errors).orR()
  io.error.paddr := Cat(io.read.bits.idx, 0.U(pgUntagBits.W))

  io.write.ready := !rst
  io.read.ready := !wen

  def dumpRead() = {
    when(io.read.fire()) {
      XSDebug("MetaArray Read: idx: %d way_en: %x tag: %x\n",
        io.read.bits.idx, io.read.bits.way_en, io.read.bits.tag)
    }
  }

  def dumpWrite() = {
    when(io.write.fire()) {
      XSDebug("MetaArray Write: idx: %d way_en: %x tag: %x new_tag: %x new_coh: %x\n",
        io.write.bits.idx, io.write.bits.way_en, io.write.bits.tag, io.write.bits.data.tag, io.write.bits.data.coh.state)
    }
  }

  // def dumpResp() = {
  //   (0 until nWays) map { i =>
  //     XSDebug(s"MetaArray Resp: way: $i tag: %x coh: %x\n",
  //       io.resp(i).tag, io.resp(i).coh.state)
  //   }
  // }

  def dump() = {
    dumpRead
    dumpWrite
    // dumpResp
  }
}

class DuplicatedMetaArray(numReadPorts: Int)(implicit p: Parameters) extends DCacheModule {
  def onReset = L1Metadata(0.U, ClientMetadata.onReset, 0.U)

  val metaBits = onReset.getWidth
  val encMetaBits = cacheParams.tagCode.width(metaBits)

  val io = IO(new DCacheBundle {
    val read = Vec(numReadPorts, Flipped(DecoupledIO(new L1MetaReadReq)))
    val write = Flipped(DecoupledIO(new L1MetaWriteReq))
    val resp = Output(Vec(numReadPorts, Vec(nWays, UInt(encMetaBits.W))))
    val errors = Output(Vec(numReadPorts, new L1CacheErrorInfo))
  })
  val meta = Seq.fill(numReadPorts) {
    Module(new L1MetadataArray(onReset _))
  }

  for (w <- 0 until numReadPorts) {
    // meta(w).io.write <> io.write
    meta(w).io.write.valid := io.write.valid
    meta(w).io.write.bits := io.write.bits
    meta(w).io.read <> io.read(w)
    io.resp(w) <> meta(w).io.resp
    io.errors(w) <> meta(w).io.error
  }
  // io.write.ready := VecInit(meta.map(_.io.write.ready)).asUInt.andR
  io.write.ready := true.B

  def dumpRead() = {
    (0 until numReadPorts) map { w =>
      when(io.read(w).fire()) {
        XSDebug(s"MetaArray Read channel: $w idx: %d way_en: %x tag: %x\n",
          io.read(w).bits.idx, io.read(w).bits.way_en, io.read(w).bits.tag)
      }
    }
  }

  def dumpWrite() = {
    when(io.write.fire()) {
      XSDebug("MetaArray Write: idx: %d way_en: %x tag: %x new_tag: %x new_coh: %x\n",
        io.write.bits.idx, io.write.bits.way_en, io.write.bits.tag, io.write.bits.data.tag, io.write.bits.data.coh.state)
    }
  }

  // def dumpResp() = {
  //   (0 until LoadPipelineWidth) map { w =>
  //     (0 until nWays) map { i =>
  //       XSDebug(s"MetaArray Resp: channel: $w way: $i tag: %x coh: %x\n",
  //         io.resp(w)(i).tag, io.resp(w)(i).coh.state)
  //     }
  //   }
  // }

  def dump() = {
    dumpRead
    dumpWrite
    // dumpResp
  }
}
