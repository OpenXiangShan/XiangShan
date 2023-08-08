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

import freechips.rocketchip.tilelink.ClientMetadata
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.L1CacheErrorInfo
import xiangshan.cache.CacheInstrucion._

class Meta(implicit p: Parameters) extends DCacheBundle {
  val coh = new ClientMetadata
}

object Meta {
  def apply(meta: UInt)(implicit p: Parameters) = {
    val m = Wire(new Meta)
    m.coh := meta.asTypeOf(new ClientMetadata)
    m
  }
}

class MetaReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class CohMetaWriteReq(implicit p: Parameters) extends MetaReadReq {
  val meta = new Meta
}

class FlagMetaWriteReq(implicit p: Parameters) extends MetaReadReq {
  val flag = Bool()
}

class L1CohMetaArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, new Meta)))
    val write = Vec(writePorts, Flipped(DecoupledIO(new CohMetaWriteReq)))
  })

  val meta_array = RegInit(
    VecInit(Seq.fill(nSets)(
      VecInit(Seq.fill(nWays)(0.U.asTypeOf(new Meta)))
    ))
  )

  val s0_way_wen = Wire(Vec(nWays, Vec(writePorts, Bool())))
  val s1_way_wen = Wire(Vec(nWays, Vec(writePorts, Bool())))
  val s1_way_waddr = Wire(Vec(nWays, Vec(writePorts, UInt(idxBits.W))))
  val s1_way_wdata = Wire(Vec(nWays, Vec(writePorts, new Meta)))

  (io.read.zip(io.resp)).zipWithIndex.foreach {
    case ((read, resp), i) =>
      read.ready := true.B
      (0 until nWays).map(way => {
        val read_way_bypass = WireInit(false.B)
        val bypass_data = Wire(new Meta)
        bypass_data := DontCare
        (0 until writePorts).map(wport =>
          when(s1_way_wen(way)(wport) && s1_way_waddr(way)(wport) === read.bits.idx){
            read_way_bypass := true.B
            bypass_data := s1_way_wdata(way)(wport)
          }
        )
        resp(way) := Mux(
          RegEnable(read_way_bypass, read.valid),
          RegEnable(bypass_data, read_way_bypass),
          RegEnable(meta_array(read.bits.idx)(way), read.valid)
        )
      })
  }

  io.write.zipWithIndex.foreach {
    case (write, wport) =>
      write.ready := true.B
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, way) =>
          s0_way_wen(way)(wport) := write.valid && wen
          s1_way_wen(way)(wport) := RegNext(s0_way_wen(way)(wport))
          s1_way_waddr(way)(wport) := RegEnable(write.bits.idx, s0_way_wen(way)(wport))
          s1_way_wdata(way)(wport) := RegEnable(write.bits.meta, s0_way_wen(way)(wport))
          when (s1_way_wen(way)(wport)) {
            meta_array(s1_way_waddr(way)(wport))(way) := s1_way_wdata(way)(wport)
          }
      }
  }
}

class L1FlagMetaArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, Bool())))
    val write = Vec(writePorts, Flipped(DecoupledIO(new FlagMetaWriteReq)))
    // customized cache op port 
    // val cacheOp = Flipped(new L1CacheInnerOpIO)
  })

  val meta_array = RegInit(
    VecInit(Seq.fill(nSets)(
      VecInit(Seq.fill(nWays)(0.U.asTypeOf(false.B)))
    ))
  )

  val s0_way_wen = Wire(Vec(nWays, Vec(writePorts, Bool())))
  val s1_way_wen = Wire(Vec(nWays, Vec(writePorts, Bool())))
  val s1_way_waddr = Wire(Vec(nWays, Vec(writePorts, UInt(idxBits.W))))
  val s1_way_wdata = Wire(Vec(nWays, Vec(writePorts, Bool())))

  (io.read.zip(io.resp)).zipWithIndex.foreach {
    case ((read, resp), i) =>
      read.ready := true.B
      (0 until nWays).map(way => {
        val read_way_bypass = WireInit(false.B)
        val bypass_data = Wire(Bool())
        bypass_data := DontCare
        (0 until writePorts).map(wport =>
          when(s1_way_wen(way)(wport) && s1_way_waddr(way)(wport) === read.bits.idx){
            read_way_bypass := true.B
            bypass_data := s1_way_wdata(way)(wport)
          }
        )
        resp(way) := Mux(
          RegEnable(read_way_bypass, read.valid),
          RegEnable(bypass_data, read_way_bypass),
          meta_array(RegEnable(read.bits.idx, read.valid))(way)
        )
      })
  }

  io.write.zipWithIndex.foreach {
    case (write, wport) =>
      write.ready := true.B
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, way) =>
          s0_way_wen(way)(wport) := write.valid && wen
          s1_way_wen(way)(wport) := RegNext(s0_way_wen(way)(wport))
          s1_way_waddr(way)(wport) := RegEnable(write.bits.idx, s0_way_wen(way)(wport))
          s1_way_wdata(way)(wport) := RegEnable(write.bits.flag, s0_way_wen(way)(wport))
          when (s1_way_wen(way)(wport)) {
            meta_array(s1_way_waddr(way)(wport))(way) := s1_way_wdata(way)(wport)
          }
      }
  }
}

class SourceMetaWriteReq(implicit p: Parameters) extends MetaReadReq {
  val source = UInt(L1PfSourceBits.W)
}

class L1PrefetchSourceArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(L1PfSourceBits.W))))
    val write = Vec(writePorts, Flipped(DecoupledIO(new SourceMetaWriteReq)))
  })

  val meta_array = RegInit(
    VecInit(Seq.fill(nSets)(
      VecInit(Seq.fill(nWays)(0.U(L1PfSourceBits.W)))
    ))
  )

  val s0_way_wen = Wire(Vec(nWays, Vec(writePorts, Bool())))
  val s1_way_wen = Wire(Vec(nWays, Vec(writePorts, Bool())))
  val s1_way_waddr = Wire(Vec(nWays, Vec(writePorts, UInt(idxBits.W))))
  val s1_way_wdata = Wire(Vec(nWays, Vec(writePorts, Bool())))

  (io.read.zip(io.resp)).zipWithIndex.foreach {
    case ((read, resp), i) =>
      read.ready := true.B
      (0 until nWays).map(way => {
        val read_way_bypass = WireInit(false.B)
        val bypass_data = Wire(UInt(L1PfSourceBits.W))
        bypass_data := DontCare
        (0 until writePorts).map(wport =>
          when(s1_way_wen(way)(wport) && s1_way_waddr(way)(wport) === read.bits.idx){
            read_way_bypass := true.B
            bypass_data := s1_way_wdata(way)(wport)
          }
        )
        resp(way) := Mux(
          RegEnable(read_way_bypass, read.valid),
          RegEnable(bypass_data, read_way_bypass),
          meta_array(RegEnable(read.bits.idx, read.valid))(way)
        )
      })
  }

  io.write.zipWithIndex.foreach {
    case (write, wport) =>
      write.ready := true.B
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, way) =>
          s0_way_wen(way)(wport) := write.valid && wen
          s1_way_wen(way)(wport) := RegNext(s0_way_wen(way)(wport))
          s1_way_waddr(way)(wport) := RegEnable(write.bits.idx, s0_way_wen(way)(wport))
          s1_way_wdata(way)(wport) := RegEnable(write.bits.source, s0_way_wen(way)(wport))
          when (s1_way_wen(way)(wport)) {
            meta_array(s1_way_waddr(way)(wport))(way) := s1_way_wdata(way)(wport)
          }
      }
  }
}
