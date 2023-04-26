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

package xiangshan.frontend.icache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.{DecoupledIO, _}
import xiangshan.cache._
import utils._
import utility._

class ICacheMetaReadReqBundle(implicit p: Parameters) extends ICacheBundle {
  val idx = UInt(idxBits.W)
}

class ICacheMetaReadRespBundle(implicit p: Parameters) extends ICacheBundle {
  val metaData   = Vec(nWays, new ICacheMetadata)
  val errors     = Vec(nWays ,Bool())
  val entryValid = Vec(nWays, Bool())

  def tags = VecInit(metaData.map(way => way.tag))
}

class ICacheBankedMetaArray(readPortNum: Int)(implicit p: Parameters) extends ICacheArray
{
  val ICacheMetaReadPortNum = readPortNum
  val ICacheMetaArrayBanks = 8
  val ICacheMetaArrayBankIdxBits = log2Up(ICacheMetaArrayBanks)
  def get_set_addr_from_vaddr(vaddr: UInt) = {
    vaddr(ICacheAboveIndexOffset - 1, ICacheSetOffset + ICacheMetaArrayBankIdxBits)
  }
  def get_bank_addr_from_vaddr(vaddr: UInt) = {
    vaddr(ICacheSetOffset + ICacheMetaArrayBankIdxBits - 1 ,ICacheSetOffset)
  }
  def get_set_addr_from_idx(idx: UInt) = {
    idx(idxBits - 1, ICacheMetaArrayBankIdxBits)
  }
  def get_bank_addr_from_idx(idx: UInt) = {
    idx(ICacheMetaArrayBankIdxBits - 1, 0)
  }

  val metaBits = ICacheMetadata(0.U).getWidth
  val metaEntryBits = cacheParams.tagCode.width(metaBits)

  val io=IO{new Bundle{
    val read     = Vec(ICacheMetaReadPortNum, Flipped(DecoupledIO(new ICacheMetaReadReqBundle)))
    // TODO : does need support old read bundle?
    val readResp = Vec(ICacheMetaReadPortNum, Output(new ICacheMetaReadRespBundle))
    val write    = Flipped(DecoupledIO(new ICacheMetaWriteBundle))
    val cacheOp  = Flipped(new L1CacheInnerOpIO)
    val fencei   = Input(Bool())
  }}

  val set_addrs = Wire(Vec(ICacheMetaReadPortNum, UInt()))
  val bank_addrs = Wire(Vec(ICacheMetaReadPortNum, UInt()))
  val read_fires = Wire(Vec(ICacheMetaReadPortNum, Bool()))
  val set_addrs_delay = RegNext(set_addrs)
  val bank_addrs_delay = RegNext(bank_addrs)
  val read_fires_delay = RegNext(read_fires)
  val write_set_addr = io.write.bits.virIdx(idxBits - 1, ICacheMetaArrayBankIdxBits)
  val write_bank_addr = io.write.bits.virIdx(ICacheMetaArrayBankIdxBits - 1, 0)
  val write_meta_bits = cacheParams.tagCode.encode(ICacheMetadata(tag = io.write.bits.phyTag).asUInt)

  (0 until ICacheMetaReadPortNum).foreach(port_idx => {
    set_addrs(port_idx) := get_set_addr_from_idx(io.read(port_idx).bits.idx)
    bank_addrs(port_idx) := get_bank_addr_from_idx(io.read(port_idx).bits.idx)
    read_fires(port_idx) := io.read(port_idx).fire
  })

  val tag_arrays = (0 until ICacheMetaArrayBanks).map { bank =>
    val tag_array = Module(new SRAMTemplate(
      UInt(metaEntryBits.W),
      set = nSets / ICacheMetaArrayBanks,
      way = nWays,
      shouldReset = true,
      holdRead = true,
      singlePort = true
    ))
    tag_array
  }
  val valid_array = RegInit(VecInit(Seq.fill(nWays)(0.U(nSets.W))))

  // deal read
  // read read bank conflict
  val rr_bank_conflict = Seq.tabulate(ICacheMetaReadPortNum)(x => Seq.tabulate(ICacheMetaReadPortNum)(y =>
    bank_addrs(x) === bank_addrs(y) && io.read(x).valid && io.read(y).valid && set_addrs(x) =/= set_addrs(y)
  ))
  // read write bank conflict
  val rw_bank_conflict = (0 until ICacheMetaReadPortNum).map(port_idx =>
    (io.write.valid && write_bank_addr === bank_addrs(port_idx)) || io.fencei
  )

  (0 until ICacheMetaReadPortNum).foreach(port_idx => {
    io.read(port_idx).ready := ~(rw_bank_conflict(port_idx) || io.cacheOp.req.valid ||
      (if (port_idx == 0) false.B else (0 until port_idx).map(rr_bank_conflict(_)(port_idx)).reduce(_||_)))
  })

  val bank_read_ens = Wire(Vec(ICacheMetaArrayBanks, Vec(ICacheMetaReadPortNum, Bool())))
  (0 until ICacheMetaArrayBanks).foreach(bank_idx => {
    val tag_bank = tag_arrays(bank_idx)
    (0 until ICacheMetaReadPortNum).foreach(i => bank_read_ens(bank_idx)(i) := bank_addrs(i) === bank_idx.U && io.read(i).valid)
    val read_set_addr = PriorityMux(Seq.tabulate(ICacheMetaReadPortNum)(i => bank_read_ens(bank_idx)(i) -> set_addrs(i)))
    tag_bank.io.r.req.valid := bank_read_ens(bank_idx).reduce(_||_)
    tag_bank.io.r.req.bits.apply(setIdx = read_set_addr)
  })

  val read_metas = Wire(Vec(ICacheMetaArrayBanks, Vec(nWays, new ICacheMetadata())))
  val read_errors = Wire(Vec(ICacheMetaArrayBanks, Vec(nWays, Bool())))
  for ((tag_array,i) <- tag_arrays.zipWithIndex) {
    val read_meta_bits = tag_array.io.r.resp.asTypeOf(Vec(nWays, UInt(metaEntryBits.W)))
    val read_meta_decoded = read_meta_bits.map{ way_bits => cacheParams.tagCode.decode(way_bits) }
    val read_meta_wrong = read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.error}
    val read_meta_corrected = VecInit(read_meta_decoded.map{ way_bits_decoded => way_bits_decoded.corrected})
    read_metas(i) := read_meta_corrected.asTypeOf(Vec(nWays,new ICacheMetadata()))
    (0 until nWays).map{ w => read_errors(i)(w) := RegNext(read_meta_wrong(w)) && RegNext(RegNext(bank_read_ens(i).reduce(_||_)))}
  }

  (0 until ICacheMetaReadPortNum).foreach(port_idx => {
    io.readResp(port_idx).metaData := read_metas(bank_addrs_delay(port_idx))
    io.readResp(port_idx).errors := read_errors(bank_addrs_delay(port_idx))
    (0 until nWays).foreach(way =>
      io.readResp(port_idx).entryValid(way) :=
        valid_array(way)(Cat(set_addrs_delay(port_idx), bank_addrs_delay(port_idx))) && read_fires_delay(port_idx)
    )
  })

  // deal write
  (0 until ICacheMetaArrayBanks).foreach( bank_idx => {
    val tag_bank = tag_arrays(bank_idx)
    tag_bank.io.w.req.valid := io.write.valid && write_bank_addr === bank_idx.U
    tag_bank.io.w.req.bits.apply(data=write_meta_bits, setIdx=write_set_addr, waymask=io.write.bits.waymask)
  })
  val way_num = OHToUInt(io.write.bits.waymask)
  when (io.write.valid) {
    valid_array(way_num) := valid_array(way_num).bitSet(io.write.bits.virIdx, true.B)
  }
  io.write.ready := !io.cacheOp.req.valid
  XSPerfAccumulate("meta_refill_num", io.write.valid)

  // deal cache op
  // TODO : What exactly is the cache op doing?
  require(nWays <= 32)
  io.cacheOp.resp.bits := DontCare
  val cacheOpShouldResp = WireInit(false.B)
  when(io.cacheOp.req.valid){
    when(
      CacheInstrucion.isReadTag(io.cacheOp.req.bits.opCode) ||
        CacheInstrucion.isReadTagECC(io.cacheOp.req.bits.opCode)
    ){
      for (i <- 0 until ICacheMetaArrayBanks) {
        tag_arrays(i).io.r.req.valid := true.B
        tag_arrays(i).io.r.req.bits.apply(setIdx = io.cacheOp.req.bits.index)
      }
      cacheOpShouldResp := true.B
    }
    when(CacheInstrucion.isWriteTag(io.cacheOp.req.bits.opCode)){
      for (i <- 0 until ICacheMetaArrayBanks) {
        tag_arrays(i).io.w.req.valid := true.B
        tag_arrays(i).io.w.req.bits.apply(
          data = io.cacheOp.req.bits.write_tag_low,
          setIdx = io.cacheOp.req.bits.index,
          waymask = UIntToOH(io.cacheOp.req.bits.wayNum(4, 0))
        )
      }
      cacheOpShouldResp := true.B
    }
  }
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  io.cacheOp.resp.bits.read_tag_low := Mux(io.cacheOp.resp.valid,
    tag_arrays(0).io.r.resp.asTypeOf(Vec(nWays, UInt(tagBits.W)))(io.cacheOp.req.bits.wayNum),
    0.U
  )
  io.cacheOp.resp.bits.read_tag_ecc := DontCare

  // fencei logic : reset valid_array
  when (io.fencei) {
    (0 until nWays).foreach( way =>
      valid_array(way) := 0.U
    )
  }
}