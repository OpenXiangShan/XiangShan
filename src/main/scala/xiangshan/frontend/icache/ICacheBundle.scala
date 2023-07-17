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
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLPermissions}
import xiangshan._
import utils._
import utility._

class ICacheReadBundle(implicit p: Parameters) extends ICacheBundle
{
  val isDoubleLine  = Bool()
  val vSetIdx       = Vec(2,UInt(log2Ceil(nSets).W))

  def port_0_read_0 =  !vSetIdx(0)(0)
  def port_0_read_1 =   vSetIdx(0)(0)
  def port_1_read_0 =  !vSetIdx(1)(0) && isDoubleLine
  def port_1_read_1 =   vSetIdx(1)(0) && isDoubleLine

  def read_bank_0 = port_0_read_0 || port_1_read_0
  def read_bank_1 =  port_0_read_1 || port_1_read_1
}


class ICacheMetaRespBundle(implicit p: Parameters) extends ICacheBundle
{
  val metaData   = Vec(2, Vec(nWays, new ICacheMetadata))
  val errors     = Vec(2, Vec(nWays ,Bool() ))
  val entryValid = Vec(2, Vec(nWays, Bool()))

  def tags = VecInit(metaData.map(port => VecInit(port.map( way=> way.tag ))))
}

class ICacheMetaWriteBundle(implicit p: Parameters) extends ICacheBundle
{
  val virIdx  = UInt(idxBits.W)
  val phyTag  = UInt(tagBits.W)
  val waymask = UInt(nWays.W)
  val bankIdx = Bool()

  def generate(tag:UInt, idx:UInt, waymask:UInt, bankIdx: Bool){
    this.virIdx  := idx
    this.phyTag  := tag
    this.waymask := waymask
    this.bankIdx   := bankIdx
  }

}

class ICacheDataWriteBundle(implicit p: Parameters) extends ICacheBundle
{
  val virIdx  = UInt(idxBits.W)
  val data    = UInt(blockBits.W)
  val waymask = UInt(nWays.W)
  val bankIdx = Bool()
  val paddr   = UInt(PAddrBits.W)

  def generate(data:UInt, idx:UInt, waymask:UInt, bankIdx: Bool, paddr: UInt){
    this.virIdx  := idx
    this.data    := data
    this.waymask := waymask
    this.bankIdx := bankIdx
    this.paddr   := paddr
  }

}

class ICacheDataRespBundle(implicit p: Parameters) extends ICacheBundle
{
  val datas = Vec(2, Vec(nWays,  UInt(blockBits.W)))
  val codes = Vec(2, Vec(nWays , UInt(dataCodeEntryBits.W)))
}

class ICacheMetaReadBundle(implicit p: Parameters) extends ICacheBundle
{
    val req     = Flipped(DecoupledIO(new ICacheReadBundle))
    val resp = Output(new ICacheMetaRespBundle)
}

class IPFBufferFilterRead(implicit p: Parameters) extends  IPrefetchBundle{
  /** input */
  val req = Flipped(new Bundle {
    val vSetIdx = Output(UInt(log2Ceil(nSets).W))
    val paddr = Output(UInt(PAddrBits.W))
  })
  /** output */
  val resp = new Bundle {
    val ipf_hit = Output(Bool())
  }
}

class IPFBufferRead(implicit p: Parameters) extends IPrefetchBundle {
  val req = Vec(PortNumber, Flipped(DecoupledIO(new Bundle {
    val vaddr = UInt(VAddrBits.W)
    val paddr = UInt(PAddrBits.W)
  })))
  val resp = Vec(PortNumber, ValidIO(new Bundle {
    val ipf_hit = Bool()
    val cacheline = UInt(blockBits.W)
  }))
}

class PIQMetaWrite(implicit p: Parameters) extends  IPrefetchBundle{
  val tag = UInt(tagBits.W)
  val index = UInt(idxBits.W)
  val paddr = UInt(PAddrBits.W)
}

class IPFBufferWrite(implicit p: Parameters) extends  IPrefetchBundle{
  val buffIdx = UInt(log2Ceil(nPrefetchEntries).W)
  val meta = new PIQMetaWrite
  val data =  UInt(blockBits.W)
}

class IPFBufferMove(implicit p: Parameters) extends  IPrefetchBundle{
  val vsetIdx = Output(UInt(idxBits.W))
  val waymask = Input(UInt(nWays.W))
}
