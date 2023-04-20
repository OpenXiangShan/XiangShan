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
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleFieldBase
import huancun.{AliasField, DirtyField, PreferCacheField, PrefetchField}
import xiangshan._
import xiangshan.frontend._
import xiangshan.cache._
import utils._
import utility._
import xiangshan.backend.fu.PMPReqBundle
import xiangshan.cache.mmu.{TlbRequestIO, TlbReq}
import difftest._

class WpuPredReqBundle(implicit p: Parameters) extends ICacheBundle {
  val vaddr = UInt(VAddrBits.W)
}

class WpuPredRespBundle(implicit p: Parameters) extends ICacheBundle {
  val way_en = UInt(nWays.W)
}

class WpuUpdateBundle(implicit p: Parameters) extends ICacheBundle {
  val vaddr = UInt(VAddrBits.W)
  val real_way_en = UInt(nWays.W)
}

abstract class ICacheWayPred(portNum: Int)(implicit p: Parameters) extends XSModule
  with HasICacheParameters
{
  val io = IO(new Bundle {
    val req = Vec(portNum, Flipped(ValidIO(new WpuPredReqBundle)))
    val resp = Vec(portNum, ValidIO(new WpuPredRespBundle))
    val update = Vec(portNum, Flipped(ValidIO(new WpuUpdateBundle)))
  })
}

class MruWpu(portNum: Int)(implicit p: Parameters) extends ICacheWayPred(portNum)
{
  val pred_regs = RegInit(VecInit(Seq.fill(ICacheSets)(0.U(wayBits.W))))

  (0 until portNum).foreach(i =>{
    io.resp(i).valid := io.req(i).valid
    io.resp(i).bits.way_en := UIntToOH(pred_regs(get_idx(io.req(i).bits.vaddr)))

    when (io.update(i).valid) {
      pred_regs(get_idx(io.update(i).bits.vaddr)) := OHToUInt(io.update(i).bits.real_way_en)
    }
  })
}