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

package xiangshan.backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.exu.{ExuConfig, Wb2Ctrl, WbArbiterWrapper, FenceIO}
import system.HasSoCParameter


class MemBlockSub(implicit p: Parameters) extends LazyModule 
  with HasWritebackSource
  with HasXSParameter 
  with HasExuWbHelper{
  lazy val module = new MemBlockSubImp(this)

  override val writebackSourceParams: Seq[WritebackSourceParams] = {
    val params = new WritebackSourceParams
    params.exuConfigs = (loadExuConfigs ++ storeExuConfigs).map(cfg => Seq(cfg))
    Seq(params)
  }
  override lazy val writebackSourceImp: HasWritebackSourceImp = module
}

class MemBlockSubImp(outer: MemBlockSub)(implicit p: Parameters) extends LazyModuleImp(outer)
  with HasWritebackSourceImp 
  with HasXSParameter
  with HasSoCParameter{
  val io = IO(new Bundle {
    val writeback_in = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, Flipped(DecoupledIO(new ExuOutput)))

    val writeback = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuOutput))
  })
  io.writeback <> io.writeback_in


  override def writebackSource1: Option[Seq[Seq[DecoupledIO[ExuOutput]]]] = Some(Seq(io.writeback))

}
