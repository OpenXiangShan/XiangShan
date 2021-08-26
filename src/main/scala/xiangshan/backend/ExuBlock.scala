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
import xiangshan.backend.exu._

class ExuBlock(
  val configs: Seq[(ExuConfig, Int, Seq[ExuConfig], Seq[ExuConfig])],
  val dpPorts: Seq[Seq[(Int, Int)]],
  val intRfWbPorts: Seq[Seq[ExuConfig]],
  val fpRfWbPorts: Seq[Seq[ExuConfig]],
  val outFastPorts: Seq[Seq[Int]],
  val outFpRfReadPorts: Int
)(implicit p: Parameters) extends LazyModule {
  val scheduler = LazyModule(new Scheduler(configs, dpPorts, intRfWbPorts, fpRfWbPorts, outFastPorts, outFpRfReadPorts))

  val allRfWbPorts = intRfWbPorts ++ fpRfWbPorts
  val wbPosition = configs.map(cfg => allRfWbPorts.zipWithIndex.filter(_._1.contains(cfg._1)).map(_._2))

  lazy val module = new ExuBlockImp(this)
}

class ExuBlockImp(outer: ExuBlock)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val scheduler = outer.scheduler.module

  val fuConfigs = outer.configs.map(c => (c._1, c._2))
  val fuBlock = Module(new FUBlock(fuConfigs))

  val io = IO(new Bundle {
    // global control
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // dispatch ports
    val allocate = scheduler.io.allocate.cloneType
    // issue and wakeup ports
    val fastUopOut = scheduler.io.fastUopOut.cloneType
    val rfWriteback = scheduler.io.writeback.cloneType
    val fastUopIn = scheduler.io.fastUopIn.cloneType
    val fuWriteback = fuBlock.io.writeback.cloneType
    // extra
    val scheExtra = scheduler.io.extra.cloneType
    val fuExtra = fuBlock.io.extra.cloneType
  })

  scheduler.io.redirect <> io.redirect
  scheduler.io.flush <> io.flush
  scheduler.io.allocate <> io.allocate
  scheduler.io.fastUopOut <> io.fastUopOut
  scheduler.io.writeback <> io.rfWriteback
  scheduler.io.fastUopIn <> io.fastUopIn
  scheduler.io.extra <> io.scheExtra

  scheduler.io.issue <> fuBlock.io.issue

  val flattenFuConfigs = fuConfigs.zip(outer.wbPosition).flatMap(c => Seq.fill(c._1._2)((c._1._1, c._2)))
  require(flattenFuConfigs.length == fuBlock.io.writeback.length)
  val directConn = flattenFuConfigs.zip(fuBlock.io.writeback).filterNot(_._1._1.hasUncertainlatency)
  if (directConn.length > 0) {
    val directWbPorts = directConn.map(_._1._2).reduce(_ ++ _).toSet.toSeq
    println(s"Ports $directWbPorts are directly connected from function units.")
    require(directConn.length == directWbPorts.length)
    val wbPortExuCfgs = directWbPorts.map(outer.allRfWbPorts(_))
    wbPortExuCfgs.foreach(cfgs => require(cfgs.length == 1))
    val schedWbPorts = directWbPorts.map(scheduler.io.writeback(_))
    val outerWbPorts = directWbPorts.map(io.rfWriteback(_))
    schedWbPorts.zip(directConn.map(_._2)).zip(outerWbPorts).map{ case ((s, f), o) =>
      s := f
      XSError((o.valid || f.valid) && o.bits.uop.roqIdx =/= f.bits.uop.roqIdx, "different instruction\n")
      XSError((o.valid || f.valid) && o.bits.data =/= f.bits.data, "different data\n")
    }
  }

  fuBlock.io.redirect <> io.redirect
  fuBlock.io.flush <> io.flush
  fuBlock.io.writeback <> io.fuWriteback
  fuBlock.io.extra <> io.fuExtra

}
