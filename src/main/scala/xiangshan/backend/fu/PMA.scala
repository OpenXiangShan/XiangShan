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

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import xiangshan.cache.mmu.TlbCmd

class PMAConfig(implicit p: Parameters) extends PMPBundle {
  val l = Bool()
  val c = Bool() // cached
  val atomic = Bool() // atomic
  val a = UInt(2.W)
  val x = Bool()
  val w = Bool()
  val r = Bool()
}

@chiselName
class PMA(implicit p: Parameters) extends PMP {
  when (reset.asBool()) {
    // add init value here
    // some thing must be check in m mode, like mmio, please lock it
  }
}

@chiselName
class PMAChecker(lgMaxSize: Int, sameCycle: Boolean = false)(implicit p: Parameters) extends PMPChecker {
  // NOTE: if itlb or dtlb may get blocked, this may also need do it
  val cfg_pma = res.cfg.asTypeOf(new PMAConfig())
  override val ld = TlbCmd.isRead(req.cmd) && !TlbCmd.isAtom(req.cmd) && !res.cfg.r
  override val st = (TlbCmd.isWrite(req.cmd) || TlbCmd.isAtom(req.cmd) && cfg_pma.atomic) && !res.cfg.w
  override val instr = TlbCmd.isExec(req.cmd) && !res.cfg.x
  if (sameCycle) {
    // io.resp.cached := cfg_pma.c
    io.resp.ld := ld
    io.resp.st := st
    io.resp.instr := instr
  } else {
//    io.resp.cache := cfg_pma.c
    io.resp.ld := RegEnable(ld, io.req.valid)
    io.resp.st := RegEnable(st, io.req.valid)
    io.resp.instr := RegEnable(instr, io.req.valid)
  }
}
