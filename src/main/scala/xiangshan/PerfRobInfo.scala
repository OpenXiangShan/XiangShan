/***************************************************************************************
* Copyright (c) 2024-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.rob.RobPtr

class PerfRedirect(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val self = Bool()
  val pc = UInt(XLEN.W)
  val isCtrlMispred = Bool()
  val isMemVio = Bool()
}

class PerfRobFlush(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val self = Bool()
  val pc = UInt(XLEN.W)
  val isException = Bool()
  val isInterrupt = Bool()
}

// Raw ROB observations shared by instruction profilers; attribution stays in each consumer.
class PerfRobInfo(implicit p: Parameters) extends XSBundle {
  val cycle = UInt(64.W)
  val retired = UInt(log2Ceil(CommitWidth * RenameWidth + 1).W)
  val commits = Vec(2 * CommitWidth, Valid(new RobPtr))
  val redirect = Valid(new PerfRedirect)
  val flush = Valid(new PerfRobFlush)
  val flushEcho = Bool()
}
