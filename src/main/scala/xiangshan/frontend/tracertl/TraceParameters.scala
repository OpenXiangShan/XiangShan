/** *************************************************************************************
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
 * ************************************************************************************* */
package xiangshan.frontend.tracertl

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}

case object TraceRTLParamKey extends Field[TraceRTLParameters]

case class TraceRTLParameters
(
  TraceRTLOnPLDM: Boolean = false, // for pldm
  TraceRTLOnFPGA: Boolean = false, // for fpga

  TraceFetchWidth: Int = 16,
  TraceEnableDuplicateFlush: Boolean = true,
  TraceEnableWrongPathEmu: Boolean = true,
  TraceWrongPathEmuWhenConvergence: Boolean = true,
  TraceSoftL1TLB: Boolean = false,
  TraceSoftL1TLBCheck: Boolean = false,
  TraceDummyFixCycleDivSqrt: Boolean = false,
  TraceDummyFixCycleIntDiv: Boolean = false,
  TraceEliminateArthi: Boolean = true,
  TraceEliminateMemory: Boolean = true,
  TraceOverrideTarget: Boolean = true,
  Trace2StageMMU: Boolean = false,

  TraceFpgaHugeBufferSize: Int = 1 * 1024, // 1K
  // TraceFpgaUnpackInstNum: Int = 128, // 16 * 8
  TraceFpgaUnpackInstNum: Int = 1 * 16, // 16 * 8
  TraceFpgaAxisWidth: Int = 512,
  TraceFpgaCollectWidth: Int = 128,
) {
  def TraceBufferSize = TraceFetchWidth * 4
  def TraceFpgaRecvWidth = TraceFetchWidth // set it same to TraceFetchWidth now
}