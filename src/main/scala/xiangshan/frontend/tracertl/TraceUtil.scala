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
import org.chipsalliance.cde.config.Parameters
import xiangshan.DebugOptionsKey

object TraceRTLChoose {
  // By-name so trace-only operands (e.g. traceInfo.get) are never elaborated in execution mode.
  def apply[T <: Data](default: => T, trtl: => T)(implicit p: Parameters): T =
    if (p(DebugOptionsKey).TraceRTLMode) trtl else default
}

object TraceRTLDontCareValue {
  def apply[T <: Data](default: T)(implicit p: Parameters): T =
    TraceRTLChoose(default, 0.U.asTypeOf(default))
}

/** Per-instruction trace payload carried through the pipeline; absent in execution mode. */
object TraceInfo {
  def apply()(implicit p: Parameters): Option[TraceInstrBundle] =
    Option.when(p(DebugOptionsKey).TraceRTLMode)(new TraceInstrBundle)
}
