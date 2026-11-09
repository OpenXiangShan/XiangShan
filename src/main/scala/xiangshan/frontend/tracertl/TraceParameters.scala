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

import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters

case object TraceRTLParamKey extends Field[TraceRTLParameters]

case class TraceRTLParameters(
    // Core widths
    TracePredictWidth: Int = 32, // 64B IFU fetch block in 2-byte instruction slots
    TraceFetchWidth:   Int = 32, // batch read size from DPI-C per refill

    // Trace data field widths
    TraceVAddrWidth:    Int = 50, // virtual address width
    TracePAddrWidth:    Int = 48, // physical address width
    TraceInstCodeWidth: Int = 32, // instruction code width
    TraceInstIDWidth:   Int = 64, // instruction ID width

    // Control flags
    TraceOverrideTarget: Boolean = true // use trace target instead of PC+offset for branch targets
) {
  def TraceBufferSize = TraceFetchWidth * 4 // 64-entry circular buffer
}
