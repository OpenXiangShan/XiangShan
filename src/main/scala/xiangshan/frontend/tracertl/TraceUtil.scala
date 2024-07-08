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
import org.chipsalliance.cde.config.Parameters
import xiangshan.{DebugOptionsKey}

// From HX's NewCSR utility
object ChiselRecordForField {
  implicit class AddRecordSpecifyFields[T <: Record](val x: T) {
    def specifyField(elemFns: (T => Unit)*): Unit = {
      elemFns.foreach(_.apply(x))
    }
  }
}

object TraceRTLChoose {
  def apply[T <: Data](f: T, t: T)(implicit p: Parameters): T = {
    val env = p(DebugOptionsKey)
    if (env.TraceRTLMode) {
      t
    } else {
      f
    }
  }
}

object TraceRTLDontCare {
  def apply[T <: Data](origin: T)(implicit p: Parameters): T = {
    val env = p(DebugOptionsKey)
    if (env.TraceRTLMode) {
      0.U.asTypeOf(origin)
    } else {
      origin
    }
  }
}
