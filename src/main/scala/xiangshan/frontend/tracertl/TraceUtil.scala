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
import chisel3.util.experimental.BoringUtils
import chisel3.util.experimental.BoringUtils.tapAndRead
import chisel3.util._
import chisel3.reflect.DataMirror.isVisible
import org.chipsalliance.cde.config.Parameters
import xiangshan.{DebugOptionsKey, XSTileKey}
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.FuType.FuTypeOrR
import chisel3.util.experimental.BoringUtils

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

object TraceRTLDontCareValue {
  def apply[T <: Data](origin: T)(implicit p: Parameters): T = {
    val env = p(DebugOptionsKey)
    if (env.TraceRTLMode) {
      0.U.asTypeOf(origin)
    } else {
      origin
    }
  }
}

object TraceRTLDontCare {
  def apply[T <: Data](origin: T)(implicit p: Parameters): Unit = {
    val env = p(DebugOptionsKey)
    if (env.TraceRTLMode) {
      origin := 0.U.asTypeOf(origin)
    }
  }
}


object TraceFastSimOoO {
  val needOoOFuType = Seq(
    FuType.jmp, FuType.brh, FuType.csr, // cfi
    // FuType.jmp, FuType.brh, FuType.csr, // cfi
    // FuType.ldu, FuType.stu, FuType.mou, // ls
    // FuType.vldu, FuType.vstu, FuType.vsegldu, FuType.vsegstu // vls
  )

  def needOoO(fuType: UInt)(implicit p: Parameters): Bool = {
    FuTypeOrR(fuType, needOoOFuType)
  }
}

object TraceFastSimMemory {
  def apply()(implicit p: Parameters): Bool = {
    val env = p(DebugOptionsKey)
    val xsParam = p(XSTileKey).head

    val fastSimFinish = WireInit(true.B)
    BoringUtils.addSink(fastSimFinish, "TraceFastSimMemoryFinish")
    if (env.TraceRTLMode) {
      xsParam.TraceEliminateMemory.B && !fastSimFinish
      // TraceFastSim.fastSimEnable()
    } else {
      false.B
    }
  }
}

object TraceBoringUtils {
  val signalMap = scala.collection.mutable.Map[String, UInt]()

  def addSource(source: UInt, name: String): Unit = {
    signalMap(name) = source
  }
  def addSink(name: String): UInt = {
    val source = signalMap(name)
    if (isVisible(source)) source else tapAndRead(source)
  }
}