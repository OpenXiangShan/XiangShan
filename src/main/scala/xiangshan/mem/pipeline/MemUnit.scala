/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomacy.{BundleBridgeSource, LazyModule, LazyModuleImp}
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.ctrlblock.{DebugLsInfoBundle, LsTopdownInfo}
import xiangshan.backend.datapath.{NewPipelineConnect, NewPipelineConnectPipe}
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu.PMPRespBundle
import xiangshan.backend.fu.NewCSR._
import xiangshan.mem.Bundles.LsPipelineBundle
import xiangshan.mem.Bundles._
import xiangshan.cache.{DCacheLoadReqIO, DCacheLoadRespIO, MemoryOpConstants}
import xiangshan.cache.{HasDCacheParameters}
import xiangshan.cache.mmu.{Pbmt, TlbReq, TlbResp, TlbCmd, TlbHintReq}

class MemUnit(val params: MemUnitParams)(implicit p: Parameters) extends LazyModule
  with HasXSParameter {
  override def shouldBeInlined: Boolean = false
  implicit val unitParams: MemUnitParams = params
  lazy val module: MemUnitImp = unitParams.unitType match {
    case Std() => new StdImp(this)
    case Sta() => new HyuImp(this)
    case Ldu() => new HyuImp(this)
    case Hyu() => new HyuImp(this)
    case Amo() => new AmoImp(this)
    case _     => throw new IllegalArgumentException("Unknown unit type")
  }
}

class MemUnitIO()(implicit p: Parameters, params: MemUnitParams) extends XSBundle {
  // from
  val fromCtrl = new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val hartId = Input(UInt(hartIdLen.W))
    val csrCtrl = Flipped(new CustomCSRCtrlIO)
    val trigger = Input(new CsrTriggerBundle)
  }
}

class MemUnitImp(override val wrapper: MemUnit)(implicit p: Parameters, val params: MemUnitParams)
  extends LazyModuleImp(wrapper)
  with HasXSParameter
  with HasDCacheParameters
  with HasCircularQueuePtrHelper {

  protected def printPipeLine(pipeline: LsPipelineBundle, cond: Bool, name: String): Unit = {
    XSDebug(cond,
      p"$name" + p" pc ${Hexadecimal(pipeline.uop.pc)} " +
        p"addr ${Hexadecimal(pipeline.vaddr)} -> ${Hexadecimal(pipeline.paddr)} " +
        p"op ${Binary(pipeline.uop.fuOpType)} " +
        p"data ${Hexadecimal(pipeline.data)} " +
        p"mask ${Hexadecimal(pipeline.mask)}\n"
    )
  }

  println(s"[MemUnitImp] ${params.name} \n" +
          s"   unitType: ${params.unitType}\n" +
          s"   dataBits: ${params.dataBits}\n" +
          s"   exceptionOut: ${params.exceptionOut}\n" +
          s"   triggerType: ${params.triggerType}")
  /**
    * --------------------------------------------------------------------
    * IOs
    * --------------------------------------------------------------------
    */
  lazy val io = IO(new MemUnitIO())
}
