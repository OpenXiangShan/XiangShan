/***************************************************************************************
 * Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          https://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.mem

import chisel3._
import chisel3.util._
import difftest._
import difftest.common.DifftestMem
import org.chipsalliance.cde.config.Parameters
import top.ArgParser
import utility._
import xiangshan.ExceptionNO.hardwareError
import xiangshan._
import xiangshan.backend.Bundles.{MemExuOutput, UopIdx, connectSamePort}
import xiangshan.backend.datapath.NewPipelineConnect
import xiangshan.backend.fu.util.CSRConst
import xiangshan.backend.rob.{RobExceptionInfo, RobPtr}
import xiangshan.cache.{DCacheWordReqWithVaddrAndPfFlag, MemoryOpConstants, UncacheWordIO}
import xiangshan.mem.Bundles.LqWriteBundle

class MemExceptionInfo(implicit p: Parameters) extends XSBundle {
  val robIdx            = new RobPtr
  val uopIdx            = UopIdx()
  val exceptionVec      = ExceptionVec()
  def hasException      = exceptionVec.asUInt.orR

  val vaddr             = UInt(XLEN.W)
  val vaNeedExt         = Bool()
  val isHyper           = Bool()
  val vstart            = UInt((log2Up(VLEN) + 1).W)
  val vl                = UInt((log2Up(VLEN) + 1).W)
  val gpaddr            = UInt(GPAddrBits.W)
  val isForVSnonLeafPTE = Bool()
}

class ExceptionOut(implicit p: Parameters) extends XSBundle {
  val vaddr             = Output(UInt(XLEN.W))
  val vstart            = Output(UInt((log2Up(VLEN) + 1).W))
  val vl                = Output(UInt((log2Up(VLEN) + 1).W))
  val gpaddr            = Output(UInt(GPAddrBits.W))
  val isForVSnonLeafPTE = Output(Bool())
}

class ExceptionInfoGen(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper{
  // loadUnit, storeUnit, VLoad, VStore, storeQueue Uncache, LoadQueue Uncache, VSegmentUnit, Atomic
  private val enqPortNum = StorePipelineWidth + LoadPipelineWidth + VecLoadPipelineWidth + VecStorePipelineWidth + 1 + 1 + 1 + 1
  val io = IO(new Bundle{
    val redirect      = Flipped(ValidIO(new Redirect))
    val fromCsr       = Input(new TlbCsrBundle)
    val req           = Vec(enqPortNum, Flipped(ValidIO(new MemExceptionInfo)))
    val exceptionInfo = new ExceptionOut // don't have valid
  })
  private def isOlder(left: MemExceptionInfo, right: MemExceptionInfo): Bool = {
    isBefore(left.robIdx, right.robIdx) || (left.robIdx === right.robIdx && left.uopIdx < right.uopIdx)
  }
  val selectOldestModule = Module(new SelectOldest(new MemExceptionInfo, enqPortNum, isOlder))

  private def GenExceptionVa(
                                mode: UInt, isVirt: Bool, vaNeedExt: Bool,
                                satp: TlbSatpBundle, vsatp: TlbSatpBundle, hgatp: TlbHgatpBundle,
                                vaddr: UInt
                              ) = {
    require(VAddrBits >= 50)

    val satpNone = satp.mode === 0.U
    val satpSv39 = satp.mode === 8.U
    val satpSv48 = satp.mode === 9.U

    val vsatpNone = vsatp.mode === 0.U
    val vsatpSv39 = vsatp.mode === 8.U
    val vsatpSv48 = vsatp.mode === 9.U

    val hgatpNone = hgatp.mode === 0.U
    val hgatpSv39x4 = hgatp.mode === 8.U
    val hgatpSv48x4 = hgatp.mode === 9.U

    // For !isVirt, mode check is necessary, as we don't want virtual memory in M-mode.
    // For isVirt, mode check is unnecessary, as virt won't be 1 in M-mode.
    // Also, isVirt includes Hyper Insts, which don't care mode either.

    val useBareAddr =
      (isVirt && vsatpNone && hgatpNone) ||
        (!isVirt && (mode === CSRConst.ModeM)) ||
        (!isVirt && (mode =/= CSRConst.ModeM) && satpNone)
    val useSv39Addr =
      (isVirt && vsatpSv39) ||
        (!isVirt && (mode =/= CSRConst.ModeM) && satpSv39)
    val useSv48Addr =
      (isVirt && vsatpSv48) ||
        (!isVirt && (mode =/= CSRConst.ModeM) && satpSv48)
    val useSv39x4Addr = isVirt && vsatpNone && hgatpSv39x4
    val useSv48x4Addr = isVirt && vsatpNone && hgatpSv48x4

    val bareAddr   = ZeroExt(vaddr(PAddrBits - 1, 0), XLEN)
    val sv39Addr   = SignExt(vaddr.take(39), XLEN)
    val sv39x4Addr = ZeroExt(vaddr.take(39 + 2), XLEN)
    val sv48Addr   = SignExt(vaddr.take(48), XLEN)
    val sv48x4Addr = ZeroExt(vaddr.take(48 + 2), XLEN)

    val ExceptionVa = Wire(UInt(XLEN.W))
    when (vaNeedExt) {
      ExceptionVa := Mux1H(Seq(
        (useBareAddr)   -> bareAddr,
        (useSv39Addr)   -> sv39Addr,
        (useSv48Addr)   -> sv48Addr,
        (useSv39x4Addr) -> sv39x4Addr,
        (useSv48x4Addr) -> sv48x4Addr,
      ))
    } .otherwise {
      ExceptionVa := vaddr
    }

    ExceptionVa
  }

  private val currentValid = RegInit(false.B)
  private val currentExcp  = Reg(new MemExceptionInfo)

  private val tlbcsr = io.fromCsr

  /*===================================================== s0 stage ===================================================*/
  private val s0Valid = io.req.map{case port =>
    port.valid && !port.bits.robIdx.needFlush(io.redirect)
  }
  /*===================================================== s1 stage ===================================================*/
  // select an oldest enq exception, compare the current exception.
  private val s1Valid = s0Valid.map(x => RegNext(x))
  private val s1Bits  = io.req.map(x => RegNext(x.bits)) // for timing, don't use RegEnable

  // have exception and don't need to be flushed.
  private val selectValid = s1Valid.zip(s1Bits).map{case (v, p) =>
    v && p.hasException && !p.robIdx.needFlush(io.redirect)
  } // for timing, generate selectValid here

  selectOldestModule.io.in.zipWithIndex.map{case (sink, i) =>
    sink.valid := selectValid(i)
    sink.bits := s1Bits(i)
  }
  private val oldestBits = selectOldestModule.io.out.bits
  private val s1OutValid = selectOldestModule.io.out.valid

  when(currentValid) {
    when(s1OutValid) {
      when(currentExcp.robIdx > oldestBits.robIdx || oldestBits.robIdx === currentExcp.robIdx && currentExcp.uopIdx > oldestBits.uopIdx) {
        currentExcp := oldestBits
      }
    }
  }.otherwise {
    currentExcp  := oldestBits
  }

  when(!currentValid && s1OutValid) { // TODO: need valid ? maby for debug.
    currentValid := true.B
  }.elsewhen(currentValid && currentExcp.robIdx.needFlush(io.redirect) && !s1OutValid) {
    currentValid := false.B
  }

  // whether vaddr need ext or is hyper inst:
  // VaNeedExt: atomicsException -> false; misalignBufExceptionOverwrite -> true; vSegmentException -> false
  // IsHyper: atomicsException -> false; vSegmentException -> false

  private val exceptionVa = GenExceptionVa(tlbcsr.priv.dmode, tlbcsr.priv.virt || currentExcp.isHyper, currentExcp.vaNeedExt,
    tlbcsr.satp, tlbcsr.vsatp, tlbcsr.hgatp, currentExcp.vaddr)

  io.exceptionInfo.vstart            := currentExcp.vstart
  io.exceptionInfo.vl                := currentExcp.vl
  io.exceptionInfo.vaddr             := exceptionVa
  io.exceptionInfo.gpaddr            := currentExcp.gpaddr
  io.exceptionInfo.isForVSnonLeafPTE := currentExcp.isForVSnonLeafPTE

}

import top.Generator
object ExceptionBufferMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "exceptionBuffer" :+ "--throw-on-first-error",
    new ExceptionInfoGen()(defaultConfig),
    firtoolOpts
  )

  println("done")
}
