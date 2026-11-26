/***************************************************************************************
* Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.ext

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.BitPat
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.MinimalConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.{FuConfig, FuType}

/** Minimal standalone plugin used to verify CoreExtPlugin wiring without external accelerators. */
class DummyCoreExtPlugin extends CoreExtPlugin {
  override def name: String = "dummy-ext"
  override def enableExtExec: Boolean = true

  // Custom-0 opcode (0x0b), funct3 = 0b000
  override def decodeTable(implicit p: Parameters): Seq[ExtDecodeRow] = Seq(
    ExtDecodeRow(
      pat       = BitPat("b?????????????????000?????0001011"),
      fuType    = FuType.extProxy,
      fuOpType  = 1,
      src1Ren   = true,
      src2Ren   = true,
      rfWen     = true,
      noSpec    = false,
      blockBack = false,
      flushPipe = false,
    ),
  )
}

/** Test harness combining ExtDecodeHelper and ExtProxyFu with an external responder interface. */
class ExtProxyUnitHarness(implicit p: Parameters) extends Module {
  private val plugin = new DummyCoreExtPlugin
  val proxy = Module(new ExtProxyFu(FuConfig.ExtProxyCfg))

  val io = IO(new Bundle {
    val rawInst       = Input(UInt(32.W))
    val decValid      = Output(Bool())
    val decOp         = Output(UInt(9.W))
    val inValid       = Input(Bool())
    val inReady       = Output(Bool())
    val inOp          = Input(UInt(9.W))
    val inRs1         = Input(UInt(64.W))
    val inRs2         = Input(UInt(64.W))
    val inRaw25       = Input(UInt(25.W))
    val inRobIdx      = Input(UInt(6.W))
    val flushValid    = Input(Bool())
    val flushRobIdx   = Input(UInt(6.W))
    val outValid      = Output(Bool())
    val outData       = Output(UInt(64.W))
    val outIllegal    = Output(Bool())
    val extReqValid   = Output(Bool())
    val extReqReady   = Input(Bool())
    val extReqSeqId   = Output(UInt(2.W))
    val extAbort      = Output(Bool())
    val extRespValid  = Input(Bool())
    val extRespData   = Input(UInt(64.W))
    val extRespSeqId  = Input(UInt(2.W))
    val extRespIll    = Input(Bool())
  })

  val dec = ExtDecodeHelper.decode(io.rawInst, plugin.decodeTable)
  io.decValid := dec.valid
  io.decOp    := dec.bits.opcode

  proxy.io.in.valid                  := io.inValid
  proxy.io.in.bits                   := 0.U.asTypeOf(proxy.io.in.bits)
  proxy.io.in.bits.ctrl.fuOpType     := io.inOp
  proxy.io.in.bits.ctrl.robIdx.value := io.inRobIdx
  proxy.io.in.bits.ctrl.rfWen.foreach(_ := true.B)
  proxy.io.in.bits.data.src(0)       := io.inRs1
  proxy.io.in.bits.data.src(1)       := io.inRs2
  proxy.io.in.bits.data.imm          := io.inRaw25
  io.inReady                         := proxy.io.in.ready

  proxy.io.flush.valid          := io.flushValid
  proxy.io.flush.bits           := 0.U.asTypeOf(proxy.io.flush.bits)
  proxy.io.flush.bits.robIdx.value := io.flushRobIdx

  proxy.io.out.ready := true.B
  io.outValid        := proxy.io.out.valid
  io.outData         := proxy.io.out.bits.res.data
  io.outIllegal      := proxy.io.out.bits.ctrl.exceptionVec.map(_(illegalInstr)).getOrElse(false.B)

  val ext = proxy.io.extExec.get
  io.extReqValid            := ext.req.valid
  ext.req.ready             := io.extReqReady
  io.extReqSeqId            := ext.req.bits.seqId
  io.extAbort               := ext.abort
  ext.resp.valid            := io.extRespValid
  ext.resp.bits             := 0.U.asTypeOf(ext.resp.bits)
  ext.resp.bits.rdData      := io.extRespData
  ext.resp.bits.seqId       := io.extRespSeqId
  ext.resp.bits.illegalInstr := io.extRespIll
}

class CoreExtPluginSpec extends AnyFlatSpec with Matchers with ChiselSim {
  behavior of "CoreExtPlugin"

  it should "register plugin and enable HasExtExec via WithCoreExtPlugin without affecting MinimalConfig" in {
    val baseCfg = new MinimalConfig(1)
    baseCfg(CoreExtPluginsKey) shouldBe empty
    baseCfg(XSTileKey).head.HasExtExec shouldBe false

    val extCfg = new WithCoreExtPlugin(new DummyCoreExtPlugin) ++ baseCfg
    extCfg(CoreExtPluginsKey).map(_.name) shouldBe Seq("dummy-ext")
    extCfg(XSTileKey).head.HasExtExec shouldBe true
  }

  it should "decode custom instructions and handle combinational response, multi-cycle response, and flush abort" in {
    val cfg = new WithCoreExtPlugin(new DummyCoreExtPlugin) ++ new MinimalConfig(1)
    implicit val p: Parameters = cfg.alterPartial { case XSCoreParamsKey => cfg(XSTileKey).head }

    simulate(new ExtProxyUnitHarness) { dut =>
      dut.io.rawInst.poke("h0020818b".U) // custom-0, funct3=0, rd=3, rs1=1, rs2=2
      dut.io.extReqReady.poke(true.B)
      dut.io.inValid.poke(false.B)
      dut.io.flushValid.poke(false.B)
      dut.io.extRespValid.poke(false.B)
      dut.io.extRespIll.poke(false.B)
      dut.clock.step(1)

      dut.io.decValid.expect(true.B)
      dut.io.decOp.expect(1.U)

      // 1. Same-cycle combinational response
      dut.io.inValid.poke(true.B)
      dut.io.inOp.poke(1.U)
      dut.io.inRs1.poke(10.U)
      dut.io.inRs2.poke(20.U)
      dut.io.inRobIdx.poke(1.U)
      dut.io.extRespValid.poke(true.B)
      dut.io.extRespSeqId.poke(0.U)
      dut.io.extRespData.poke(30.U)
      dut.clock.step(1)

      dut.io.inValid.poke(false.B)
      dut.io.extRespValid.poke(false.B)
      dut.io.outValid.expect(true.B)
      dut.io.outData.expect(30.U)
      dut.io.outIllegal.expect(false.B)
      dut.clock.step(1)

      // 2. Multi-cycle request aborted by flush; stale response with old seqId must be ignored
      dut.io.inValid.poke(true.B)
      dut.io.inRobIdx.poke(5.U)
      dut.io.extReqSeqId.expect(1.U)
      dut.clock.step(1)

      dut.io.inValid.poke(false.B)
      dut.io.flushValid.poke(true.B)
      dut.io.flushRobIdx.poke(4.U)
      dut.io.extAbort.expect(true.B)
      dut.clock.step(1)

      dut.io.flushValid.poke(false.B)
      dut.io.extRespValid.poke(true.B)
      dut.io.extRespSeqId.poke(1.U) // stale seqId
      dut.io.extRespData.poke(999.U)
      dut.clock.step(1)
      dut.io.outValid.expect(false.B)
    }
  }
}
