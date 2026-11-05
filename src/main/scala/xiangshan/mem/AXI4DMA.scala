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

package xiangshan.mem

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import system.SoCParamsKey
import xiangshan.{HasXSParameter, XSModule}
import xiangshan.cache.{DCacheLineReq, DCacheToSbufferIO, HasDCacheParameters}
import xiangshan.cache.MemoryOpConstants

/** MemBlock accepts one DMA store and reports completion only after DCache succeeds. */
class DMADCacheWriteIO(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(new DCacheLineReq))
  val resp = Valid(Bool())
}

/** Keep a DMA store outstanding across DCache misses and replay responses.
  * Its reserved ID distinguishes it from every in-flight Sbuffer store.
  */
class DMAStoreArbiter(implicit p: Parameters) extends XSModule with HasDCacheParameters {
  val io = IO(new Bundle {
    val dma = new DMADCacheWriteIO
    val sbuffer = new DCacheToSbufferIO
    val dcache = Flipped(new DCacheToSbufferIO)
  })
  private val idle :: send :: waitResp :: Nil = Enum(3)
  private val state = RegInit(idle)
  private val req = Reg(new DCacheLineReq)
  private val dmaId = StoreBufferSize.U(reqIdWidth.W)
  require(StoreBufferSize < (BigInt(1) << reqIdWidth))

  io.dma.req.ready := state === idle
  when(io.dma.req.fire) {
    req := io.dma.req.bits
    req.id := dmaId
    state := send
  }

  private val arb = Module(new RRArbiter(new DCacheLineReq, 2))
  arb.io.in(0) <> io.sbuffer.req
  arb.io.in(1).valid := state === send
  arb.io.in(1).bits := req
  io.dcache.req <> arb.io.out
  when(arb.io.in(1).fire) { state := waitResp }

  private val hit = io.dcache.main_pipe_hit_resp
  private val replay = io.dcache.replay_resp
  private val dmaHit = hit.valid && hit.bits.id === dmaId
  private val dmaReplay = replay.valid && replay.bits.id === dmaId
  io.sbuffer.main_pipe_hit_resp := hit
  io.sbuffer.main_pipe_hit_resp.valid := hit.valid && !dmaHit
  io.sbuffer.replay_resp := replay
  io.sbuffer.replay_resp.valid := replay.valid && !dmaReplay

  io.dma.resp.valid := state === waitResp && dmaHit
  io.dma.resp.bits := true.B
  when(state === waitResp) {
    when(dmaReplay) { state := send }
    when(dmaHit) { state := idle }
  }
  assert(!(dmaHit && dmaReplay), "DMA store cannot succeed and replay together")
  when(dmaHit || dmaReplay) {
    assert(state === waitResp, "DMA store response without an outstanding request")
  }
}

/** AXI terminates inside MemBlock. AXI4Fragmenter upstream supplies single
  * beats; the bridge retains every ID/echo field until the response is accepted.
  * Reads use coherent, full-line Get transactions, just like ICache/PTW, then
  * select the AXI data beat. Writes never issue TileLink Put transactions.
  */
class AXI4DMABridge(implicit p: Parameters) extends LazyModule with HasXSParameter {
  require(coreParams.dcacheParametersOpt.isDefined, "DMA requires DCache")
  private val beatBytes = 32
  private val blockBytes = coreParams.dcacheParametersOpt.get.blockBytes
  private val lgBeat = log2Ceil(beatBytes)
  private val lgBlock = log2Ceil(blockBytes)
  require(blockBytes >= beatBytes && isPow2(blockBytes))

  val axiNode = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address = Seq(AddressSet(0, (BigInt(1) << PAddrBits) - 1)),
      regionType = RegionType.CACHED,
      supportsRead = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0)
    )),
    beatBytes = beatBytes
  )))
  val readNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(
    TLMasterParameters.v1(name = "dma_read", sourceId = IdRange(0, 1))
  ))))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val write = Flipped(new DMADCacheWriteIO)
    })
    private val (axi, _) = axiNode.in.head
    private val (tl, edge) = readNode.out.head
    require(edge.manager.beatBytes == beatBytes, "DMA and L2 data beats must have equal width")

    // Reject peripheral/non-memory addresses and unsupported exclusive accesses.
    private def validAddress(a: AXI4BundleA): Bool = {
      val memory = p(SoCParamsKey).PmemRanges.map { range =>
        range.cover(a.addr) && range.cover(a.addr | (blockBytes - 1).U)
      }.reduce(_ || _)
      memory && a.len === 0.U && a.size <= lgBeat.U && a.lock === 0.U
    }

    private val rIdle :: rSend :: rWait :: rResp :: Nil = Enum(4)
    private val rState = RegInit(rIdle)
    private val ar = Reg(chiselTypeOf(axi.ar.bits))
    private val rError = RegInit(false.B)
    private val rDecodeError = RegInit(false.B)
    private val nBeats = blockBytes / beatBytes
    private val rBeat = RegInit(0.U((log2Ceil(nBeats) max 1).W))
    private val rData = Reg(Vec(nBeats, UInt((beatBytes * 8).W)))
    axi.ar.ready := rState === rIdle
    when(axi.ar.fire) {
      ar := axi.ar.bits
      rBeat := 0.U
      rError := false.B
      rDecodeError := !validAddress(axi.ar.bits)
      rState := Mux(validAddress(axi.ar.bits), rSend, rResp)
    }
    tl.a.valid := rState === rSend
    tl.a.bits := edge.Get(0.U, (ar.addr >> lgBlock) << lgBlock, lgBlock.U)._2
    when(tl.a.fire) { rState := rWait }
    tl.d.ready := rState === rWait
    when(tl.d.fire) {
      assert(tl.d.bits.opcode === TLMessages.AccessAckData)
      rData(rBeat) := tl.d.bits.data
      rError := rError || tl.d.bits.denied || tl.d.bits.corrupt
      rBeat := rBeat + 1.U
      when(edge.last(tl.d)) { rState := rResp }
    }
    tl.b.ready := true.B
    tl.c.valid := false.B
    tl.c.bits := DontCare
    tl.e.valid := false.B
    tl.e.bits := DontCare

    axi.r.valid := rState === rResp
    axi.r.bits := 0.U.asTypeOf(axi.r.bits)
    axi.r.bits.id := ar.id
    axi.r.bits.echo := ar.echo
    axi.r.bits.last := true.B
    axi.r.bits.resp := Mux(rDecodeError, AXI4Parameters.RESP_DECERR,
      Mux(rError, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY))
    private val selectedData = if (nBeats == 1) rData.head else rData(ar.addr(lgBlock - 1, lgBeat))
    axi.r.bits.data := Mux(rDecodeError, 0.U, selectedData)
    when(axi.r.fire) { rState := rIdle }

    private val wIdle :: wData :: wSend :: wWait :: wResp :: Nil = Enum(5)
    private val wState = RegInit(wIdle)
    private val aw = Reg(chiselTypeOf(axi.aw.bits))
    private val w = Reg(chiselTypeOf(axi.w.bits))
    private val wError = RegInit(false.B)
    axi.aw.ready := wState === wIdle
    axi.w.ready := wState === wData
    when(axi.aw.fire) {
      aw := axi.aw.bits
      wError := !validAddress(axi.aw.bits)
      wState := wData
    }
    when(axi.w.fire) {
      w := axi.w.bits
      // The upstream fragmenter presents each W beat as a complete transfer.
      assert(axi.w.bits.last, "DMA requires AXI4Fragmenter before MemBlock")
      wState := Mux(wError || !axi.w.bits.strb.orR, wResp, wSend)
    }

    // WDATA/WSTRB already occupy their AXI byte lanes. Place that whole beat
    // into the corresponding DCache line lanes, without shifting subwords twice.
    private val offset = if (blockBytes == beatBytes) 0.U else
      Cat(aw.addr(lgBlock - 1, lgBeat), 0.U(lgBeat.W))
    io.write.req.valid := wState === wSend
    io.write.req.bits := 0.U.asTypeOf(io.write.req.bits)
    io.write.req.bits.cmd := MemoryOpConstants.M_XWR
    io.write.req.bits.addr := (aw.addr >> lgBlock) << lgBlock
    io.write.req.bits.vaddr := (aw.addr >> lgBlock) << lgBlock
    io.write.req.bits.data := w.data << (offset << 3)
    io.write.req.bits.mask := w.strb << offset
    when(io.write.req.fire) { wState := wWait }
    when(wState === wWait && io.write.resp.valid) { wState := wResp }

    axi.b.valid := wState === wResp
    axi.b.bits := 0.U.asTypeOf(axi.b.bits)
    axi.b.bits.id := aw.id
    axi.b.bits.echo := aw.echo
    axi.b.bits.resp := Mux(wError, AXI4Parameters.RESP_DECERR, AXI4Parameters.RESP_OKAY)
    when(axi.b.fire) { wState := wIdle }
  }
}
