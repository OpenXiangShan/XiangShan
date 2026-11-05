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

package xiangshan.backend

import chisel3._
import chisel3.util.Decoupled
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.{DecodeInUop, DecodeOutUop, EnqRobUop, RenameOutUop}
import xiangshan.backend.dispatch.Dispatch
import xiangshan.backend.fu.FuType
import xiangshan.frontend.{BackendRedirectTopdown, FetchToIBuffer, HasFrontendParameters}
import xiangshan.frontend.ibuffer.IBuffer
import xiangshan.mem.{InstrType, LoadEntrance, LoadUnitS1, StoreEntrance, StoreUnitS1}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey}

class TeaHelperTest extends XSTester {
  behavior of "TEA helper metadata"

  private def teaBitValue(event: Int): BigInt = BigInt(1) << event

  class TeaPropagationSmoke(p: Parameters) extends Module {
    private val ctrlTea = TeaPsvOps.setBit(TeaEvent.bit(TeaEvent.DR_L1), TeaEvent.FL_MB)

    val io = IO(new Bundle {
      val decodeInTea = Output(UInt(TeaEvent.width.W))
      val decodeOutTea = Output(UInt(TeaEvent.width.W))
      val renameTea = Output(UInt(TeaEvent.width.W))
      val enqRobTea = Output(UInt(TeaEvent.width.W))
    })

    val cf = WireInit(0.U.asTypeOf(new CtrlFlow()(p)))
    cf.teaPsv := ctrlTea
    val decodeIn = WireInit(0.U.asTypeOf(new DecodeInUop()(p)))
    decodeIn.connectCtrlFlow(cf)
    val decodeOut = WireInit(0.U.asTypeOf(new DecodeOutUop()(p)))
    decodeOut.connectDecodeInUop(decodeIn)
    val renameOut = WireInit(0.U.asTypeOf(new RenameOutUop()(p)))
    Bundles.connectSamePort(renameOut, decodeOut)
    val enqRob = WireInit(0.U.asTypeOf(new EnqRobUop()(p)))
    enqRob.connectEnqRobUop(renameOut)

    io.decodeInTea := decodeIn.teaPsv
    io.decodeOutTea := decodeOut.teaPsv
    io.renameTea := renameOut.teaPsv
    io.enqRobTea := enqRob.teaPsv
  }

  class PacketPsvHarness extends Module {
    val io = IO(new Bundle {
      val valids = Input(Vec(4, Bool()))
      val packetPsv = Input(UInt(TeaEvent.width.W))
      val out = Output(Vec(4, UInt(TeaEvent.width.W)))
    })
    io.out := TeaFrontend.bindPacketPsv(io.valids, io.packetPsv)
  }

  class IBufferBypassRegressionHarness(implicit val p: Parameters) extends Module with HasFrontendParameters {
    require(IBufferEnqueueWidth > DecodeWidth, "IBuffer bypass regression needs enqueue width > decode width")

    val io = IO(new Bundle {
      val inValid = Input(Bool())
      val validMask = Input(UInt(IBufferEnqueueWidth.W))
      val enqEnableMask = Input(UInt(IBufferEnqueueWidth.W))
      val instrs = Input(Vec(IBufferEnqueueWidth, UInt(32.W)))
      val decodeCanAccept = Input(Bool())
      val packetDrL1 = Input(Bool())
      val packetDrTlb = Input(Bool())
      val inReady = Output(Bool())
      val outValid = Output(Vec(DecodeWidth, Bool()))
      val outInstr = Output(Vec(DecodeWidth, UInt(32.W)))
      val outTeaPsv = Output(Vec(DecodeWidth, UInt(TeaEvent.width.W)))
    })

    val ibuffer = Module(new IBuffer()(p))
    val inBits = WireInit(0.U.asTypeOf(new FetchToIBuffer()(p)))

    inBits.instrs := io.instrs
    inBits.valid := io.validMask
    inBits.enqEnable := io.enqEnableMask
    inBits.teaPsv := Mux(
      io.packetDrTlb,
      TeaPsvOps.setBit(
        Mux(io.packetDrL1, TeaEvent.bit(TeaEvent.DR_L1), TeaPsvOps.empty),
        TeaEvent.DR_TLB
      ),
      Mux(io.packetDrL1, TeaEvent.bit(TeaEvent.DR_L1), TeaPsvOps.empty)
    )

    ibuffer.io.flush := false.B
    ibuffer.io.fromBackend := 0.U.asTypeOf(new BackendToIBufBundle()(p))
    ibuffer.io.fromBackend.decodeCanAccept := io.decodeCanAccept
    ibuffer.io.backendRedirectTopdown := 0.U.asTypeOf(new BackendRedirectTopdown()(p))
    ibuffer.io.in.valid := io.inValid
    ibuffer.io.in.bits := inBits
    ibuffer.io.out.foreach(_.ready := true.B)

    io.inReady := ibuffer.io.in.ready
    io.outValid := VecInit(ibuffer.io.out.map(_.valid))
    io.outInstr := VecInit(ibuffer.io.out.map(_.bits.instr))
    io.outTeaPsv := VecInit(ibuffer.io.out.map(_.bits.teaPsv))
  }

  class TeaTlbMissHarness(implicit val p: Parameters) extends Module with HasXSParameter {
    val io = IO(new Bundle {
      val valid = Input(Bool())
      val miss = Input(Bool())
      val replay = Input(Bool())
      val prefetch = Input(Bool())
      val kill = Input(Bool())
      val loadMiss = Output(Bool())
      val loadFirstMiss = Output(Bool())
      val storeMiss = Output(Bool())
      val storeFirstMiss = Output(Bool())
    })
    val load = Module(new LoadUnitS1(backendParams.allExuParams.find(_.hasLoadFu).get)(p))
    load.io.redirect := 0.U.asTypeOf(load.io.redirect)
    load.io.kill := io.kill
    load.io.specialMode := false.B
    load.io.robHeadPtr := 0.U.asTypeOf(load.io.robHeadPtr)
    load.io.tlbResp.valid := true.B
    load.io.tlbResp.bits := 0.U.asTypeOf(load.io.tlbResp.bits)
    load.io.tlbResp.bits.miss := io.miss
    load.io.sqForwardResp := 0.U.asTypeOf(load.io.sqForwardResp)
    load.io.uncacheBypassResp := 0.U.asTypeOf(load.io.uncacheBypassResp)
    load.io.staNukeQueryReq := 0.U.asTypeOf(load.io.staNukeQueryReq)
    load.io.csrTrigger := 0.U.asTypeOf(load.io.csrTrigger)
    load.io.unalignTail.ready := true.B
    load.io_pipeIn.get.valid := io.valid
    load.io_pipeIn.get.bits := 0.U.asTypeOf(load.io_pipeIn.get.bits)
    load.io_pipeIn.get.bits.entrance := Mux(io.prefetch, LoadEntrance.prefetchHiConf.U,
      Mux(io.replay, LoadEntrance.replayHiPrio.U, LoadEntrance.scalarIssue.U))
    load.io_pipeIn.get.bits.accessType.instrType := Mux(io.prefetch, InstrType.prefetch.U, InstrType.scalar.U)
    load.io_pipeIn.get.bits.hasROBEntry := !io.prefetch
    load.io_pipeOut.get.ready := true.B

    val store = Module(new StoreUnitS1(backendParams.allExuParams.find(_.hasStoreAddrFu).get)(p))
    store.io.redirect := 0.U.asTypeOf(store.io.redirect)
    store.io.csrTrigger := 0.U.asTypeOf(store.io.csrTrigger)
    store.io.tlbResp.valid := true.B
    store.io.tlbResp.bits := 0.U.asTypeOf(store.io.tlbResp.bits)
    store.io.tlbResp.bits.miss := io.miss
    store.io.unalignTail.ready := true.B
    store.io.toUnalignQueue.ready := true.B
    store.io.unalignHeadTlbHit := false.B
    store.io_pipeIn.get.valid := io.valid
    store.io_pipeIn.get.bits := 0.U.asTypeOf(store.io_pipeIn.get.bits)
    store.io_pipeIn.get.bits.entrance := Mux(io.prefetch, StoreEntrance.prefetch.U, StoreEntrance.scalarIssue.U)
    store.io_pipeIn.get.bits.accessType.instrType := Mux(io.prefetch, InstrType.prefetch.U, InstrType.scalar.U)
    store.io_pipeIn.get.bits.isFirstIssue := !io.replay
    store.io_pipeIn.get.bits.illegalIssue := io.kill
    store.io_pipeOut.get.ready := true.B

    io.loadMiss := load.io.debugInfo.isTlbMiss
    io.loadFirstMiss := load.io.debugInfo.isTlbFirstMiss
    io.storeMiss := store.io.debugInfo.s1_isTlbMiss
    io.storeFirstMiss := store.io.debugInfo.s1_isTlbFirstMiss
  }

  class TeaDispatchHarness(implicit val p: Parameters) extends Module with HasXSParameter {
    // Match Backend's parameter binding before constructing Dispatch.
    backendParams.allExuParams.zipWithIndex.foreach { case (exu, i) =>
      exu.bindBackendParam(backendParams)
      exu.updateIQWakeUpConfigs(backendParams.iqWakeUpParams)
      exu.updateExuIdx(i)
    }
    backendParams.allSchdParams.foreach(_.bindBackendParam(backendParams))
    backendParams.allIssueParams.foreach(_.bindBackendParam(backendParams))
    val io = IO(new Bundle {
      val validMask = Input(UInt(RenameWidth.W))
      val storeMask = Input(UInt(RenameWidth.W))
      val robEmpty = Input(Bool())
      val sqHasRoom = Input(Bool())
      val flush = Input(Bool())
      val inReady = Output(Bool())
      val dispatchValid = Output(Vec(RenameWidth, Bool()))
      val enqValid = Output(Vec(RenameWidth, Bool()))
      val enqPsv = Output(Vec(RenameWidth, UInt(TeaEvent.width.W)))
    })
    val dispatch = Module(new Dispatch()(p))
    val renamed = Wire(Vec(RenameWidth, Decoupled(new RenameOutUop()(p))))
    for (i <- 0 until RenameWidth) {
      renamed(i).valid := io.validMask(i)
      renamed(i).bits := 0.U.asTypeOf(renamed(i).bits)
      renamed(i).bits.fuType := Mux(io.storeMask(i), FuType.stu.U, FuType.alu.U)
      renamed(i).bits.firstUop := true.B
      renamed(i).bits.lastUop := true.B
      renamed(i).bits.lsqIdxStart.sqIdx.first_flag := true.B
      renamed(i).bits.lsqIdxEnd.sqIdx.first_flag := true.B
      renamed(i).bits.lsqIdxEnd.sqIdx.value := 1.U
      renamed(i).bits.teaPsv := Mux(io.storeMask(i), TeaEvent.bit(TeaEvent.DR_L1), TeaPsvOps.empty)
      dispatch.io.renameIn(i).valid := renamed(i).valid
      dispatch.io.renameIn(i).bits := 0.U.asTypeOf(dispatch.io.renameIn(i).bits)
      dispatch.io.renameIn(i).bits.fuType := renamed(i).bits.fuType
    }
    PipeGroupConnect(renamed, dispatch.io.fromRename, io.flush, dispatch.io.toRenameAllFire)
    dispatch.io.enqRob.canAccept := true.B
    dispatch.io.enqRob.canAcceptForDispatch := true.B
    dispatch.io.enqRob.isEmpty := io.robEmpty
    dispatch.io.enqRob.resp := 0.U.asTypeOf(dispatch.io.enqRob.resp)
    dispatch.io.IQValidNumVec := 0.U.asTypeOf(dispatch.io.IQValidNumVec)
    dispatch.io.toIssueQueues.foreach(_.ready := true.B)
    dispatch.io.wbPregsInt := 0.U.asTypeOf(dispatch.io.wbPregsInt)
    dispatch.io.wbPregsFp := 0.U.asTypeOf(dispatch.io.wbPregsFp)
    dispatch.io.wbPregsV0 := 0.U.asTypeOf(dispatch.io.wbPregsV0)
    dispatch.io.wbPregsVl := 0.U.asTypeOf(dispatch.io.wbPregsVl)
    dispatch.io.wakeUpAll := 0.U.asTypeOf(dispatch.io.wakeUpAll)
    dispatch.io.wakeUpFp := 0.U.asTypeOf(dispatch.io.wakeUpFp)
    dispatch.io.wakeUpVec := 0.U.asTypeOf(dispatch.io.wakeUpVec)
    dispatch.io.og0Cancel := 0.U.asTypeOf(dispatch.io.og0Cancel)
    dispatch.io.ldCancel := 0.U.asTypeOf(dispatch.io.ldCancel)
    dispatch.io.vlWriteBackInfo := 0.U.asTypeOf(dispatch.io.vlWriteBackInfo)
    dispatch.io.fromLsqEnqCtrl := 0.U.asTypeOf(dispatch.io.fromLsqEnqCtrl)
    dispatch.io.fromLsqEnqCtrl.lsqHeadPtr.sqIdx.value := Mux(io.sqHasRoom, 2.U, 0.U)
    dispatch.io.toLsqEnqCtrl.lsqEnqIO.canAccept := true.B
    dispatch.io.toLsqEnqCtrl.lsqEnqIO.recoverStall := false.B
    dispatch.io.toLsqEnqCtrl.lsqEnqIO.resp := 0.U.asTypeOf(dispatch.io.toLsqEnqCtrl.lsqEnqIO.resp)
    dispatch.io.redirect := 0.U.asTypeOf(dispatch.io.redirect)
    dispatch.io.redirect.valid := io.flush
    dispatch.io.singleStep := false.B
    dispatch.io.lfst.resp := 0.U.asTypeOf(dispatch.io.lfst.resp)
    dispatch.io.stallReason.reason := 0.U.asTypeOf(dispatch.io.stallReason.reason)
    dispatch.io.debugBlockBackward.foreach(_ := false.B)
    dispatch.io.debugWaitForward.foreach(_ := false.B)
    dispatch.io.debugIQValidNumVec.foreach(x => x := 0.U.asTypeOf(x))
    dispatch.io.debugIQEnqHasIssuedVec.foreach(x => x := 0.U.asTypeOf(x))
    dispatch.io.debugRobHeadStall.foreach(x => x := 0.U.asTypeOf(x))
    dispatch.io.debugRobTrueCommit.foreach(_ := 0.U)
    io.inReady := renamed.head.ready
    io.dispatchValid := VecInit(dispatch.io.fromRename.map(_.valid))
    io.enqValid := VecInit(dispatch.io.enqRob.req.map(_.valid))
    io.enqPsv := VecInit(dispatch.io.enqRob.req.map(_.bits.teaPsv))
  }

  class TeaStateHarness extends Module {
    val io = IO(new Bundle {
      val empty = Input(Bool())
      val walking = Input(Bool())
      val hasCommit = Input(Bool())
      val flushCauseValid = Input(Bool())
      val state = Output(UInt(4.W))
    })
    io.state := TeaState.classify(io.empty, io.walking, io.hasCommit, io.flushCauseValid)
  }

  private def initSelector(dut: TeaSampleSelector): Unit = {
    dut.io.enable.poke(true.B)
    dut.io.samplePeriod.poke(1.U)
    dut.io.commitMask.poke(0.U)
    dut.io.commitPc.foreach(_.poke(0.U))
    dut.io.commitPsv.foreach(_.poke(0.U))
    dut.io.stalledTarget.valid.poke(false.B)
    dut.io.flushCause.valid.poke(false.B)
  }

  it should "preserve every stalled boundary and expand all architectural commit members" in {
    simulate(new TeaSampleSelector) { dut =>
      initSelector(dut)
      dut.io.commitPc.length shouldBe config(XSCoreParamsKey).RenameWidth * config(XSCoreParamsKey).CommitWidth
      dut.io.state.poke(TeaState.stalled)
      dut.io.stalledTarget.valid.poke(true.B)
      dut.io.stalledTarget.bits.allocationCycle.poke(17.U)
      dut.io.stalledTarget.bits.robIndex.poke(2.U)
      dut.io.stalledTarget.bits.rank.poke(1.U)
      for (cycle <- 0 until 100) {
        dut.io.sampleValid.expect(true.B)
        dut.io.sample.cycle.expect(cycle.U)
        dut.io.sample.state.expect(TeaState.stalled)
        dut.io.sample.targetValid.expect(true.B)
        dut.io.sample.target.rank.expect(1.U)
        dut.io.sample.target.allocationCycle.expect(17.U)
        dut.clock.step()
      }
      dut.io.state.poke(TeaState.computing)
      dut.io.commitMask.poke(3.U)
      dut.io.commitPc(0).poke("h80001000".U)
      dut.io.commitPc(1).poke("h80001800".U)
      dut.io.commitPsv(1).poke(TeaEvent.bit(TeaEvent.ST_L1))
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.cycle.expect(100.U)
      dut.io.sample.state.expect(TeaState.computing)
      dut.io.sample.validMask(0).expect(3.U)
      dut.io.sample.pcVec(0).expect("h80001000".U)
      dut.io.sample.pcVec(1).expect("h80001800".U)
      dut.io.sample.psvVec(1).expect(TeaEvent.bit(TeaEvent.ST_L1))
    }
  }

  it should "keep wall-clock sampling cadence through long stalls" in {
    simulate(new TeaSampleSelector) { dut =>
      initSelector(dut)
      dut.io.samplePeriod.poke(3.U)
      dut.io.state.poke(TeaState.stalled)
      for (cycle <- 0 until 12) {
        dut.io.sampleValid.expect((cycle % 3 == 0).B)
        dut.io.sample.cycle.expect(cycle.U)
        dut.clock.step()
      }
    }
  }

  it should "resolve drained samples to the first valid successor after sampling is disabled" in {
    simulate(new TeaSampleSelector) { dut =>
      initSelector(dut)
      dut.io.state.poke(TeaState.drained)
      for (cycle <- 0 until 5) {
        dut.io.sampleValid.expect(true.B)
        dut.io.sample.cycle.expect(cycle.U)
        dut.io.drainFinal.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.enable.poke(false.B)
      dut.io.state.poke(TeaState.computing)
      dut.io.commitMask.poke(8.U)
      dut.io.commitPc(3).poke("h80002100".U)
      dut.io.commitPsv(3).poke(TeaEvent.bit(TeaEvent.DR_L1))
      dut.io.sampleValid.expect(false.B)
      dut.io.drainFinal.valid.expect(true.B)
      dut.io.drainFinal.bits.cycle.expect(5.U)
      dut.io.drainFinal.bits.member.pc.expect("h80002100".U)
      dut.io.drainFinal.bits.member.psv.expect(TeaEvent.bit(TeaEvent.DR_L1))
      dut.clock.step()
      dut.io.drainFinal.valid.expect(false.B)
    }
  }

  it should "retain a flush cause across all recovery boundaries" in {
    simulate(new TeaSampleSelector) { dut =>
      initSelector(dut)
      dut.io.state.poke(TeaState.flushed)
      dut.io.flushCause.valid.poke(true.B)
      dut.io.flushCause.pc.poke("h80003000".U)
      dut.io.flushCause.psv.poke(TeaEvent.bit(TeaEvent.FL_EX))
      for (_ <- 0 until 5) {
        dut.io.sampleValid.expect(true.B)
        dut.io.sample.validMask(0).expect(1.U)
        dut.io.sample.pcVec(0).expect("h80003000".U)
        dut.io.sample.psvVec(0).expect(TeaEvent.bit(TeaEvent.FL_EX))
        dut.clock.step()
      }
      dut.io.flushCause.valid.poke(false.B)
      dut.io.sampleValid.expect(true.B)
      dut.io.sample.validMask.foreach(_.expect(0.U))
    }
  }

  it should "classify an empty ROB with a remembered flush cause as flushed" in {
    simulate(new TeaStateHarness) { dut =>
      dut.io.empty.poke(true.B)
      dut.io.walking.poke(false.B)
      dut.io.hasCommit.poke(false.B)
      dut.io.flushCauseValid.poke(true.B)
      dut.io.state.expect(TeaState.flushed)

      dut.io.flushCauseValid.poke(false.B)
      dut.io.state.expect(TeaState.drained)

      dut.io.empty.poke(false.B)
      dut.io.walking.poke(true.B)
      dut.io.state.expect(TeaState.flushed)

      dut.io.walking.poke(false.B)
      dut.io.hasCommit.poke(true.B)
      dut.io.flushCauseValid.poke(true.B)
      dut.io.state.expect(TeaState.computing)

      dut.io.hasCommit.poke(false.B)
      dut.io.state.expect(TeaState.stalled)
    }
  }

  it should "define the active TEA event space" in {
    TeaEvent.width shouldBe 8
    TeaEvent.bit(TeaEvent.ST_TLB).getWidth shouldBe TeaEvent.width
  }

  it should "propagate teaPsv through the decode, rename, and enqueue bundles" in {
    val ctrlTea = teaBitValue(TeaEvent.DR_L1) | teaBitValue(TeaEvent.FL_MB)

    simulate(new TeaPropagationSmoke(config)) { dut =>
      dut.clock.step()
      dut.io.decodeInTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.decodeOutTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.renameTea.expect(ctrlTea.U(TeaEvent.width.W))
      dut.io.enqRobTea.expect(ctrlTea.U(TeaEvent.width.W))
    }
  }

  it should "bind a packet PSV only to the first valid instruction slot" in {
    simulate(new PacketPsvHarness) { dut =>
      dut.io.valids(0).poke(false.B)
      dut.io.valids(1).poke(true.B)
      dut.io.valids(2).poke(true.B)
      dut.io.valids(3).poke(false.B)
      dut.io.packetPsv.poke(TeaEvent.bit(TeaEvent.DR_L1))
      dut.clock.step()
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(TeaEvent.bit(TeaEvent.DR_L1))
      dut.io.out(2).expect(0.U)
      dut.io.out(3).expect(0.U)
    }
  }

  it should "keep DR_L1 on the earliest valid instruction even when that instruction bypasses IBuffer" in {
    val ibufferConfig = config.alterPartial {
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
    }

    simulate(new IBufferBypassRegressionHarness()(ibufferConfig)) { dut =>
      val predictWidth = dut.io.instrs.length
      val decodeWidth = dut.io.outInstr.length
      val fullMask = ((BigInt(1) << predictWidth) - 1).U
      val queuedInstr = (0x1000 + decodeWidth).U(32.W)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.decodeCanAccept.poke(true.B)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      for (i <- 0 until predictWidth) {
        dut.io.instrs(i).poke((0x1000 + i).U(32.W))
      }
      dut.clock.step()

      dut.io.inValid.poke(true.B)
      dut.io.validMask.poke(fullMask)
      dut.io.enqEnableMask.poke(fullMask)
      dut.io.packetDrL1.poke(true.B)
      dut.io.packetDrTlb.poke(false.B)
      dut.io.inReady.expect(true.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(0x1000.U)
      dut.io.outTeaPsv(0).expect(TeaEvent.bit(TeaEvent.DR_L1))
      dut.io.outInstr(decodeWidth - 1).expect((0x1000 + decodeWidth - 1).U(32.W))
      dut.io.outTeaPsv(decodeWidth - 1).expect(0.U)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(queuedInstr)
      dut.io.outTeaPsv(0).expect(0.U)
    }
  }

  it should "bind DR_TLB to the earliest valid instruction when the fetch packet reports an ITLB miss" in {
    val ibufferConfig = config.alterPartial {
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
    }

    simulate(new IBufferBypassRegressionHarness()(ibufferConfig)) { dut =>
      val predictWidth = dut.io.instrs.length
      val fullMask = ((BigInt(1) << predictWidth) - 1).U

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.decodeCanAccept.poke(true.B)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      for (i <- 0 until predictWidth) {
        dut.io.instrs(i).poke((0x2000 + i).U(32.W))
      }
      dut.clock.step()

      dut.io.inValid.poke(true.B)
      dut.io.validMask.poke(fullMask)
      dut.io.enqEnableMask.poke(fullMask)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(true.B)
      dut.io.inReady.expect(true.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outInstr(0).expect(0x2000.U)
      dut.io.outTeaPsv(0).expect(TeaEvent.bit(TeaEvent.DR_TLB))
      dut.io.outTeaPsv(1).expect(0.U)
    }
  }

  it should "bind both front-end drain bits when icache and itlb miss reasons coexist in one packet" in {
    val ibufferConfig = config.alterPartial {
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
    }

    simulate(new IBufferBypassRegressionHarness()(ibufferConfig)) { dut =>
      val predictWidth = dut.io.instrs.length
      val fullMask = ((BigInt(1) << predictWidth) - 1).U
      val combinedDrain = teaBitValue(TeaEvent.DR_L1) | teaBitValue(TeaEvent.DR_TLB)

      dut.io.inValid.poke(false.B)
      dut.io.validMask.poke(0.U)
      dut.io.enqEnableMask.poke(0.U)
      dut.io.decodeCanAccept.poke(true.B)
      dut.io.packetDrL1.poke(false.B)
      dut.io.packetDrTlb.poke(false.B)
      for (i <- 0 until predictWidth) {
        dut.io.instrs(i).poke((0x3000 + i).U(32.W))
      }
      dut.clock.step()

      dut.io.inValid.poke(true.B)
      dut.io.validMask.poke(fullMask)
      dut.io.enqEnableMask.poke(fullMask)
      dut.io.packetDrL1.poke(true.B)
      dut.io.packetDrTlb.poke(true.B)
      dut.io.inReady.expect(true.B)
      dut.clock.step()

      dut.io.outValid(0).expect(true.B)
      dut.io.outTeaPsv(0).expect(combinedDrain.U(TeaEvent.width.W))
      dut.io.outTeaPsv(1).expect(0.U)
    }
  }

  private val moduleTestConfig = config.alterPartial {
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false,
      perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
  }

  it should "retain DR_SQ on the actual store after partial dispatch drains the ROB" in {
    val coreParams = moduleTestConfig(XSCoreParamsKey)
    val dispatchConfig = moduleTestConfig.alterPartial { case XSCoreParamsKey => coreParams }
    simulate(new TeaDispatchHarness()(dispatchConfig)) { dut =>
      dut.io.validMask.poke(0.U)
      dut.io.storeMask.poke(2.U)
      dut.io.robEmpty.poke(false.B)
      dut.io.sqHasRoom.poke(false.B)
      dut.io.flush.poke(false.B)
      dut.clock.step(3)

      dut.io.inReady.expect(true.B)
      dut.io.validMask.poke(3.U) // ALU in lane 0, store in lane 1.
      dut.clock.step()
      dut.io.validMask.poke(0.U)
      dut.io.enqValid(0).expect(true.B)
      dut.io.enqPsv(0).expect(0.U)
      dut.io.enqValid(1).expect(false.B)
      dut.clock.step()
      dut.io.dispatchValid(0).expect(false.B)
      dut.io.dispatchValid(1).expect(true.B)

      dut.io.robEmpty.poke(true.B)
      dut.clock.step(2)
      dut.io.sqHasRoom.poke(true.B)
      dut.io.enqValid(1).expect(true.B)
      dut.io.enqPsv(1).expect(5.U) // Preserve DR_L1 and add DR_SQ.
      dut.clock.step()
      dut.io.dispatchValid(1).expect(false.B)

      // Consuming the delayed store must not tag the next, unblocked store.
      dut.io.storeMask.poke(1.U)
      dut.io.validMask.poke(1.U)
      dut.clock.step()
      dut.io.validMask.poke(0.U)
      dut.io.enqValid(0).expect(true.B)
      dut.io.enqPsv(0).expect(1.U)
      dut.clock.step()

      // SQ pressure while the ROB remains nonempty is not a drain event.
      dut.io.robEmpty.poke(false.B)
      dut.io.sqHasRoom.poke(false.B)
      dut.io.storeMask.poke(2.U)
      dut.io.validMask.poke(2.U)
      dut.clock.step()
      dut.io.validMask.poke(0.U)
      dut.clock.step(2)
      dut.io.sqHasRoom.poke(true.B)
      dut.io.enqValid(1).expect(true.B)
      dut.io.enqPsv(1).expect(1.U)
      dut.clock.step()

      // The original lane-0 case still carries the drain event.
      dut.io.robEmpty.poke(true.B)
      dut.io.sqHasRoom.poke(false.B)
      dut.io.storeMask.poke(1.U)
      dut.io.validMask.poke(1.U)
      dut.clock.step()
      dut.io.validMask.poke(0.U)
      dut.clock.step(2)
      dut.io.sqHasRoom.poke(true.B)
      dut.io.enqValid(0).expect(true.B)
      dut.io.enqPsv(0).expect(5.U)
      dut.clock.step()

      // Redirect discards a blocked store's pending event before a new group.
      dut.io.sqHasRoom.poke(false.B)
      dut.io.storeMask.poke(2.U)
      dut.io.validMask.poke(2.U)
      dut.clock.step()
      dut.io.validMask.poke(0.U)
      dut.clock.step(2)
      dut.io.flush.poke(true.B)
      dut.clock.step()
      dut.io.dispatchValid(1).expect(false.B)
      dut.io.flush.poke(false.B)
      dut.io.sqHasRoom.poke(true.B)
      dut.io.storeMask.poke(1.U)
      dut.io.validMask.poke(1.U)
      dut.clock.step()
      dut.io.validMask.poke(0.U)
      dut.io.enqValid(0).expect(true.B)
      dut.io.enqPsv(0).expect(1.U)
      dut.clock.step()
    }
  }

  it should "capture actual TLB misses on replay without attributing invalid or killed requests" in {
    simulate(new TeaTlbMissHarness()(moduleTestConfig)) { dut =>
      dut.io.valid.poke(true.B)
      dut.io.miss.poke(true.B)
      dut.io.replay.poke(false.B)
      dut.io.prefetch.poke(false.B)
      dut.io.kill.poke(false.B)
      dut.io.loadMiss.expect(true.B)
      dut.io.loadFirstMiss.expect(true.B)
      dut.io.storeMiss.expect(true.B)
      dut.io.storeFirstMiss.expect(true.B)

      dut.io.replay.poke(true.B)
      dut.io.loadMiss.expect(true.B)
      dut.io.loadFirstMiss.expect(false.B)
      dut.io.storeMiss.expect(true.B)
      dut.io.storeFirstMiss.expect(false.B)

      dut.io.prefetch.poke(true.B)
      dut.io.loadMiss.expect(false.B)
      dut.io.storeMiss.expect(false.B)
      dut.io.prefetch.poke(false.B)
      dut.io.valid.poke(false.B)
      dut.io.loadMiss.expect(false.B)
      dut.io.storeMiss.expect(false.B)
      dut.io.valid.poke(true.B)
      dut.io.kill.poke(true.B)
      dut.io.loadMiss.expect(false.B)
      dut.io.storeMiss.expect(false.B)
      dut.io.kill.poke(false.B)
      dut.io.miss.poke(false.B)
      dut.io.loadMiss.expect(false.B)
      dut.io.storeMiss.expect(false.B)
    }
  }
}
