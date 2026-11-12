// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package cache

import chisel3._
import chisel3.experimental.UnlocatableSourceInfo
import chisel3.reflect.DataMirror
import chisel3.simulator.EphemeralSimulator._
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import utility._
import xscache.chi.{CHIAddrWidthKey, CHIIssue}
import xscache.common.BankBitsKey
import xscache.coupledL2._

class SDBPMetadataTest extends AnyFlatSpec {
  private def parameters: Parameters = {
    val clients = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      name = "dcache", sourceId = IdRange(0, 4), supportsProbe = TransferSizes(64))),
      requestFields = Seq(PCField(48), PCValidField(), ReqSourceField()))
    val managers = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, (BigInt(1) << 48) - 1)), regionType = RegionType.TRACKED,
      supportsAcquireB = TransferSizes(64), supportsAcquireT = TransferSizes(64),
      supportsGet = TransferSizes(1, 64))), beatBytes = 32, endSinkId = 4, requestKeys = Seq(PCKey, PCValidKey, ReqSourceKey))
    val base = Parameters.empty.alterPartial {
      case L2ParamKey => L2Param(sets = 8, ways = 4, sdbp = SDBPParams(samplerSets = 2),
        clientCaches = Seq(L1Param(pcBitOpt = Some(48))), enablePerf = false,
        enableMonitor = false, enableRollingDB = false, elaboratedTopDown = false)
      case BankBitsKey => 0
      case SliceIdKey => 0
      case MaxHartIdBits => 1
      case CHIAddrWidthKey => 48
      case CHIIssue => "E.b"
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    }
    base.alterPartial {
      case EdgeInKey => new TLEdgeIn(clients, managers, base, UnlocatableSourceInfo)
      case EdgeOutKey => new TLEdgeOut(clients, managers, base, UnlocatableSourceInfo)
    }
  }
  private def zeroInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zeroInputs)
    case vector: Vec[_] => vector.foreach(zeroInputs)
    case bit: Bool if DataMirror.directionOf(bit) == ActualDirection.Input => bit.poke(false.B)
    case uint: UInt if DataMirror.directionOf(uint) == ActualDirection.Input => uint.poke(0.U)
    case _ =>
  }
  behavior of "SDBP MSHR metadata"
  it should "preserve normal refill PCs and replace a prefetch's metadata with its merged demand" in {
    implicit val p: Parameters = parameters
    simulate(new MSHR) { dut =>
      zeroInputs(dut.io)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      def allocate(prefetch: Boolean, pc: BigInt, signature: Int): Unit = {
        dut.io.alloc.bits.state.elements.values.foreach(_.asInstanceOf[Bool].poke(true.B))
        dut.io.alloc.bits.state.s_refill.poke(false.B)
        dut.io.alloc.bits.task.channel.poke(1.U)
        dut.io.alloc.bits.task.opcode.poke(if (prefetch) TLMessages.Hint else TLMessages.AcquireBlock)
        dut.io.alloc.bits.task.param.poke(0.U)
        dut.io.alloc.bits.task.pc.get.poke(pc.U)
        dut.io.alloc.bits.task.pcValid.poke((!prefetch).B)
        dut.io.alloc.bits.task.signature.poke(signature.U)
        dut.io.alloc.bits.task.sdbpPrefetch.poke(prefetch.B)
        for (table <- 0 until 3) dut.io.alloc.bits.task.indices(table).poke((11 + table).U)
        dut.io.alloc.valid.poke(true.B); dut.clock.step(); dut.io.alloc.valid.poke(false.B)
      }
      allocate(prefetch = false, pc = 0, signature = 17)
      dut.io.tasks.mainpipe.valid.expect(true.B)
      dut.io.tasks.mainpipe.bits.pc.get.expect(0.U)
      dut.io.tasks.mainpipe.bits.pcValid.expect(true.B)
      dut.io.tasks.mainpipe.bits.signature.expect(17.U)
      dut.io.tasks.mainpipe.bits.sdbpPrefetch.expect(false.B)
      for (table <- 0 until 3) dut.io.tasks.mainpipe.bits.indices(table).expect((11 + table).U)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      zeroInputs(dut.io)
      allocate(prefetch = true, pc = 0, signature = 0)
      dut.io.tasks.mainpipe.bits.pcValid.expect(false.B)
      dut.io.tasks.mainpipe.bits.sdbpPrefetch.expect(true.B)
      dut.io.aMergeTask.bits.channel.poke(1.U)
      dut.io.aMergeTask.bits.opcode.poke(TLMessages.AcquireBlock)
      dut.io.aMergeTask.bits.pc.get.poke("h81234567".U)
      dut.io.aMergeTask.bits.pcValid.poke(true.B)
      dut.io.aMergeTask.bits.signature.poke(23.U)
      for (table <- 0 until 3) dut.io.aMergeTask.bits.indices(table).poke((31 + table).U)
      dut.io.aMergeTask.bits.reqSource.poke(MemReqSource.CPULoadData.id.U)
      dut.io.aMergeTask.valid.poke(true.B); dut.clock.step(); dut.io.aMergeTask.valid.poke(false.B)
      dut.io.tasks.mainpipe.valid.expect(true.B)
      dut.io.tasks.mainpipe.bits.mergeA.expect(true.B)
      dut.io.tasks.mainpipe.bits.pc.get.expect("h81234567".U)
      dut.io.tasks.mainpipe.bits.pcValid.expect(true.B)
      dut.io.tasks.mainpipe.bits.signature.expect(23.U)
      dut.io.tasks.mainpipe.bits.sdbpPrefetch.expect(false.B)
      dut.io.tasks.mainpipe.bits.reqSource.expect(MemReqSource.CPULoadData.id.U)
      for (table <- 0 until 3) dut.io.tasks.mainpipe.bits.indices(table).expect((31 + table).U)
    }
  }
  behavior of "SDBP Directory victim selection"
  it should "respect invalid priority, I/D copies, busy masks and true LRU without bypass" in {
    val base = parameters
    implicit val p: Parameters = base.alterPartial {
      case L2ParamKey => base(L2ParamKey).copy(replacement = "sdbp",
        sdbp = SDBPParams(samplerSets = 2, samplerWays = Some(1), predictorEntries = 64))
    }
    val reference = scala.io.Source.fromResource("sdbp-root-hash-vectors.csv")
    val indices = try reference.getLines().next().split(',').drop(2).map(_.toInt & 63) finally reference.close()
    simulate(new Directory) { dut =>
      zeroInputs(dut.io)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B); dut.clock.step(16)
      def meta(way: Int, clients: Int, invalid: Boolean = false): Unit = {
        dut.io.metaWReq.valid.poke(true.B)
        dut.io.metaWReq.bits.set.poke(0.U); dut.io.metaWReq.bits.wayOH.poke((1 << way).U)
        dut.io.metaWReq.bits.wmeta.state.poke(if (invalid) MetaData.INVALID else MetaData.TIP)
        dut.io.metaWReq.bits.wmeta.clients.poke(clients.U)
        dut.clock.step(); dut.io.metaWReq.valid.poke(false.B)
      }
      for (way <- 0 until 4) {
        dut.io.tagWReq.valid.poke(true.B)
        dut.io.tagWReq.bits.set.poke(0.U); dut.io.tagWReq.bits.wayOH.poke((1 << way).U)
        dut.io.tagWReq.bits.wtag.poke((10 + way).U)
        meta(way, 0)
        dut.io.tagWReq.valid.poke(false.B)
      }
      def lookup(tag: Int, refill: Boolean, mask: Int = 15, commitHit: Boolean = false): Int = {
        dut.io.read.bits.set.poke(0.U); dut.io.read.bits.tag.poke(tag.U)
        dut.io.read.bits.refill.poke(refill.B); dut.io.read.bits.wayMask.poke(mask.U)
        dut.io.read.bits.replacerInfo.channel.poke(1.U)
        dut.io.read.bits.replacerInfo.opcode.poke(if (refill) TLMessages.GrantData else TLMessages.Get)
        dut.io.read.bits.replacerInfo.pcValid.poke(true.B)
        for (table <- 0 until 3) dut.io.read.bits.replacerInfo.indices(table).poke(indices(table).U)
        var waited = 0
        while (!dut.io.read.ready.peek().litToBoolean) { dut.clock.step(); waited += 1; assert(waited < 100) }
        dut.io.read.valid.poke(true.B); dut.clock.step()
        dut.io.read.valid.poke(false.B); dut.clock.step()
        val way = dut.io.resp.bits.way.peek().litValue.toInt
        if (commitHit) {
          dut.io.resp.bits.hit.expect(true.B)
          dut.io.sdbpCommit.valid.poke(true.B)
          dut.io.sdbpCommit.bits.set.poke(0.U); dut.io.sdbpCommit.bits.way.poke(way.U)
          dut.io.sdbpCommit.bits.refill.poke(false.B)
        }
        if (refill) dut.io.replResp.bits.retry.expect((mask == 0).B)
        dut.clock.step(); dut.io.sdbpCommit.valid.poke(false.B)
        // MainPipe uses this victim throughout DataStorage's two-cycle request.
        // A busy-way mask must remain effective after refillReqValid_s3 drops.
        if (refill && mask != 0) {
          dut.io.replResp.bits.validHold.expect(true.B)
          dut.io.replResp.bits.way.expect(way.U)
        }
        dut.clock.step(3)
        way
      }
      // One sampler way: three different-tag reuses train signature zero dead.
      for (way <- 0 until 4) assert(lookup(10 + way, refill = false, commitHit = true) == way)
      // These hits write dead bits on ways 0/1 while making both younger than way 2.
      assert(lookup(10, refill = false, commitHit = true) == 0)
      assert(lookup(11, refill = false, commitHit = true) == 1)
      assert(lookup(99, refill = true) == 0) // First dead, not global LRU.
      dut.io.l1ICopy.poke(1.U); meta(1, clients = 1)
      assert(lookup(99, refill = true) == 2) // Every dead has an L1 copy: full LRU fallback.
      dut.io.l1ICopy.poke(0.U)
      assert(lookup(99, refill = true) == 0) // Protection never erased its dead bit.
      dut.io.l1ICopy.poke(1.U); meta(1, clients = 0)
      assert(lookup(99, refill = true) == 1)
      meta(2, clients = 0, invalid = true)
      assert(lookup(99, refill = true) == 2) // Available invalid wins over dead.
      meta(1, clients = 1)
      assert(lookup(99, refill = true, mask = 11) == 3) // Invalid way 2 is not eligible.
      dut.io.msInfo(0).valid.poke(true.B)
      dut.io.msInfo(0).bits.set.poke(0.U); dut.io.msInfo(0).bits.way.poke(2.U)
      dut.io.msInfo(0).bits.blockRefill.poke(true.B)
      assert(lookup(99, refill = true) == 0) // Busy invalid skipped; LRU may evict an L1-resident way.
      dut.io.msInfo(0).valid.poke(false.B)
      lookup(99, refill = true, mask = 0) // Retry only when no way is eligible.
      // CMO opcodes must retain their fourth bit; truncating 12/13/14 aliases
      // Get/Hint/Acquire and incorrectly changes replacement recency.
      for (opcode <- Seq(TLMessages.CBOClean, TLMessages.CBOFlush, TLMessages.CBOInval)) {
        dut.io.read.bits.set.poke(0.U); dut.io.read.bits.tag.poke(10.U)
        dut.io.read.bits.refill.poke(false.B); dut.io.read.bits.wayMask.poke(15.U)
        dut.io.read.bits.replacerInfo.opcode.poke(opcode)
        while (!dut.io.read.ready.peek().litToBoolean) dut.clock.step()
        dut.io.read.valid.poke(true.B); dut.clock.step()
        dut.io.read.valid.poke(false.B); dut.clock.step()
        dut.io.read.ready.expect(true.B) // No replacement write on CMO hit.
        dut.clock.step(2)
      }
    }
  }

  behavior of "SDBP request classification"
  it should "distinguish L1 prefetch Acquires, missing PCs and PC zero" in {
    implicit val p: Parameters = parameters
    simulate(new SinkA) { dut =>
      zeroInputs(dut.io)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      dut.io.a.valid.poke(true.B)
      dut.io.a.bits.opcode.poke(TLMessages.AcquireBlock)
      dut.io.a.bits.user(PCKey).poke(0.U)
      dut.io.a.bits.user(PCValidKey).poke(true.B)
      dut.io.a.bits.user(ReqSourceKey).poke(MemReqSource.CPULoadData.id.U)
      dut.io.task.bits.pcValid.expect(true.B)
      dut.io.task.bits.signature.expect(0.U)
      dut.io.task.bits.sdbpPrefetch.expect(false.B)
      dut.io.a.bits.user(PCKey).poke(((BigInt(1) << 48) - 1).U)
      dut.io.task.bits.signature.expect(7.U) // ROOT signature of canonical RV64 PC -1.
      dut.io.a.bits.user(PCKey).poke(0.U)
      for (source <- Seq(MemReqSource.L1DataPrefetch, MemReqSource.L1InstPrefetch)) {
        dut.io.a.bits.user(ReqSourceKey).poke(source.id.U)
        dut.io.task.bits.sdbpPrefetch.expect(true.B)
        dut.io.task.bits.pcValid.expect(false.B)
      }
      dut.io.a.bits.opcode.poke(TLMessages.Hint)
      dut.io.a.bits.user(ReqSourceKey).poke(MemReqSource.NoWhere.id.U)
      dut.io.task.bits.sdbpPrefetch.expect(true.B)
      dut.io.task.bits.pcValid.expect(false.B)
      dut.io.a.bits.opcode.poke(TLMessages.Get)
      dut.io.a.bits.user(ReqSourceKey).poke(MemReqSource.PTW.id.U)
      dut.io.a.bits.user(PCValidKey).poke(false.B)
      dut.io.task.bits.sdbpPrefetch.expect(false.B)
      dut.io.task.bits.pcValid.expect(false.B)
    }
  }

  behavior of "SDBP RequestArb spacing"
  it should "space sampled Hints and propagate Directory backpressure to the actual s1 fire" in {
    val base = parameters
    implicit val p: Parameters = base.alterPartial {
      case L2ParamKey => base(L2ParamKey).copy(replacement = "sdbp")
    }
    simulate(new RequestArb) { dut =>
      zeroInputs(dut.io)
      dut.io.dirRead_s1.ready.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B); dut.clock.step(12)
      dut.io.sinkA.valid.poke(true.B)
      dut.io.sinkA.bits.channel.poke(1.U)
      dut.io.sinkA.bits.opcode.poke(TLMessages.Hint)
      dut.io.sinkA.bits.sdbpPrefetch.poke(true.B)
      dut.io.sinkA.bits.set.poke(0.U)
      dut.io.sinkA.ready.expect(true.B); dut.clock.step()
      dut.io.sinkA.ready.expect(false.B)
      dut.io.sinkA.valid.poke(false.B); dut.clock.step()
      dut.io.sinkA.valid.poke(true.B)
      dut.io.sinkA.bits.set.poke(1.U) // Unsampled Hint retains the original entrance behavior.
      dut.io.sinkA.ready.expect(true.B); dut.clock.step()
      dut.io.sinkA.ready.expect(true.B)
      dut.io.sinkA.bits.opcode.poke(TLMessages.Get)
      dut.io.sinkA.bits.sdbpPrefetch.poke(false.B)
      dut.clock.step(); dut.io.sinkA.ready.expect(false.B)
      dut.io.sinkA.valid.poke(false.B); dut.clock.step()
      dut.io.sinkA.valid.poke(true.B); dut.io.dirRead_s1.ready.poke(false.B)
      dut.io.sinkA.ready.expect(false.B); dut.clock.step()
      dut.io.taskToPipe_s2.valid.expect(false.B)
    }
  }

  behavior of "SDBP MainPipe commit qualification"
  it should "commit initial hits and successful refills once, excluding misses, retry, denied and permission completion" in {
    implicit val p: Parameters = parameters
    simulate(new MainPipe) { dut =>
      zeroInputs(dut.io)
      dut.io.toSourceD.ready.poke(true.B)
      dut.io.toTXREQ.ready.poke(true.B); dut.io.toTXRSP.ready.poke(true.B); dut.io.toTXDAT.ready.poke(true.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B); dut.clock.step(16)
      dut.io.dirResp_s3.meta.state.poke(MetaData.TIP)
      dut.io.metaOnHit_s3.state.poke(MetaData.TIP)
      dut.io.dirResp_s3.way.poke(1.U); dut.io.dirWayOH_s3.poke(2.U)
      dut.io.dirReplWayOH_s3.poke(4.U)
      def request(mshr: Boolean, hit: Boolean, tagWrite: Boolean, retry: Boolean = false,
        denied: Boolean = false, corrupt: Boolean = false, opcode: UInt = TLMessages.Get,
        expected: Boolean): Unit = {
        val task = dut.io.taskFromArb_s2.bits
        task.channel.poke(1.U); task.opcode.poke(opcode)
        task.mshrTask.poke(mshr.B); task.tagWen.poke(tagWrite.B)
        task.metaWen.poke(mshr.B); task.replTask.poke(tagWrite.B)
        task.denied.poke(denied.B); task.corrupt.poke(corrupt.B)
        dut.io.dirResp_s3.hit.poke(hit.B)
        dut.io.replResp.bits.validHold.poke(retry.B)
        dut.io.replResp.bits.retry.poke(retry.B)
        dut.io.taskFromArb_s2.valid.poke(true.B); dut.clock.step()
        dut.io.taskFromArb_s2.valid.poke(false.B)
        dut.io.sdbpCommit.valid.expect(expected.B)
        if (expected) {
          dut.io.sdbpCommit.bits.refill.expect(tagWrite.B)
          dut.io.sdbpCommit.bits.way.expect((if (tagWrite) 2 else 1).U)
        }
        dut.clock.step(5)
        dut.io.sdbpCommit.valid.expect(false.B)
      }
      request(mshr = false, hit = false, tagWrite = false, expected = false)
      request(mshr = false, hit = true, tagWrite = false, expected = true)
      request(mshr = false, hit = true, tagWrite = false, opcode = TLMessages.Hint, expected = true)
      request(mshr = true, hit = false, tagWrite = true, opcode = TLMessages.GrantData, expected = true)
      request(mshr = true, hit = false, tagWrite = true, retry = true, opcode = TLMessages.GrantData, expected = false)
      request(mshr = true, hit = false, tagWrite = true, denied = true, opcode = TLMessages.GrantData, expected = false)
      request(mshr = true, hit = false, tagWrite = true, corrupt = true, opcode = TLMessages.GrantData, expected = false)
      request(mshr = true, hit = true, tagWrite = false, opcode = TLMessages.Grant, expected = false)
      for (opcode <- Seq(TLMessages.CBOClean, TLMessages.CBOFlush, TLMessages.CBOInval))
        request(mshr = false, hit = true, tagWrite = false, opcode = opcode, expected = false)
    }
  }

}
