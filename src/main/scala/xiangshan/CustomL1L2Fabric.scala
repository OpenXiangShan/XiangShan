/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  ***************************************************************************************/

package xiangshan

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleField
import org.chipsalliance.cde.config.Parameters
import utility.{MemReqSource, ReqSourceKey}
import xscache.common.{AliasKey, PrefetchKey}
import xscache.coupledL2.{IsKeywordKey, MemBackTypeMM, MemPageTypeNC, PCKey, VaddrKey}
import xiangshan.cache.SpecMissFabricIO

/**
  * The private L1-to-L2 TileLink fabric.
  *
  * Unlike the old topology, arbitration and the downstream channel buffers live in one
  * module. There is deliberately no input buffer in front of the arbitration point.
  * In SpecMiss mode, tentative DCache Acquire requests share the input and xbar A
  * arbiters with ordinary requests, then wait in the post-arbitration A queue for S2.
  *
  * The nexus keeps all protocol duties of TLXbar (global source IDs, B/D return routing,
  * C/E arbitration and multi-beat locking). Each downstream port (one per L2 slice) has
  * its own buffers, placed after arbitration on A/C/E and before return routing on B/D,
  * matching a downstream TLBuffer while removing the former per-DCache-client TLBuffer.
  */
class CustomL1L2Fabric(
  policy: TLArbiter.Policy = TLArbiter.roundRobin,
  a: BufferParams = BufferParams.default,
  b: BufferParams = BufferParams.default,
  c: BufferParams = BufferParams.default,
  d: BufferParams = BufferParams.default,
  e: BufferParams = BufferParams.default,
  specChannels: Int = 0,
  specOwners: Int = 0,
  specIdBits: Int = 0,
  specBlockBytes: Int = 0
)(implicit p: Parameters) extends LazyModule {

  require(specChannels == 0 || (specOwners > 0 && specIdBits > 0 && specBlockBytes > 0 && a.depth >= 2))
  val specSink = Option.when(specChannels > 0)(
    BundleBridgeSink[SpecMissFabricIO](Some(() => new SpecMissFabricIO))
  )

  val node = new TLNexusNode(
    clientFn = { ports =>
      require(ports.nonEmpty, "CustomL1L2Fabric requires at least one upstream client")
      ports.head.v1copy(
        echoFields = BundleField.union(ports.flatMap(_.echoFields)),
        requestFields = BundleField.union(ports.flatMap(_.requestFields)),
        responseKeys = ports.flatMap(_.responseKeys).distinct,
        minLatency = ports.map(_.minLatency).min + b.latency + c.latency,
        clients = (TLXbar.mapInputIds(ports) zip ports).flatMap { case (range, port) =>
          port.clients.map { client =>
            client.v1copy(sourceId = client.sourceId.shift(range.start))
          }
        }
      )
    },
    managerFn = { ports =>
      require(ports.nonEmpty, "CustomL1L2Fabric requires at least one downstream manager port")
      // BankBinder partitions the L2 address space before it propagates through
      // CoupledL2. Routing is unicast, never broadcast to overlapping slice ports.
      val portAddresses = ports.map(_.managers.flatMap(_.address))
      for (i <- ports.indices; j <- 0 until i) {
        require(!portAddresses(i).exists(a => portAddresses(j).exists(a.overlaps)),
          s"CustomL1L2Fabric downstream ports $j and $i have overlapping address sets")
      }
      val fifoIdFactory = TLXbar.relabeler()
      ports.head.v1copy(
        responseFields = BundleField.union(ports.flatMap(_.responseFields)),
        requestKeys = ports.flatMap(_.requestKeys).distinct,
        minLatency = ports.map(_.minLatency).min + a.latency + d.latency,
        endSinkId = TLXbar.mapOutputIds(ports).map(_.end).max,
        managers = ports.flatMap { port =>
          require(port.beatBytes == ports.head.beatBytes,
            s"CustomL1L2Fabric data widths do not match: ${port.beatBytes}B vs ${ports.head.beatBytes}B")
          val fifoIdMapper = fifoIdFactory()
          port.managers.map { manager =>
            manager.v1copy(fifoId = manager.fifoId.map(fifoIdMapper))
          }
        }
      )
    }
  ) {
    // This fabric contains real channel queues and must never collapse to a wire.
    override def circuitIdentity: Boolean = false
  }

  lazy val module = new LazyModuleImp(this) {
    require(node.out.nonEmpty, "CustomL1L2Fabric requires at least one downstream connection")

    val arbitrationOutputs = node.out.map { case (downstream, downstreamEdge) =>
      (Wire(TLBundle(downstream.params)), downstreamEdge)
    }

    // Reuse the protocol-complete TileLink routing primitive, but do not instantiate a
    // TLXbar LazyModule. Buffering and the module boundary are owned by this fabric.
    // Pass every slice to the circuit so A/C use address routing and E uses the global
    // sink namespace. B/D are arbitrated back to each originating client by source ID.
    if (specChannels == 0) {
      TLXbar.circuit(policy, node.in, arbitrationOutputs)
      (node.out zip arbitrationOutputs).foreach { case ((downstream, _), (arbitrationOut, _)) =>
        downstream.a <> a(arbitrationOut.a)
        arbitrationOut.b <> b(downstream.b)
        downstream.c <> c(arbitrationOut.c)
        arbitrationOut.d <> d(downstream.d)
        downstream.e <> e(arbitrationOut.e)
      }
    } else {
      val spec = specSink.get.bundle
      val dcacheInputs = (0 until specChannels).map { ch =>
        val name = if (ch == 0) "dcache" else s"dcache_ch$ch"
        val index = node.in.indexWhere { case (_, edge) => edge.client.clients.exists(_.name == name) }
        require(index >= 0, s"SpecMiss DCache client $name is not connected to fabric")
        index
      }
      require(dcacheInputs.distinct.size == specChannels)

      val candidateFire = Wire(Vec(specOwners, Bool()))
      val candidateReady = Wire(Vec(specChannels, Vec(specOwners, Bool())))
      val candidateAccepted = Wire(Vec(specChannels, Vec(specOwners, Bool())))
      val xbarInputs = node.in.zipWithIndex.map { case ((input, edge), index) =>
        val ch = dcacheInputs.indexOf(index)
        if (ch < 0) {
          (input, edge)
        } else {
          val merged = Wire(TLBundle(input.params))
          val candidates = (0 until specOwners).map { w =>
            val request = spec.candidate(w)
            val candidate = Wire(Decoupled(new TLBundleA(input.params)))
            candidate.valid := request.valid && request.bits.channel === ch.U
            candidate.bits := 0.U.asTypeOf(candidate.bits)
            candidate.bits.opcode := TLMessages.AcquireBlock
            candidate.bits.param := request.bits.grow
            candidate.bits.size := log2Ceil(specBlockBytes).U
            candidate.bits.source := request.bits.id
            candidate.bits.address := request.bits.paddr
            candidate.bits.mask := Fill(input.params.dataBits / 8, 1.U(1.W))
            candidate.bits.user.lift(AliasKey).foreach(_ := request.bits.alias)
            candidate.bits.user.lift(VaddrKey).foreach(_ := request.bits.vaddr >> log2Ceil(specBlockBytes))
            candidate.bits.user.lift(PCKey).foreach(_ := request.bits.pc)
            candidate.bits.user.lift(PrefetchKey).foreach(_ := request.bits.prefetch)
            candidate.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.CPULoadData.id.U)
            candidate.bits.user.lift(MemBackTypeMM).foreach(_ := true.B)
            candidate.bits.user.lift(MemPageTypeNC).foreach(_ := false.B)
            candidate.bits.echo.lift(IsKeywordKey).foreach(_ := false.B)
            candidateReady(ch)(w) := candidate.ready
            candidateAccepted(ch)(w) := candidate.fire
            candidate
          }
          TLArbiter.robin(edge, merged.a, (Seq(input.a) ++ candidates):_*)
          input.b.valid := merged.b.valid
          input.b.bits := merged.b.bits
          merged.b.ready := input.b.ready
          merged.c.valid := input.c.valid
          merged.c.bits := input.c.bits
          input.c.ready := merged.c.ready
          input.d.valid := merged.d.valid
          input.d.bits := merged.d.bits
          merged.d.ready := input.d.ready
          merged.e.valid := input.e.valid
          merged.e.bits := input.e.bits
          input.e.ready := merged.e.ready
          (merged, edge)
        }
      }
      for (w <- 0 until specOwners) {
        val request = spec.candidate(w)
        when(request.valid) {
          assert(request.bits.channel < specChannels.U)
        }
        request.ready := Mux1H((0 until specChannels).map(ch =>
          (request.bits.channel === ch.U) -> candidateReady(ch)(w)))
        candidateFire(w) := VecInit((0 until specChannels).map(ch => candidateAccepted(ch)(w))).asUInt.orR
      }
      TLXbar.circuit(policy, xbarInputs, arbitrationOutputs)

      val committedBySlice = Wire(Vec(node.out.size, Vec(specOwners, Bool())))
      val matchedBySlice = Wire(Vec(node.out.size, Vec(specOwners, Bool())))
      (node.out zip arbitrationOutputs).zipWithIndex.foreach { case (((downstream, _), (arbitrationOut, _)), slice) =>
        val buffer = Module(new SpecMissABuffer(chiselTypeOf(arbitrationOut.a.bits), a.depth, specOwners, specIdBits))
        val winner = VecInit((0 until specOwners).map(w =>
          candidateFire(w) && arbitrationOut.a.bits.address === spec.candidate(w).bits.paddr))
        assert(PopCount(winner) <= 1.U, "multiple SpecMiss owners selected one A output")
        buffer.io.enq.valid := arbitrationOut.a.valid
        arbitrationOut.a.ready := buffer.io.enq.ready
        buffer.io.enq.bits.data := arbitrationOut.a.bits
        buffer.io.enq.bits.speculative := winner.asUInt.orR
        buffer.io.enq.bits.committed := !winner.asUInt.orR
        buffer.io.enq.bits.owner := Mux1H(winner, (0 until specOwners).map(_.U))
        buffer.io.enq.bits.id := Mux1H(winner, (0 until specOwners).map(w => spec.candidate(w).bits.id))
        for (w <- 0 until specOwners) {
          buffer.io.resolve(w) := spec.resolve(w)
          committedBySlice(slice)(w) := buffer.io.committed(w)
          matchedBySlice(slice)(w) := buffer.io.matched(w)
        }
        downstream.a <> buffer.io.deq
        arbitrationOut.b <> b(downstream.b)
        downstream.c <> c(arbitrationOut.c)
        arbitrationOut.d <> d(downstream.d)
        downstream.e <> e(arbitrationOut.e)
      }
      for (w <- 0 until specOwners) {
        val committed = VecInit(committedBySlice.map(_(w)))
        val matched = VecInit(matchedBySlice.map(_(w)))
        when(spec.resolve(w).valid) {
          assert(PopCount(matched) === 1.U, "SpecMiss resolution must match one post-xbar A slot")
        }
        assert(PopCount(committed) <= 1.U, "SpecMiss committed in multiple L2 slices")
        spec.committed(w) := committed.asUInt.orR
      }
    }
  }
}
