/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  ***************************************************************************************/

package xiangshan

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.BundleField
import org.chipsalliance.cde.config.Parameters

/**
  * The private L1-to-L2 TileLink fabric.
  *
  * Unlike the old topology, arbitration and the downstream channel buffers live in one
  * module. There is deliberately no input buffer in front of the arbitration point.
  * Requests accepted on `node.in` are ordinary, irrevocable TileLink transactions; the
  * tentative/commit/kill protocol needed by speculative misses is not part of this first
  * implementation.
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
  e: BufferParams = BufferParams.default
)(implicit p: Parameters) extends LazyModule {

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
    TLXbar.circuit(policy, node.in, arbitrationOutputs)

    // Request-direction channels are registered after arbitration. Response-direction
    // channels are registered before source-ID based return routing.
    (node.out zip arbitrationOutputs).foreach { case ((downstream, _), (arbitrationOut, _)) =>
      downstream.a <> a(arbitrationOut.a)
      arbitrationOut.b <> b(downstream.b)
      downstream.c <> c(arbitrationOut.c)
      arbitrationOut.d <> d(downstream.d)
      downstream.e <> e(arbitrationOut.e)
    }
  }
}
