
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
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.{TLArbiter, TLXbar}
import freechips.rocketchip.util.BundleField
import chisel3.util._
import device.{AXI4SlaveModule, AXI4SlaveModuleImp}
import utility.DataHoldBypass

class TraceAXI4Memory
(
  val address: Seq[AddressSet],
  val memByte: Long,
  val useBlackBox: Boolean = false,
  val executable: Boolean = true,
  val beatBytes: Int,
  val burstLen: Int,
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable, beatBytes, burstLen)
{
  override lazy val module = new TraceAXI4MemoryImp(this)
}

class TraceAXI4MemoryImp[T <: Data](outer: TraceAXI4Memory) extends AXI4SlaveModuleImp(outer) {
  val readReqSize = 1 << in.ar.bits.id.getWidth
  val writeReqSize = 1 << in.aw.bits.id.getWidth
  class ReadBundle extends Bundle {
    val id = UInt(in.ar.bits.id.getWidth.W)
    val len = UInt(in.ar.bits.len.getWidth.W)
  }
  class WriteBundle extends Bundle {
    val id = UInt(in.aw.bits.id.getWidth.W)
  }
  val rQueue = Module(new Queue(new ReadBundle, readReqSize))
  val wQueue = Module(new Queue(new WriteBundle, writeReqSize))

  // accept a read request and send it to the external model
  rQueue.io.enq.valid := in.ar.valid
  rQueue.io.enq.bits.id := DataHoldBypass(in.ar.bits.id, in.ar.fire)
  rQueue.io.enq.bits.len := DataHoldBypass(in.ar.bits.len, in.ar.fire)
  in.ar.ready := rQueue.io.enq.ready

  // accept a write request (including address and data) and send it to the external model
  wQueue.io.enq.valid := in.w.valid && in.w.bits.last
  wQueue.io.enq.bits.id := DataHoldBypass(in.aw.bits.id, in.aw.fire)
  in.aw.ready := wQueue.io.enq.ready
  in.w.ready := true.B

  val rCurRequest = Reg(new ReadBundle)
  val rCurRequestValid = RegInit(false.B)
  val rdataCnt = Counter(outer.burstLen)
  val rRespLen = rCurRequest.len
  val rRespLast = rdataCnt.value === rRespLen // minus 1?
  when (rRespLast) {
    rdataCnt.reset()
  }.elsewhen (in.r.fire) {
    rdataCnt.inc()
  }
  when (rQueue.io.deq.fire) {
    rCurRequestValid := true.B
    rCurRequest := rQueue.io.deq.bits
  }.elsewhen (in.r.fire && in.r.bits.last) {
    rCurRequestValid := false.B
  }
  rQueue.io.deq.ready := !rCurRequestValid || (in.r.ready/*fire*/ && in.r.bits.last)

  in.r.valid := rCurRequestValid
  in.r.bits.data := DontCare
  in.r.bits.id := rCurRequest.id
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  in.r.bits.last := rRespLast

  // write response
  val wCurRequest = Reg(new WriteBundle)
  val wCurRequestValid = RegInit(false.B)
  when (wQueue.io.deq.fire) {
    wCurRequestValid := true.B
    wCurRequest := wQueue.io.deq.bits
  }.elsewhen (in.b.fire) {
    wCurRequestValid := false.B
  }
  wQueue.io.deq.ready := in.b.ready || !wCurRequestValid

  in.b.valid := wCurRequestValid
  in.b.bits.id := wCurRequest.id
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
}

class TraceAXI4Xbar(
  arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
  maxFlightPerId:    Int = 7,
  awQueueDepth:      Int = 2)(implicit p: Parameters) extends LazyModule
{
  require (maxFlightPerId >= 1)
  require (awQueueDepth >= 1)

  val node = new AXI4NexusNode(
    masterFn  = { seq =>
      seq(0).copy(
        echoFields    = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys  = seq.flatMap(_.responseKeys).distinct,
        masters = (TraceAXI4Xbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.masters map { master => master.copy(id = master.id.shift(range.start)) }
        }
      )
    },
    slaveFn = { seq =>
      seq(0).copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys    = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        slaves = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Xbar data widths don't match: ${port.slaves.map(_.name)} has ${port.beatBytes}B vs ${seq(0).slaves.map(_.name)} has ${seq(0).beatBytes}B")
          port.slaves
        }
      )
    }
  ){
    override def circuitIdentity = outputs == 1 && inputs == 1
  }

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip

    // Grab the port ID mapping
    val inputIdRanges = TraceAXI4Xbar.mapInputIds(edgesIn.map(_.master))

    // Find a good mask for address decoding
    val port_addrs = edgesOut.map(_.slave.slaves.map(_.address).flatten)
    val routingMask = AddressDecoder(port_addrs)
    val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
    // val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))
    // val slaveIsFastSim = route_addrs
    val outputPorts = route_addrs.map(seq => {
      (addr: UInt) => {
        // origin: seq.map(_.contains(addr)).reduce(_ || _)
        // addr is not used, changed to fastSimEnable
        val imFastSimSeq = TraceAddressRange.imTraceFastSim(seq)
        val fastSimEnable = TraceFastSimMemory()
        ~(imFastSimSeq.B ^ fastSimEnable)
      }
    })
    require(io_out.size == 2)

    // To route W we need to record where the AWs went
    val awIn  = Seq.fill(io_in .size) { Module(new Queue(UInt(io_out.size.W), awQueueDepth, flow = true)) }
    val awOut = Seq.fill(io_out.size) { Module(new Queue(UInt(io_in .size.W), awQueueDepth, flow = true)) }

    val requestARIO = io_in.map  { i => VecInit(outputPorts.map   { o => o(i.ar.bits.addr) }) }
    val requestAWIO = io_in.map  { i => VecInit(outputPorts.map   { o => o(i.aw.bits.addr) }) }
    val requestROI  = io_out.map { o => inputIdRanges.map { i => i.contains(o.r.bits.id) } }
    val requestBOI  = io_out.map { o => inputIdRanges.map { i => i.contains(o.b.bits.id) } }

    // W follows the path dictated by the AW Q
    for (i <- 0 until io_in.size) { awIn(i).io.enq.bits := requestAWIO(i).asUInt }
    val requestWIO = awIn.map { q => if (io_out.size > 1) q.io.deq.bits.asBools else Seq(true.B) }

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = AXI4BundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Transform input bundles
    val in = Wire(Vec(io_in.size, new AXI4Bundle(wide_bundle)))
    for (i <- 0 until in.size) {
      in(i).squeezeAll :<>= io_in(i).squeezeAll

      // Handle size = 1 gracefully (Chisel3 empty range is broken)
      def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)
      // Manipulate the AXI IDs to differentiate masters
      val r = inputIdRanges(i)
      in(i).aw.bits.id := io_in(i).aw.bits.id | (r.start).U
      in(i).ar.bits.id := io_in(i).ar.bits.id | (r.start).U
      io_in(i).r.bits.id := trim(in(i).r.bits.id, r.size)
      io_in(i).b.bits.id := trim(in(i).b.bits.id, r.size)

      if (io_out.size > 1) {
        // Block A[RW] if we switch ports, to ensure responses stay ordered (also: beware the dining philosophers)
        val endId = edgesIn(i).master.endId
        val arFIFOMap = WireDefault(VecInit.fill(endId) { true.B })
        val awFIFOMap = WireDefault(VecInit.fill(endId) { true.B })
        val arSel = UIntToOH(io_in(i).ar.bits.id, endId)
        val awSel = UIntToOH(io_in(i).aw.bits.id, endId)
        val rSel  = UIntToOH(io_in(i).r .bits.id, endId)
        val bSel  = UIntToOH(io_in(i).b .bits.id, endId)
        val arTag = OHToUInt(requestARIO(i).asUInt, io_out.size)
        val awTag = OHToUInt(requestAWIO(i).asUInt, io_out.size)

        for (master <- edgesIn(i).master.masters) {
          def idTracker(port: UInt, req_fire: Bool, resp_fire: Bool) = {
            if (master.maxFlight == Some(0)) {
              true.B
            } else {
              val legalFlight = master.maxFlight.getOrElse(maxFlightPerId+1)
              val flight = legalFlight min maxFlightPerId
              val canOverflow = legalFlight > flight
              val count = RegInit(0.U(log2Ceil(flight+1).W))
              val last = Reg(UInt(log2Ceil(io_out.size).W))
              count := count + req_fire.asUInt - resp_fire.asUInt
              assert (!resp_fire || count =/= 0.U)
              assert (!req_fire  || count =/= flight.U)
              when (req_fire) { last := port }
              // No need to track where it went if we cap it at 1 request
              val portMatch = if (flight == 1) { true.B } else { last === port }
              (count === 0.U || portMatch) && ((!canOverflow).B || count =/= flight.U)
            }
          }

          for (id <- master.id.start until master.id.end) {
            arFIFOMap(id) := idTracker(
              arTag,
              arSel(id) && io_in(i).ar.fire,
              rSel(id) && io_in(i).r.fire && io_in(i).r.bits.last)
            awFIFOMap(id) := idTracker(
              awTag,
              awSel(id) && io_in(i).aw.fire,
              bSel(id) && io_in(i).b.fire)
          }
        }

        val allowAR = arFIFOMap(io_in(i).ar.bits.id)
        in(i).ar.valid := io_in(i).ar.valid && allowAR
        io_in(i).ar.ready := in(i).ar.ready && allowAR

        // Keep in mind that slaves may do this: awready := wvalid, wready := awvalid
        // To not cause a loop, we cannot have: wvalid := awready

        // Block AW if we cannot record the W destination
        val allowAW = awFIFOMap(io_in(i).aw.bits.id)
        val latched = RegInit(false.B) // cut awIn(i).enq.valid from awready
        in(i).aw.valid := io_in(i).aw.valid && (latched || awIn(i).io.enq.ready) && allowAW
        io_in(i).aw.ready := in(i).aw.ready && (latched || awIn(i).io.enq.ready) && allowAW
        awIn(i).io.enq.valid := io_in(i).aw.valid && !latched
        when (awIn(i).io.enq.fire) { latched := true.B }
        when (in(i).aw.fire) { latched := false.B }

        // Block W if we do not have an AW destination
        in(i).w.valid := io_in(i).w.valid && awIn(i).io.deq.valid // depends on awvalid (but not awready)
        io_in(i).w.ready := in(i).w.ready && awIn(i).io.deq.valid
        awIn(i).io.deq.ready := io_in(i).w.valid && io_in(i).w.bits.last && in(i).w.ready
      } else {
        awIn(i).io := DontCare // aw in queue is not used when outsize == 1
      }
    }

    // Transform output bundles
    val out = Wire(Vec(io_out.size, new AXI4Bundle(wide_bundle)))
    for (i <- 0 until out.size) {
      io_out(i).squeezeAll :<>= out(i).squeezeAll

      if (io_in.size > 1) {
        // Block AW if we cannot record the W source
        val latched = RegInit(false.B) // cut awOut(i).enq.valid from awready
        io_out(i).aw.valid := out(i).aw.valid && (latched || awOut(i).io.enq.ready)
        out(i).aw.ready := io_out(i).aw.ready && (latched || awOut(i).io.enq.ready)
        awOut(i).io.enq.valid := out(i).aw.valid && !latched
        when (awOut(i).io.enq.fire) { latched := true.B }
        when (out(i).aw.fire) { latched := false.B }

        // Block W if we do not have an AW source
        io_out(i).w.valid := out(i).w.valid && awOut(i).io.deq.valid // depends on awvalid (but not awready)
        out(i).w.ready := io_out(i).w.ready && awOut(i).io.deq.valid
        awOut(i).io.deq.ready := out(i).w.valid && out(i).w.bits.last && io_out(i).w.ready
      } else {
        awOut(i).io := DontCare // aw out queue is not used when io_in.size == 1
      }
    }

    // Fanout the input sources to the output sinks
    def transpose[T](x: Seq[Seq[T]]) = Seq.tabulate(x(0).size) { i => Seq.tabulate(x.size) { j => x(j)(i) } }
    val portsAROI = transpose((in  zip requestARIO) map { case (i, r) => TraceAXI4Xbar.fanout(i.ar, r) })
    val portsAWOI = transpose((in  zip requestAWIO) map { case (i, r) => TraceAXI4Xbar.fanout(i.aw, r) })
    val portsWOI  = transpose((in  zip requestWIO)  map { case (i, r) => TraceAXI4Xbar.fanout(i.w,  r) })
    val portsRIO  = transpose((out zip requestROI)  map { case (o, r) => TraceAXI4Xbar.fanout(o.r,  r) })
    val portsBIO  = transpose((out zip requestBOI)  map { case (o, r) => TraceAXI4Xbar.fanout(o.b,  r) })

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      awOut(o).io.enq.bits := // Record who won AW arbitration to select W
        AXI4Arbiter.returnWinner(arbitrationPolicy)(out(o).aw, portsAWOI(o):_*).asUInt
      AXI4Arbiter(arbitrationPolicy)(out(o).ar, portsAROI(o):_*)
      // W arbitration is informed by the Q, not policy
      out(o).w.valid := Mux1H(awOut(o).io.deq.bits, portsWOI(o).map(_.valid))
      out(o).w.bits :<= Mux1H(awOut(o).io.deq.bits, portsWOI(o).map(_.bits))
      portsWOI(o).zipWithIndex.map { case (p, i) =>
        if (in.size > 1) {
          p.ready := out(o).w.ready && awOut(o).io.deq.bits(i)
        } else {
          p.ready := out(o).w.ready
        }
      }
    }

    for (i <- 0 until in.size) {
      AXI4Arbiter(arbitrationPolicy)(in(i).r, portsRIO(i):_*)
      AXI4Arbiter(arbitrationPolicy)(in(i).b, portsBIO(i):_*)
    }
  }
}

object TraceAXI4Xbar
{
  def apply(
    arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
    maxFlightPerId:    Int = 7,
    awQueueDepth:      Int = 2)(implicit p: Parameters) =
  {
    val axi4xbar = LazyModule(new TraceAXI4Xbar(arbitrationPolicy, maxFlightPerId, awQueueDepth))
    axi4xbar.node
  }

  def mapInputIds(ports: Seq[AXI4MasterPortParameters]) = TLXbar.assignRanges(ports.map(_.endId))

  // Replicate an input port to each output port
  def fanout[T <: AXI4BundleBase](input: IrrevocableIO[T], select: Seq[Bool]) = {
    val filtered = Wire(Vec(select.size, chiselTypeOf(input)))
    for (i <- 0 until select.size) {
      filtered(i).bits :<= input.bits
      filtered(i).valid := input.valid && select(i)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

object TraceAddressRange {
  def bottom = 0x80000000L
  def range = 0x7fffffffL
  def deadbeefAddr = bottom

  def addressSet(): AddressSet = {
    AddressSet(bottom, range)
  }

  def imTraceFastSim(ass: Seq[AddressSet]): Boolean = {
    ass.map(_.contains(deadbeefAddr)).reduce(_ || _)
  }
}