package device

import chisel3._
import chisel3.util._
import utils._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, RegionType, TransferSizes}
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4.{AXI4Parameters, AXI4SlaveNode, AXI4SlaveParameters, AXI4SlavePortParameters}

abstract class AXI4SlaveModule[T <: Data]
(
  address: AddressSet,
  executable: Boolean = true,
  beatBytes: Int = 8,
  burstLen: Int = 1,
  val _extra: T = null
)(implicit p: Parameters) extends LazyModule {

  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      Seq(address),
      regionType = RegionType.UNCACHED,
      executable = executable,
      supportsWrite = TransferSizes(1, beatBytes * burstLen),
      supportsRead = TransferSizes(1, beatBytes * burstLen),
      interleavedId = Some(0)
    )),
    beatBytes = beatBytes
  )))

  lazy val module = new AXI4SlaveModuleImp[T](this)

}

class AXI4SlaveModuleImp[T<:Data](outer: AXI4SlaveModule[T])
  extends LazyModuleImp(outer)
{
  val io = IO(new Bundle {
    val extra = if(outer._extra == null) None else Some(outer._extra.cloneType)
  })

  val (in, edge) = outer.node.in.head

  val timer = GTimer()
  when(in.ar.fire()){
    printf(p"[$timer][ar] addr: ${Hexadecimal(in.ar.bits.addr)} " +
      p"arlen:${in.ar.bits.len} arsize:${in.ar.bits.size} " +
      p"id: ${in.ar.bits.id}\n"
    )
  }
  when(in.aw.fire()){
    printf(p"[$timer][aw] addr: ${Hexadecimal(in.aw.bits.addr)} " +
      p"awlen:${in.aw.bits.len} awsize:${in.aw.bits.size} " +
      p"id: ${in.aw.bits.id}\n"
    )
  }
  when(in.w.fire()){
    printf(p"[$timer][w] wmask: ${Binary(in.w.bits.strb)} last:${in.w.bits.last}\n")
  }
  when(in.b.fire()){
    printf(p"[$timer][b] id: ${in.b.bits.id}\n")
  }
  when(in.r.fire()){
    printf(p"[$timer][r] id: ${in.r.bits.id}\n")
  }

  val fullMask = MaskExpand(in.w.bits.strb)

  def genWdata(originData: UInt) = (originData & (~fullMask).asUInt()) | (in.w.bits.data & fullMask)

  val raddr = Wire(UInt())
  val ren = Wire(Bool())
  val (readBeatCnt, rLast) = {
    val c = Counter(256)
    val beatCnt = Counter(256)
    val len = HoldUnless(in.ar.bits.len, in.ar.fire())
    val burst = HoldUnless(in.ar.bits.burst, in.ar.fire())
    val wrapAddr = in.ar.bits.addr & (~(in.ar.bits.len << in.ar.bits.size)).asUInt()
    raddr := HoldUnless(wrapAddr, in.ar.fire())
    in.r.bits.last := (c.value === len)
    when(ren) {
      beatCnt.inc()
      when(burst === AXI4Parameters.BURST_WRAP && beatCnt.value === len) {
        beatCnt.value := 0.U
      }
    }
    when(in.r.fire()) {
      c.inc()
      when(in.r.bits.last) {
        c.value := 0.U
      }
    }
    when(in.ar.fire()) {
      beatCnt.value := (in.ar.bits.addr >> in.ar.bits.size).asUInt() & in.ar.bits.len
      when(in.ar.bits.len =/= 0.U && in.ar.bits.burst === AXI4Parameters.BURST_WRAP) {
        assert(in.ar.bits.len === 1.U || in.ar.bits.len === 3.U ||
          in.ar.bits.len === 7.U || in.ar.bits.len === 15.U)
      }
    }
    (beatCnt.value, in.r.bits.last)
  }

  val r_busy = BoolStopWatch(in.ar.fire(), in.r.fire() && rLast, startHighPriority = true)
  in.ar.ready := in.r.ready || !r_busy
  in.r.bits.resp := AXI4Parameters.RESP_OKAY
  ren := RegNext(in.ar.fire()) || (in.r.fire() && !rLast)
  in.r.valid := BoolStopWatch(ren && (in.ar.fire() || r_busy), in.r.fire(), startHighPriority = true)


  val waddr = Wire(UInt())
  val (writeBeatCnt, wLast) = {
    val c = Counter(256)
    waddr := HoldUnless(in.aw.bits.addr, in.aw.fire())
    when(in.w.fire()) {
      c.inc()
      when(in.w.bits.last) {
        c.value := 0.U
      }
    }
    (c.value, in.w.bits.last)
  }

  val w_busy = BoolStopWatch(in.aw.fire(), in.b.fire(), startHighPriority = true)
  in.aw.ready := !w_busy
  in.w.ready := in.aw.valid || (w_busy)
  in.b.bits.resp := AXI4Parameters.RESP_OKAY
  in.b.valid := BoolStopWatch(in.w.fire() && wLast, in.b.fire(), startHighPriority = true)

  in.b.bits.id := RegEnable(in.aw.bits.id, in.aw.fire())
  in.b.bits.user := RegEnable(in.aw.bits.user, in.aw.fire())
  in.r.bits.id := RegEnable(in.ar.bits.id, in.ar.fire())
  in.r.bits.user := RegEnable(in.ar.bits.user, in.ar.fire())
}
