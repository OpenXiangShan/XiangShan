package bus.simplebus

import chisel3._
import chisel3.util._

import utils._

/*
class SimpleBusCrossbar(m: Int, addressSpace: List[(Long, Long)]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(m, new SimpleBus))
    val out = Vec(addressSpace.length, new SimpleBus)
  })

  // choose an input
  val aBundleArb = Module(new RRArbiter(chiselTypeOf(io.in(0).a.bits), m))
  aBundleArb.io.in <> io.in.map(_.a)
  val busy = RegInit(false.B)
  val busyStart = aBundleArb.io.out.fire() && !busy
  when (busyStart) { busy := true.B }
  aBundleArb.io.out.ready := !busy

  val inChosenIdx = Mux(busyStart, aBundleArb.io.chosen, RegNext(aBundleArb.io.chosen, busyStart))
  // w channel is attached to a channel
  val wBundle = io.in(inChosenIdx).w
  val isRead = aBundleArb.io.out.valid && !wBundle.valid

  when (io.out(0).a.fire()) {
//    printf(p"${GTimer()}: ${io.out(0).a.bits}, wen = ${io.out(0).w.valid}, ${io.out(0).w.bits}\n")
  }

  //printf(p"io.in = ${io.in}")
  //printf(p"io.out = ${io.out}\n")

  // select the output according to the address
  val addr = aBundleArb.io.out.bits.addr
  //val addrLatched = RegEnable(addr, isRead)
  val outChosenVec_0 = VecInit(addressSpace.map(
    range => (addr >= range._1.U && addr < range._2.U && aBundleArb.io.out.valid)))
  val outChosenVec = Mux(busyStart, outChosenVec_0, RegNext(outChosenVec_0, busyStart))
  val outChosenIdx = PriorityEncoder(outChosenVec)
  val outChosen = io.out(outChosenIdx)

  when (aBundleArb.io.out.fire()) {
//    printf("addr = %x\n", addr)
//    printf(p"outChosenVec = ${outChosenVec}, outChosenIdx = ${outChosenIdx}\n")
//    printf(p"${GTimer()}: arb.out.a: ${aBundleArb.io.out.bits}, wen = ${wBundle.valid}, arb.out.w: ${wBundle.bits}\n")
  }


  // bind a and w channel
  io.out.map { out => {
    out.a.bits := aBundleArb.io.out.bits
    out.w := wBundle
  }}

  //val busy = BoolStopWatch(isRead, io.out(outChosenIdx).r.fire())
  val isReadLatched = Mux(busyStart, isRead, RegEnable(isRead, busyStart))
  val busy = BoolStopWatch(busyStart, Mux(isReadLatched, outChosen.r.fire(), outChosen.a.fire()))
  (io.out.map(_.a) zip outChosenVec).map { case (a, v) => a.valid := v }

  when (io.in(0).a.valid) {
    printf(p"${GTimer()}: xbar.in.a.ready = ${io.in(0).a.ready}, busy = ${busy}\n")
  }

  // bind r channel
  (io.out.map(_.r) zip outChosenVec).map { case (r, v) => r.ready := v && busy }
  io.in.map(_.r).map (_.bits := outChosen.r.bits)
  io.in.map(_.r).zipWithIndex.map { case (r, i) =>
    r.valid := outChosen.r.valid && (i.U === inChosenIdx)
  }
}
*/
