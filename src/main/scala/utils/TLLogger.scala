package utils

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.StringParam
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLAdapterNode, TLBundle, TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE, TLChannel, TLEdgeIn}

class TLLog extends Bundle {
  // c++ firendly data types
  def uint8_t = UInt(8.W)
  def uint32_t = UInt(32.W)
  def uint64_t = UInt(64.W)
  // a b c d e
  // 0 1 2 3 4
  val channel = uint8_t
  val opcode = uint8_t
  val param = uint8_t
  val source = uint8_t
  val sink = uint8_t
  val address = uint64_t
  val data = Vec(4, uint64_t)
  val stamp = uint64_t
}

class TLLogWriter(prefix: String) extends
  BlackBox( Map("prefix" -> StringParam(prefix)) ) with HasBlackBoxResource
{
  val io = IO(Input(new TLLog {
    val wen = Bool()
    val clock = Clock()
    val reset = Reset()
  })).suggestName("io")
  addResource("/vsrc/TLLogWriter.v")
}

class TLLogger(name: String)(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode()
  lazy val module = new TLLoggerImp(this, name)
}

class TLLoggerImp(outer: TLLogger, name: String) extends LazyModuleImp(outer) {
  val node = outer.node
  for(((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)) {
    out <> in
    TLLogger.track(in, edgeIn, this.clock, this.reset)(name)
  }
}

object TLLogger {

  def a = 0.U
  def b = 1.U
  def c = 2.U
  def d = 3.U
  def e = 4.U // not used

  def writeChannel[T <: TLChannel](log: TLLog, chn: T, stamp: UInt): Unit = {
    for((name, data) <- log.elements.filterNot(_._1 == "data")) {
      val e = chn.elements.find(_._1 == name)
      if(e.nonEmpty){
        data := e.get._2
      } else {
        data := 0.U
      }
    }
    chn match {
      case _: TLBundleA => log.channel := a
      case _: TLBundleB => log.channel := b
      case _: TLBundleC => log.channel := c
      case _: TLBundleD => log.channel := d
    }
    log.stamp := stamp
  }

  def logA(log: TLLog, a: TLBundleA, stamp: UInt) = {
    writeChannel(log, a, stamp)
  }

  def logB(log: TLLog, b: TLBundleB, stamp: UInt) = {
    writeChannel(log, b, stamp)
  }

  def logC(log: TLLog, c: TLBundleC, stamp: UInt) = {
    writeChannel(log, c, stamp)
    log.data := c.data.asTypeOf(log.data)
  }

  def logD(log: TLLog, d: TLBundleD, stamp: UInt, addr: UInt) = {
    writeChannel(log, d, stamp)
    log.address := addr
    log.data := d.data.asTypeOf(log.data)
  }

  def track(in: TLBundle, edge: TLEdgeIn, clock: Clock, reset: Reset)(name: String) = {
    val numClients = edge.client.endSourceId

    // address map for Grant/AccesAck/ReleaseAck
    val d_addrs = Reg(Vec(numClients, UInt(edge.bundle.addressBits.W)))
    val a_log, b_log, c_log, d_log = WireInit(0.U.asTypeOf(new TLLog))
    val a_writer, b_writer, c_writer, d_writer = Module(new TLLogWriter(name))
    val timer = GTimer()

    def connect(writer: TLLogWriter, log: TLLog, wen: Bool) = {
      writer.io.channel := log.channel
      writer.io.opcode := log.opcode
      writer.io.param := log.param
      writer.io.source := log.source
      writer.io.sink := log.sink
      writer.io.address := log.address
      writer.io.data := log.data
      writer.io.stamp := log.stamp
      writer.io.wen := wen
      writer.io.clock := clock
      writer.io.reset := reset
    }

    connect(a_writer, a_log, in.a.fire())
    connect(b_writer, b_log, in.b.fire())
    connect(c_writer, c_log, in.c.fire())
    connect(d_writer, d_log, in.d.fire())

    when(in.a.fire()){
      logA(a_log, in.a.bits, timer)
      d_addrs(in.a.bits.source) := in.a.bits.address
    }

    when(in.b.fire()){
      logB(b_log, in.b.bits, timer)
    }

    when(in.c.fire()){
      logC(c_log, in.c.bits, timer)
    }

    when(in.d.fire()){
      logD(d_log, in.d.bits, timer, d_addrs(in.d.bits.source))
    }

  }

  def apply(name: String)(implicit p: Parameters): TLAdapterNode = {
    val logger = LazyModule(new TLLogger(name))
    logger.node
  }

}

