package system

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.amba.axi4.{AXI4Arbiter, AXI4Imp, AXI4MasterPortParameters, AXI4NexusNode, AXI4SlavePortParameters, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, CustomNode, LazyModule, LazyModuleImp, SimpleDevice, ValName}
import freechips.rocketchip.regmapper.RegFieldGroup
import freechips.rocketchip.tilelink.{TLArbiter, TLRegisterNode}
import freechips.rocketchip.util.BundleField
import xiangshan.{HasXSParameter, XSCoreParamsKey}
import xiangshan.backend.fu.{PMAConst, PMPChecker, TLPMAConfig, TLPMAMethod}

case class AXI4SpliterNode()(implicit valName: ValName) extends CustomNode(AXI4Imp) {

  override def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require(
      iKnown == 1 && oKnown == 2 && iStars == 0 && oStars == 0,
      "the only supported params are: (iKnown, oKnown, iStars, oStars) = (1, 2, 0, 0)"
    )
    (0, 0)
  }

  override def mapParamsD(n: Int, p: Seq[AXI4MasterPortParameters]): Seq[AXI4MasterPortParameters] = {
    require(p.size == 1)
    require(n == 2)
    Seq.fill(n){ p.head }
  }

  override def mapParamsU(n: Int, p: Seq[AXI4SlavePortParameters]): Seq[AXI4SlavePortParameters] = {
    require(n == 1)
    require(p.size == 2)
    def slaveFn(seq: Seq[AXI4SlavePortParameters]) = {
      seq(0).copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys    = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        slaves = seq.last.slaves
//        slaves = seq.flatMap { port =>
//          require (port.beatBytes == seq(0).beatBytes,
//            s"Xbar data widths don't match: ${port.slaves.map(_.name)} has ${port.beatBytes}B vs ${seq(0).slaves.map(_.name)} has ${seq(0).beatBytes}B")
//          port.slaves
//        }.distinct
      )
    }
//    for((port, i) <- p.zipWithIndex){
//      println(s"port $i")
//      for((slave, j) <- port.slaves.zipWithIndex){
//        println(s"slave $j")
//        println(slave.name)
//        println(slave.address)
//        println(slave.maxTransfer)
//      }
//    }
//    val x = p(0).slaves(0).copy(nodePath = Nil)
//    val y = p(1).slaves(1).copy(nodePath = Nil)
//    println("x")
//    println(x)
//
//    println("y")
//    println(y)
//    println(x == y)
    Seq(slaveFn(p))
  }
}

/**
  *     In -> Out_0 (uncache)
  *        -> Out_1 (cache)
  */
class AXI4Spliter
(
  policy: TLArbiter.Policy,
  entries: Int = 16
)(implicit p: Parameters) extends LazyModule {

  val node = AXI4SpliterNode()
  // NOTE: register node, may only instantiation with different address?
  val pmaNode = TLRegisterNode(
    address = Seq(AddressSet(0x31120000/*pmaParam.address*/, 0xffff)),
    device = new SimpleDevice("tl-pma", Nil),
    concurrency = 1,
    beatBytes = 8
  )

  lazy val module = new LazyModuleImp(this) with HasXSParameter with PMAConst with TLPMAMethod{


    val (in, edgeIn) = node.in.head
    val (out_ports, out_edges) = node.out.unzip

    val (cfg_map, addr_map, pma) = gen_tlpma_mapping(NumPMA)
    pmaNode.regmap(
      0x0000 -> RegFieldGroup(
        "TLPMA Config Register", Some("TL PMA configuation register"),
        cfg_map
      ),
      0x0100 -> RegFieldGroup(
        "TLPMA Address Register", Some("TL PMA Address register"),
        addr_map
      )
    )

    val pmaarPort = 0
    val pmaawPort = 1
    val pma_check = Vec(2/*pmaParam*/, Module(new PMPChecker(3/*pmaParam.lgMaxSize*/, true/* pmaParam.sameCycle*/, false)).io)
    pma_check.map(_.check_env.apply(3.U, pma/*placeHolde*/, pma))
    pma_check(pmaarPort).req_apply(in.ar.valid, in.aw.bits.addr)
    pma_check(pmaawPort).req_apply(in.aw.valid, in.aw.bits.addr)

    val in_aw = WireInit(in.aw)

    val ar_mmio = pma_check(pmaarPort).resp.mmio
    val aw_mmio = pma_check(pmaawPort).resp.mmio

    val ar_select = Seq(ar_mmio, !ar_mmio)
    val aw_select = Seq(aw_mmio, !aw_mmio)
    out_ports.map(_.ar).zip(AXI4Xbar.fanout(in.ar, ar_select)).foreach(x => AXI4Arbiter(policy)(x._1, x._2))
    out_ports.map(_.aw).zip(AXI4Xbar.fanout(in_aw, aw_select)).foreach(x => AXI4Arbiter(policy)(x._1, x._2))

//    for(((out, edgeOut), i) <- node.out.zipWithIndex){
//      when(out.ar.fire()){
//        printf(p"chn $i ar fire\n")
//      }
//      when(out.aw.fire()){
//        printf(p"chn $i aw fire\n")
//      }
//    }

    val aw_fifo = Module(new Queue(Bool(), entries))
    aw_fifo.io.enq.valid := in.aw.fire
    aw_fifo.io.enq.bits := aw_mmio

    in_aw.valid := in.aw.valid && aw_fifo.io.enq.ready
    in_aw.bits := in.aw.bits
    in.aw.ready := in_aw.ready

    aw_fifo.io.deq.ready := in.w.fire && in.w.bits.last

    val w_sel = Seq(aw_fifo.io.deq.bits, !aw_fifo.io.deq.bits)
    for((w, i) <- out_ports.map(_.w).zipWithIndex){
      w.valid := in.w.valid && w_sel(i)
      w.bits := in.w.bits
    }
    in.w.ready := Mux1H(w_sel, out_ports.map(_.w.ready))

    AXI4Arbiter(policy)(in.r, node.out.map(_._1.r): _*)
    AXI4Arbiter(policy)(in.b, node.out.map(_._1.b): _*)

  }

}

object AXI4Spliter {
  def apply
  (
    policy: TLArbiter.Policy = TLArbiter.roundRobin,
    entries: Int = 16
  )(implicit p: Parameters) = {
    val spliter = LazyModule(new AXI4Spliter(policy, entries))
    spliter
  }
}

