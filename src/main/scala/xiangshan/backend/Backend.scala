package xiangshan.backend

import bus.simplebus.SimpleBusUC
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import noop.MemMMUIO
import xiangshan.{FetchPacket, Redirect, XSConfig, XSModule}


class Backend(implicit val p: XSConfig) extends XSModule {
  val io = IO(new Bundle {
    val dmem = new SimpleBusUC(addrBits = VAddrBits)
    val memMMU = Flipped(new MemMMUIO)

    val fetchPacket = Flipped(DecoupledIO(new FetchPacket)) // from frontend
    val redirect = ValidIO(new Redirect)
  })

  io.dmem <> DontCare
  io.memMMU <> DontCare
  io.redirect.valid := false.B
  io.redirect.bits <> DontCare

  io.fetchPacket.ready := true.B



  // TODO: Remove sink and source
  val tmp = WireInit(0.U)
  val sinks = Array[String](
    "DTLBFINISH",
    "DTLBPF",
    "DTLBENABLE",
    "perfCntCondMdcacheLoss",
    "perfCntCondMl2cacheLoss",
    "perfCntCondMdcacheHit",
    "lsuMMIO",
    "perfCntCondMl2cacheHit",
    "perfCntCondMl2cacheReq",
    "mtip",
    "perfCntCondMdcacheReq",
    "meip"
  )
  for (s <- sinks){ BoringUtils.addSink(tmp, s) }

  // A fake commit
  // TODO: difftest 6 insts per cycle
  val commit = io.fetchPacket.fire()
  val pc = io.fetchPacket.bits.pc
  val inst = io.fetchPacket.bits.instrs(0)

  if(!p.FPGAPlatform){
    BoringUtils.addSource(commit, "difftestCommit")
    BoringUtils.addSource(pc, "difftestThisPC")
    BoringUtils.addSource(inst, "difftestThisINST")
    BoringUtils.addSource(tmp, "difftestIsMMIO")
    BoringUtils.addSource(tmp, "difftestIsRVC")
    BoringUtils.addSource(tmp, "difftestIntrNO")
    BoringUtils.addSource(VecInit(Seq.fill(64)(tmp)), "difftestRegs")
    BoringUtils.addSource(tmp, "difftestMode")
    BoringUtils.addSource(tmp, "difftestMstatus")
    BoringUtils.addSource(tmp, "difftestSstatus")
    BoringUtils.addSource(tmp, "difftestMepc")
    BoringUtils.addSource(tmp, "difftestSepc")
    BoringUtils.addSource(tmp, "difftestMcause")
    BoringUtils.addSource(tmp, "difftestScause")
  }

}
