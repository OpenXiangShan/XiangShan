package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import top.{ArgParser, Generator}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, VirtMode}

object CSRConfig {
  final val GEILEN = 63

  final val ASIDLEN = 16 // the length of ASID of XS implementation

  final val ASIDMAX = 16 // the max value of ASIDLEN defined by spec

  final val HIIDWidth = 12 // support Hvictl[27:16](IID)

  final val VMIDLEN = 14 // the length of VMID of XS implementation

  final val VMIDMAX = 14 // the max value of VMIDLEN defined by spec

  final val VaddrWidth = 39 // only Sv39

}

class NewCSR extends Module
  with MachineLevel
  with SupervisorLevel
  with HypervisorLevel
  with VirtualSupervisorLevel
  with Unprivileged
  with HasExternalInterruptBundle
  with HasInstCommitBundle
  with SupervisorMachineAliasConnect {

  val io = IO(new Bundle {
    val w = Flipped(ValidIO(new Bundle {
      val addr = UInt(12.W)
      val data = UInt(64.W)
    }))
    val rAddr = Input(UInt(12.W))
    val rData = Output(UInt(64.W))
    val trap = Flipped(ValidIO(new Bundle {
      val toPRVM = PrivMode()
      val toV = VirtMode()
    }))
    val tret = Flipped(ValidIO(new Bundle {
      val toPRVM = PrivMode()
      val toV = VirtMode()
    }))
    // from interrupt controller
    val fromIC = Input(new Bundle {
      val vs = new CSRIRCBundle
    })
  })

  val addr = io.w.bits.addr
  val data = io.w.bits.data
  val wen = io.w.valid

  val PRVM = RegInit(PrivMode.M)
  val V = RegInit(VirtMode.Off)

  val trap = io.trap.valid
  val trapToPRVM = io.trap.bits.toPRVM
  val trapToV = io.trap.bits.toV
  val trapToM = trapToPRVM === PrivMode.M
  val trapToHS = trapToPRVM === PrivMode.S && trapToV === VirtMode.Off
  val trapToHU = trapToPRVM === PrivMode.U && trapToV === VirtMode.Off
  val trapToVS = trapToPRVM === PrivMode.S && trapToV === VirtMode.On
  val trapToVU = trapToPRVM === PrivMode.U && trapToV === VirtMode.On

  val tret = io.tret.valid
  val tretPRVM = io.tret.bits.toPRVM
  val tretV = io.tret.bits.toV
  val isSret = tret && tretPRVM === PrivMode.S
  val isMret = tret && tretPRVM === PrivMode.M

  var csrRwMap = machineLevelCSRMap ++ supervisorLevelCSRMap ++ hypervisorCSRMap ++ virtualSupervisorCSRMap ++ unprivilegedCSRMap

  val csrMods = machineLevelCSRMods ++ supervisorLevelCSRMods ++ hypervisorCSRMods ++ virtualSupervisorCSRMods ++ unprivilegedCSRMods

  for ((id, (wBundle, _)) <- csrRwMap) {
    wBundle.wen := wen && addr === id.U
    wBundle.wdata := data
  }
  io.rData := Mux1H(csrRwMap.map { case (id, (_, rBundle)) =>
    (io.rAddr === id.U) -> rBundle.asUInt
  })

  csrMods.foreach { mod =>
    mod match {
      case m: HypervisorBundle =>
        m.hstatus := hstatus.regOut
        m.hvip := hvip.regOut
        m.hideleg := hideleg.regOut
        m.hedeleg := hedeleg.regOut
        m.hgeip := hgeip.regOut
        m.hgeie := hgeie.regOut
        m.hip := hip.regOut
        m.hie := hie.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineInterruptBundle =>
        m.mvien := mvien.regOut
        m.mvip := mvip.regOut
        m.mip := mip.regOut
        m.mie := mie.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineDelegBundle =>
        m.mideleg := mideleg.regOut
        m.medeleg := medeleg.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineCounterControlBundle =>
        m.mcountinhibit := mcountinhibit.regOut
      case _ =>
    }
    mod match {
      case m: HasExternalInterruptBundle =>
        m.platformIRP := this.platformIRP
      case _ =>
    }
    mod match {
      case m: HasInstCommitBundle =>
        m.commitValid := this.commitValid
        m.commitInstNum := this.commitInstNum
      case _ =>
    }
  }

  csrMods.foreach { mod =>
    mod.commonIn.status := mstatus.mstatus
    mod.commonIn.prvm := PRVM
    mod.commonIn.v := V
    mod.commonIn.hstatus := hstatus.rdata
    println(s"${mod.modName}: ")
    println(mod.dumpFields)
  }
}

trait SupervisorMachineAliasConnect { self: NewCSR with MachineLevel with SupervisorLevel =>
  mip.fromMvip := mvip.toMip
  mip.fromSip := sip.toMip
  mie.fromSie := sie.toMie
}

object NewCSRMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new NewCSR,
    firtoolOpts
  )

  println("done")
}