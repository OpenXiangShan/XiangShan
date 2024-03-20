package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import top.{ArgParser, Generator}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, VirtMode}

class NewCSR extends Module with MachineLevel with SupervisorLevel with Hypervisor with Unprivileged {
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

  val CSRWMap = machineLevelCSRMap ++ supervisorLevelCSRMap ++ hypervisorCSRMap ++ unprivilegedCSRMap

  val csrMods = machineLevelCSRMods ++ supervisorLevelCSRMods ++ hypervisorCSRMods ++ unprivilegedCSRMods

  for ((id, (wBundle, _)) <- CSRWMap) {
    wBundle.wen := wen && addr === id.U
    wBundle.wdata := data
  }
  io.rData := Mux1H(CSRWMap.map { case (id, (_, rBundle)) =>
    (io.rAddr === id.U) -> rBundle.asUInt
  })

  csrMods.foreach { mod =>
    mod.commonIn.status := mstatus.mstatus
    mod.commonIn.prvm := PRVM
    mod.commonIn.v := V
    println(s"${mod.modName}: ")
    println(mod.dumpFields)
  }
}

object NewCSRMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform")

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new NewCSR,
    Array()
  )
  println("done")
}