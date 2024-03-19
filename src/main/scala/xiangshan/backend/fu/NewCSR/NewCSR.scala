package xiangshan.backend.fu.NewCSR

import chisel3.util._
import chisel3._
import top.{ArgParser, Generator}
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRFieldWARLBits, MtvecMode, PrivMode, VirtMode}

import scala.collection.{SeqMap, immutable}

class NewCSR extends Module with CSRFuncTrait with Unprivileged {
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

  val hip = Module(new CSRModule("Hip", new CSRBundle {
    val VSSIP = CSRFieldWARLBits( 2, wNoFilter)
    val VSTIP = CSRFieldWARLBits( 6, wNoEffect)
    val VSEIP = CSRFieldWARLBits(10, wNoEffect)
    val SGEIP = CSRFieldWARLBits(12, wNoEffect)
  }) {} )

  val mstatus = Module(new MstatusModule)

  val mtvec = Module(new CSRModule("Mtvec", new CSRBundle {
      val mode = MtvecMode(1, 0, wNoFilter)
      val addr = CSRFieldWARLBits(63, 2, wNoFilter)
    }
  ) {
    when(wen && wdata.mode.isLegal) {
      reg.mode := wdata.mode
    }.otherwise(reg.mode := reg.mode)
  })


  val CSRWMap: immutable.SeqMap[Int, (CSRAddrWriteBundle[_], Data)] = SeqMap(
    0x001 -> (fcsr.wAliasFflags -> fcsr.fflags),
    0x002 -> (fcsr.wAliasFfm -> fcsr.frm),
    0x003 -> (fcsr.w -> fcsr.rdata),
    0x100 -> (mstatus.wAliasSstatus -> mstatus.sstatus),
    0x300 -> (mstatus.w -> mstatus.rdata),
    0x305 -> (mtvec.w -> mtvec.rdata),
    0x644 -> (hip.w -> hip.rdata),
  )

  val csrMods = Seq(
    fcsr,
    mstatus,
    mtvec,
    hip,
  )

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