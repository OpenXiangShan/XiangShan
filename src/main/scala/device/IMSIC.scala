package device

import chisel3._
import chisel3.util._
import chisel3.experimental._

class IMSIC(
  NumIRFiles: Int = 7,
  NumIRSrc: Int = 12,
  NumHart: Int = 2,
  XLEN: Int = 64,
) extends Module {
  // has default clock and reset
  val i = IO(Input(new Bundle {
    val setIpNum = ValidIO(UInt(log2Up(NumIRSrc).W))
    val hartId = UInt(log2Up(NumHart).W)
    val csr = new Bundle {
      val addr = ValidIO(new Bundle {
        val addr = UInt(12.W)
        val prvm = UInt(2.W)
        val v = UInt(1.W)
      })
      val vgein = UInt(6.W)
      val mClaim = Bool()
      val sClaim = Bool()
      val vsClaim = Bool()
      val wdata = ValidIO(new Bundle{
        val data = UInt(XLEN.W)
      })
    }
  }))
  val o = Output(new Bundle {
    val csr = new Bundle {
      val rdata = ValidIO(new Bundle {
        val rdata = UInt(XLEN.W)
        val illegal = Bool()
      })
    }
    val mtopei = ValidIO(UInt(32.W))
    val stopei = ValidIO(UInt(32.W))
    val vstopei = ValidIO(UInt(32.W))
  })

  val imsicTop = Module(new imsic_csr_top)
}

class imsic_csr_top extends BlackBox(Map(
  "NR_INTP_FILES" -> 7,
  "NR_HARTS"      -> 1,
  "XLEN"          -> 64,
  "NR_SRC"        -> 32,
)) {
  val csr_clk = Input(Clock())
  val csr_rstn = Input(AsyncReset())
  // Todo: more bundles
}

