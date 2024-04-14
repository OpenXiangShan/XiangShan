package device

import chisel3._
import chisel3.util._
import chisel3.experimental._

class IMSIC(
  NumIRFiles: Int = 7,
  NumHart: Int = 2,
  XLEN: Int = 64,
  NumIRSrc: Int = 12,
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

  imsicTop.csr_clk := clock
  imsicTop.csr_rstn := reset
}

class imsic_csr_top(
  NumIRFiles: Int = 7,
  NumIRSrc: Int = 12,
  NumHart: Int = 2,
  XLEN: Int = 64,
) extends BlackBox(Map(
  "NR_INTP_FILES" -> NumIRFiles,
  "NR_HARTS"      -> NumHart,
  "XLEN"          -> XLEN,
  "NR_SRC"        -> NumIRSrc,
)) {
  val csr_clk = Input(Clock())
  val csr_rstn = Input(Reset())
  // Todo: more bundles
}

