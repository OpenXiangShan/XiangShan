package device

import chisel3._
import chisel3.util._
import chisel3.experimental._

class IMSIC(
  NumIRFiles: Int = 7,
  NumHart: Int = 64,
  XLEN: Int = 64,
  NumIRSrc: Int = 256,
) extends Module {
  private val NR_SRC_WIDTH = log2Up(NumIRSrc)
  private val NR_HARTS_WIDTH = log2Up(NumHart)
  private val INTP_FILE_WIDTH = log2Up(NumIRFiles)
  private val MSI_INFO_WIDTH = NR_HARTS_WIDTH + INTP_FILE_WIDTH + NR_SRC_WIDTH

  // has default clock and reset
  val i = IO(Input(new Bundle {
    val msiInfo           = ValidIO(new MsiInfoBundle(NumIRFiles = NumIRFiles, NumHart = NumHart, NumIRSrc = NumIRSrc))
    val hartId            = UInt(log2Up(NumHart).W)
    val csr = new Bundle {
      val addr = ValidIO(new Bundle {
        val addr = UInt(12.W)
        val prvm = UInt(2.W)
        val v    = UInt(1.W)
      })
      val vgein = UInt(6.W)
      val mClaim  = Bool()
      val sClaim  = Bool()
      val vsClaim = Bool()
      val wdata = ValidIO(new Bundle{
        val op    = UInt(2.W)
        val data  = UInt(XLEN.W)
      })
    }
  }))
  val o = IO(Output(new Bundle {
    val csr = new Bundle {
      val rdata = ValidIO(new Bundle {
        val rdata   = UInt(XLEN.W)
        val illegal = Bool()
      })
    }
    val mtopei  = ValidIO(UInt(32.W))
    val stopei  = ValidIO(UInt(32.W))
    val vstopei = ValidIO(UInt(32.W))
  }))

  val imsicTop = Module(new imsic_csr_top(
    NumIRFiles = this.NumIRFiles,
    NumHart = this.NumHart,
    XLEN = this.XLEN,
    NumIRSrc = this.NumIRSrc,
  ))

  imsicTop.io.csr_clk         := clock
  imsicTop.io.csr_rstn        := reset
  imsicTop.io.hart_id         := i.hartId
  imsicTop.io.i.msi_info_vld  := i.msiInfo.valid
  imsicTop.io.i.msi_info      := i.msiInfo.bits.info
  imsicTop.io.i.csr.addr_vld  := i.csr.addr.valid
  imsicTop.io.i.csr.addr      := i.csr.addr.bits.addr
  imsicTop.io.i.csr.priv_lvl  := i.csr.addr.bits.prvm
  imsicTop.io.i.csr.v         := i.csr.addr.bits.v
  imsicTop.io.i.csr.vgein     := i.csr.vgein
  imsicTop.io.i.csr.claim     := Cat(i.csr.vsClaim, i.csr.sClaim, i.csr.mClaim)
  imsicTop.io.i.csr.wdata_vld := i.csr.wdata.valid
  imsicTop.io.i.csr.wdata     := i.csr.wdata.bits.data
  imsicTop.io.i.csr.wdata_op  := i.csr.wdata.bits.op

  o.csr.rdata.valid        := imsicTop.io.o.csr.rdata_vld
  o.csr.rdata.bits.rdata   := imsicTop.io.o.csr.rdata
  o.csr.rdata.bits.illegal := imsicTop.io.o.csr.illegal
  o.mtopei.valid           := imsicTop.io.o.irq(0)
  o.stopei.valid           := imsicTop.io.o.irq(1)
  o.vstopei.valid          := imsicTop.io.o.irq(2)
  o.mtopei.bits            := imsicTop.io.o.mtopei
  o.stopei.bits            := imsicTop.io.o.stopei
  o.vstopei.bits           := imsicTop.io.o.vstopei
}

class imsic_csr_top(
  NumIRFiles: Int = 7,
  XLEN: Int = 64,
  NumIRSrc: Int = 256,
  NumHart: Int = 4,
  EidVldDlyNum: Int = 0,
) extends BlackBox(Map(
  "NR_INTP_FILES"  -> NumIRFiles,
  "NR_HARTS"       -> NumHart,
  "XLEN"           -> XLEN,
  "NR_SRC"         -> NumIRSrc,
  "EID_VLD_DLY_NUM"-> EidVldDlyNum,
)) with HasBlackBoxResource {
  private val NR_SRC_WIDTH = log2Up(NumIRSrc)
  private val NR_HARTS_WIDTH = log2Up(NumHart)
  private val INTP_FILE_WIDTH = log2Up(NumIRFiles)
  private val MSI_INFO_WIDTH = NR_HARTS_WIDTH + INTP_FILE_WIDTH + NR_SRC_WIDTH

  val io = IO(new Bundle {
    val csr_clk = Input(Clock())
    val csr_rstn = Input(Reset())
    val hart_id = Input(UInt(NR_HARTS_WIDTH.W))

    val i = Input(new Bundle {
      val msi_info = UInt(MSI_INFO_WIDTH.W)
      val msi_info_vld = Bool()
      val csr = new Bundle {
        val addr_vld = Bool()
        val addr = UInt(12.W)
        val priv_lvl = UInt(2.W)
        val v = UInt(1.W)
        val vgein = UInt(6.W)
        val claim = UInt(3.W)
        val wdata_vld = Bool()
        val wdata_op = UInt(2.W)
        val wdata = UInt(XLEN.W)
      }
    })

    val o = Output(new Bundle {
      val csr = new Bundle {
        val rdata_vld = Bool()
        val rdata = UInt(XLEN.W)
        val illegal = Bool()
      }
      val irq = UInt(3.W)
      val mtopei = UInt(32.W)
      val stopei = UInt(32.W)
      val vstopei = UInt(32.W)
    })
  })

  addResource("/vsrc/imsic/imsic_csr_top.v")
  addResource("/vsrc/imsic/imsic_csr_gate.v")
  addResource("/vsrc/imsic/imsic_csr_reg.v")
  addResource("/vsrc/cmip_dff_sync.sv")
}

class MsiInfoBundle(
  NumIRFiles: Int = 7,
  NumHart: Int = 64,
  NumIRSrc: Int = 256,
) extends Bundle {
  private val NR_SRC_WIDTH = log2Up(NumIRSrc)
  private val NR_HARTS_WIDTH = log2Up(NumHart)
  private val INTP_FILE_WIDTH = log2Up(NumIRFiles)
  private val MSI_INFO_WIDTH = NR_HARTS_WIDTH + INTP_FILE_WIDTH + NR_SRC_WIDTH

  val info = UInt(MSI_INFO_WIDTH.W)
}
