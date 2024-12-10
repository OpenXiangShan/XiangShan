package device

import chisel3._
import chisel3.util._
import chisel3.experimental._

class IMSIC(
  NumVSIRFiles: Int = 5,
  NumHart: Int = 1,
  XLEN: Int = 64,
  NumIRSrc: Int = 256,
) extends Module {
  private val NumIRFiles: Int = /*M*/ 1 + /*S*/ 1 + NumVSIRFiles
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
    val meip  = Bool()
    val seip  = Bool()
    val vseip = UInt(NumVSIRFiles.W)
    val mtopei  = UInt(32.W)
    val stopei  = UInt(32.W)
    val vstopei = UInt(32.W)
  }))

  val imsicTop = Module(new imsic_csr_top(
    NumIRFiles = this.NumIRFiles,
    NumHart = this.NumHart,
    XLEN = this.XLEN,
    NumIRSrc = this.NumIRSrc,
  ))

  imsicTop.io.csr_clk         := clock
  imsicTop.io.csr_rst         := reset
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
  o.meip                   := imsicTop.io.o.irq(0)
  o.seip                   := imsicTop.io.o.irq(1)
  o.vseip                  := imsicTop.io.o.irq(NumIRFiles - 1, 2)
  o.mtopei                 := imsicTop.io.o.mtopei
  o.stopei                 := imsicTop.io.o.stopei
  o.vstopei                := imsicTop.io.o.vstopei
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
    val csr_rst = Input(Reset())
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
      val irq = UInt(NumIRFiles.W)
      val mtopei = UInt(32.W)
      val stopei = UInt(32.W)
      val vstopei = UInt(32.W)
    })
  })
  addResource("/aia/src/rtl/imsic/imsic_csr_top.sv")
  addResource("/aia/src/rtl/imsic/imsic_csr_gate.sv")
  addResource("/aia/src/rtl/imsic/imsic_csr_reg.sv")
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

class PMU extends Module {
  val io = IO(new Bundle {
    // Inputs
    val coreActive = Input(Bool())  // Core active signal
    val coreWakeReq = Input(Bool()) // Core wake request signal

    // Outputs
    val coreReset = Output(Bool())  // Core reset output (active low)
    val coreIsolate = Output(Bool()) // Core isolation output (active low)
    val coreClken = Output(Bool())  // Core clock enable output (active low)
    val coreHWstat = Output(UInt(8.W)) // Core hardware status output (POFF, PON, etc.)
  })

  // FSM states
  val POFF     = "b00000001".U(8.W)  
  val POFF_CLK = "b00000010".U(8.W)  
  val POFF_ISO = "b00000100".U(8.W)  
  val POFF_RST = "b00001000".U(8.W)  
  val PON_ISO  = "b00100000".U(8.W)  
  val PON_RST  = "b01000000".U(8.W)  
  val PON      = "b10000000".U(8.W)  

  // Define the FSM
  val sPOFF :: sPOFF_CLK :: sPOFF_ISO :: sPOFF_RST :: sPON_ISO :: sPON_RST :: sPON :: Nil = Enum(7) 
  val state = RegInit(sPON)

  // Configurable delays in clock cycles (use appropriate values for custom design)
  val PON_DLY_CLKEN_ISO = 5.U(8.W) // PowerOn Delay for clockEn -> Isolate
  val PON_DLY_ISO_RST = 5.U(8.W) // PowerOn Delay for Isolate -> Reset 
  val PON_DLY_ISO_CLKEN = 5.U(8.W) // PowerOff delay for Isolate -> clockEn
  val PON_DLY_CLKEN_RST = 5.U(8.W) // PowerOff delay for clockEn -> Reset

  // Delay counters (in clock cycles)
  val delayCounter = RegInit(0.U(8.W)) 
  val delayMax = 10.U 

  // Control signals (active low)
  val coreResetReg = RegInit(true.B) 
  val coreIsolateReg = RegInit(true.B)
  val coreClkenReg = RegInit(true.B)

  // Output control signals
  io.coreReset := coreResetReg
  io.coreIsolate := coreIsolateReg
  io.coreClken := coreClkenReg
  io.coreHWstat := state

  // FSM logic for power on/off sequence
  switch(state) {
    //Power Off Sequence
    is(sPON) {
      when(io.coreActive === false.B) {
        state := sPOFF_CLK
      }
    }
    is(sPOFF_CLK) {
      // Step 1: Set coreClken = 0
      coreClkenReg := false.B
      delayCounter := PON_DLY_CLKEN_ISO
      state := sPOFF_ISO
    }
    is(sPOFF_ISO) {
      when(delayCounter === 0.U) {
        // Step 2: Set coreIsolate =0 after PDN_DLY_CLKEN_ISO
        delayCounter := PON_DLY_ISO_RST
        coreIsolateReg := false.B
        state := sPOFF_RST
      }
    }
    is(sPOFF_RST) {
      when(delayCounter === 0.U) {
        // Step 3: Set coreReset =0 after PDN_DLY_ISO_RST
        delayCounter := 0.U
        coreResetReg := false.B
        state := sPOFF
      }
    }

    //Power On Sequence
    is(sPOFF) {
      // Power on sequence (coreWakeReq = 1)
      when(io.coreWakeReq === true.B) {
        // Step 1: Set coreIsolate = 1
        coreIsolateReg := true.B
        delayCounter := PON_DLY_ISO_CLKEN
        state := sPON_ISO 
      }
    }
    is(sPON_ISO) {
      when(delayCounter === 0.U) {
        // Step 2: Set coreClken =1 after PON_DLY_CLKEN_ISO
        delayCounter := PON_DLY_CLKEN_RST
        coreClkenReg := true.B
        state := sPON_RST
      }
    }
    is(sPON_RST) {
      when(delayCounter === 0.U) {
        // Step 3: Set coreReset =1 after PON_DLY_CLKEN_RST
        delayCounter := 0.U
        coreResetReg := true.B
        state := sPON
      }
    }
  }

  // Delay counter management
  when(delayCounter > 0.U) {
    delayCounter := delayCounter - 1.U
  }

  //Physical control signals
  val power_off = (state === sPOFF)
  val nPOWRUP = power_off
  val nISOLATE = ~power_off
}
