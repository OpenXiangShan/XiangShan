/***************************************************************************************
* Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package device

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4.HasAXI4ControlRegMap
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts.HasInterruptSources
import freechips.rocketchip.regmapper._
import difftest.UARTIO

case class UART16550Params(
  address:    BigInt,
  beatBytes:  Int     = 8,
  fifoDepth:  Int     = 16,
  baudBase:   Int     = 1843200,
  clockFreq:  BigInt  = 50000000L
)

class UART16550Port extends Bundle {
  val uart = new UARTIO
  val tx   = Decoupled(UInt(8.W))           // Optional decoupled sink for transmitted chars
  val rx   = Flipped(Decoupled(UInt(8.W)))  // Optional decoupled source for incoming chars
}

class AXI4UART16550(params: UART16550Params)(implicit p: Parameters)
  extends RegisterRouter(
    RegisterRouterParams(
      name      = "uart16550",
      compat    = Seq("ns16550a"),
      base      = params.address,
      size      = 0x20,
      beatBytes = params.beatBytes)
  )
  with HasInterruptSources
  with HasAXI4ControlRegMap {

  def nInterrupts: Int = 1

  private val fifoDepth = params.fifoDepth

  lazy val module = new AXI4UART16550Imp
    
  class AXI4UART16550Imp extends LazyModuleImp(this) {
    val rxTriggerLut = VecInit(1.U, 4.U, 8.U, 14.U)
    val io = IO(new UART16550Port)

    // Registers
    val rbr      = RegInit(0.U(8.W))
    val thr      = RegInit(0.U(8.W))
    val thrValid = RegInit(false.B)
    val ier      = RegInit(0.U(8.W))
    val lcr      = RegInit(0.U(8.W))
    val mcr      = RegInit("h08".U(8.W)) // match ns16550a reset: OUT2 set
    val lsrReg   = RegInit("h60".U(8.W)) // TEMT|THRE set after reset
    val msrReg   = RegInit("hB0".U(8.W)) // DCD/DSR/CTS high, delta bits clear
    val scr      = RegInit(0.U(8.W))
    val dll      = RegInit(0.U(8.W))
    val dlm      = RegInit(0.U(8.W))
    val fcrSticky = RegInit(0.U(8.W)) // FE + fifo indicator

    val thrIPending    = RegInit(false.B)
    val timeoutPending = RegInit(false.B)

    // FIFOs
    val recvFifo = Module(new Queue(UInt(8.W), fifoDepth))
    val xmitFifo = Module(new Queue(UInt(8.W), fifoDepth))

    // Flush controls
    val rxFlush = RegInit(false.B)
    val txFlush = RegInit(false.B)

    val lsrNext = lsrReg.asBools.map(x => WireDefault(x))
    val msrNext = WireDefault("hB0".U(8.W)) // default: DCD=1, RI=0, DSR=1, CTS=1, delta bits clear
    def setLsr(bits: Int): Unit = (0 until 8).foreach { i =>
      if (((bits >> i) & 1) != 0) lsrNext(i) := true.B
    }
    def clrLsr(bits: Int): Unit = (0 until 8).foreach { i =>
      if (((bits >> i) & 1) != 0) lsrNext(i) := false.B
    }

    // Handshake helpers
    val recvEnq  = WireInit(false.B)
    val recvData = WireDefault(0.U(8.W))
    val recvPop  = WireInit(false.B)
    recvFifo.io.enq.valid := recvEnq
    recvFifo.io.enq.bits  := recvData
    recvFifo.io.deq.ready := recvPop || rxFlush
    when (rxFlush && recvFifo.io.count === 0.U) { rxFlush := false.B }

    val xmitPop = WireInit(false.B)
    xmitFifo.io.enq.valid := false.B
    xmitFifo.io.enq.bits  := 0.U
    xmitFifo.io.deq.ready := xmitPop || txFlush
    when (txFlush && xmitFifo.io.count === 0.U) { txFlush := false.B }

    // Baud / timing estimation (cycle based, coarse)
    val divisor     = Cat(dlm, dll)
    val divisorWide = Mux(divisor === 0.U, 1.U, divisor)
    val baud        = (params.baudBase.U + (divisorWide >> 1)) / divisorWide
    val frameBits   = 1.U + (lcr(1,0) + 5.U) + Mux(lcr(2), 2.U, 1.U) + Mux(lcr(3), 1.U, 0.U)
    // Always set this to 1 to avoid large delays in simulation
    val charCycles  = 1.U // ((params.clockFreq.U(32.W) / baud) * frameBits)(31,0)
    val timeoutReload = charCycles << 2
    val timeoutCnt  = RegInit(0.U(32.W))

    // TX datapath
    val txBusy    = RegInit(false.B)
    val txCounter = RegInit(0.U(32.W))
    val tsr       = Reg(UInt(8.W))

    // Derived flags
    val fifoEnabled = fcrSticky(0)
    val recvFifoITL = RegInit(1.U(4.W))
    val canReceive = Mux(fifoEnabled, recvFifo.io.count =/= fifoDepth.U, !lsrReg(0))

    // Push into RX path (used for loopback and external RX)
    def pushRx(data: UInt): Unit = {
      when (fifoEnabled) {
        when (recvFifo.io.count === fifoDepth.U) {
          setLsr(2) // OE
        }.otherwise {
          recvEnq := true.B
          recvData := data
        }
      }.otherwise {
        when (lsrReg(0)) { setLsr(2) }.otherwise { rbr := data }
      }
      setLsr(1) // DR
      timeoutCnt := timeoutReload
      timeoutPending := false.B
    }

    // External RX hookup
    io.rx.ready := canReceive
    when (io.rx.fire && canReceive) {
      pushRx(io.rx.bits)
    }

    // THR write handler
    def writeTHR(data: UInt): Unit = {
      when (fifoEnabled) {
        when (xmitFifo.io.count === fifoDepth.U) { xmitPop := true.B }
        xmitFifo.io.enq.valid := true.B
        xmitFifo.io.enq.bits  := data
      }.otherwise {
        thr := data
        thrValid := true.B
      }
      thrIPending := false.B
      // Clear THRE (bit5) and TEMT (bit6) when new data enters THR/FIFO
      clrLsr(0b1100000)
    }

    // Start TX when idle and data exists
    val txHasData = Mux(fifoEnabled, xmitFifo.io.deq.valid, thrValid)
    when (!txBusy && txHasData) {
      tsr := Mux(fifoEnabled, xmitFifo.io.deq.bits, thr)
      txBusy := true.B
      txCounter := Mux(charCycles === 0.U, 1.U, charCycles)
      when (fifoEnabled) { xmitPop := true.B } .otherwise { thrValid := false.B }
      clrLsr(0b1100000)
    }

    // TX progress
    val txFire = txBusy && txCounter === 0.U
    when (txBusy) {
      when (txCounter === 0.U) { txBusy := false.B } .otherwise { txCounter := txCounter - 1.U }
    }

    // Emit character when done
    io.tx.valid := txFire && !mcr(4)
    io.tx.bits  := tsr
    io.uart.out.valid := txFire
    io.uart.out.ch    := tsr

    when (txFire) {
      when (mcr(4)) { pushRx(tsr) }
      val txEmptyNow = Mux(fifoEnabled, xmitFifo.io.count === 0.U, !thrValid)
      when (txEmptyNow) {
        setLsr(0x20) // THRE only when FIFO/THR empty
        setLsr(0x40) // TEMT when TSR drained
      }
      when (!thrIPending && lsrReg(5)) { thrIPending := true.B }
    }

    // TEMT/THRE maintenance when idle
    val txEmpty = !txBusy && Mux(fifoEnabled, xmitFifo.io.count === 0.U, !thrValid)
    when (!txBusy && txEmpty) { setLsr(0x60) }

    // Timeout counting for CTI
    when (timeoutCnt =/= 0.U) {
      timeoutCnt := timeoutCnt - 1.U
      when (timeoutCnt === 1.U && recvFifo.io.count =/= 0.U) { timeoutPending := true.B }
    }

    // Modem status (minimal model)
    when (mcr(4)) {
      // Loopback mapping per 16550A: OUT2->DCD, OUT1->RI, RTS->CTS, DTR->DSR; delta bits remain 0
      msrNext := Cat(mcr(3), mcr(2), mcr(0), mcr(1), 0.U(4.W))
    }

    // Interrupt generation
    val lsrIntAny = lsrNext(4) || lsrNext(3) || lsrNext(2) || lsrNext(1)
    val rdiHit = lsrNext(0) && (!fifoEnabled || (recvFifo.io.count >= recvFifoITL))
    val msiHit = (msrNext & 0x0f.U) =/= 0.U
    val irqSel = WireInit(1.U(8.W))
    when (ier(2) && lsrIntAny) {
      irqSel := 0x06.U
    }.elsewhen (ier(0) && timeoutPending) {
      irqSel := 0x0c.U
    }.elsewhen (ier(0) && rdiHit) {
      irqSel := 0x04.U
    }.elsewhen (ier(1) && thrIPending) {
      irqSel := 0x02.U
    }.elsewhen (ier(3) && msiHit) {
      irqSel := 0x00.U
    }

    val fifoStatus = Mux(fifoEnabled, "hC0".U(8.W), 0.U) // IIR[7:6] reflect FIFO enabled
    val iirVal = fifoStatus | irqSel
    interrupts.head := irqSel =/= 1.U

    // FCR write helper
    def writeFCR(data: UInt): Unit = {
      when ((data ^ fcrSticky)(0)) {
        rxFlush := true.B; txFlush := true.B
      }
      when (data(1)) { // RFR
        rxFlush := true.B
        clrLsr(1 | 0x10)
        timeoutPending := false.B
        timeoutCnt := 0.U
      }
      when (data(2)) { // XFR
        txFlush := true.B
        setLsr(0x20)
        thrIPending := true.B
      }
      fcrSticky := data & "hC9".U
      recvFifoITL := rxTriggerLut(data(7,6))
    }

    // Regmap definitions (byte offsets 0..7)
    val reg0 = RegField(8,
      RegReadFn { en =>
        val data = WireDefault(0.U(8.W))
        when (lcr(7)) {
          data := dll
        }.otherwise {
          when (fifoEnabled) {
            data := Mux(recvFifo.io.deq.valid, recvFifo.io.deq.bits, 0.U)
            when (en && recvFifo.io.deq.valid) {
              recvPop := true.B
              when (recvFifo.io.count === 1.U) { clrLsr(1) }
              timeoutPending := false.B
              timeoutCnt := timeoutReload // restart character-timeout window after each read
            }
          }.otherwise {
            data := rbr
            when (en) { clrLsr(1); timeoutPending := false.B }
          }
        }
        (true.B, data)
      },
      RegWriteFn { (valid, data) =>
        when (valid) {
          when (lcr(7)) { dll := data } .otherwise { writeTHR(data) }
        }
        true.B
      })

    val reg1 = RegField(8,
      RegReadFn { en => (true.B, Mux(lcr(7), dlm, ier)) },
      RegWriteFn { (valid, data) =>
        when (valid) {
          when (lcr(7)) { dlm := data }
            .otherwise {
              val changed = (ier ^ data)(3,0)
              ier := data & 0x0f.U
              when (changed(1)) { thrIPending := data(1) && lsrReg(5) }
            }
        }
        true.B
      })

    val reg2 = RegField(8,
      RegReadFn { en =>
        when (en && (iirVal(2,0) === 2.U)) { thrIPending := false.B }
        (true.B, iirVal)
      },
      RegWriteFn { (valid, data) =>
        when (valid) { writeFCR(data) }
        true.B
      },
      RegFieldDesc("iir_fcr", "IIR/FCR"))

    val reg3 = RegField(8, lcr, RegFieldDesc("lcr", "Line Control"))
    val reg4 = RegField(8, mcr, RegFieldDesc("mcr", "Modem Control"))

    val reg5 = RegField(8,
      RegReadFn { en =>
        when (en) { clrLsr(0x10 | 0x02) }
        (true.B, lsrReg)
      },
      RegWriteFn { (_, _) => true.B },
      RegFieldDesc("lsr", "Line Status"))

    val reg6 = RegField(8,
      RegReadFn { en =>
        when (en && !mcr(4) && ((msrReg & 0x0f.U) =/= 0.U)) { msrReg := msrReg & 0xf0.U }
        (true.B, msrReg)
      },
      RegWriteFn { (_, _) => true.B },
      RegFieldDesc("msr", "Modem Status"))

    val reg7 = RegField(8, scr)

    regmap(
      0x00 -> Seq(reg0),
      0x04 -> Seq(reg1),
      0x08 -> Seq(reg2),
      0x0C -> Seq(reg3),
      0x10 -> Seq(reg4),
      0x14 -> Seq(reg5),
      0x18 -> Seq(reg6),
      0x1C -> Seq(reg7)
    )

    // Default outputs (no pull-based RX source on UARTIO)
    io.uart.in.valid := false.B

    lsrReg := VecInit(lsrNext).asUInt
    msrReg := msrNext
  }
}
