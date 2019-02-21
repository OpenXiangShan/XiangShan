package memory

import chisel3._
import chisel3.util._

object AHBParameters {
  // These are all fixed by the AHB standard:
  val transBits = 2
  val burstBits = 3
  val protBits  = 4
  val sizeBits  = 3  // 8*2^s

  def TRANS_IDLE   = 0.U(transBits.W) // No transfer requested, not in a burst
  def TRANS_BUSY   = 1.U(transBits.W) // No transfer requested, in a burst
  def TRANS_NONSEQ = 2.U(transBits.W) // First (potentially only) request in a burst
  def TRANS_SEQ    = 3.U(transBits.W) // Following requests in a burst

  def BURST_SINGLE = 0.U(burstBits.W) // Single access (no burst)
  def BURST_INCR   = 1.U(burstBits.W) // Incrementing burst of arbitrary length, not crossing 1KB
  def BURST_WRAP4  = 2.U(burstBits.W) // 4-beat wrapping burst
  def BURST_INCR4  = 3.U(burstBits.W) // 4-beat incrementing burst
  def BURST_WRAP8  = 4.U(burstBits.W) // 8-beat wrapping burst
  def BURST_INCR8  = 5.U(burstBits.W) // 8-beat incrementing burst
  def BURST_WRAP16 = 6.U(burstBits.W) // 16-beat wrapping burst
  def BURST_INCR16 = 7.U(burstBits.W) // 16-beat incrementing burst

  val maxTransfer = 16

  def RESP_OKAY  = false.B
  def RESP_ERROR = true.B

  def PROT_DATA        = 1.U(protBits.W)
  def PROT_PRIVILEDGED = 2.U(protBits.W)
  def PROT_BUFFERABLE  = 4.U(protBits.W)
  def PROT_CACHEABLE   = 8.U(protBits.W)
  def PROT_DEFAULT = PROT_DATA | PROT_PRIVILEDGED
}

class AHBLiteIO extends Bundle {
  val haddr  = Output(UInt(32.W))
  val hsize  = Output(UInt(AHBParameters.sizeBits.W))
  val hburst = Output(UInt(AHBParameters.burstBits.W))
  val hprot  = Output(UInt(AHBParameters.protBits.W))
  val hsel   = Output(Bool())
  val htrans = Output(UInt(AHBParameters.transBits.W))
  val hready = Output(Bool())
  val hreadyout = Input(Bool())

  val hwdata = Output(UInt(32.W))
  val hwrite = Output(Bool())

  val hrdata = Input(UInt(32.W))
  val hresp  = Input(Bool())
}

class MemIO2AHBLiteConverter extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new MemIO)
    val out = new AHBLiteIO
  })

  val mem = io.in
  val ahb = io.out
  ahb.haddr := mem.a.bits.addr
  ahb.hsize := mem.a.bits.size
  ahb.hburst := AHBParameters.BURST_SINGLE
  ahb.hprot := AHBParameters.PROT_DEFAULT
  ahb.htrans := Mux(mem.a.valid, AHBParameters.TRANS_NONSEQ, AHBParameters.TRANS_IDLE)
  ahb.hwdata := RegNext(mem.w.bits.data)
  ahb.hwrite := mem.w.valid
  ahb.hready := ahb.hreadyout
  ahb.hsel := true.B

  mem.r.bits.data := ahb.hrdata

  val s_idle :: s_read_phase :: s_write_phase :: Nil = Enum(3)
  val state = RegInit(s_idle)

  mem.a.ready := false.B
  switch (state) {
    is (s_idle) {
      when (ahb.htrans === AHBParameters.TRANS_NONSEQ && ahb.hreadyout) {
        when (ahb.hwrite) {
          state := s_write_phase
        }
          .otherwise {
            state := s_read_phase
            mem.a.ready := true.B
          }
      }
    }
    is (s_read_phase) {
      when (ahb.hreadyout) { state := s_idle }
    }
    is (s_write_phase) {
      when (ahb.hreadyout) {
        state := s_idle
        mem.a.ready := true.B
      }
    }
  }

  mem.r.valid := (state === s_read_phase) && ahb.hreadyout
}
