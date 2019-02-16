package memory

import chisel3._
import chisel3.util._

class MemAddrBundle extends Bundle {
  val addr = Output(UInt(32.W))
  val size = Output(UInt(3.W))
}

class MemDataBundle(val dataBits: Int) extends Bundle {
  val data = Output(UInt(dataBits.W))
}

class MemMaskDataBundle(dataBits: Int) extends MemDataBundle(dataBits) {
  val mask = Output(UInt((dataBits / 8).W))
}

class MemIO(val dataBits: Int = 32) extends Bundle {
  val a = Decoupled(new MemAddrBundle)
  val r = Flipped(Decoupled(new MemDataBundle(dataBits)))
  val w = Valid(new MemMaskDataBundle(dataBits))

  def toAHBLite(): AHBLiteIO = {
    val ahb = Wire(new AHBLiteIO)
    ahb.haddr := this.a.bits.addr
    ahb.hsize := this.a.bits.size
    ahb.hburst := 0.U
    ahb.hprot := "b0011".U
    ahb.htrans := Mux(this.a.valid, "b10".U, "b00".U)
    ahb.hwdata := RegNext(this.w.bits.data)
    ahb.hwrite := this.w.valid

    this.r.bits.data := ahb.hrdata

    val s_idle :: s_read_phase :: s_write_phase :: Nil = Enum(3)
    val state = RegInit(s_idle)

    this.a.ready := false.B
    switch (state) {
      is (s_idle) {
        when (ahb.htrans === "b10".U && ahb.hready) {
          when (ahb.hwrite) {
            state := s_write_phase
          }
          .otherwise {
            state := s_read_phase
            this.a.ready := true.B
          }
        }
      }
      is (s_read_phase) {
        when (ahb.hready) { state := s_idle }
      }
      is (s_write_phase) {
        when (ahb.hready) {
          state := s_idle
          this.a.ready := true.B
        }
      }
    }

    this.r.valid := (state === s_read_phase) && ahb.hready

    ahb
  }
}

class AHBLiteIO extends Bundle {
  val haddr = Output(UInt(32.W))
  val hsize = Output(UInt(3.W))
  val hburst = Output(UInt(3.W))
  val hprot = Output(UInt(4.W))
  val htrans = Output(UInt(2.W))
  val hready = Input(Bool())

  val hwdata = Output(UInt(32.W))
  val hwrite = Output(Bool())

  val hrdata = Input(UInt(32.W))
  val hresp = Input(Bool())
}
