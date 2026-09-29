package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import difftest.DifftestModule
import org.chipsalliance.cde.config.Parameters
import xiangshan.Redirect
import xiangshan.backend.decode.opcode.Opcode.FDivOpcodes
import xiangshan.backend.vector.fu.{Func, VecFuConfig}

private[wrapper] class FDivSqrtDpiBlackBox extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Input(UInt(FDivSqrtDpiBlackBox.InputWidth.W))
    val valid = Input(Bool())
    val out = Output(UInt(FDivSqrtDpiBlackBox.OutputWidth.W))
  })

  override def desiredName: String = "FDivSqrtDpiBlackBox"

  private val dpiFuncName = "fdivsqrt_dpic_call"
  setInline(
    s"$desiredName.v",
    s"""
       |module $desiredName (
       |  input             clock,
       |  input             reset,
       |  input  [${FDivSqrtDpiBlackBox.InputWidth - 1}:0] in,
       |  input             valid,
       |  output [${FDivSqrtDpiBlackBox.OutputWidth - 1}:0] out
       |);
       |  reg [${FDivSqrtDpiBlackBox.OutputWidth - 1}:0] dpi_out;
       |  reg [${FDivSqrtDpiBlackBox.OutputWidth - 1}:0] dpi_result;
       |`ifndef SYNTHESIS
       |  import "DPI-C" function void $dpiFuncName(
       |    input bit [${FDivSqrtDpiBlackBox.InputWidth - 1}:0] in_bits,
       |    output bit [${FDivSqrtDpiBlackBox.OutputWidth - 1}:0] out_bits
       |  );
       |`endif
       |  always @(posedge clock or posedge reset) begin
       |    if (reset) begin
       |      dpi_out <= '0;
       |      dpi_result = '0;
       |    end else if (valid) begin
       |      dpi_result = '0;
       |`ifndef SYNTHESIS
       |      $dpiFuncName(in, dpi_result);
       |`endif
       |      dpi_out <= dpi_result;
       |    end
       |  end
       |  assign out = dpi_out;
       |endmodule
       |""".stripMargin,
  )

  DifftestModule.createCppDPICModule(
    dpiFuncName,
    s"""
       |extern "C" void fdivsqrt_dpic(
       |  const uint32_t *in_bits,
       |  uint32_t *out_bits
       |);
       |
       |extern "C" void $dpiFuncName(
       |  const uint32_t *in_bits,
       |  uint32_t *out_bits
       |) {
       |  fdivsqrt_dpic(in_bits, out_bits);
       |}
       |""".stripMargin,
  )
}

private[wrapper] object FDivSqrtDpiBlackBox {
  // 64-bit operands plus eight-bit fields for DPI's packed frame alignment.
  val InputWidth = 192
  val OutputWidth = 96
}

private[wrapper] class FDivSqrtDpiIO extends Bundle {
  val valid = Input(Bool())
  val fpFormat = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val isSqrt = Input(Bool())
  val opa = Input(UInt(64.W))
  val opb = Input(UInt(64.W))
  val opaCanonicalNan = Input(Bool())
  val opbCanonicalNan = Input(Bool())
  val result = Output(UInt(64.W))
  val fflags = Output(UInt(5.W))
}

private[wrapper] class FDivSqrtDpi(implicit p: Parameters) extends Module {
  val io = IO(new FDivSqrtDpiIO)

  private val packedIn = Cat(
    0.U(16.W),
    0.U(7.W), io.opbCanonicalNan,
    0.U(7.W), io.opaCanonicalNan,
    0.U(7.W), io.isSqrt,
    0.U(5.W), io.rm,
    0.U(6.W), io.fpFormat,
    0.U(7.W), io.valid,
    io.opb,
    io.opa,
  )

  private val dpi = Module(new FDivSqrtDpiBlackBox)
  dpi.io.clock := clock
  dpi.io.reset := reset
  dpi.io.in := packedIn
  dpi.io.valid := io.valid

  io.result := dpi.io.out(63, 0)
  io.fflags := dpi.io.out(68, 64)
}

private[wrapper] class FDivSqrtFltDpiPipe(cfg: VecFuConfig, latency: Int = FDivOpcodes.FixedLatency - 1)(implicit p: Parameters) extends Module {
  require(latency >= 4)
  implicit val _cfg: VecFuConfig = cfg
  val io = IO(new Bundle {
    val flush = Flipped(ValidIO(new Redirect))
    val in = Flipped(ValidIO(new Func.OutUop))
    val out = ValidIO(new Func.OutUop)
    val wakeUp = ValidIO(UInt(in.bits.ctrl.pdest.getWidth.W))
  })

  private val valid = RegInit(VecInit.fill(latency)(false.B))
  private val data = Reg(Vec(latency, new Func.OutUop))
  private val flushed = Wire(Vec(latency, Bool()))
  flushed := VecInit(valid.zip(data).map { case (v, d) => v && d.ctrl.robIdx.needFlush(io.flush) })

  for (i <- 0 until latency) {
    val previousValid = if (i == 0) io.in.valid else valid(i - 1)
    val previousData = if (i == 0) io.in.bits else data(i - 1)
    val previousFlushed = if (i == 0) io.in.bits.ctrl.robIdx.needFlush(io.flush) else flushed(i - 1)
    valid(i) := previousValid && !previousFlushed
    when(previousValid && !previousFlushed) {
      data(i) := previousData
    }
  }

  io.out.valid := valid(latency - 1) && !flushed(latency - 1)
  io.out.bits := data(latency - 1)
  // The issue pipe registers this once; its M2 wakeup is two cycles ahead of the result.
  io.wakeUp.valid := valid(latency - 4) && !flushed(latency - 4)
  io.wakeUp.bits := data(latency - 4).ctrl.pdest
}

private[wrapper] object FDivSqrtDpiFltCtrl {
  def copyToOutput(dst: Func.OutCtrl, src: Func.InCtrl): Unit = {
    dst.robIdx := src.robIdx
    dst.pdest := src.pdest
    dst.pdestV0.zip(src.pdestV0).foreach { case (d, s) => d := s }
    dst.pdestVl.zip(src.pdestVl).foreach { case (d, s) => d := s }
    dst.rfWen.zip(src.rfWen).foreach { case (d, s) => d := s }
    dst.fpWen.zip(src.fpWen).foreach { case (d, s) => d := s }
    dst.vecWen.zip(src.vecWen).foreach { case (d, s) => d := s }
    dst.v0Wen.zip(src.v0Wen).foreach { case (d, s) => d := s }
    dst.vlWen.zip(src.vlWen).foreach { case (d, s) => d := s }
    dst.exceptionVec.zeroInit()
    dst.flushPipe.zip(src.flushPipe).foreach { case (d, s) => d := s }
    dst.replay.foreach(_ := false.B)
    dst.isRVC.foreach(_ := false.B)
    dst.fflagsWen.zip(src.fflagsWen).foreach { case (d, s) => d := s }
  }
}
