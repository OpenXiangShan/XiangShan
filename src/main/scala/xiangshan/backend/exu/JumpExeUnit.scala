package xiangshan.backend.exu


import chisel3._
import chisel3.util._
import xiangshan.backend.fu.fpu.FPUOpType.FU_I2F
import xiangshan.backend.fu.{CSR, Fence, FenceToSbuffer, FunctionUnit, Jump}
import xiangshan.{FuType, SfenceBundle, TlbCsrBundle}
import xiangshan.backend.fu.fpu.{Fflags, IntToFloatSingleCycle, boxF32ToF64}

class JumpExeUnit extends Exu(
  exuName = "JmpExeUnit",
  fuGen = Seq(
    (FunctionUnit.jmp _, (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.jmp),
    (FunctionUnit.csr _, (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.csr),
    (FunctionUnit.fence _, (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.fence),
    (FunctionUnit.i2f _, (x: FunctionUnit) => x.io.in.bits.uop.ctrl.fuType === FuType.i2f)
  ),
  wbIntPriority = 2,
  wbFpPriority = Int.MaxValue
)
{
  val fflags = IO(Input(new Fflags))
  val dirty_fs = IO(Input(Bool()))
  val frm = IO(Output(UInt(3.W)))

  val fenceToSbuffer = IO(new FenceToSbuffer)
  val sfence = IO(Output(new SfenceBundle))
  val fencei = IO(Output(Bool()))

  val tlbCsrIO = IO(Output(new TlbCsrBundle))

  val jmp = supportedFunctionUnits.collectFirst{
    case j: Jump => j
  }.get
  val csr = supportedFunctionUnits.collectFirst{
    case c: CSR => c
  }.get
  val fence = supportedFunctionUnits.collectFirst{
    case f: Fence => f
  }.get
  val i2f = supportedFunctionUnits.collectFirst {
    case i: IntToFloatSingleCycle => i
  }.get


  val uop = io.in.bits.uop

  fenceToSbuffer <> fence.toSbuffer
  sfence <> fence.sfence
  fencei := fence.fencei
  frm := csr.fpu_csr.frm
  tlbCsrIO := csr.tlbCsrIO

  csr.fpu_csr.fflags := fflags
  csr.fpu_csr.isIllegal := false.B // TODO: check illegal rounding mode
  csr.fpu_csr.dirty_fs := dirty_fs

  csr.perf <> DontCare

  csr.exception := io.csrOnly.exception
  csr.isInterrupt := io.redirect.bits.isFlushPipe
  csr.memExceptionVAddr := io.csrOnly.memExceptionVAddr
  csr.mtip := io.csrOnly.externalInterrupt.mtip
  csr.msip := io.csrOnly.externalInterrupt.msip
  csr.meip := io.csrOnly.externalInterrupt.meip
  io.csrOnly.trapTarget := csr.trapTarget
  io.csrOnly.interrupt := csr.interrupt

  val instr_rm = uop.cf.instr(14, 12)
  i2f.rm := Mux(instr_rm =/= 7.U, instr_rm, csr.fpu_csr.frm)

  val isDouble = !uop.ctrl.isRVF

  when(i2f.io.in.valid){
    when(uop.ctrl.fuOpType.head(4)===s"b$FU_I2F".U){
      io.out.bits.data := Mux(isDouble, i2f.io.out.bits.data, boxF32ToF64(i2f.io.out.bits.data))
      io.out.bits.fflags := i2f.fflags
    }.otherwise({
      // a mov.(s/d).x instruction
      io.out.bits.data := Mux(isDouble, io.in.bits.src1, boxF32ToF64(io.in.bits.src1))
      io.out.bits.fflags := 0.U.asTypeOf(new Fflags)
    })
  }

  when(csr.io.out.valid){
    io.out.bits.redirectValid := csr.redirectOutValid
    io.out.bits.redirect.brTag := uop.brTag
    io.out.bits.redirect.isException := false.B
    io.out.bits.redirect.isMisPred := false.B
    io.out.bits.redirect.isFlushPipe := false.B
    io.out.bits.redirect.isReplay := false.B
    io.out.bits.redirect.roqIdx := uop.roqIdx
    io.out.bits.redirect.target := csr.redirectOut.target
    io.out.bits.redirect.pc := uop.cf.pc
  }.elsewhen(jmp.io.out.valid){
    io.out.bits.redirectValid := jmp.redirectOutValid
    io.out.bits.redirect := jmp.redirectOut
    io.out.bits.brUpdate := jmp.brUpdate
  }
}
