package xiangshan.backend.exu


import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.Exu.jumpExeUnitCfg
import xiangshan.backend.fu.fpu.IntToFP
import xiangshan.backend.fu.{CSR, Fence, FenceToSbuffer, FunctionUnit, Jump}

class JumpExeUnit extends Exu(jumpExeUnitCfg)
{
  val csrio = IO(new Bundle {
    val fflags = Flipped(ValidIO(UInt(5.W)))
    val dirty_fs = Input(Bool())
    val frm = Output(UInt(3.W))
    val exception = Flipped(ValidIO(new MicroOp))
    val isInterrupt = Input(Bool())
    val trapTarget = Output(UInt(VAddrBits.W))
    val interrupt = Output(Bool())
    val memExceptionVAddr = Input(UInt(VAddrBits.W))
    val externalInterrupt = new ExternalInterruptIO
    val tlb = Output(new TlbCsrBundle)
  })
  val fenceio = IO(new Bundle {
    val sfence = Output(new SfenceBundle)
    val fencei = Output(Bool())
    val sbuffer = new FenceToSbuffer
  })

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
    case i: IntToFP => i
  }.get

  csr.csrio.perf <> DontCare
  csr.csrio.fpu.fflags <> csrio.fflags
  csr.csrio.fpu.isIllegal := false.B
  csr.csrio.fpu.dirty_fs <> csrio.dirty_fs
  csr.csrio.fpu.frm <> csrio.frm
  csr.csrio.exception <> csrio.exception
  csr.csrio.isInterrupt <> csrio.isInterrupt
  csr.csrio.trapTarget <> csrio.trapTarget
  csr.csrio.interrupt <> csrio.interrupt
  csr.csrio.memExceptionVAddr <> csrio.memExceptionVAddr
  csr.csrio.externalInterrupt <> csrio.externalInterrupt
  csr.csrio.tlb <> csrio.tlb

  fenceio.sfence <> fence.sfence
  fenceio.fencei <> fence.fencei
  fenceio.sbuffer <> fence.toSbuffer
  fence.io.out.ready := true.B

  val uop = io.fromInt.bits.uop
  val instr_rm = uop.cf.instr(14, 12)
  i2f.rm := Mux(instr_rm =/= 7.U, instr_rm, csr.csrio.fpu.frm)

  val isDouble = !uop.ctrl.isRVF

  when(csr.io.out.valid){
    io.toInt.bits.redirectValid := csr.csrio.redirectOut.valid
    io.toInt.bits.redirect.brTag := uop.brTag
    io.toInt.bits.redirect.isException := false.B
    io.toInt.bits.redirect.isMisPred := false.B
    io.toInt.bits.redirect.isFlushPipe := false.B
    io.toInt.bits.redirect.isReplay := false.B
    io.toInt.bits.redirect.roqIdx := uop.roqIdx
    io.toInt.bits.redirect.target := csr.csrio.redirectOut.bits
    io.toInt.bits.redirect.pc := uop.cf.pc
  }.elsewhen(jmp.io.out.valid){
    io.toInt.bits.redirectValid := jmp.redirectOutValid
    io.toInt.bits.redirect := jmp.redirectOut
    io.toInt.bits.brUpdate := jmp.brUpdate
  }
}
