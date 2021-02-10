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
    val exception = Flipped(ValidIO(new ExceptionInfo))
    val trapTarget = Output(UInt(VAddrBits.W))
    val isXRet = Output(Bool())
    val interrupt = Output(Bool())
    val memExceptionVAddr = Input(UInt(VAddrBits.W))
    val externalInterrupt = new ExternalInterruptIO
    val tlb = Output(new TlbCsrBundle)
    val perfinfo = new Bundle {
      val retiredInstr = Input(UInt(3.W))
    }
  })
  val fenceio = IO(new Bundle {
    val sfence = Output(new SfenceBundle)
    val fencei = Output(Bool())
    val sbuffer = new FenceToSbuffer
  })
  val difftestIO = IO(new Bundle() {
    val fromCSR = new Bundle() {
      val intrNO = Output(UInt(64.W))
      val cause = Output(UInt(64.W))
      val priviledgeMode = Output(UInt(2.W))
      val mstatus = Output(UInt(64.W))
      val sstatus = Output(UInt(64.W))
      val mepc = Output(UInt(64.W))
      val sepc = Output(UInt(64.W))
      val mtval = Output(UInt(64.W))
      val stval = Output(UInt(64.W))
      val mtvec = Output(UInt(64.W))
      val stvec = Output(UInt(64.W))
      val mcause = Output(UInt(64.W))
      val scause = Output(UInt(64.W))
      val satp = Output(UInt(64.W))
      val mip = Output(UInt(64.W))
      val mie = Output(UInt(64.W))
      val mscratch = Output(UInt(64.W))
      val sscratch = Output(UInt(64.W))
      val mideleg = Output(UInt(64.W))
      val medeleg = Output(UInt(64.W))
    }
  })
  difftestIO <> DontCare

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
  csr.csrio.perf.retiredInstr <> csrio.perfinfo.retiredInstr
  csr.csrio.fpu.fflags <> csrio.fflags
  csr.csrio.fpu.isIllegal := false.B
  csr.csrio.fpu.dirty_fs <> csrio.dirty_fs
  csr.csrio.fpu.frm <> csrio.frm
  csr.csrio.exception <> csrio.exception
  csr.csrio.trapTarget <> csrio.trapTarget
  csr.csrio.isXRet <> csrio.isXRet
  csr.csrio.interrupt <> csrio.interrupt
  csr.csrio.memExceptionVAddr <> csrio.memExceptionVAddr
  csr.csrio.externalInterrupt <> csrio.externalInterrupt
  csr.csrio.tlb <> csrio.tlb

  if (!env.FPGAPlatform) {
    difftestIO.fromCSR <> csr.difftestIO
  }

  fenceio.sfence <> fence.sfence
  fenceio.fencei <> fence.fencei
  fenceio.sbuffer <> fence.toSbuffer
  fence.io.out.ready := true.B

  val uop = io.fromInt.bits.uop
  val instr_rm = uop.ctrl.fpu.rm
  i2f.rm := Mux(instr_rm =/= 7.U, instr_rm, csr.csrio.fpu.frm)

  val isDouble = !uop.ctrl.isRVF


  io.toInt.bits.redirectValid := jmp.redirectOutValid
  io.toInt.bits.redirect := jmp.redirectOut
}
