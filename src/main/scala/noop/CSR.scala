package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object CSROpType {
  def jmp  = "b00".U
  def wrt  = "b01".U
  def set  = "b10".U
  def clr  = "b11".U
}

object CSRInstr extends HasInstrType {
  def CSRRW   = BitPat("b????????????_?????_001_?????_1110011")
  def CSRRS   = BitPat("b????????????_?????_010_?????_1110011")
  def ECALL   = BitPat("b001100000010_00000_000_00000_1110011")
  def MRET    = BitPat("b000000000000_00000_000_00000_1110011")
  def SRET    = BitPat("b000100000010_00000_000_00000_1110011")

  val table = Array(
    CSRRW          -> List(InstrI, FuType.csr, CSROpType.wrt),
    CSRRS          -> List(InstrI, FuType.csr, CSROpType.set),
    ECALL          -> List(InstrI, FuType.csr, CSROpType.jmp),
    MRET           -> List(InstrI, FuType.csr, CSROpType.jmp),
    SRET           -> List(InstrI, FuType.csr, CSROpType.jmp)
  )
}

trait HasCSRConst {
  val Mstatus       = 0x300
  val Mtvec         = 0x305
  val Mepc          = 0x341
  val Mcause        = 0x342

  def privEcall = 0x000.U
  def privMret  = 0x302.U
}

class CSRIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
  // exception
  val isInvOpcode = Input(Bool())
}

class CSR(implicit val p: NOOPConfig) extends Module with HasCSRConst {
  val io = IO(new CSRIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }
   
  val mtvec = Reg(UInt(32.W))
  val mcause = Reg(UInt(32.W))
  val mstatus = RegInit("h000c0100".U)
  val mepc = Reg(UInt(32.W))

  val hasPerfCnt = !p.FPGAPlatform
  val nrPerfCnts = if (hasPerfCnt) 0x80 else 0x3
  val perfCnts = List.fill(nrPerfCnts)(RegInit(0.U(64.W)))
  val perfCntsLoMapping = (0 until nrPerfCnts).map { case i => (0xb00 + i, perfCnts(i)) }
  val perfCntsHiMapping = (0 until nrPerfCnts).map { case i => (0xb80 + i, perfCnts(i)(63, 32)) }

  val scalaMapping = Map(
    Mtvec   -> mtvec,
    Mcause  -> mcause,
    Mepc    -> mepc,
    Mstatus -> mstatus
  ) ++ perfCntsLoMapping ++ perfCntsHiMapping

  val chiselMapping = scalaMapping.map { case (x, y) => (x.U -> y) }

  def readWithScala(addr: Int): UInt = scalaMapping(addr)

  val addr = src2(11, 0)
  val rdata = LookupTree(addr, chiselMapping)(31, 0)
  val wdata = LookupTree(func, List(
    CSROpType.wrt -> src1,
    CSROpType.set -> (rdata | src1),
    CSROpType.clr -> (rdata & ~src1)
  ))

  when (valid && func =/= CSROpType.jmp) {
    when (addr === Mtvec.U) { mtvec := wdata(31, 0) }
    when (addr === Mstatus.U) { mstatus := wdata(31, 0) }
    when (addr === Mepc.U) { mepc := wdata(31, 0) }
    when (addr === Mcause.U) { mcause := wdata(31, 0) }
  }

  // when (valid && func =/= CSROpType.jmp){
  //   when (addr === Mtvec.U) {printf("[CSR] %x pc: %x inst: %x\n", GTimer(), io.cfIn.pc, io.cfIn.instr)}
  // }
  // when (valid && func =/= CSROpType.jmp){
  //   when (addr === Mcause.U) {printf("[CSR] %x pc: %x inst: %x mcause: r %x w %x\n", GTimer(), io.cfIn.pc, io.cfIn.instr, rdata, wdata)}
  // }

  io.out.bits := rdata

  val isMret = addr === privMret
  val isException = io.isInvOpcode && valid
  val isEcall = (addr === privEcall) && !isException
  val exceptionNO = Mux1H(List(
    io.isInvOpcode -> 2.U,
    isEcall -> 9.U
  ))

  Debug(){
    when(io.isInvOpcode && valid){
      printf("[CSR] Invalid Op at %x\n", io.cfIn.pc)
    }
  }

  io.redirect.valid := (valid && func === CSROpType.jmp) || isException
  io.redirect.target := Mux(isMret, mepc, mtvec)

  when (io.redirect.valid && !isMret) {
    mepc := io.cfIn.pc
    mcause := exceptionNO
  }

  io.in.ready := true.B
  io.out.valid := valid

  // perfcnt
  val perfCntList = Map(
    "Mcycle"      -> (0xb00, "perfCntCondMcycle"     ),
    "Minstret"    -> (0xb02, "perfCntCondMinstret"   ),
    "MimemStall"  -> (0xb03, "perfCntCondMimemStall" ),
    "MaluInstr"   -> (0xb04, "perfCntCondMaluInstr"  ),
    "MbruInstr"   -> (0xb05, "perfCntCondMbruInstr"  ),
    "MlsuInstr"   -> (0xb06, "perfCntCondMlsuInstr"  ),
    "MmduInstr"   -> (0xb07, "perfCntCondMmduInstr"  ),
    "McsrInstr"   -> (0xb08, "perfCntCondMcsrInstr"  ),
    "MloadInstr"  -> (0xb09, "perfCntCondMloadInstr" ),
    "MloadStall"  -> (0xb0a, "perfCntCondMloadStall" ),
    "MstoreStall" -> (0xb0b, "perfCntCondMstoreStall"),
    "MmmioInstr"  -> (0xb0c, "perfCntCondMmmioInstr" ),
    "MicacheHit"  -> (0xb0d, "perfCntCondMicacheHit" ),
    "MdcacheHit"  -> (0xb0e, "perfCntCondMdcacheHit" ),
    "MmulInstr"   -> (0xb0f, "perfCntCondMmulInstr"  ),
    "MifuFlush"   -> (0xb10, "perfCntCondMifuFlush"  ),
    "MrawStall"   -> (0xb11, "perfCntCondMrawStall"  ),
    "MexuBusy"    -> (0xb12, "perfCntCondMexuBusy"   ),
    "MbpBRight"   -> (0xb13, "MbpBRight"             ),
    "MbpBWrong"   -> (0xb14, "MbpBWrong"             ),
    "MbpJRight"   -> (0xb15, "MbpJRight"             ),
    "MbpJWrong"   -> (0xb16, "MbpJWrong"             ),
    "MbpIRight"   -> (0xb17, "MbpIRight"             ),
    "MbpIWrong"   -> (0xb18, "MbpIWrong"             ),
    "MbpRRight"   -> (0xb19, "MbpRRight"             ),
    "MbpRWrong"   -> (0xb1a, "MbpRWrong"             )
  )

  val perfCntCond = List.fill(0x80)(WireInit(false.B))
  (perfCnts zip perfCntCond).map { case (c, e) => { when (e) { c := c + 1.U } } }

  BoringUtils.addSource(WireInit(true.B), "perfCntCondMcycle")
  perfCntList.map { case (name, (addr, boringId)) => {
    BoringUtils.addSink(perfCntCond(addr & 0x7f), boringId)
    if (!hasPerfCnt) {
      // do not enable perfcnts except for Mcycle and Minstret
      if (addr != perfCntList("Mcycle")._1 && addr != perfCntList("Minstret")._1) {
        perfCntCond(addr & 0x7f) := false.B
      }
    }
  }}

  val nooptrap = WireInit(false.B)
  BoringUtils.addSink(nooptrap, "nooptrap")
  if (!p.FPGAPlatform) {
    // to monitor
    BoringUtils.addSource(readWithScala(perfCntList("Mcycle")._1), "simCycleCnt")
    BoringUtils.addSource(readWithScala(perfCntList("Minstret")._1), "simInstrCnt")

    // display all perfcnt when nooptrap is executed
    when (nooptrap) {
      printf("======== PerfCnt =========\n")
      perfCntList.toSeq.sortBy(_._2._1).map { case (name, (addr, boringId)) =>
        printf("%d <- " + name + "\n", readWithScala(addr)) }
    }
  }
}
