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

trait HasCSRConst {
  val Mstatus       = 0x300
  val Mtvec         = 0x305
  val Mepc          = 0x341
  val Mcause        = 0x342
  val Mie           = 0x304
  val Mip           = 0x344

  def privEcall = 0x000.U
  def privMret  = 0x302.U
}

trait HasExceptionNO {
  def instrAddrMisaligned = 0
  def instrAccessFault    = 1
  def illegalInstr        = 2
  def breakPoint          = 3
  def loadAddrMisaligned  = 4
  def loadAccessFault     = 5
  def storeAddrMisaligned = 6
  def storeAccessFault    = 7
  def ecallU              = 8
  def ecallS              = 9
  def ecallM              = 11
  def instrPageFault      = 12
  def loadPageFault       = 13
  def storePageFault      = 15
}

class CSRIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val redirect = new RedirectIO
  // for exception check
  val instrValid = Input(Bool())
}

class CSR(implicit val p: NOOPConfig) extends NOOPModule with HasCSRConst {
  val io = IO(new CSRIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  class Priv extends Bundle {
    val m = Output(Bool())
    val h = Output(Bool())
    val s = Output(Bool())
    val u = Output(Bool())
  }
   
  // exceptions
  class MstatusStruct extends Bundle {
    val sd = Output(UInt(1.W))
    val pad1 = Output(UInt(37.W))
    val sxl = Output(UInt(2.W))
    val uxl = Output(UInt(2.W))
    val pad0 = Output(UInt(9.W))
    val tsr = Output(UInt(1.W))
    val tw = Output(UInt(1.W))
    val tvm = Output(UInt(1.W))
    val mxr = Output(UInt(1.W))
    val sum = Output(UInt(1.W))
    val mprv = Output(UInt(1.W))
    val xs = Output(UInt(2.W))
    val fs = Output(UInt(2.W))
    val mpp = Output(UInt(2.W))
    val hpp = Output(UInt(2.W))
    val spp = Output(UInt(1.W))
    val pie = new Priv
    val ie = new Priv
  }
  val mtvec = Reg(UInt(XLEN.W))
  val mcause = Reg(UInt(XLEN.W))
  val mstatus = RegInit(UInt(XLEN.W), "h000c0100".U)
  val mepc = Reg(UInt(XLEN.W))

  val mstatusStruct = mstatus.asTypeOf(new MstatusStruct)

  // interrupts
  class Interrupt extends Bundle {
    val e = new Priv
    val t = new Priv
    val s = new Priv
  }
  val mie = RegInit(0.U(XLEN.W))
  val mip = WireInit(0.U.asTypeOf(new Interrupt))

  val mtip = WireInit(false.B)
  BoringUtils.addSink(mtip, "mtip")
  mip.t.m := mtip

  val intrVec = mie(11,0) & mip.asUInt & Fill(12, mstatusStruct.ie.m)
  BoringUtils.addSource(intrVec, "intrVecIDU")
  val raiseIntr = io.cfIn.intrVec.asUInt.orR

  // perfcnt
  val hasPerfCnt = !p.FPGAPlatform
  val nrPerfCnts = if (hasPerfCnt) 0x80 else 0x3
  val perfCnts = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val perfCntsLoMapping = (0 until nrPerfCnts).map { case i => RegMap(0xb00 + i, perfCnts(i)) }
  val perfCntsHiMapping = (0 until nrPerfCnts).map { case i => RegMap(0xb80 + i, perfCnts(i)(63, 32)) }

  val mapping = Map(
    RegMap(Mtvec   ,mtvec   ),
    RegMap(Mcause  ,mcause  ),
    RegMap(Mepc    ,mepc    ),
    RegMap(Mstatus ,mstatus ),
    RegMap(Mie     ,mie     ),
    RegMap(Mip     ,mip.asUInt, RegMap.Unwritable)
  ) ++ perfCntsLoMapping ++ (if (XLEN == 32) perfCntsHiMapping else Nil)

  val addr = src2(11, 0)
  val rdata = Wire(UInt(XLEN.W))
  val wdata = LookupTree(func, List(
    CSROpType.wrt -> src1,
    CSROpType.set -> (rdata | src1),
    CSROpType.clr -> (rdata & ~src1)
  ))

  val wen = (valid && func =/= CSROpType.jmp)
  RegMap.generate(mapping, addr, rdata, wen, wdata, wmask = Fill(XLEN, true.B))
  io.out.bits := rdata

  val isMret = addr === privMret
  val raiseException = io.cfIn.exceptionVec.asUInt.orR && io.instrValid
  val exceptionNO = PriorityEncoder(io.cfIn.exceptionVec)
  val intrNO = PriorityEncoder(intrVec) // FIXME: check this
  val causeNO = (raiseIntr << (XLEN-1)) | Mux(raiseIntr, intrNO, exceptionNO)

  io.redirect.valid := (valid && func === CSROpType.jmp) || raiseException || raiseIntr
  io.redirect.target := Mux(isMret, mepc, mtvec)

  when (io.redirect.valid && !isMret) {
    mepc := io.cfIn.pc  // FIXME: what if interrupt comes when io.cfIn.pc is not valid?
    mcause := causeNO
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
    def readWithScala(addr: Int): UInt = mapping(addr)._1

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
