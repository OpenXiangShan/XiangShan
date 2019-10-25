package noop

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import utils._

object CSROpType {
  def jmp  = "b000".U
  def wrt  = "b001".U
  def set  = "b010".U
  def clr  = "b011".U
  def wrti = "b101".U
  def seti = "b110".U
  def clri = "b111".U
}

trait HasCSRConst {
  // User Trap Setup
  val Ustatus       = 0x000 
  val Uie           = 0x004
  val Utvec         = 0x005
  
  // User Trap Handling
  val Uscratch      = 0x040
  val Uepc          = 0x041
  val Ucause        = 0x042
  val Utval         = 0x043
  val Uip           = 0x044

  // User Floating-Point CSRs (not implemented)
  val Fflags        = 0x001
  val Frm           = 0x002
  val Fcsr          = 0x003

  // User Counter/Timers
  val Cycle         = 0xC00
  val Time          = 0xC01
  val Instret       = 0xC02
  
  // Supervisor Trap Setup
  val Sstatus       = 0x100
  val Sedeleg       = 0x102
  val Sideleg       = 0x103
  val Sie           = 0x104
  val Stvec         = 0x105
  val Scounteren    = 0x106

  // Supervisor Trap Handling
  val Sscratch      = 0x140
  val Sepc          = 0x141
  val Scause        = 0x142
  val Stval         = 0x143
  val Sip           = 0x144

  // Supervisor Protection and Translation
  val Satp          = 0x180

  // Machine Information Registers 
  val Mvendorid     = 0xF11 
  val Marchid       = 0xF12 
  val Mimpid        = 0xF13 
  val Mhartid       = 0xF14 

  // Machine Trap Setup
  val Mstatus       = 0x300
  val Misa          = 0x301
  val Medeleg       = 0x302
  val Mideleg       = 0x303
  val Mie           = 0x304
  val Mtvec         = 0x305
  val Mcounteren    = 0x306 

  // Machine Trap Handling
  val Mscratch      = 0x340 
  val Mepc          = 0x341
  val Mcause        = 0x342
  val Mtval         = 0x343
  val Mip           = 0x344

  // Machine Memory Protection
  // TBD
  val Pmpcfg0       = 0x3A0
  val Pmpcfg1       = 0x3A1
  val Pmpcfg2       = 0x3A2
  val Pmpcfg3       = 0x3A3
  val PmpaddrBase   = 0x3B0 

  // Machine Counter/Timers 
  // Currently, NOOP uses perfcnt csr set instead of standard Machine Counter/Timers 
  // 0xB80 - 0x89F are also used as perfcnt csr

  // Machine Counter Setup (not implemented)
  // Debug/Trace Registers (shared with Debug Mode) (not implemented)
  // Debug Mode Registers (not implemented)

  def privEcall = 0x000.U
  def privMret  = 0x302.U
  def privSret  = 0x102.U
  def privUret  = 0x002.U

  def ModeM     = 0x3.U
  def ModeH     = 0x2.U
  def ModeS     = 0x1.U
  def ModeU     = 0x0.U
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
  // for differential testing
  val intrNO = Output(UInt(XLEN.W))
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

  // CSR define

  class Priv extends Bundle {
    val m = Output(Bool())
    val h = Output(Bool())
    val s = Output(Bool())
    val u = Output(Bool())
  }

  val csrNotImplemented = RegInit(UInt(XLEN.W), 0.U)
   
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

  class Interrupt extends Bundle {
    val e = new Priv
    val t = new Priv
    val s = new Priv
  }

  // Machine-Level CSRs
  
  val mtvec = RegInit(UInt(XLEN.W), 0.U)
  val mcause = Reg(UInt(XLEN.W))
  val mepc = Reg(UInt(XLEN.W))

  val mie = RegInit(0.U(XLEN.W))
  val mip = WireInit(0.U.asTypeOf(new Interrupt))

  val misa = RegInit(UInt(XLEN.W), "h8000000000141101".U) 
  // MXL = 2          | 0 | EXT = b 00 0001 0100 0001 0001 0000 0100
  // (XLEN-1, XLEN-2) |   |(25, 0)  ZY XWVU TSRQ PONM LKJI HGFE DCBA

  val mvendorid = RegInit(UInt(XLEN.W), 0.U) // this is a non-commercial implementation
  val marchid = RegInit(UInt(XLEN.W), 0.U) // return 0 to indicate the field is not implemented
  val mimpid = RegInit(UInt(XLEN.W), 0.U) // provides a unique encoding of the version of the processor implementation
  val mhartid = RegInit(UInt(XLEN.W), 0.U) // the hardware thread running the code
  val mstatus = RegInit(UInt(XLEN.W), "h00000000".U)
  // val mstatus = RegInit(UInt(XLEN.W), "h8000c0100".U)
  // mstatus Value Table
  // | sd   |
  // | pad1 |
  // | sxl  | hardlinked to 10, use 00 to pass xv6 test  
  // | uxl  | hardlinked to 00
  // | pad0 |
  // | tsr  |
  // | tw   |
  // | tvm  |
  // | mxr  |
  // | sum  |
  // | mprv |
  // | xs   | 00 |
  // | fs   | 00 |
  // | mpp  | 00 |
  // | hpp  | 00 |
  // | spp  | 0 |
  // | pie  | 0000 |
  // | ie   | 0000 | uie hardlinked to 0, as N ext is not implemented
  val mstatusStruct = mstatus.asTypeOf(new MstatusStruct)

  val medeleg = RegInit(UInt(XLEN.W), 0.U)
  val mideleg = RegInit(UInt(XLEN.W), 0.U)
  val mscratch = RegInit(UInt(XLEN.W), 0.U)

  // Superviser-Level CSRs

  val sstatus = RegInit(UInt(XLEN.W), "h00000000".U)
  val stvec = RegInit(UInt(XLEN.W), 0.U)
  val sie = RegInit(0.U(XLEN.W))
  val satp = RegInit(UInt(XLEN.W), 0.U)
  val sepc = Reg(UInt(XLEN.W))
  val scause = Reg(UInt(XLEN.W))
  val sscratch = RegInit(UInt(XLEN.W), 0.U)
  val sstatusStruct = sstatus.asTypeOf(new MstatusStruct)

  // User-Level CSRs
  val uepc = Reg(UInt(XLEN.W))

  // Hart Priviledge Mode

  val priviledgeMode = RegInit(UInt(2.W), ModeM)
  // val globalInterruptEnable = LookupTree(priviledgeMode, List(
  //   ModeM -> mstatusStruct.ie.m,
  //   ModeH -> mstatusStruct.ie.h,
  //   ModeS -> mstatusStruct.ie.s,
  //   ModeU -> mstatusStruct.ie.u
  // ))

  // perfcnt
  val hasPerfCnt = !p.FPGAPlatform
  val nrPerfCnts = if (hasPerfCnt) 0x80 else 0x3
  val perfCnts = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val perfCntsLoMapping = (0 until nrPerfCnts).map { case i => MaskedRegMap(0xb00 + i, perfCnts(i)) }
  val perfCntsHiMapping = (0 until nrPerfCnts).map { case i => MaskedRegMap(0xb80 + i, perfCnts(i)(63, 32)) }

  // CSR reg map
  val mapping = Map(

    // User Trap Setup
    // MaskedRegMap(Ustatus, ustatus), 
    // MaskedRegMap(Uie, uie, 0.U, MaskedRegMap.Unwritable),
    // MaskedRegMap(Utvec, utvec),
    
    // User Trap Handling
    // MaskedRegMap(Uscratch, uscratch),
    // MaskedRegMap(Uepc, uepc),
    // MaskedRegMap(Ucause, ucause),
    // MaskedRegMap(Utval, utval),
    // MaskedRegMap(Uip, uip),

    // User Floating-Point CSRs (not implemented)
    // MaskedRegMap(Fflags, fflags),
    // MaskedRegMap(Frm, frm),
    // MaskedRegMap(Fcsr, fcsr),

    // User Counter/Timers
    // MaskedRegMap(Cycle, cycle),
    // MaskedRegMap(Time, time),
    // MaskedRegMap(Instret, instret),
    
    // Supervisor Trap Setup
    MaskedRegMap(Sstatus, sstatus),
    // MaskedRegMap(Sedeleg, Sedeleg),
    // MaskedRegMap(Sideleg, Sideleg),
    MaskedRegMap(Sie, sie),
    MaskedRegMap(Stvec, stvec),
    // MaskedRegMap(Scounteren, Scounteren),

    // Supervisor Trap Handling
    MaskedRegMap(Sscratch, sscratch),
    MaskedRegMap(Sepc, sepc),
    MaskedRegMap(Scause, scause),
    // MaskedRegMap(Stval, stval),
    // MaskedRegMap(Sip, sip),

    // Supervisor Protection and Translation
    MaskedRegMap(Satp, satp),

    // Machine Information Registers 
    MaskedRegMap(Mvendorid, mvendorid, 0.U, MaskedRegMap.Unwritable), 
    MaskedRegMap(Marchid, marchid, 0.U, MaskedRegMap.Unwritable), 
    MaskedRegMap(Mimpid, mimpid, 0.U, MaskedRegMap.Unwritable), 
    MaskedRegMap(Mhartid, mhartid, 0.U, MaskedRegMap.Unwritable), 

    // Machine Trap Setup
    MaskedRegMap(Mstatus, mstatus, "hffffffffffffffee".U),
    MaskedRegMap(Misa, misa, "h6ffffffffc000000".U), // now MXL, EXT is not changeable
    MaskedRegMap(Medeleg, medeleg),
    MaskedRegMap(Mideleg, mideleg),
    MaskedRegMap(Mie, mie),
    MaskedRegMap(Mtvec, mtvec),
    // MaskedRegMap(Mcounteren, mcounteren), 

    // Machine Trap Handling
    MaskedRegMap(Mscratch, mscratch),
    MaskedRegMap(Mepc, mepc),
    MaskedRegMap(Mcause, mcause),
    // MaskedRegMap(Mtval, mtval)
    MaskedRegMap(Mip, mip.asUInt, 0.U, MaskedRegMap.Unwritable)

    // Machine Memory Protection
    // MaskedRegMap(Pmpcfg0, Pmpcfg0),
    // MaskedRegMap(Pmpcfg1, Pmpcfg1),
    // MaskedRegMap(Pmpcfg2, Pmpcfg2),
    // MaskedRegMap(Pmpcfg3, Pmpcfg3),

  ) ++ perfCntsLoMapping ++ (if (XLEN == 32) perfCntsHiMapping else Nil)

  val addr = src2(11, 0)
  val rdata = Wire(UInt(XLEN.W))
  val csri = ZeroExt(io.cfIn.instr(19,15), XLEN) //unsigned imm for csri. [TODO]
  val wdata = LookupTree(func, List(
    CSROpType.wrt  -> src1,
    CSROpType.set  -> (rdata | src1),
    CSROpType.clr  -> (rdata & ~src1),
    CSROpType.wrti -> csri,//TODO: csri --> src2
    CSROpType.seti -> (rdata | csri),
    CSROpType.clri -> (rdata & ~csri)
  ))

  val wen = (valid && func =/= CSROpType.jmp)
  MaskedRegMap.generate(mapping, addr, rdata, wen, wdata)
  io.out.bits := rdata

  // CSR inst decode
  val ret = Wire(Bool())
  val isEcall = addr === privEcall
  val isMret = addr === privMret
  val isSret = addr === privSret
  val isUret = addr === privUret

  Debug(false){
    when(wen){
      printf("[CSR] csr write: pc %x addr %x rdata %x wdata %x\n", io.cfIn.pc, addr, rdata, wdata)
    }
  }

  // Exception and Intr

  // interrupts

  val intrVecMEnable = Mux(priviledgeMode < ModeM, true.B, mstatusStruct.ie.m)
  // val intrVecSEnable = Mux(priviledgeMode > ModeS, false.B, Mux(priviledgeMode < ModeS, true.B, sstatusStruct.ie.s))
  // val intrVecUEnable = Mux(priviledgeMode > ModeU, false.B, ustatusStruct.ie.u)
  val intrVecM = mie(11,0) & mip.asUInt & (Fill(12, intrVecMEnable))
  // val intrVecS = sie(11,0) & sip.asUInt
  // val intrVecU = uie(11,0) & uip.asUInt
  // val raiseIntrM = intrVecM.asUint.orR & intrVecMEnable
  // val raiseIntrS = intrVecS.asUint.orR & intrVecSEnable
  // val raiseIntrU = intrVecU.asUint.orR & intrVecUEnable
  val intrVec = intrVecM
  BoringUtils.addSource(intrVec, "intrVecIDU")
  // val intrNO = PriorityEncoder(intrVec)
  val intrNO = PriorityEncoder(io.cfIn.intrVec)
  val raiseIntr = io.cfIn.intrVec.asUInt.orR

  val mtip = WireInit(false.B)
  val meip = WireInit(false.B)
  BoringUtils.addSink(mtip, "mtip")
  BoringUtils.addSink(meip, "meip")
  mip.t.m := mtip
  mip.e.m := meip

  // exceptions

  // TODO: merge iduExceptionVec, csrExceptionVec as raiseExceptionVec
  val csrExceptionVec = Wire(Vec(16, Bool()))
  csrExceptionVec.map(_ := false.B)
  csrExceptionVec(ecallM) := priviledgeMode === ModeM && io.in.valid && isEcall
  csrExceptionVec(ecallS) := priviledgeMode === ModeS && io.in.valid && isEcall
  csrExceptionVec(ecallU) := priviledgeMode === ModeU && io.in.valid && isEcall
  val iduExceptionVec = io.cfIn.exceptionVec
  val raiseExceptionVec = csrExceptionVec.asUInt() | iduExceptionVec.asUInt()
  val raiseException = raiseExceptionVec.orR
  val exceptionNO = PriorityEncoder(raiseExceptionVec)

  val causeNO = (raiseIntr << (XLEN-1)) | Mux(raiseIntr, intrNO, exceptionNO)
  io.intrNO := Mux(raiseIntr, causeNO, 0.U)

  val raiseExceptionIntr = (raiseException || raiseIntr) && io.instrValid
  val retTarget = Wire(UInt(AddrBits.W))
  val trapTarget = Wire(UInt(AddrBits.W))
  io.redirect.valid := (valid && func === CSROpType.jmp) || raiseExceptionIntr
  io.redirect.target := Mux(raiseExceptionIntr, trapTarget, retTarget)

  Debug(false){
    when(raiseExceptionIntr){
      printf("[CSR] int/exc: pc %x int (%d):%x exc: (%d):%x\n",io.cfIn.pc, intrNO, io.cfIn.intrVec.asUInt, exceptionNO, raiseExceptionVec.asUInt)
    }
  }

  // Branch control

  val deleg = Mux(raiseIntr, mideleg , medeleg)
  // val delegS = ((deleg & (1 << (causeNO & 0xf))) != 0) && (priviledgeMode < ModeM);
  val delegS = (deleg(causeNO(3,0))) && (priviledgeMode < ModeM)

  ret := isMret || isSret || isUret
  trapTarget := Mux(delegS, stvec, mtvec)
  retTarget := DontCare
  // TODO redircet target
  // val illegalEret = TODO

  when (valid && isMret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // mstatusNew.mpp.m := ModeU //TODO: add mode U
    mstatusNew.ie.m := mstatusOld.pie.m
    priviledgeMode := mstatusOld.mpp
    mstatusNew.pie.m := true.B
    mstatusNew.mpp := ModeU
    mstatus := mstatusNew.asUInt
    retTarget := mepc
  }

  when (valid && isSret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // mstatusNew.mpp.m := ModeU //TODO: add mode U
    mstatusNew.ie.s := mstatusOld.pie.s
    priviledgeMode := Cat(0.U(1.W), mstatusOld.spp)
    mstatusNew.pie.s := true.B
    mstatusNew.spp := ModeU
    mstatus := mstatusNew.asUInt
    retTarget := sepc
  }

  when (valid && isUret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // mstatusNew.mpp.m := ModeU //TODO: add mode U
    mstatusNew.ie.u := mstatusOld.pie.u
    priviledgeMode := 0.U //ModeU
    mstatusNew.pie.m := true.B
    mstatus := mstatusNew.asUInt
    retTarget := uepc
  }

  when (raiseExceptionIntr) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))

    when (delegS) {
      scause := causeNO
      sepc := io.cfIn.pc
      mstatusNew.spp := priviledgeMode
      mstatusNew.pie.s := mstatusOld.ie.s
      mstatusNew.ie.s := false.B
      priviledgeMode := ModeS
      // trapTarget := stvec
    }.otherwise {
      mcause := causeNO
      mepc := io.cfIn.pc
      mstatusNew.mpp := priviledgeMode
      mstatusNew.pie.m := mstatusOld.ie.m
      mstatusNew.ie.m := false.B
      priviledgeMode := ModeM
      // trapTarget := mtvec
    }
    // mstatusNew.pie.m := LookupTree(priviledgeMode, List(
    //   ModeM -> mstatusOld.ie.m,
    //   ModeH -> mstatusOld.ie.h, //ERROR
    //   ModeS -> mstatusOld.ie.s,
    //   ModeU -> mstatusOld.ie.u
    // ))

    mstatus := mstatusNew.asUInt
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
    "MbpRWrong"   -> (0xb1a, "MbpRWrong"             ),
    "Custom1"     -> (0xb1b, "Custom1"             ),
    "Custom2"     -> (0xb1c, "Custom2"             ),
    "Custom3"     -> (0xb1d, "Custom3"             ),
    "Custom4"     -> (0xb1e, "Custom4"             ),
    "Custom5"     -> (0xb1f, "Custom5"             ),
    "Custom6"     -> (0xb20, "Custom6"             ),
    "Custom7"     -> (0xb21, "Custom7"             ),
    "Custom8"     -> (0xb22, "Custom8"             )
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
