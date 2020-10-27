package xiangshan.backend.fu

import chisel3._
import chisel3.ExcitingUtils.ConnectionType
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import fpu.Fflags
import noop.MMUIO
import utils._
import xiangshan._
import xiangshan.backend._
import xiangshan.backend.fu.FunctionUnit._
import utils.XSDebug

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
  // Currently, we uses perfcnt csr set instead of standard Machine Counter/Timers 
  // 0xB80 - 0x89F are also used as perfcnt csr

  // Machine Counter Setup (not implemented)
  // Debug/Trace Registers (shared with Debug Mode) (not implemented)
  // Debug Mode Registers (not implemented)

  def privEcall  = 0x000.U
  def privEbreak = 0x001.U
  def privMret   = 0x302.U
  def privSret   = 0x102.U
  def privUret   = 0x002.U

  def ModeM     = 0x3.U
  def ModeH     = 0x2.U
  def ModeS     = 0x1.U
  def ModeU     = 0x0.U

  def IRQ_UEIP  = 0
  def IRQ_SEIP  = 1
  def IRQ_MEIP  = 3

  def IRQ_UTIP  = 4
  def IRQ_STIP  = 5
  def IRQ_MTIP  = 7

  def IRQ_USIP  = 8
  def IRQ_SSIP  = 9
  def IRQ_MSIP  = 11

  val IntPriority = Seq(
    IRQ_MEIP, IRQ_MSIP, IRQ_MTIP,
    IRQ_SEIP, IRQ_SSIP, IRQ_STIP,
    IRQ_UEIP, IRQ_USIP, IRQ_UTIP
  )

  def csrAccessPermissionCheck(addr: UInt, wen: Bool, mode: UInt): Bool = {
    val readOnly = addr(11,10) === "b11".U
    val lowestAccessPrivilegeLevel = addr(9,8)
    mode >= lowestAccessPrivilegeLevel && !(wen && readOnly)
  }
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

  val ExcPriority = Seq(
      breakPoint, // TODO: different BP has different priority
      instrPageFault,
      instrAccessFault,
      illegalInstr,
      instrAddrMisaligned,
      ecallM, ecallS, ecallU,
      storePageFault,
      loadPageFault,
      storeAccessFault,
      loadAccessFault,
      storeAddrMisaligned,
      loadAddrMisaligned
  )
}

class FpuCsrIO extends XSBundle {
  val fflags = Output(new Fflags)
  val isIllegal = Output(Bool())
  val dirty_fs = Output(Bool())
  val frm = Input(UInt(3.W))
}

class CSRIO extends FunctionUnitIO {
  val cfIn = Input(new CtrlFlow)
  val redirect = Output(new Redirect)
  val redirectValid = Output(Bool())
  val fpu_csr = Flipped(new FpuCsrIO)
  val cfOut = Output(new CtrlFlow)
  // from rob
  val exception = Flipped(ValidIO(new MicroOp))
  // for exception check
  val instrValid = Input(Bool())
  val flushPipe = Output(Bool())
  // for differential testing
//  val intrNO = Output(UInt(XLEN.W))
  val wenFix = Output(Bool())
}

class CSR extends FunctionUnit(csrCfg) with HasCSRConst{
  val io = IO(new CSRIO)

  io.cfOut := io.cfIn

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

    val pad1 = if (XLEN == 64) Output(UInt(27.W)) else null
    val sxl  = if (XLEN == 64) Output(UInt(2.W))  else null
    val uxl  = if (XLEN == 64) Output(UInt(2.W))  else null
    val pad0 = if (XLEN == 64) Output(UInt(9.W))  else Output(UInt(8.W))

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
    assert(this.getWidth == XLEN)
  }

  class SatpStruct extends Bundle {
    val mode = UInt(4.W)
    val asid = UInt(16.W)
    val ppn  = UInt(44.W)
  }

  class Interrupt extends Bundle {
    val e = new Priv
    val t = new Priv
    val s = new Priv
  }

  // Machine-Level CSRs

  val mtvec = RegInit(UInt(XLEN.W), 0.U)
  val mcounteren = RegInit(UInt(XLEN.W), 0.U)
  val mcause = RegInit(UInt(XLEN.W), 0.U)
  val mtval = RegInit(UInt(XLEN.W), 0.U)
  val mepc = Reg(UInt(XLEN.W))

  val mie = RegInit(0.U(XLEN.W))
  val mipWire = WireInit(0.U.asTypeOf(new Interrupt))
  val mipReg  = RegInit(0.U.asTypeOf(new Interrupt).asUInt)
  val mipFixMask = GenMask(9) | GenMask(5) | GenMask(1)
  val mip = (mipWire.asUInt | mipReg).asTypeOf(new Interrupt)

  def getMisaMxl(mxl: Int): UInt = {mxl.U << (XLEN-2)}
  def getMisaExt(ext: Char): UInt = {1.U << (ext.toInt - 'a'.toInt)}
  var extList = List('a', 's', 'i', 'u')
  if(HasMExtension){ extList = extList :+ 'm'}
  if(HasCExtension){ extList = extList :+ 'c'}
  if(HasFPU){ extList = extList ++ List('f', 'd')}
  val misaInitVal = getMisaMxl(2) | extList.foldLeft(0.U)((sum, i) => sum | getMisaExt(i)) //"h8000000000141105".U 
  val misa = RegInit(UInt(XLEN.W), misaInitVal) 
  // MXL = 2          | 0 | EXT = b 00 0000 0100 0001 0001 0000 0101
  // (XLEN-1, XLEN-2) |   |(25, 0)  ZY XWVU TSRQ PONM LKJI HGFE DCBA

  val mvendorid = RegInit(UInt(XLEN.W), 0.U) // this is a non-commercial implementation
  val marchid = RegInit(UInt(XLEN.W), 0.U) // return 0 to indicate the field is not implemented
  val mimpid = RegInit(UInt(XLEN.W), 0.U) // provides a unique encoding of the version of the processor implementation
  val mhartid = RegInit(UInt(XLEN.W), 0.U) // the hardware thread running the code
  val mstatus = RegInit(UInt(XLEN.W), "h00001800".U)
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
  // | pie  | 0000 | pie.h is used as UBE
  // | ie   | 0000 | uie hardlinked to 0, as N ext is not implemented
  val mstatusStruct = mstatus.asTypeOf(new MstatusStruct)
  def mstatusUpdateSideEffect(mstatus: UInt): UInt = {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = Cat(mstatusOld.xs === "b11".U || mstatusOld.fs === "b11".U, mstatus(XLEN-2, 0))
    mstatusNew
  }

  val mstatusMask = ~ZeroExt((
    GenMask(XLEN-2, 38) | GenMask(31, 23) | GenMask(10, 9) | GenMask(2) |
    GenMask(37) | // MBE
    GenMask(36) | // SBE
    GenMask(6)    // UBE
  ), 64)

  val medeleg = RegInit(UInt(XLEN.W), 0.U)
  val mideleg = RegInit(UInt(XLEN.W), 0.U)
  val mscratch = RegInit(UInt(XLEN.W), 0.U)

  val pmpcfg0 = RegInit(UInt(XLEN.W), 0.U)
  val pmpcfg1 = RegInit(UInt(XLEN.W), 0.U)
  val pmpcfg2 = RegInit(UInt(XLEN.W), 0.U)
  val pmpcfg3 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr0 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr1 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr2 = RegInit(UInt(XLEN.W), 0.U)
  val pmpaddr3 = RegInit(UInt(XLEN.W), 0.U)

  // Superviser-Level CSRs

  // val sstatus = RegInit(UInt(XLEN.W), "h00000000".U)
  val sstatusWmask = "hc6122".U
  // Sstatus Write Mask
  // -------------------------------------------------------
  //    19           9   5     2
  // 0  1100 0000 0001 0010 0010
  // 0  c    0    1    2    2
  // -------------------------------------------------------
  val sstatusRmask = sstatusWmask | "h8000000300018000".U
  // Sstatus Read Mask = (SSTATUS_WMASK | (0xf << 13) | (1ull << 63) | (3ull << 32))
  val stvec = RegInit(UInt(XLEN.W), 0.U)
  // val sie = RegInit(0.U(XLEN.W))
  val sieMask = "h222".U & mideleg
  val sipMask  = "h222".U & mideleg
  val satp = RegInit(0.U(XLEN.W))
  // val satp = RegInit(UInt(XLEN.W), "h8000000000087fbe".U) // only use for tlb naive debug
  val satpMask = "h80000fffffffffff".U // disable asid, mode can only be 8 / 0 
  // val satp = RegInit(UInt(XLEN.W), 0.U)
  val sepc = RegInit(UInt(XLEN.W), 0.U)
  val scause = RegInit(UInt(XLEN.W), 0.U)
  val stval = Reg(UInt(XLEN.W))
  val sscratch = RegInit(UInt(XLEN.W), 0.U)
  val scounteren = RegInit(UInt(XLEN.W), 0.U)

  val tlbBundle = Wire(new TlbCsrBundle)
  // val sfence    = Wire(new SfenceBundle)
  tlbBundle.satp := satp.asTypeOf(new SatpStruct)
  // sfence := 0.U.asTypeOf(new SfenceBundle)
  BoringUtils.addSource(tlbBundle, "TLBCSRIO")
  // BoringUtils.addSource(sfence, "SfenceBundle") // FIXME: move to MOU

  // User-Level CSRs
  val uepc = Reg(UInt(XLEN.W))

  // fcsr
  class FcsrStruct extends Bundle{
    val reserved = UInt((XLEN-3-5).W)
    val frm = UInt(3.W)
    val fflags = UInt(5.W)
    assert(this.getWidth == XLEN)
  }
  val fcsr = RegInit(0.U(XLEN.W))
  // set mstatus->sd and mstatus->fs when true
  val csrw_dirty_fp_state = WireInit(false.B)

  def frm_wfn(wdata: UInt): UInt = {
    val fcsrOld = WireInit(fcsr.asTypeOf(new FcsrStruct))
    csrw_dirty_fp_state := true.B
    fcsrOld.frm := wdata(2,0)
    fcsrOld.asUInt()
  }
  def frm_rfn(rdata: UInt): UInt = rdata(7,5)

  def fflags_wfn(wdata: UInt): UInt = {
    val fcsrOld = WireInit(fcsr.asTypeOf(new FcsrStruct))
    csrw_dirty_fp_state := true.B
    fcsrOld.fflags := wdata(4,0)
    fcsrOld.asUInt()
  }
  def fflags_rfn(rdata:UInt): UInt = rdata(4,0)

  def fcsr_wfn(wdata: UInt): UInt = {
    val fcsrOld = WireInit(fcsr.asTypeOf(new FcsrStruct))
    csrw_dirty_fp_state := true.B
    Cat(fcsrOld.reserved, wdata.asTypeOf(fcsrOld).frm, wdata.asTypeOf(fcsrOld).fflags)
  }

  val fcsrMapping = Map(
    MaskedRegMap(Fflags, fcsr, wfn = fflags_wfn, rfn = fflags_rfn),
    MaskedRegMap(Frm, fcsr, wfn = frm_wfn, rfn = frm_rfn),
    MaskedRegMap(Fcsr, fcsr, wfn = fcsr_wfn)
  )

  // Atom LR/SC Control Bits
//  val setLr = WireInit(Bool(), false.B)
//  val setLrVal = WireInit(Bool(), false.B)
//  val setLrAddr = WireInit(UInt(AddrBits.W), DontCare) //TODO : need check
//  val lr = RegInit(Bool(), false.B)
//  val lrAddr = RegInit(UInt(AddrBits.W), 0.U)
//  BoringUtils.addSink(setLr, "set_lr")
//  BoringUtils.addSink(setLrVal, "set_lr_val")
//  BoringUtils.addSink(setLrAddr, "set_lr_addr")
//  BoringUtils.addSource(lr, "lr")
//  BoringUtils.addSource(lrAddr, "lr_addr")
//
//  when(setLr){
//    lr := setLrVal
//    lrAddr := setLrAddr
//  }

  // Hart Priviledge Mode
  val priviledgeMode = RegInit(UInt(2.W), ModeM)

  // perfcnt
  val hasPerfCnt = !env.FPGAPlatform
  val nrPerfCnts = if (hasPerfCnt) 0x80 else 0x3
  val perfCnts = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val perfCntsLoMapping = (0 until nrPerfCnts).map(i => MaskedRegMap(0xb00 + i, perfCnts(i)))
  val perfCntsHiMapping = (0 until nrPerfCnts).map(i => MaskedRegMap(0xb80 + i, perfCnts(i)(63, 32)))

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

    // User Counter/Timers
    // MaskedRegMap(Cycle, cycle),
    // MaskedRegMap(Time, time),
    // MaskedRegMap(Instret, instret),

    // Supervisor Trap Setup
    MaskedRegMap(Sstatus, mstatus, sstatusWmask, mstatusUpdateSideEffect, sstatusRmask),

    // MaskedRegMap(Sedeleg, Sedeleg),
    // MaskedRegMap(Sideleg, Sideleg),
    MaskedRegMap(Sie, mie, sieMask, MaskedRegMap.NoSideEffect, sieMask),
    MaskedRegMap(Stvec, stvec),
    MaskedRegMap(Scounteren, scounteren),

    // Supervisor Trap Handling
    MaskedRegMap(Sscratch, sscratch),
    MaskedRegMap(Sepc, sepc),
    MaskedRegMap(Scause, scause),
    MaskedRegMap(Stval, stval),
    MaskedRegMap(Sip, mip.asUInt, sipMask, MaskedRegMap.Unwritable, sipMask),

    // Supervisor Protection and Translation
    MaskedRegMap(Satp, satp, satpMask, MaskedRegMap.NoSideEffect, satpMask),

    // Machine Information Registers
    MaskedRegMap(Mvendorid, mvendorid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Marchid, marchid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Mimpid, mimpid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Mhartid, mhartid, 0.U, MaskedRegMap.Unwritable),

    // Machine Trap Setup
    // MaskedRegMap(Mstatus, mstatus, "hffffffffffffffee".U, (x=>{printf("mstatus write: %x time: %d\n", x, GTimer()); x})),
    MaskedRegMap(Mstatus, mstatus, mstatusMask, mstatusUpdateSideEffect, mstatusMask),
    MaskedRegMap(Misa, misa), // now MXL, EXT is not changeable
    MaskedRegMap(Medeleg, medeleg, "hf3ff".U),
    MaskedRegMap(Mideleg, mideleg, "h222".U),
    MaskedRegMap(Mie, mie),
    MaskedRegMap(Mtvec, mtvec),
    MaskedRegMap(Mcounteren, mcounteren),

    // Machine Trap Handling
    MaskedRegMap(Mscratch, mscratch),
    MaskedRegMap(Mepc, mepc),
    MaskedRegMap(Mcause, mcause),
    MaskedRegMap(Mtval, mtval),
    MaskedRegMap(Mip, mip.asUInt, 0.U, MaskedRegMap.Unwritable),

    // Machine Memory Protection
    MaskedRegMap(Pmpcfg0, pmpcfg0),
    MaskedRegMap(Pmpcfg1, pmpcfg1),
    MaskedRegMap(Pmpcfg2, pmpcfg2),
    MaskedRegMap(Pmpcfg3, pmpcfg3),
    MaskedRegMap(PmpaddrBase + 0, pmpaddr0),
    MaskedRegMap(PmpaddrBase + 1, pmpaddr1),
    MaskedRegMap(PmpaddrBase + 2, pmpaddr2),
    MaskedRegMap(PmpaddrBase + 3, pmpaddr3)

  ) ++
    perfCntsLoMapping ++ (if (XLEN == 32) perfCntsHiMapping else Nil) ++
    (if(HasFPU) fcsrMapping else Nil)

  val addr = src2(11, 0)
  val rdata = Wire(UInt(XLEN.W))
  val csri = ZeroExt(io.cfIn.instr(19,15), XLEN) //unsigned imm for csri. [TODO]
  val wdata = LookupTree(func, List(
    CSROpType.wrt  -> src1,
    CSROpType.set  -> (rdata | src1),
    CSROpType.clr  -> (rdata & (~src1).asUInt()),
    CSROpType.wrti -> csri,//TODO: csri --> src2
    CSROpType.seti -> (rdata | csri),
    CSROpType.clri -> (rdata & (~csri).asUInt())
  ))

  // satp wen check
  val satpLegalMode = (wdata.asTypeOf(new SatpStruct).mode===0.U) || (wdata.asTypeOf(new SatpStruct).mode===8.U)	  

  // general CSR wen check
  val wen = valid && func =/= CSROpType.jmp && (addr=/=Satp.U || satpLegalMode)
  val permitted = csrAccessPermissionCheck(addr, false.B, priviledgeMode) 
  // Writeable check is ingored.
  // Currently, write to illegal csr addr will be ignored
  MaskedRegMap.generate(mapping, addr, rdata, wen && permitted, wdata)
  io.out.bits := rdata

  // Fix Mip/Sip write
  val fixMapping = Map(
    MaskedRegMap(Mip, mipReg.asUInt, mipFixMask),
    MaskedRegMap(Sip, mipReg.asUInt, sipMask, MaskedRegMap.NoSideEffect, sipMask)
  )
  val rdataDummy = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(fixMapping, addr, rdataDummy, wen, wdata)

  when(io.fpu_csr.fflags.asUInt() =/= 0.U){
    fcsr := fflags_wfn(io.fpu_csr.fflags.asUInt())
  }
  // set fs and sd in mstatus
  when(csrw_dirty_fp_state || io.fpu_csr.dirty_fs){
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.fs := "b11".U
    mstatusNew.sd := true.B
    mstatus := mstatusNew.asUInt()
  }
  io.fpu_csr.frm := fcsr.asTypeOf(new FcsrStruct).frm

  // CSR inst decode
  val isEbreak = addr === privEbreak && func === CSROpType.jmp
  val isEcall = addr === privEcall && func === CSROpType.jmp
  val isMret = addr === privMret   && func === CSROpType.jmp
  val isSret = addr === privSret   && func === CSROpType.jmp
  val isUret = addr === privUret   && func === CSROpType.jmp

  XSDebug(wen, "csr write: pc %x addr %x rdata %x wdata %x func %x\n", io.cfIn.pc, addr, rdata, wdata, func)
  XSDebug(wen, "pc %x mstatus %x mideleg %x medeleg %x mode %x\n", io.cfIn.pc, mstatus, mideleg , medeleg, priviledgeMode)

  // Illegal priviledged operation list
  val illegalSModeSret = valid && isSret && priviledgeMode === ModeS && mstatusStruct.tsr.asBool

  // Illegal priviledged instruction check
  val isIllegalAddr = MaskedRegMap.isIllegalAddr(mapping, addr)
  val isIllegalAccess = !permitted
  val isIllegalPrivOp = illegalSModeSret

  // def MMUPermissionCheck(ptev: Bool, pteu: Bool): Bool = ptev && !(priviledgeMode === ModeU && !pteu) && !(priviledgeMode === ModeS && pteu && mstatusStruct.sum.asBool)
  // def MMUPermissionCheckLoad(ptev: Bool, pteu: Bool): Bool = ptev && !(priviledgeMode === ModeU && !pteu) && !(priviledgeMode === ModeS && pteu && mstatusStruct.sum.asBool) && (pter || (mstatusStruct.mxr && ptex))
  // imem
  // val imemPtev = true.B
  // val imemPteu = true.B
  // val imemPtex = true.B
  // val imemReq = true.B
  // val imemPermissionCheckPassed = MMUPermissionCheck(imemPtev, imemPteu)
  // val hasInstrPageFault = imemReq && !(imemPermissionCheckPassed && imemPtex)
  // assert(!hasInstrPageFault)

  // dmem
  // val dmemPtev = true.B
  // val dmemPteu = true.B
  // val dmemReq = true.B
  // val dmemPermissionCheckPassed = MMUPermissionCheck(dmemPtev, dmemPteu)
  // val dmemIsStore = true.B

  // val hasLoadPageFault  = dmemReq && !dmemIsStore && !(dmemPermissionCheckPassed)
  // val hasStorePageFault = dmemReq &&  dmemIsStore && !(dmemPermissionCheckPassed)
  // assert(!hasLoadPageFault)
  // assert(!hasStorePageFault)

  //TODO: Havn't test if io.dmemMMU.priviledgeMode is correct yet
  tlbBundle.priv.mxr   := mstatusStruct.mxr.asBool
  tlbBundle.priv.sum   := mstatusStruct.sum.asBool
  tlbBundle.priv.imode := priviledgeMode
  tlbBundle.priv.dmode := Mux(mstatusStruct.mprv.asBool, mstatusStruct.mpp, priviledgeMode)

  val hasInstrPageFault = io.exception.bits.cf.exceptionVec(instrPageFault) && io.exception.valid
  val hasLoadPageFault = io.exception.bits.cf.exceptionVec(loadPageFault) && io.exception.valid
  val hasStorePageFault = io.exception.bits.cf.exceptionVec(storePageFault) && io.exception.valid
  val hasStoreAddrMisaligned = io.exception.bits.cf.exceptionVec(storeAddrMisaligned) && io.exception.valid
  val hasLoadAddrMisaligned = io.exception.bits.cf.exceptionVec(loadAddrMisaligned) && io.exception.valid

  // mtval write logic
  val lsroqExceptionAddr = WireInit(0.U(VAddrBits.W))
  if(EnableUnifiedLSQ){
    ExcitingUtils.addSource(io.exception.bits.lsroqIdx, "EXECPTION_LSROQIDX")
    ExcitingUtils.addSink(lsroqExceptionAddr, "EXECPTION_VADDR")
  } else {
    val lsIdx = WireInit(0.U.asTypeOf(new LSIdx()))
    lsIdx.lqIdx := io.exception.bits.lqIdx
    lsIdx.sqIdx := io.exception.bits.sqIdx
    ExcitingUtils.addSource(lsIdx, "EXECPTION_LSROQIDX")
    val lqExceptionAddr = WireInit(0.U(VAddrBits.W))
    val sqExceptionAddr = WireInit(0.U(VAddrBits.W))
    ExcitingUtils.addSink(lqExceptionAddr, "EXECPTION_LOAD_VADDR")
    ExcitingUtils.addSink(sqExceptionAddr, "EXECPTION_STORE_VADDR")
    lsroqExceptionAddr := Mux(CommitType.lsInstIsStore(io.exception.bits.ctrl.commitType), sqExceptionAddr, lqExceptionAddr)
  }

  val atomExceptionAddr = WireInit(0.U(VAddrBits.W))
  val atomOverrideXtval = WireInit(false.B)
  ExcitingUtils.addSink(atomExceptionAddr, "ATOM_EXECPTION_VADDR")
  ExcitingUtils.addSink(atomOverrideXtval, "ATOM_OVERRIDE_XTVAL")
  val memExceptionAddr = Mux(atomOverrideXtval, atomExceptionAddr, lsroqExceptionAddr)
  when(hasInstrPageFault || hasLoadPageFault || hasStorePageFault){
    val tval = Mux(
      hasInstrPageFault,
      Mux(
        io.exception.bits.cf.crossPageIPFFix,
        SignExt(io.exception.bits.cf.pc + 2.U, XLEN),
        SignExt(io.exception.bits.cf.pc, XLEN)
      ),
      SignExt(memExceptionAddr, XLEN)
    )
    when(priviledgeMode === ModeM){
      mtval := tval
    }.otherwise{
      stval := tval
    }
  }

  when(hasLoadAddrMisaligned || hasStoreAddrMisaligned)
  {
    mtval := SignExt(memExceptionAddr, XLEN)
  }

  // Exception and Intr

  // interrupts

  val ideleg =  (mideleg & mip.asUInt)
  def priviledgedEnableDetect(x: Bool): Bool = Mux(x, ((priviledgeMode === ModeS) && mstatusStruct.ie.s) || (priviledgeMode < ModeS),
    ((priviledgeMode === ModeM) && mstatusStruct.ie.m) || (priviledgeMode < ModeM))

  val intrVecEnable = Wire(Vec(12, Bool()))
  intrVecEnable.zip(ideleg.asBools).map{case(x,y) => x := priviledgedEnableDetect(y)}
  val intrVec = mie(11,0) & mip.asUInt & intrVecEnable.asUInt
  val intrBitSet = intrVec.orR()
  ExcitingUtils.addSource(intrBitSet, "intrBitSetIDU")
  val intrNO = IntPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(intrVec(i), i.U, sum))
  val raiseIntr = intrBitSet && io.exception.valid
  XSDebug(raiseIntr, "interrupt: pc=0x%x, %d\n", io.exception.bits.cf.pc, intrNO)

  val mtip = WireInit(false.B)
  val msip = WireInit(false.B)
  val meip = WireInit(false.B)
  ExcitingUtils.addSink(mtip, "mtip")
  ExcitingUtils.addSink(msip, "msip")
  ExcitingUtils.addSink(meip, "meip")
  mipWire.t.m := mtip
  mipWire.s.m := msip
  mipWire.e.m := meip

  // exceptions
  val csrExceptionVec = Wire(Vec(16, Bool()))
  csrExceptionVec.map(_ := false.B)
  csrExceptionVec(breakPoint) := io.in.valid && isEbreak
  csrExceptionVec(ecallM) := priviledgeMode === ModeM && io.in.valid && isEcall
  csrExceptionVec(ecallS) := priviledgeMode === ModeS && io.in.valid && isEcall
  csrExceptionVec(ecallU) := priviledgeMode === ModeU && io.in.valid && isEcall
  // Trigger an illegal instr exception when:
  // * unimplemented csr is being read/written
  // * csr access is illegal
  csrExceptionVec(illegalInstr) := (isIllegalAddr || isIllegalAccess) && wen
  csrExceptionVec(loadPageFault) := hasLoadPageFault
  csrExceptionVec(storePageFault) := hasStorePageFault
  val iduExceptionVec = io.cfIn.exceptionVec
  val exceptionVec = csrExceptionVec.asUInt() | iduExceptionVec.asUInt()
  io.cfOut.exceptionVec.zipWithIndex.map{case (e, i) => e := exceptionVec(i) }
  io.wenFix := DontCare

  val raiseExceptionVec = io.exception.bits.cf.exceptionVec.asUInt()
  val exceptionNO = ExcPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(raiseExceptionVec(i), i.U, sum))
  val causeNO = (raiseIntr << (XLEN-1)).asUInt() | Mux(raiseIntr, intrNO, exceptionNO)
  val difftestIntrNO = Mux(raiseIntr, causeNO, 0.U)
  ExcitingUtils.addSource(difftestIntrNO, "difftestIntrNOfromCSR")
  ExcitingUtils.addSource(causeNO, "difftestCausefromCSR")

  val raiseExceptionIntr = io.exception.valid
  val retTarget = Wire(UInt(VAddrBits.W))
  val trapTarget = Wire(UInt(VAddrBits.W))
  ExcitingUtils.addSource(trapTarget, "trapTarget")
  val resetSatp = addr === Satp.U && wen // write to satp will cause the pipeline be flushed
  io.redirect := DontCare
  io.redirectValid := valid && func === CSROpType.jmp && !isEcall
  io.redirect.target := retTarget
  io.flushPipe := resetSatp

  XSDebug(io.redirectValid, "redirect to %x, pc=%x\n", io.redirect.target, io.cfIn.pc)

  XSDebug(raiseExceptionIntr, "int/exc: pc %x int (%d):%x exc: (%d):%x\n",io.exception.bits.cf.pc, intrNO, io.exception.bits.cf.intrVec.asUInt, exceptionNO, raiseExceptionVec.asUInt)
  XSDebug(raiseExceptionIntr, "pc %x mstatus %x mideleg %x medeleg %x mode %x\n", io.exception.bits.cf.pc, mstatus, mideleg, medeleg, priviledgeMode)

  // Branch control

  val deleg = Mux(raiseIntr, mideleg , medeleg)
  // val delegS = ((deleg & (1 << (causeNO & 0xf))) != 0) && (priviledgeMode < ModeM);
  val delegS = (deleg(causeNO(3,0))) && (priviledgeMode < ModeM)
  val tvalWen = !(hasInstrPageFault || hasLoadPageFault || hasStorePageFault || hasLoadAddrMisaligned || hasStoreAddrMisaligned) || raiseIntr // TODO: need check

  trapTarget := Mux(delegS, stvec, mtvec)(VAddrBits-1, 0)
  retTarget := DontCare
  // val illegalEret = TODO

  when (valid && isMret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.ie.m := mstatusOld.pie.m
    priviledgeMode := mstatusOld.mpp
    mstatusNew.pie.m := true.B
    mstatusNew.mpp := ModeU
    mstatusNew.mprv := 0.U
    mstatus := mstatusNew.asUInt
//    lr := false.B
    retTarget := mepc(VAddrBits-1, 0)
  }

  when (valid && isSret && !illegalSModeSret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.ie.s := mstatusOld.pie.s
    priviledgeMode := Cat(0.U(1.W), mstatusOld.spp)
    mstatusNew.pie.s := true.B
    mstatusNew.spp := ModeU
    mstatus := mstatusNew.asUInt
    mstatusNew.mprv := 0.U
    // lr := false.B
    retTarget := sepc(VAddrBits-1, 0)
  }

  when (valid && isUret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // mstatusNew.mpp.m := ModeU //TODO: add mode U
    mstatusNew.ie.u := mstatusOld.pie.u
    priviledgeMode := ModeU
    mstatusNew.pie.u := true.B
    mstatus := mstatusNew.asUInt
    retTarget := uepc(VAddrBits-1, 0)
  }

  when (raiseExceptionIntr) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))

    when (delegS) {
      scause := causeNO
      sepc := SignExt(io.exception.bits.cf.pc, XLEN)
      mstatusNew.spp := priviledgeMode
      mstatusNew.pie.s := mstatusOld.ie.s
      mstatusNew.ie.s := false.B
      priviledgeMode := ModeS
      when(tvalWen){stval := 0.U}
      // trapTarget := stvec(VAddrBits-1. 0)
    }.otherwise {
      mcause := causeNO
      mepc := SignExt(io.exception.bits.cf.pc, XLEN)
      mstatusNew.mpp := priviledgeMode
      mstatusNew.pie.m := mstatusOld.ie.m
      mstatusNew.ie.m := false.B
      priviledgeMode := ModeM
      when(tvalWen){mtval := 0.U}
      // trapTarget := mtvec(VAddrBits-1. 0)
    }

    mstatus := mstatusNew.asUInt
  }

  io.in.ready := true.B
  io.out.valid := valid


  XSDebug(io.redirectValid, "Rediret %x raiseExcepIntr:%d isSret:%d retTarget:%x sepc:%x delegs:%d deleg:%x cfInpc:%x valid:%d instrValid:%x \n",
    io.redirect.target, raiseExceptionIntr, isSret, retTarget, sepc, delegS, deleg, io.cfIn.pc, valid, io.instrValid)
  XSDebug(raiseExceptionIntr && delegS, "Red(%d, %x) raiseExcepIntr:%d isSret:%d retTarget:%x sepc:%x delegs:%d deleg:%x cfInpc:%x valid:%d instrValid:%x \n",
    io.redirectValid, io.redirect.target, raiseExceptionIntr, isSret, retTarget, sepc, delegS, deleg, io.cfIn.pc, valid, io.instrValid)
  XSDebug(raiseExceptionIntr && delegS, "sepc is writen!!! pc:%x\n", io.cfIn.pc)


  // perfcnt

  val perfCntList = Map(
//    "Mcycle"      -> (0xb00, "perfCntCondMcycle"     ),
//    "Minstret"    -> (0xb02, "perfCntCondMinstret"   ),
    "MbpInstr"    -> (0xb03, "perfCntCondMbpInstr"   ),
    "MbpRight"    -> (0xb04, "perfCntCondMbpRight"   ),
    "MbpWrong"    -> (0xb05, "perfCntCondMbpWrong"   ),
    "MbpBRight"   -> (0xb06, "perfCntCondMbpBRight"   ),
    "MbpBWrong"   -> (0xb07, "perfCntCondMbpBWrong"   ),
    "MbpJRight"   -> (0xb08, "perfCntCondMbpJRight"   ),
    "MbpJWrong"   -> (0xb09, "perfCntCondMbpJWrong"   ),
    "MbpIRight"   -> (0xb0a, "perfCntCondMbpIRight"   ),
    "MbpIWrong"   -> (0xb0b, "perfCntCondMbpIWrong"   ),
    "MbpRRight"   -> (0xb0c, "perfCntCondMbpRRight"   ),
    "MbpRWrong"   -> (0xb0d, "perfCntCondMbpRWrong"   ),
    "DTlbReqCnt0" -> (0xb15, "perfCntDtlbReqCnt0"     ),
    "DTlbReqCnt1" -> (0xb16, "perfCntDtlbReqCnt1"     ),
    "DTlbReqCnt2" -> (0xb17, "perfCntDtlbReqCnt2"     ),
    "DTlbReqCnt3" -> (0xb18, "perfCntDtlbReqCnt3"     ),
    "DTlbMissCnt0"-> (0xb19, "perfCntDtlbMissCnt0"    ),
    "DTlbMissCnt1"-> (0xb20, "perfCntDtlbMissCnt1"    ),
    "DTlbMissCnt2"-> (0xb21, "perfCntDtlbMissCnt2"    ),
    "DTlbMissCnt3"-> (0xb22, "perfCntDtlbMissCnt3"    ),
    "PtwReqCnt"   -> (0xb23, "perfCntPtwReqCnt"       ),
    "PtwCycleCnt" -> (0xb24, "perfCntPtwCycleCnt"     ),
    "PtwL2TlbHit" -> (0xb25, "perfCntPtwL2TlbHit"     ),
    "CacheLoadMiss0" -> (0xb26, "perfCntCacheLoadMiss0"  ),
    "CacheLoadMiss1" -> (0xb27, "perfCntCacheLoadMiss1"  ),
    "ITlbReqCnt0" -> (0xb28, "perfCntItlbReqCnt0"     ),
    "ITlbMissCnt0"-> (0xb29, "perfCntItlbMissCnt0"    ),
    "PtwReqCnt"   -> (0xb2a, "perfCntPtwReqCnt"       ),
    "PtwCycleCnt" -> (0xb2b, "perfCntPtwCycleCnt"     ),
    "PtwL2TlbHit" -> (0xb2c, "perfCntPtwL2TlbHit"     ),
    "ICacheReq"   -> (0xb2d, "perfCntIcacheReqCnt"     ),
    "ICacheMiss"   -> (0xb2e, "perfCntIcacheMissCnt"     )
//    "Custom1"     -> (0xb1b, "Custom1"             ),
//    "Custom2"     -> (0xb1c, "Custom2"             ),
//    "Custom3"     -> (0xb1d, "Custom3"             ),
//    "Custom4"     -> (0xb1e, "Custom4"             ),
//    "Custom5"     -> (0xb1f, "Custom5"             ),
//    "Custom6"     -> (0xb20, "Custom6"             ),
//    "Custom7"     -> (0xb21, "Custom7"             ),
//    "Custom8"     -> (0xb22, "Custom8"             ),
//    "Ml2cacheHit" -> (0xb23, "perfCntCondMl2cacheHit")
  )
  val perfCntCond = List.fill(0x80)(WireInit(false.B))
  (perfCnts zip perfCntCond).map { case (c, e) => when (e) { c := c + 1.U } }

//  ExcitingUtils.addSource(WireInit(true.B), "perfCntCondMcycle", ConnectionType.Perf)
  perfCntList.foreach {
    case (_, (address, boringId)) =>
      if(hasPerfCnt){
        ExcitingUtils.addSink(perfCntCond(address & 0x7f), boringId, ConnectionType.Perf)
      }
//      if (!hasPerfCnt) {
//        // do not enable perfcnts except for Mcycle and Minstret
//        if (address != perfCntList("Mcycle")._1 && address != perfCntList("Minstret")._1) {
//          perfCntCond(address & 0x7f) := false.B
//        }
//      }
  }

  val xstrap = WireInit(false.B)
  if(!env.FPGAPlatform && EnableBPU){
    ExcitingUtils.addSink(xstrap, "XSTRAP", ConnectionType.Debug)
  }
  def readWithScala(addr: Int): UInt = mapping(addr)._1

  if (!env.FPGAPlatform) {

    // display all perfcnt when nooptrap is executed
    when (xstrap) {
      printf("======== PerfCnt =========\n")
      perfCntList.toSeq.sortBy(_._2._1).foreach { case (str, (address, boringId)) =>
        printf("%d <- " + str + "\n", readWithScala(address))
      }
    }

    // for differential testing
//    BoringUtils.addSource(RegNext(priviledgeMode), "difftestMode")
//    BoringUtils.addSource(RegNext(mstatus), "difftestMstatus")
//    BoringUtils.addSource(RegNext(mstatus & sstatusRmask), "difftestSstatus")
//    BoringUtils.addSource(RegNext(mepc), "difftestMepc")
//    BoringUtils.addSource(RegNext(sepc), "difftestSepc")
//    BoringUtils.addSource(RegNext(mcause), "difftestMcause")
//    BoringUtils.addSource(RegNext(scause), "difftestScause")
    BoringUtils.addSource(priviledgeMode, "difftestMode")
    BoringUtils.addSource(mstatus, "difftestMstatus")
    BoringUtils.addSource(mstatus & sstatusRmask, "difftestSstatus")
    BoringUtils.addSource(mepc, "difftestMepc")
    BoringUtils.addSource(sepc, "difftestSepc")
    BoringUtils.addSource(mtval, "difftestMtval")
    BoringUtils.addSource(stval, "difftestStval")
    BoringUtils.addSource(mtvec, "difftestMtvec")
    BoringUtils.addSource(stvec, "difftestStvec")
    BoringUtils.addSource(mcause, "difftestMcause")
    BoringUtils.addSource(scause, "difftestScause")
    BoringUtils.addSource(satp, "difftestSatp")
    BoringUtils.addSource(mipReg, "difftestMip")
    BoringUtils.addSource(mie, "difftestMie")
    BoringUtils.addSource(mscratch, "difftestMscratch")
    BoringUtils.addSource(sscratch, "difftestSscratch")
    BoringUtils.addSource(mideleg, "difftestMideleg")
    BoringUtils.addSource(medeleg, "difftestMedeleg")
  } else {
//    BoringUtils.addSource(readWithScala(perfCntList("Minstret")._1), "ilaInstrCnt")
  }
}
