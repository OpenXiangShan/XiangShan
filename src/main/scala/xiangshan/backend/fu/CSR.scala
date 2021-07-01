/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend._
import xiangshan.frontend.BPUCtrl
import xiangshan.backend.fu.util._
import difftest._

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
  val frontendSet = List(
    // instrAddrMisaligned,
    instrAccessFault,
    illegalInstr,
    instrPageFault
  )
  val csrSet = List(
    illegalInstr,
    breakPoint,
    ecallU,
    ecallS,
    ecallM
  )
  val loadUnitSet = List(
    loadAddrMisaligned,
    loadAccessFault,
    loadPageFault
  )
  val storeUnitSet = List(
    storeAddrMisaligned,
    storeAccessFault,
    storePageFault
  )
  val atomicsUnitSet = (loadUnitSet ++ storeUnitSet).distinct
  val allPossibleSet = (frontendSet ++ csrSet ++ loadUnitSet ++ storeUnitSet).distinct
  val csrWbCount = (0 until 16).map(i => if (csrSet.contains(i)) 1 else 0)
  val loadWbCount = (0 until 16).map(i => if (loadUnitSet.contains(i)) 1 else 0)
  val storeWbCount = (0 until 16).map(i => if (storeUnitSet.contains(i)) 1 else 0)
  val atomicsWbCount = (0 until 16).map(i => if (atomicsUnitSet.contains(i)) 1 else 0)
  val writebackCount = (0 until 16).map(i => csrWbCount(i) + atomicsWbCount(i) + loadWbCount(i) + 2 * storeWbCount(i))
  def partialSelect(vec: Vec[Bool], select: Seq[Int], dontCareBits: Boolean = true, falseBits: Boolean = false): Vec[Bool] = {
    if (dontCareBits) {
      val new_vec = Wire(ExceptionVec())
      new_vec := DontCare
      select.map(i => new_vec(i) := vec(i))
      return new_vec
    }
    else if (falseBits) {
      val new_vec = Wire(ExceptionVec())
      new_vec.map(_ := false.B)
      select.map(i => new_vec(i) := vec(i))
      return new_vec
    }
    else {
      val new_vec = Wire(Vec(select.length, Bool()))
      select.zipWithIndex.map{ case(s, i) => new_vec(i) := vec(s) }
      return new_vec
    }
  }
  def selectFrontend(vec: Vec[Bool], dontCareBits: Boolean = true, falseBits: Boolean = false): Vec[Bool] =
    partialSelect(vec, frontendSet, dontCareBits, falseBits)
  def selectCSR(vec: Vec[Bool], dontCareBits: Boolean = true, falseBits: Boolean = false): Vec[Bool] =
    partialSelect(vec, csrSet, dontCareBits, falseBits)
  def selectLoad(vec: Vec[Bool], dontCareBits: Boolean = true, falseBits: Boolean = false): Vec[Bool] =
    partialSelect(vec, loadUnitSet, dontCareBits, falseBits)
  def selectStore(vec: Vec[Bool], dontCareBits: Boolean = true, falseBits: Boolean = false): Vec[Bool] =
    partialSelect(vec, storeUnitSet, dontCareBits, falseBits)
  def selectAtomics(vec: Vec[Bool], dontCareBits: Boolean = true, falseBits: Boolean = false): Vec[Bool] =
    partialSelect(vec, atomicsUnitSet, dontCareBits, falseBits)
  def selectAll(vec: Vec[Bool], dontCareBits: Boolean = true, falseBits: Boolean = false): Vec[Bool] =
    partialSelect(vec, allPossibleSet, dontCareBits, falseBits)
}

class FpuCsrIO extends Bundle {
  val fflags = Output(Valid(UInt(5.W)))
  val isIllegal = Output(Bool())
  val dirty_fs = Output(Bool())
  val frm = Input(UInt(3.W))
}


class PerfCounterIO(implicit p: Parameters) extends XSBundle {
  val retiredInstr = UInt(3.W)
  val frontendInfo = new Bundle {
    val ibufFull  = Bool()
  }
  val ctrlInfo = new Bundle {
    val roqFull   = Bool()
    val intdqFull = Bool()
    val fpdqFull  = Bool()
    val lsdqFull  = Bool()
  }
  val memInfo = new Bundle {
    val sqFull = Bool()
    val lqFull = Bool()
    val dcacheMSHRFull = Bool()
  }
  val bpuInfo = new Bundle {
    val bpRight = UInt(XLEN.W)
    val bpWrong = UInt(XLEN.W)
  }
  val cacheInfo = new Bundle {
    val l2MSHRFull = Bool()
    val l3MSHRFull = Bool()
    val l2nAcquire = UInt(XLEN.W)
    val l2nAcquireMiss = UInt(XLEN.W)
    val l3nAcquire = UInt(XLEN.W)
    val l3nAcquireMiss = UInt(XLEN.W)
  }
}

class CSRFileIO(implicit p: Parameters) extends XSBundle {
  val hartId = Input(UInt(64.W))
  // output (for func === CSROpType.jmp)
  val perf = Input(new PerfCounterIO)
  val isPerfCnt = Output(Bool())
  // to FPU
  val fpu = Flipped(new FpuCsrIO)
  // from rob
  val exception = Flipped(ValidIO(new ExceptionInfo))
  // to ROB
  val isXRet = Output(Bool())
  val trapTarget = Output(UInt(VAddrBits.W))
  val interrupt = Output(Bool())
  // from LSQ
  val memExceptionVAddr = Input(UInt(VAddrBits.W))
  // from outside cpu,externalInterrupt
  val externalInterrupt = new ExternalInterruptIO
  // TLB
  val tlb = Output(new TlbCsrBundle)
  // Custom microarchiture ctrl signal
  val customCtrl = Output(new CustomCSRCtrlIO)
}

class CSR(implicit p: Parameters) extends FunctionUnit with HasCSRConst
{
  val csrio = IO(new CSRFileIO)

  val cfIn = io.in.bits.uop.cf
  val cfOut = Wire(new CtrlFlow)
  cfOut := cfIn
  val flushPipe = Wire(Bool())

  val (valid, src1, src2, func) = (
    io.in.valid,
    io.in.bits.src(0),
    io.in.bits.uop.ctrl.imm,
    io.in.bits.uop.ctrl.fuOpType
  )

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

  def getMisaMxl(mxl: Int): UInt = {mxl.U << (XLEN-2)}.asUInt()
  def getMisaExt(ext: Char): UInt = {1.U << (ext.toInt - 'a'.toInt)}.asUInt()
  var extList = List('a', 's', 'i', 'u')
  if (HasMExtension) { extList = extList :+ 'm' }
  if (HasCExtension) { extList = extList :+ 'c' }
  if (HasFPU) { extList = extList ++ List('f', 'd') }
  val misaInitVal = getMisaMxl(2) | extList.foldLeft(0.U)((sum, i) => sum | getMisaExt(i)) //"h8000000000141105".U
  val misa = RegInit(UInt(XLEN.W), misaInitVal)

  // MXL = 2          | 0 | EXT = b 00 0000 0100 0001 0001 0000 0101
  // (XLEN-1, XLEN-2) |   |(25, 0)  ZY XWVU TSRQ PONM LKJI HGFE DCBA

  val mvendorid = RegInit(UInt(XLEN.W), 0.U) // this is a non-commercial implementation
  val marchid = RegInit(UInt(XLEN.W), 0.U) // return 0 to indicate the field is not implemented
  val mimpid = RegInit(UInt(XLEN.W), 0.U) // provides a unique encoding of the version of the processor implementation
  val mhartid = RegInit(UInt(XLEN.W), csrio.hartId) // the hardware thread running the code
  val mstatus = RegInit(UInt(XLEN.W), 0.U)

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

  val mstatusMask = (~ZeroExt((
    GenMask(XLEN-2, 38) | GenMask(31, 23) | GenMask(10, 9) | GenMask(2) |
    GenMask(37) | // MBE
    GenMask(36) | // SBE
    GenMask(6)    // UBE
  ), 64)).asUInt()

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
  val sipMask = "h222".U & mideleg
  val sipWMask = "h2".U // ssip is writeable in smode
  val satp = if(EnbaleTlbDebug) RegInit(UInt(XLEN.W), "h8000000000087fbe".U) else RegInit(0.U(XLEN.W))
  // val satp = RegInit(UInt(XLEN.W), "h8000000000087fbe".U) // only use for tlb naive debug
  val satpMask = "h80000fffffffffff".U // disable asid, mode can only be 8 / 0
  val sepc = RegInit(UInt(XLEN.W), 0.U)
  val scause = RegInit(UInt(XLEN.W), 0.U)
  val stval = Reg(UInt(XLEN.W))
  val sscratch = RegInit(UInt(XLEN.W), 0.U)
  val scounteren = RegInit(UInt(XLEN.W), 0.U)

  // sbpctl
  // Bits 0-7: {LOOP, RAS, SC, TAGE, BIM, BTB, uBTB}
  val sbpctl = RegInit(UInt(XLEN.W), "h7f".U)
  csrio.customCtrl.bp_ctrl.ubtb_enable := sbpctl(0)
  csrio.customCtrl.bp_ctrl.btb_enable  := sbpctl(1)
  csrio.customCtrl.bp_ctrl.bim_enable  := sbpctl(2)
  csrio.customCtrl.bp_ctrl.tage_enable := sbpctl(3)
  csrio.customCtrl.bp_ctrl.sc_enable   := sbpctl(4)
  csrio.customCtrl.bp_ctrl.ras_enable  := sbpctl(5)
  csrio.customCtrl.bp_ctrl.loop_enable := sbpctl(6)

  // spfctl Bit 0: L1plusCache Prefetcher Enable
  // spfctl Bit 1: L2Cache Prefetcher Enable
  val spfctl = RegInit(UInt(XLEN.W), "h3".U)
  csrio.customCtrl.l1plus_pf_enable := spfctl(0)
  csrio.customCtrl.l2_pf_enable := spfctl(1)

  // sdsid: Differentiated Services ID
  val sdsid = RegInit(UInt(XLEN.W), 0.U)
  csrio.customCtrl.dsid := sdsid

  // slvpredctl: load violation predict settings
  val slvpredctl = RegInit(UInt(XLEN.W), "h70".U) // default reset period: 2^17
  csrio.customCtrl.lvpred_disable := slvpredctl(0)
  csrio.customCtrl.no_spec_load := slvpredctl(1)
  csrio.customCtrl.waittable_timeout := slvpredctl(8, 4)

  // smblockctl: memory block configurations
  // bits 0-3: store buffer flush threshold (default: 8 entries)
  val smblockctl = RegInit(UInt(XLEN.W), "hf".U & StoreBufferThreshold.U)
  csrio.customCtrl.sbuffer_threshold := smblockctl(3, 0)

  val srnctl = RegInit(UInt(XLEN.W), "h1".U)
  csrio.customCtrl.move_elim_enable := srnctl(0)

  val tlbBundle = Wire(new TlbCsrBundle)
  tlbBundle.satp := satp.asTypeOf(new SatpStruct)
  csrio.tlb := tlbBundle

  // User-Level CSRs
  val uepc = Reg(UInt(XLEN.W))

  // fcsr
  class FcsrStruct extends Bundle {
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

  def fflags_wfn(update: Boolean)(wdata: UInt): UInt = {
    val fcsrOld = fcsr.asTypeOf(new FcsrStruct)
    val fcsrNew = WireInit(fcsrOld)
    csrw_dirty_fp_state := true.B
    if (update) {
      fcsrNew.fflags := wdata(4,0) | fcsrOld.fflags
    } else {
      fcsrNew.fflags := wdata(4,0)
    }
    fcsrNew.asUInt()
  }
  def fflags_rfn(rdata:UInt): UInt = rdata(4,0)

  def fcsr_wfn(wdata: UInt): UInt = {
    val fcsrOld = WireInit(fcsr.asTypeOf(new FcsrStruct))
    csrw_dirty_fp_state := true.B
    Cat(fcsrOld.reserved, wdata.asTypeOf(fcsrOld).frm, wdata.asTypeOf(fcsrOld).fflags)
  }

  val fcsrMapping = Map(
    MaskedRegMap(Fflags, fcsr, wfn = fflags_wfn(update = false), rfn = fflags_rfn),
    MaskedRegMap(Frm, fcsr, wfn = frm_wfn, rfn = frm_rfn),
    MaskedRegMap(Fcsr, fcsr, wfn = fcsr_wfn)
  )

  // Atom LR/SC Control Bits
  //  val setLr = WireInit(Bool(), false.B)
  //  val setLrVal = WireInit(Bool(), false.B)
  //  val setLrAddr = WireInit(UInt(AddrBits.W), DontCare) //TODO : need check
  //  val lr = RegInit(Bool(), false.B)
  //  val lrAddr = RegInit(UInt(AddrBits.W), 0.U)
  //
  //  when (setLr) {
  //    lr := setLrVal
  //    lrAddr := setLrAddr
  //  }

  // Hart Priviledge Mode
  val priviledgeMode = RegInit(UInt(2.W), ModeM)

  // Emu perfcnt
  val hasEmuPerfCnt = !env.FPGAPlatform
  val nrEmuPerfCnts = if (hasEmuPerfCnt) 0x80 else 0x3

  val emuPerfCnts    = List.fill(nrEmuPerfCnts)(RegInit(0.U(XLEN.W)))
  val emuPerfCntCond = List.fill(nrEmuPerfCnts)(WireInit(false.B))
  (emuPerfCnts zip emuPerfCntCond).map { case (c, e) => when (e) { c := c + 1.U } }

  val emuPerfCntsLoMapping = (0 until nrEmuPerfCnts).map(i => MaskedRegMap(0x1000 + i, emuPerfCnts(i)))
  val emuPerfCntsHiMapping = (0 until nrEmuPerfCnts).map(i => MaskedRegMap(0x1080 + i, emuPerfCnts(i)(63, 32)))
  println(s"CSR: hasEmuPerfCnt:${hasEmuPerfCnt}")

  // Perf Counter
  val nrPerfCnts = 29  // 3...31
  val perfCnts   = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val perfEvents = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val mcountinhibit = RegInit(0.U(XLEN.W))
  val mcycle = RegInit(0.U(XLEN.W))
  mcycle := mcycle + 1.U
  val minstret = RegInit(0.U(XLEN.W))
  minstret := minstret + RegNext(csrio.perf.retiredInstr)
  val ibufFull  = RegInit(0.U(XLEN.W))
  ibufFull := ibufFull + RegNext(csrio.perf.frontendInfo.ibufFull)
  val roqFull   = RegInit(0.U(XLEN.W))
  roqFull := roqFull + RegNext(csrio.perf.ctrlInfo.roqFull)
  val intdqFull = RegInit(0.U(XLEN.W))
  intdqFull := intdqFull + RegNext(csrio.perf.ctrlInfo.intdqFull)
  val fpdqFull  = RegInit(0.U(XLEN.W))
  fpdqFull := fpdqFull + RegNext(csrio.perf.ctrlInfo.fpdqFull)
  val lsdqFull  = RegInit(0.U(XLEN.W))
  lsdqFull := lsdqFull + RegNext(csrio.perf.ctrlInfo.lsdqFull)
  val sqFull    = RegInit(0.U(XLEN.W))
  sqFull := sqFull + RegNext(csrio.perf.memInfo.sqFull)
  val lqFull    = RegInit(0.U(XLEN.W))
  lqFull := lqFull + RegNext(csrio.perf.memInfo.lqFull)
  val dcacheMSHRFull = RegInit(0.U(XLEN.W))
  dcacheMSHRFull := dcacheMSHRFull + RegNext(csrio.perf.memInfo.dcacheMSHRFull)
  val bpRight   = RegInit(0.U(XLEN.W))
  bpRight := bpRight + RegNext(csrio.perf.bpuInfo.bpRight)
  val bpWrong   = RegInit(0.U(XLEN.W))
  bpWrong := bpWrong + RegNext(csrio.perf.bpuInfo.bpWrong)

  // CSR reg map
  val basicPrivMapping = Map(

    //--- User Trap Setup ---
    // MaskedRegMap(Ustatus, ustatus),
    // MaskedRegMap(Uie, uie, 0.U, MaskedRegMap.Unwritable),
    // MaskedRegMap(Utvec, utvec),

    //--- User Trap Handling ---
    // MaskedRegMap(Uscratch, uscratch),
    // MaskedRegMap(Uepc, uepc),
    // MaskedRegMap(Ucause, ucause),
    // MaskedRegMap(Utval, utval),
    // MaskedRegMap(Uip, uip),

    //--- User Counter/Timers ---
    // MaskedRegMap(Cycle, cycle),
    // MaskedRegMap(Time, time),
    // MaskedRegMap(Instret, instret),

    //--- Supervisor Trap Setup ---
    MaskedRegMap(Sstatus, mstatus, sstatusWmask, mstatusUpdateSideEffect, sstatusRmask),
    // MaskedRegMap(Sedeleg, Sedeleg),
    // MaskedRegMap(Sideleg, Sideleg),
    MaskedRegMap(Sie, mie, sieMask, MaskedRegMap.NoSideEffect, sieMask),
    MaskedRegMap(Stvec, stvec),
    MaskedRegMap(Scounteren, scounteren),

    //--- Supervisor Trap Handling ---
    MaskedRegMap(Sscratch, sscratch),
    MaskedRegMap(Sepc, sepc),
    MaskedRegMap(Scause, scause),
    MaskedRegMap(Stval, stval),
    MaskedRegMap(Sip, mip.asUInt, sipWMask, MaskedRegMap.Unwritable, sipMask),

    //--- Supervisor Protection and Translation ---
    MaskedRegMap(Satp, satp, satpMask, MaskedRegMap.NoSideEffect, satpMask),

    //--- Supervisor Custom Read/Write Registers
    MaskedRegMap(Sbpctl, sbpctl),
    MaskedRegMap(Spfctl, spfctl),
    MaskedRegMap(Sdsid, sdsid),
    MaskedRegMap(Slvpredctl, slvpredctl),
    MaskedRegMap(Smblockctl, smblockctl),
    MaskedRegMap(Srnctl, srnctl),

    //--- Machine Information Registers ---
    MaskedRegMap(Mvendorid, mvendorid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Marchid, marchid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Mimpid, mimpid, 0.U, MaskedRegMap.Unwritable),
    MaskedRegMap(Mhartid, mhartid, 0.U, MaskedRegMap.Unwritable),

    //--- Machine Trap Setup ---
    MaskedRegMap(Mstatus, mstatus, mstatusMask, mstatusUpdateSideEffect, mstatusMask),
    MaskedRegMap(Misa, misa), // now MXL, EXT is not changeable
    MaskedRegMap(Medeleg, medeleg, "hf3ff".U),
    MaskedRegMap(Mideleg, mideleg, "h222".U),
    MaskedRegMap(Mie, mie),
    MaskedRegMap(Mtvec, mtvec),
    MaskedRegMap(Mcounteren, mcounteren),

    //--- Machine Trap Handling ---
    MaskedRegMap(Mscratch, mscratch),
    MaskedRegMap(Mepc, mepc),
    MaskedRegMap(Mcause, mcause),
    MaskedRegMap(Mtval, mtval),
    MaskedRegMap(Mip, mip.asUInt, 0.U, MaskedRegMap.Unwritable),
  )

  // PMP is unimplemented yet
  val pmpMapping = Map(
    MaskedRegMap(Pmpcfg0, pmpcfg0),
    MaskedRegMap(Pmpcfg1, pmpcfg1),
    MaskedRegMap(Pmpcfg2, pmpcfg2),
    MaskedRegMap(Pmpcfg3, pmpcfg3),
    MaskedRegMap(PmpaddrBase + 0, pmpaddr0),
    MaskedRegMap(PmpaddrBase + 1, pmpaddr1),
    MaskedRegMap(PmpaddrBase + 2, pmpaddr2),
    MaskedRegMap(PmpaddrBase + 3, pmpaddr3)
  )

  var perfCntMapping = Map(
    MaskedRegMap(Mcountinhibit, mcountinhibit),
    MaskedRegMap(Mcycle, mcycle),
    MaskedRegMap(Minstret, minstret),
    MaskedRegMap(Mhpmevent3, ibufFull),
    MaskedRegMap(Mhpmevent4, roqFull),
    MaskedRegMap(Mhpmevent5, intdqFull),
    MaskedRegMap(Mhpmevent6, fpdqFull),
    MaskedRegMap(Mhpmevent7, lsdqFull),
    MaskedRegMap(Mhpmevent8, sqFull),
    MaskedRegMap(Mhpmevent9, lqFull),
    MaskedRegMap(Mhpmevent10, dcacheMSHRFull),
    MaskedRegMap(Mhpmevent11, bpRight),
    MaskedRegMap(Mhpmevent12, bpWrong),
  )
  // TODO: mechanism should be implemented later
  // val MhpmcounterStart = Mhpmcounter3
  // val MhpmeventStart   = Mhpmevent3
  // for (i <- 0 until nrPerfCnts) {
  //   perfCntMapping += MaskedRegMap(MhpmcounterStart + i, perfCnts(i))
  //   perfCntMapping += MaskedRegMap(MhpmeventStart + i, perfEvents(i))
  // }

  val mapping = basicPrivMapping ++
                perfCntMapping ++
                pmpMapping ++
                emuPerfCntsLoMapping ++
                (if (XLEN == 32) emuPerfCntsHiMapping else Nil) ++
                (if (HasFPU) fcsrMapping else Nil)

  val addr = src2(11, 0)
  val csri = ZeroExt(src2(16, 12), XLEN)
  val rdata = Wire(UInt(XLEN.W))
  val wdata = LookupTree(func, List(
    CSROpType.wrt  -> src1,
    CSROpType.set  -> (rdata | src1),
    CSROpType.clr  -> (rdata & (~src1).asUInt()),
    CSROpType.wrti -> csri,
    CSROpType.seti -> (rdata | csri),
    CSROpType.clri -> (rdata & (~csri).asUInt())
  ))

  val addrInPerfCnt = (addr >= Mcycle.U) && (addr <= Mhpmcounter31.U)
  csrio.isPerfCnt := addrInPerfCnt

  // satp wen check
  val satpLegalMode = (wdata.asTypeOf(new SatpStruct).mode===0.U) || (wdata.asTypeOf(new SatpStruct).mode===8.U)

  // general CSR wen check
  val wen = valid && func =/= CSROpType.jmp && (addr=/=Satp.U || satpLegalMode)
  val modePermitted = csrAccessPermissionCheck(addr, false.B, priviledgeMode)
  val perfcntPermitted = perfcntPermissionCheck(addr, priviledgeMode, mcounteren, scounteren)
  val permitted = Mux(addrInPerfCnt, perfcntPermitted, modePermitted)
  // Writeable check is ingored.
  // Currently, write to illegal csr addr will be ignored
  MaskedRegMap.generate(mapping, addr, rdata, wen && permitted, wdata)
  io.out.bits.data := rdata
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.uop.cf := cfOut
  io.out.bits.uop.ctrl.flushPipe := flushPipe

  // Fix Mip/Sip write
  val fixMapping = Map(
    MaskedRegMap(Mip, mipReg.asUInt, mipFixMask),
    MaskedRegMap(Sip, mipReg.asUInt, sipWMask, MaskedRegMap.NoSideEffect, sipMask)
  )
  val rdataFix = Wire(UInt(XLEN.W))
  val wdataFix = LookupTree(func, List(
    CSROpType.wrt  -> src1,
    CSROpType.set  -> (rdataFix | src1),
    CSROpType.clr  -> (rdataFix & (~src1).asUInt()),
    CSROpType.wrti -> csri,
    CSROpType.seti -> (rdataFix | csri),
    CSROpType.clri -> (rdataFix & (~csri).asUInt())
  ))
  MaskedRegMap.generate(fixMapping, addr, rdataFix, wen && permitted, wdataFix)

  when (csrio.fpu.fflags.valid) {
    fcsr := fflags_wfn(update = true)(csrio.fpu.fflags.bits)
  }
  // set fs and sd in mstatus
  when (csrw_dirty_fp_state || csrio.fpu.dirty_fs) {
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.fs := "b11".U
    mstatusNew.sd := true.B
    mstatus := mstatusNew.asUInt()
  }
  csrio.fpu.frm := fcsr.asTypeOf(new FcsrStruct).frm

  // CSR inst decode
  val isEbreak = addr === privEbreak && func === CSROpType.jmp
  val isEcall  = addr === privEcall  && func === CSROpType.jmp
  val isMret   = addr === privMret   && func === CSROpType.jmp
  val isSret   = addr === privSret   && func === CSROpType.jmp
  val isUret   = addr === privUret   && func === CSROpType.jmp

  XSDebug(wen, "csr write: pc %x addr %x rdata %x wdata %x func %x\n", cfIn.pc, addr, rdata, wdata, func)
  XSDebug(wen, "pc %x mstatus %x mideleg %x medeleg %x mode %x\n", cfIn.pc, mstatus, mideleg , medeleg, priviledgeMode)

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

  // Branch control
  val retTarget = Wire(UInt(VAddrBits.W))
  val resetSatp = addr === Satp.U && wen // write to satp will cause the pipeline be flushed
  flushPipe := resetSatp || (valid && func === CSROpType.jmp && !isEcall)

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
    // lr := false.B
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

  io.in.ready := true.B
  io.out.valid := valid

  val csrExceptionVec = WireInit(cfIn.exceptionVec)
  csrExceptionVec(breakPoint) := io.in.valid && isEbreak
  csrExceptionVec(ecallM) := priviledgeMode === ModeM && io.in.valid && isEcall
  csrExceptionVec(ecallS) := priviledgeMode === ModeS && io.in.valid && isEcall
  csrExceptionVec(ecallU) := priviledgeMode === ModeU && io.in.valid && isEcall
  // Trigger an illegal instr exception when:
  // * unimplemented csr is being read/written
  // * csr access is illegal
  csrExceptionVec(illegalInstr) := (isIllegalAddr || isIllegalAccess) && wen
  cfOut.exceptionVec := csrExceptionVec

  /**
    * Exception and Intr
    */
  val ideleg =  (mideleg & mip.asUInt)
  def priviledgedEnableDetect(x: Bool): Bool = Mux(x, ((priviledgeMode === ModeS) && mstatusStruct.ie.s) || (priviledgeMode < ModeS),
    ((priviledgeMode === ModeM) && mstatusStruct.ie.m) || (priviledgeMode < ModeM))

  // send interrupt information to ROQ
  val intrVecEnable = Wire(Vec(12, Bool()))
  intrVecEnable.zip(ideleg.asBools).map{case(x,y) => x := priviledgedEnableDetect(y)}
  val intrVec = mie(11,0) & mip.asUInt & intrVecEnable.asUInt
  val intrBitSet = intrVec.orR()
  csrio.interrupt := intrBitSet
  mipWire.t.m := csrio.externalInterrupt.mtip
  mipWire.s.m := csrio.externalInterrupt.msip
  mipWire.e.m := csrio.externalInterrupt.meip
  mipWire.e.s := csrio.externalInterrupt.meip

  // interrupts
  val intrNO = IntPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(intrVec(i), i.U, sum))
  val raiseIntr = csrio.exception.valid && csrio.exception.bits.isInterrupt
  XSDebug(raiseIntr, "interrupt: pc=0x%x, %d\n", csrio.exception.bits.uop.cf.pc, intrNO)

  // exceptions
  val raiseException = csrio.exception.valid && !csrio.exception.bits.isInterrupt
  val hasInstrPageFault = csrio.exception.bits.uop.cf.exceptionVec(instrPageFault) && raiseException
  val hasLoadPageFault = csrio.exception.bits.uop.cf.exceptionVec(loadPageFault) && raiseException
  val hasStorePageFault = csrio.exception.bits.uop.cf.exceptionVec(storePageFault) && raiseException
  val hasStoreAddrMisaligned = csrio.exception.bits.uop.cf.exceptionVec(storeAddrMisaligned) && raiseException
  val hasLoadAddrMisaligned = csrio.exception.bits.uop.cf.exceptionVec(loadAddrMisaligned) && raiseException
  val hasInstrAccessFault = csrio.exception.bits.uop.cf.exceptionVec(instrAccessFault) && raiseException
  val hasLoadAccessFault = csrio.exception.bits.uop.cf.exceptionVec(loadAccessFault) && raiseException
  val hasStoreAccessFault = csrio.exception.bits.uop.cf.exceptionVec(storeAccessFault) && raiseException

  val raiseExceptionVec = csrio.exception.bits.uop.cf.exceptionVec
  val exceptionNO = ExcPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(raiseExceptionVec(i), i.U, sum))
  val causeNO = (raiseIntr << (XLEN-1)).asUInt() | Mux(raiseIntr, intrNO, exceptionNO)

  val raiseExceptionIntr = csrio.exception.valid
  XSDebug(raiseExceptionIntr, "int/exc: pc %x int (%d):%x exc: (%d):%x\n",
    csrio.exception.bits.uop.cf.pc, intrNO, intrVec, exceptionNO, raiseExceptionVec.asUInt
  )
  XSDebug(raiseExceptionIntr,
    "pc %x mstatus %x mideleg %x medeleg %x mode %x\n",
    csrio.exception.bits.uop.cf.pc,
    mstatus,
    mideleg,
    medeleg,
    priviledgeMode
  )

  // mtval write logic
  val memExceptionAddr = SignExt(csrio.memExceptionVAddr, XLEN)
  when (hasInstrPageFault || hasLoadPageFault || hasStorePageFault) {
    val tval = Mux(
      hasInstrPageFault,
      Mux(
        csrio.exception.bits.uop.cf.crossPageIPFFix,
        SignExt(csrio.exception.bits.uop.cf.pc + 2.U, XLEN),
        SignExt(csrio.exception.bits.uop.cf.pc, XLEN)
      ),
      memExceptionAddr
    )
    when (priviledgeMode === ModeM) {
      mtval := tval
    }.otherwise {
      stval := tval
    }
  }

  when (hasLoadAddrMisaligned || hasStoreAddrMisaligned) {
    mtval := memExceptionAddr
  }

  val deleg = Mux(raiseIntr, mideleg , medeleg)
  // val delegS = ((deleg & (1 << (causeNO & 0xf))) != 0) && (priviledgeMode < ModeM);
  val delegS = deleg(causeNO(3,0)) && (priviledgeMode < ModeM)
  val tvalWen = !(hasInstrPageFault || hasLoadPageFault || hasStorePageFault || hasLoadAddrMisaligned || hasStoreAddrMisaligned) || raiseIntr // TODO: need check
  val isXRet = io.in.valid && func === CSROpType.jmp && !isEcall

  // ctrl block will use theses later for flush
  val isXRetFlag = RegInit(false.B)
  val retTargetReg = Reg(retTarget.cloneType)
  when (io.flushIn) {
    isXRetFlag := false.B
  }.elsewhen (isXRet) {
    isXRetFlag := true.B
    retTargetReg := retTarget
  }
  csrio.isXRet := isXRetFlag
  csrio.trapTarget := Mux(isXRetFlag,
    retTargetReg,
    Mux(delegS, stvec, mtvec)(VAddrBits-1, 0)
  )

  when (raiseExceptionIntr) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))

    when (delegS) {
      scause := causeNO
      sepc := SignExt(csrio.exception.bits.uop.cf.pc, XLEN)
      mstatusNew.spp := priviledgeMode
      mstatusNew.pie.s := mstatusOld.ie.s
      mstatusNew.ie.s := false.B
      priviledgeMode := ModeS
      when (tvalWen) { stval := 0.U }
    }.otherwise {
      mcause := causeNO
      mepc := SignExt(csrio.exception.bits.uop.cf.pc, XLEN)
      mstatusNew.mpp := priviledgeMode
      mstatusNew.pie.m := mstatusOld.ie.m
      mstatusNew.ie.m := false.B
      priviledgeMode := ModeM
      when (tvalWen) { mtval := 0.U }
    }

    mstatus := mstatusNew.asUInt
  }

  XSDebug(raiseExceptionIntr && delegS, "sepc is writen!!! pc:%x\n", cfIn.pc)

  def readWithScala(addr: Int): UInt = mapping(addr)._1

  val difftestIntrNO = Mux(raiseIntr, causeNO, 0.U)

  if (!env.FPGAPlatform) {
    val difftest = Module(new DifftestArchEvent)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.intrNO := RegNext(difftestIntrNO)
    difftest.io.cause := RegNext(Mux(csrio.exception.valid, causeNO, 0.U))
    difftest.io.exceptionPC := RegNext(SignExt(csrio.exception.bits.uop.cf.pc, XLEN))
  }

  if (!env.FPGAPlatform) {
    val difftest = Module(new DifftestCSRState)
    difftest.io.clock := clock
    difftest.io.coreid := hardId.U
    difftest.io.priviledgeMode := priviledgeMode
    difftest.io.mstatus := mstatus
    difftest.io.sstatus := mstatus & sstatusRmask
    difftest.io.mepc := mepc
    difftest.io.sepc := sepc
    difftest.io.mtval:= mtval
    difftest.io.stval:= stval
    difftest.io.mtvec := mtvec
    difftest.io.stvec := stvec
    difftest.io.mcause := mcause
    difftest.io.scause := scause
    difftest.io.satp := satp
    difftest.io.mip := mipReg
    difftest.io.mie := mie
    difftest.io.mscratch := mscratch
    difftest.io.sscratch := sscratch
    difftest.io.mideleg := mideleg
    difftest.io.medeleg := medeleg
  }
}
