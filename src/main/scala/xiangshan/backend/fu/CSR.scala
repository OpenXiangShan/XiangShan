/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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
import freechips.rocketchip.util._
import utils.MaskedRegMap.WritableMask
import utils._
import xiangshan._
import xiangshan.backend._
import xiangshan.cache._
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

  def singleStep          = 14

  val ExcPriority = Seq(
    breakPoint, // TODO: different BP has different priority
    singleStep,
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
  val perfEventsFrontend  = (new PerfEventsBundle(numCSRPCntFrontend ))
  val perfEventsCtrl      = (new PerfEventsBundle(numCSRPCntCtrl     ))
  val perfEventsLsu       = (new PerfEventsBundle(numCSRPCntLsu      ))
  val perfEventsHc        = Vec(numPCntHc * coreParams.L2NBanks,(UInt(6.W)))
  val retiredInstr = UInt(3.W)
  val frontendInfo = new Bundle {
    val ibufFull  = Bool()
    val bpuInfo = new Bundle {
      val bpRight = UInt(XLEN.W)
      val bpWrong = UInt(XLEN.W)
    }
  }
  val ctrlInfo = new Bundle {
    val robFull   = Bool()
    val intdqFull = Bool()
    val fpdqFull  = Bool()
    val lsdqFull  = Bool()
  }
  val memInfo = new Bundle {
    val sqFull = Bool()
    val lqFull = Bool()
    val dcacheMSHRFull = Bool()
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
  // Debug Mode
  val singleStep = Output(Bool())
  val debugMode = Output(Bool())
  // Custom microarchiture ctrl signal
  val customCtrl = Output(new CustomCSRCtrlIO)
  val distributedUpdate = Flipped(new DistributedCSRUpdateReq)
  // to Fence to disable sfence
  val disableSfence = Output(Bool())
  // distributed csr w
}

class CSR(implicit p: Parameters) extends FunctionUnit with HasCSRConst with PMPMethod with PMAMethod
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

  class DcsrStruct extends Bundle {
    val xdebugver = Output(UInt(2.W))
    val zero4 = Output(UInt(2.W))
    val zero3 = Output(UInt(12.W))
    val ebreakm = Output(Bool())
    val ebreakh = Output(Bool())
    val ebreaks = Output(Bool())
    val ebreaku = Output(Bool())
    val zero2 = Output(Bool())
    val stopcycle = Output(Bool())
    val stoptime = Output(Bool())
    val cause = Output(UInt(3.W))
    val zero1 = Output(UInt(3.W))
    val step = Output(Bool())
    val prv = Output(UInt(2.W))
  }

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

  class Interrupt extends Bundle {
//  val d = Output(Bool())    // Debug
    val e = new Priv
    val t = new Priv
    val s = new Priv
  }

  // Debug CSRs
  val dcsr = RegInit(UInt(32.W), 0x4000b010.U)
  val dpc = Reg(UInt(64.W))
  val dscratch = Reg(UInt(64.W))
  val dscratch1 = Reg(UInt(64.W))
  val debugMode = RegInit(false.B)
  val debugIntrEnable = RegInit(true.B)
  csrio.debugMode := debugMode

  val dpcPrev = RegNext(dpc)
  XSDebug(dpcPrev =/= dpc, "Debug Mode: dpc is altered! Current is %x, previous is %x.", dpc, dpcPrev)

  // dcsr value table
  // | debugver | 0100
  // | zero     | 10 bits of 0
  // | ebreakvs | 0
  // | ebreakvu | 0
  // | ebreakm  | 1 if ebreak enters debug
  // | zero     | 0
  // | ebreaks  |
  // | ebreaku  |
  // | stepie   | 0 disable interrupts in singlestep
  // | stopcount| stop counter, 0
  // | stoptime | stop time, 0
  // | cause    | 3 bits read only
  // | v        | 0
  // | mprven   | 1
  // | nmip     | read only
  // | step     |
  // | prv      | 2 bits

  val dcsrData = Wire(new DcsrStruct)
  dcsrData := dcsr.asTypeOf(new DcsrStruct)
  val dcsrMask = ZeroExt(GenMask(15) | GenMask(13, 11) | GenMask(2, 0), XLEN)// Dcsr write mask
  def dcsrUpdateSideEffect(dcsr: UInt): UInt = {
    val dcsrOld = WireInit(dcsr.asTypeOf(new DcsrStruct))
    val dcsrNew = dcsr | (dcsrOld.prv(0) | dcsrOld.prv(1)).asUInt // turn 10 priv into 11
    dcsrNew
  }
  csrio.singleStep := dcsrData.step

  // Machine-Level CSRs

  val mtvec = RegInit(UInt(XLEN.W), 0.U)
  val mcounteren = RegInit(UInt(XLEN.W), 0.U)
  val mcause = RegInit(UInt(XLEN.W), 0.U)
  val mtval = RegInit(UInt(XLEN.W), 0.U)
  val mepc = Reg(UInt(XLEN.W))

  val mie = RegInit(0.U(XLEN.W))
  val mipWire = WireInit(0.U.asTypeOf(new Interrupt))
  val mipReg  = RegInit(0.U(XLEN.W))
  val mipFixMask = ZeroExt(GenMask(9) | GenMask(5) | GenMask(1), XLEN)
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

  // PMP Mapping
  val pmp = Wire(Vec(NumPMP, new PMPEntry())) // just used for method parameter
  val pma = Wire(Vec(NumPMA, new PMPEntry())) // just used for method parameter
  val pmpMapping = pmp_gen_mapping(pmp_init, NumPMP, PmpcfgBase, PmpaddrBase, pmp)
  val pmaMapping = pmp_gen_mapping(pma_init, NumPMA, PmacfgBase, PmaaddrBase, pma)

  // Superviser-Level CSRs

  // val sstatus = RegInit(UInt(XLEN.W), "h00000000".U)
  val sstatusWmask = "hc6122".U(XLEN.W)
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
  val sipWMask = "h2".U(XLEN.W) // ssip is writeable in smode
  val satp = if(EnbaleTlbDebug) RegInit(UInt(XLEN.W), "h8000000000087fbe".U) else RegInit(0.U(XLEN.W))
  // val satp = RegInit(UInt(XLEN.W), "h8000000000087fbe".U) // only use for tlb naive debug
  // val satpMask = "h80000fffffffffff".U(XLEN.W) // disable asid, mode can only be 8 / 0
  // TODO: use config to control the length of asid
  // val satpMask = "h8fffffffffffffff".U(XLEN.W) // enable asid, mode can only be 8 / 0
  val satpMask = Cat("h8".U(4.W),Asid_true_mask(AsidLength),"hfffffffffff".U((XLEN - 4 - 16).W))
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
  csrio.customCtrl.storeset_wait_store := slvpredctl(2)
  csrio.customCtrl.storeset_no_fast_wakeup := slvpredctl(3)
  csrio.customCtrl.lvpred_timeout := slvpredctl(8, 4)

  // smblockctl: memory block configurations
  // bits 0-3: store buffer flush threshold (default: 8 entries)
  val smblockctl_init_val =
    ("hf".U & StoreBufferThreshold.U) |
    (EnableLdVioCheckAfterReset.B.asUInt << 4)
  val smblockctl = RegInit(UInt(XLEN.W), smblockctl_init_val)
  csrio.customCtrl.sbuffer_threshold := smblockctl(3, 0)
  // bits 4: enable load load violation check
  csrio.customCtrl.ldld_vio_check := smblockctl(4)

  val srnctl = RegInit(UInt(XLEN.W), "h3".U)
  csrio.customCtrl.move_elim_enable := srnctl(0)
  csrio.customCtrl.svinval_enable := srnctl(1)

  val tlbBundle = Wire(new TlbCsrBundle)
  tlbBundle.satp.apply(satp)

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

  //val perfEventscounten = List.fill(nrPerfCnts)(RegInit(false(Bool())))
  // Perf Counter
  val nrPerfCnts = 29  // 3...31
  val priviledgeModeOH = UIntToOH(priviledgeMode)
  val perfEventscounten = RegInit(0.U.asTypeOf(Vec(nrPerfCnts, Bool())))
  val perfCnts   = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val perfEvents = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  for (i <-0 until nrPerfCnts) {
    perfEventscounten(i) := (Cat(perfEvents(i)(62),perfEvents(i)(61),(perfEvents(i)(61,60))) & priviledgeModeOH).orR
  }

  val hpmEvents = Wire(new PerfEventsBundle(numPCntHc * coreParams.L2NBanks))
  val pfevent = Module(new PFEvent)
  pfevent.io.distribute_csr := csrio.customCtrl.distribute_csr
  for(i <- 0 until numPCntHc * coreParams.L2NBanks) {
    hpmEvents.perf_events(i).incr_step := csrio.perf.perfEventsHc(i)
  }

  val hpm_hc = Module(new HPerfmonitor(numPCntHc * coreParams.L2NBanks,numCSRPCntHc))
  val csrevents = pfevent.io.hpmevent.slice(24,29)
  hpm_hc.io.hpm_event := csrevents
  hpm_hc.io.events_sets := hpmEvents
  val mcountinhibit = RegInit(0.U(XLEN.W))
  val mcycle = RegInit(0.U(XLEN.W))
  mcycle := mcycle + 1.U
  val minstret = RegInit(0.U(XLEN.W))
  minstret := minstret + RegNext(csrio.perf.retiredInstr)
  perfCnts( 0)  := Mux((mcountinhibit( 3) | perfEventscounten( 0)),perfCnts( 0) , (perfCnts( 0) + RegNext(csrio.perf.perfEventsFrontend.perf_events(0 ).incr_step)))
  perfCnts( 1)  := Mux((mcountinhibit( 4) | perfEventscounten( 1)),perfCnts( 1) , (perfCnts( 1) + RegNext(csrio.perf.perfEventsFrontend.perf_events(1 ).incr_step)))
  perfCnts( 2)  := Mux((mcountinhibit( 5) | perfEventscounten( 2)),perfCnts( 2) , (perfCnts( 2) + RegNext(csrio.perf.perfEventsFrontend.perf_events(2 ).incr_step)))
  perfCnts( 3)  := Mux((mcountinhibit( 6) | perfEventscounten( 3)),perfCnts( 3) , (perfCnts( 3) + RegNext(csrio.perf.perfEventsFrontend.perf_events(3 ).incr_step)))
  perfCnts( 4)  := Mux((mcountinhibit( 7) | perfEventscounten( 4)),perfCnts( 4) , (perfCnts( 4) + RegNext(csrio.perf.perfEventsFrontend.perf_events(4 ).incr_step)))
  perfCnts( 5)  := Mux((mcountinhibit( 8) | perfEventscounten( 5)),perfCnts( 5) , (perfCnts( 5) + RegNext(csrio.perf.perfEventsFrontend.perf_events(5 ).incr_step)))
  perfCnts( 6)  := Mux((mcountinhibit( 9) | perfEventscounten( 6)),perfCnts( 6) , (perfCnts( 6) + RegNext(csrio.perf.perfEventsFrontend.perf_events(6 ).incr_step)))
  perfCnts( 7)  := Mux((mcountinhibit(10) | perfEventscounten( 7)),perfCnts( 7) , (perfCnts( 7) + RegNext(csrio.perf.perfEventsFrontend.perf_events(7 ).incr_step)))
  perfCnts( 8)  := Mux((mcountinhibit(11) | perfEventscounten( 8)),perfCnts( 8) , (perfCnts( 8) + RegNext(csrio.perf.perfEventsCtrl.perf_events(0 ).incr_step)))
  perfCnts( 9)  := Mux((mcountinhibit(12) | perfEventscounten( 9)),perfCnts( 9) , (perfCnts( 9) + RegNext(csrio.perf.perfEventsCtrl.perf_events(1 ).incr_step)))
  perfCnts(10)  := Mux((mcountinhibit(13) | perfEventscounten(10)),perfCnts(10) , (perfCnts(10) + RegNext(csrio.perf.perfEventsCtrl.perf_events(2 ).incr_step)))
  perfCnts(11)  := Mux((mcountinhibit(14) | perfEventscounten(11)),perfCnts(11) , (perfCnts(11) + RegNext(csrio.perf.perfEventsCtrl.perf_events(3 ).incr_step)))
  perfCnts(12)  := Mux((mcountinhibit(15) | perfEventscounten(12)),perfCnts(12) , (perfCnts(12) + RegNext(csrio.perf.perfEventsCtrl.perf_events(4 ).incr_step)))
  perfCnts(13)  := Mux((mcountinhibit(16) | perfEventscounten(13)),perfCnts(13) , (perfCnts(13) + RegNext(csrio.perf.perfEventsCtrl.perf_events(5 ).incr_step)))
  perfCnts(14)  := Mux((mcountinhibit(17) | perfEventscounten(14)),perfCnts(14) , (perfCnts(14) + RegNext(csrio.perf.perfEventsCtrl.perf_events(6 ).incr_step)))
  perfCnts(15)  := Mux((mcountinhibit(18) | perfEventscounten(15)),perfCnts(15) , (perfCnts(15) + RegNext(csrio.perf.perfEventsCtrl.perf_events(7 ).incr_step)))
  perfCnts(16)  := Mux((mcountinhibit(19) | perfEventscounten(16)),perfCnts(16) , (perfCnts(16) + RegNext(csrio.perf.perfEventsLsu.perf_events(0 ).incr_step)))
  perfCnts(17)  := Mux((mcountinhibit(20) | perfEventscounten(17)),perfCnts(17) , (perfCnts(17) + RegNext(csrio.perf.perfEventsLsu.perf_events(1 ).incr_step)))
  perfCnts(18)  := Mux((mcountinhibit(21) | perfEventscounten(18)),perfCnts(18) , (perfCnts(18) + RegNext(csrio.perf.perfEventsLsu.perf_events(2 ).incr_step)))
  perfCnts(19)  := Mux((mcountinhibit(22) | perfEventscounten(19)),perfCnts(19) , (perfCnts(19) + RegNext(csrio.perf.perfEventsLsu.perf_events(3 ).incr_step)))
  perfCnts(20)  := Mux((mcountinhibit(23) | perfEventscounten(20)),perfCnts(20) , (perfCnts(20) + RegNext(csrio.perf.perfEventsLsu.perf_events(4 ).incr_step)))
  perfCnts(21)  := Mux((mcountinhibit(24) | perfEventscounten(21)),perfCnts(21) , (perfCnts(21) + RegNext(csrio.perf.perfEventsLsu.perf_events(5 ).incr_step)))
  perfCnts(22)  := Mux((mcountinhibit(25) | perfEventscounten(22)),perfCnts(22) , (perfCnts(22) + RegNext(csrio.perf.perfEventsLsu.perf_events(6 ).incr_step)))
  perfCnts(23)  := Mux((mcountinhibit(26) | perfEventscounten(23)),perfCnts(23) , (perfCnts(23) + RegNext(csrio.perf.perfEventsLsu.perf_events(7 ).incr_step)))
  perfCnts(24)  := Mux((mcountinhibit(27) | perfEventscounten(24)),perfCnts(24) , (perfCnts(24) + RegNext(hpm_hc.io.events_selected.perf_events(0 ).incr_step)))
  perfCnts(25)  := Mux((mcountinhibit(28) | perfEventscounten(25)),perfCnts(25) , (perfCnts(25) + RegNext(hpm_hc.io.events_selected.perf_events(1 ).incr_step)))
  perfCnts(26)  := Mux((mcountinhibit(29) | perfEventscounten(26)),perfCnts(26) , (perfCnts(26) + RegNext(hpm_hc.io.events_selected.perf_events(2 ).incr_step)))
  perfCnts(27)  := Mux((mcountinhibit(30) | perfEventscounten(27)),perfCnts(27) , (perfCnts(27) + RegNext(hpm_hc.io.events_selected.perf_events(3 ).incr_step)))
  perfCnts(28)  := Mux((mcountinhibit(31) | perfEventscounten(28)),perfCnts(28) , (perfCnts(28) + RegNext(hpm_hc.io.events_selected.perf_events(4 ).incr_step)))

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
    MaskedRegMap(Mvendorid, mvendorid, 0.U(XLEN.W), MaskedRegMap.Unwritable),
    MaskedRegMap(Marchid, marchid, 0.U(XLEN.W), MaskedRegMap.Unwritable),
    MaskedRegMap(Mimpid, mimpid, 0.U(XLEN.W), MaskedRegMap.Unwritable),
    MaskedRegMap(Mhartid, mhartid, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    //--- Machine Trap Setup ---
    MaskedRegMap(Mstatus, mstatus, mstatusMask, mstatusUpdateSideEffect, mstatusMask),
    MaskedRegMap(Misa, misa), // now MXL, EXT is not changeable
    MaskedRegMap(Medeleg, medeleg, "hf3ff".U(XLEN.W)),
    MaskedRegMap(Mideleg, mideleg, "h222".U(XLEN.W)),
    MaskedRegMap(Mie, mie),
    MaskedRegMap(Mtvec, mtvec),
    MaskedRegMap(Mcounteren, mcounteren),

    //--- Machine Trap Handling ---
    MaskedRegMap(Mscratch, mscratch),
    MaskedRegMap(Mepc, mepc),
    MaskedRegMap(Mcause, mcause),
    MaskedRegMap(Mtval, mtval),
    MaskedRegMap(Mip, mip.asUInt, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    //--- Debug Mode ---
    MaskedRegMap(Dcsr, dcsr, dcsrMask, dcsrUpdateSideEffect),
    MaskedRegMap(Dpc, dpc),
    MaskedRegMap(Dscratch, dscratch),
    MaskedRegMap(Dscratch1, dscratch1)
  )

  var perfCntMapping = Map(
    MaskedRegMap(Mcountinhibit, mcountinhibit),
    MaskedRegMap(Mcycle, mcycle),
    MaskedRegMap(Minstret, minstret),
    MaskedRegMap(Mhpmevent3 , perfEvents( 0)),
    MaskedRegMap(Mhpmevent4 , perfEvents( 1)),
    MaskedRegMap(Mhpmevent5 , perfEvents( 2)),
    MaskedRegMap(Mhpmevent6 , perfEvents( 3)),
    MaskedRegMap(Mhpmevent7 , perfEvents( 4)),
    MaskedRegMap(Mhpmevent8 , perfEvents( 5)),
    MaskedRegMap(Mhpmevent9 , perfEvents( 6)),
    MaskedRegMap(Mhpmevent10, perfEvents( 7)),
    MaskedRegMap(Mhpmevent11, perfEvents( 8)),
    MaskedRegMap(Mhpmevent12, perfEvents( 9)),
    MaskedRegMap(Mhpmevent13, perfEvents(10)),
    MaskedRegMap(Mhpmevent14, perfEvents(11)),
    MaskedRegMap(Mhpmevent15, perfEvents(12)),
    MaskedRegMap(Mhpmevent16, perfEvents(13)),
    MaskedRegMap(Mhpmevent17, perfEvents(14)),
    MaskedRegMap(Mhpmevent18, perfEvents(15)),
    MaskedRegMap(Mhpmevent19, perfEvents(16)),
    MaskedRegMap(Mhpmevent20, perfEvents(17)),
    MaskedRegMap(Mhpmevent21, perfEvents(18)),
    MaskedRegMap(Mhpmevent22, perfEvents(19)),
    MaskedRegMap(Mhpmevent23, perfEvents(20)),
    MaskedRegMap(Mhpmevent24, perfEvents(21)),
    MaskedRegMap(Mhpmevent25, perfEvents(22)),
    MaskedRegMap(Mhpmevent26, perfEvents(23)),
    MaskedRegMap(Mhpmevent27, perfEvents(24)),
    MaskedRegMap(Mhpmevent28, perfEvents(25)),
    MaskedRegMap(Mhpmevent29, perfEvents(26)),
    MaskedRegMap(Mhpmevent30, perfEvents(27)),
    MaskedRegMap(Mhpmevent31, perfEvents(28)),
    MaskedRegMap(Mhpmcounter3 , perfCnts( 0)),
    MaskedRegMap(Mhpmcounter4 , perfCnts( 1)),
    MaskedRegMap(Mhpmcounter5 , perfCnts( 2)),
    MaskedRegMap(Mhpmcounter6 , perfCnts( 3)),
    MaskedRegMap(Mhpmcounter7 , perfCnts( 4)),
    MaskedRegMap(Mhpmcounter8 , perfCnts( 5)),
    MaskedRegMap(Mhpmcounter9 , perfCnts( 6)),
    MaskedRegMap(Mhpmcounter10, perfCnts( 7)),
    MaskedRegMap(Mhpmcounter11, perfCnts( 8)),
    MaskedRegMap(Mhpmcounter12, perfCnts( 9)),
    MaskedRegMap(Mhpmcounter13, perfCnts(10)),
    MaskedRegMap(Mhpmcounter14, perfCnts(11)),
    MaskedRegMap(Mhpmcounter15, perfCnts(12)),
    MaskedRegMap(Mhpmcounter16, perfCnts(13)),
    MaskedRegMap(Mhpmcounter17, perfCnts(14)),
    MaskedRegMap(Mhpmcounter18, perfCnts(15)),
    MaskedRegMap(Mhpmcounter19, perfCnts(16)),
    MaskedRegMap(Mhpmcounter20, perfCnts(17)),
    MaskedRegMap(Mhpmcounter21, perfCnts(18)),
    MaskedRegMap(Mhpmcounter22, perfCnts(19)),
    MaskedRegMap(Mhpmcounter23, perfCnts(20)),
    MaskedRegMap(Mhpmcounter24, perfCnts(21)),
    MaskedRegMap(Mhpmcounter25, perfCnts(22)),
    MaskedRegMap(Mhpmcounter26, perfCnts(23)),
    MaskedRegMap(Mhpmcounter27, perfCnts(24)),
    MaskedRegMap(Mhpmcounter28, perfCnts(25)),
    MaskedRegMap(Mhpmcounter29, perfCnts(26)),
    MaskedRegMap(Mhpmcounter30, perfCnts(27)),
    MaskedRegMap(Mhpmcounter31, perfCnts(28)),
  )
  // TODO: mechanism should be implemented later
  // val MhpmcounterStart = Mhpmcounter3
  // val MhpmeventStart   = Mhpmevent3
  // for (i <- 0 until nrPerfCnts) {
  //   perfCntMapping += MaskedRegMap(MhpmcounterStart + i, perfCnts(i))
  //   perfCntMapping += MaskedRegMap(MhpmeventStart + i, perfEvents(i))
  // }

  val cacheopRegs = CacheInstrucion.CacheInsRegisterList.map{case (name, attribute) => {
    name -> RegInit(0.U(attribute("width").toInt.W))
  }}
  val cacheopMapping = CacheInstrucion.CacheInsRegisterList.map{case (name, attribute) => {
    MaskedRegMap(
      Scachebase + attribute("offset").toInt, 
      cacheopRegs(name)
    )
  }}

  val mapping = basicPrivMapping ++
                perfCntMapping ++
                pmpMapping ++
                pmaMapping ++
                emuPerfCntsLoMapping ++
                (if (XLEN == 32) emuPerfCntsHiMapping else Nil) ++
                (if (HasFPU) fcsrMapping else Nil) ++
                (if (HasCustomCSRCacheOp) cacheopMapping else Nil)

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

  // csr access check, special case
  val tvmNotPermit = (priviledgeMode === ModeS && mstatusStruct.tvm.asBool)
  val accessPermitted = !(addr === Satp.U && tvmNotPermit)
  csrio.disableSfence := tvmNotPermit

  // general CSR wen check
  val wen = valid && func =/= CSROpType.jmp && (addr=/=Satp.U || satpLegalMode)
  val modePermitted = csrAccessPermissionCheck(addr, false.B, priviledgeMode)
  val perfcntPermitted = perfcntPermissionCheck(addr, priviledgeMode, mcounteren, scounteren)
  val permitted = Mux(addrInPerfCnt, perfcntPermitted, modePermitted) && accessPermitted

  // Writeable check is ingored.
  // Currently, write to illegal csr addr will be ignored
  MaskedRegMap.generate(mapping, addr, rdata, wen && permitted, wdata)
  io.out.bits.data := rdata
  io.out.bits.uop := io.in.bits.uop
  io.out.bits.uop.cf := cfOut
  io.out.bits.uop.ctrl.flushPipe := flushPipe

  // send distribute csr a w signal
  csrio.customCtrl.distribute_csr.w.valid := wen && permitted
  csrio.customCtrl.distribute_csr.w.bits.data := wdata
  csrio.customCtrl.distribute_csr.w.bits.addr := addr

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
  val isDret   = addr === privDret   && func === CSROpType.jmp

  XSDebug(wen, "csr write: pc %x addr %x rdata %x wdata %x func %x\n", cfIn.pc, addr, rdata, wdata, func)
  XSDebug(wen, "pc %x mstatus %x mideleg %x medeleg %x mode %x\n", cfIn.pc, mstatus, mideleg , medeleg, priviledgeMode)

  // Illegal priviledged operation list
  val illegalSModeSret = valid && isSret && priviledgeMode === ModeS && mstatusStruct.tsr.asBool

  // Illegal priviledged instruction check
  val isIllegalAddr = MaskedRegMap.isIllegalAddr(mapping, addr)
  val isIllegalAccess = !permitted
  val isIllegalPrivOp = illegalSModeSret

  // expose several csr bits for tlb
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

  when (valid && isDret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val dcsrNew = WireInit(dcsr.asTypeOf(new DcsrStruct))
    val debugModeNew = WireInit(debugMode)
    when (dcsr.asTypeOf(new DcsrStruct).prv =/= ModeM) {mstatusNew.mprv := 0.U} //If the new privilege mode is less privileged than M-mode, MPRV in mstatus is cleared.
    mstatus := mstatusNew.asUInt
    priviledgeMode := dcsrNew.prv
    retTarget := dpc(VAddrBits-1, 0)
    debugModeNew := false.B
    debugIntrEnable := true.B
    debugMode := debugModeNew
    XSDebug("Debug Mode: Dret executed, returning to %x.", retTarget)
  }

  when (valid && isMret) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.ie.m := mstatusOld.pie.m
    priviledgeMode := mstatusOld.mpp
    mstatusNew.pie.m := true.B
    mstatusNew.mpp := ModeU
    when (mstatusOld.mpp =/= ModeM) { mstatusNew.mprv := 0.U }
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

  val ebreakCauseException = (priviledgeMode === ModeM && dcsrData.ebreakm) || (priviledgeMode === ModeS && dcsrData.ebreaks) || (priviledgeMode === ModeU && dcsrData.ebreaku)

  val csrExceptionVec = WireInit(cfIn.exceptionVec)
  csrExceptionVec(breakPoint) := io.in.valid && isEbreak && ebreakCauseException
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

  val debugIntr = csrio.externalInterrupt.debug & debugIntrEnable
  XSDebug(debugIntr, "Debug Mode: debug interrupt is asserted and valid!")
  // send interrupt information to ROB
  val intrVecEnable = Wire(Vec(12, Bool()))
  intrVecEnable.zip(ideleg.asBools).map{case(x,y) => x := priviledgedEnableDetect(y)}
  val intrVec = Cat(debugIntr, (mie(11,0) & mip.asUInt & intrVecEnable.asUInt))
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
  val raiseDebugIntr = intrNO === IRQ_DEBUG.U && raiseIntr

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
  val hasbreakPoint = csrio.exception.bits.uop.cf.exceptionVec(breakPoint) && raiseException
  val hasSingleStep = csrio.exception.bits.uop.cf.exceptionVec(singleStep) && raiseException

  val raiseExceptionVec = csrio.exception.bits.uop.cf.exceptionVec
  val exceptionNO = ExcPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(raiseExceptionVec(i), i.U, sum))
  val causeNO = (raiseIntr << (XLEN-1)).asUInt() | Mux(raiseIntr, intrNO, exceptionNO)

  val raiseExceptionIntr = csrio.exception.valid

  val raiseDebugExceptionIntr = !debugMode && hasbreakPoint || raiseDebugIntr || hasSingleStep
  val ebreakEnterParkLoop = debugMode && raiseExceptionIntr // exception in debug mode (except ebrk) changes cmderr. how ???

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

  val debugTrapTarget = Mux(!isEbreak && debugMode, 0x38020808.U, 0x38020800.U) // 0x808 is when an exception occurs in debug mode prog buf exec
  val deleg = Mux(raiseIntr, mideleg , medeleg)
  // val delegS = ((deleg & (1 << (causeNO & 0xf))) != 0) && (priviledgeMode < ModeM);
  val delegS = deleg(causeNO(3,0)) && (priviledgeMode < ModeM)
  val tvalWen = !(hasInstrPageFault || hasLoadPageFault || hasStorePageFault || hasLoadAddrMisaligned || hasStoreAddrMisaligned) || raiseIntr // TODO: need check
  val isXRet = io.in.valid && func === CSROpType.jmp && !isEcall && !isEbreak

  // ctrl block will use theses later for flush
  val isXRetFlag = RegInit(false.B)
  val retTargetReg = Reg(retTarget.cloneType)
  when (io.redirectIn.valid) {
    isXRetFlag := false.B
  }.elsewhen (isXRet) {
    isXRetFlag := true.B
    retTargetReg := retTarget
  }
  csrio.isXRet := isXRetFlag
  csrio.trapTarget := Mux(isXRetFlag,
    retTargetReg,
    Mux(raiseDebugExceptionIntr || ebreakEnterParkLoop, debugTrapTarget,
      Mux(delegS, stvec, mtvec))(VAddrBits-1, 0)
  )

  when (raiseExceptionIntr) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val dcsrNew = WireInit(dcsr.asTypeOf(new DcsrStruct))
    val debugModeNew = WireInit(debugMode)

    when (raiseDebugExceptionIntr) {
      when (raiseDebugIntr) {
        debugModeNew := true.B
        mstatusNew.mprv := false.B
        dpc := SignExt(csrio.exception.bits.uop.cf.pc, XLEN)
        dcsrNew.cause := 1.U
        dcsrNew.prv := priviledgeMode
        priviledgeMode := ModeM
        XSDebug(raiseDebugIntr, "Debug Mode: Trap to %x at pc %x\n", debugTrapTarget, dpc)
      }.elsewhen ((hasbreakPoint || hasSingleStep) && !debugMode) {
        // ebreak or ss in running hart
        debugModeNew := true.B
        dpc := SignExt(csrio.exception.bits.uop.cf.pc, XLEN)
        dcsrNew.cause := Mux(hasbreakPoint, 3.U, 0.U)
        dcsrNew.prv := priviledgeMode // TODO
        priviledgeMode := ModeM
        mstatusNew.mprv := false.B
      }
      dcsr := dcsrNew.asUInt
      debugIntrEnable := false.B
    }.elsewhen (delegS) {
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
    debugMode := debugModeNew
  }

  XSDebug(raiseExceptionIntr && delegS, "sepc is writen!!! pc:%x\n", cfIn.pc)

  // Distributed CSR update req
  //
  // For now we use it to implement customized cache op

  when(csrio.distributedUpdate.w.valid){
    // cacheopRegs can be distributed updated
    CacheInstrucion.CacheInsRegisterList.map{case (name, attribute) => {
      when((Scachebase + attribute("offset").toInt).U === csrio.distributedUpdate.w.bits.addr){
        cacheopRegs(name) := csrio.distributedUpdate.w.bits.data
      }
    }}
  }

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
class PFEvent(implicit p: Parameters) extends XSModule with HasCSRConst  {
  val io = IO(new Bundle {
    val distribute_csr = Flipped(new DistributedCSRIO())
    val hpmevent = Output(Vec(29, UInt(XLEN.W)))
  })

  val w = io.distribute_csr.w

  //val csrevents = Vec(29,RegInit(UInt(XLEN.W), 0.U))
  val csrevent3  = RegInit(UInt(XLEN.W), 0.U)
  val csrevent4  = RegInit(UInt(XLEN.W), 0.U)
  val csrevent5  = RegInit(UInt(XLEN.W), 0.U)
  val csrevent6  = RegInit(UInt(XLEN.W), 0.U)
  val csrevent7  = RegInit(UInt(XLEN.W), 0.U)
  val csrevent8  = RegInit(UInt(XLEN.W), 0.U)
  val csrevent9  = RegInit(UInt(XLEN.W), 0.U)
  val csrevent10 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent11 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent12 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent13 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent14 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent15 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent16 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent17 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent18 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent19 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent20 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent21 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent22 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent23 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent24 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent25 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent26 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent27 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent28 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent29 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent30 = RegInit(UInt(XLEN.W), 0.U)
  val csrevent31 = RegInit(UInt(XLEN.W), 0.U)

  var perfEventMapping = Map(
    MaskedRegMap(Mhpmevent3, csrevent3 ),
    MaskedRegMap(Mhpmevent4, csrevent4 ),
    MaskedRegMap(Mhpmevent5, csrevent5 ),
    MaskedRegMap(Mhpmevent6, csrevent6 ),
    MaskedRegMap(Mhpmevent7, csrevent7 ),
    MaskedRegMap(Mhpmevent8, csrevent8 ),
    MaskedRegMap(Mhpmevent9, csrevent9 ),
    MaskedRegMap(Mhpmevent10,csrevent10),
    MaskedRegMap(Mhpmevent11,csrevent11),
    MaskedRegMap(Mhpmevent12,csrevent12),
    MaskedRegMap(Mhpmevent13,csrevent13),
    MaskedRegMap(Mhpmevent14,csrevent14),
    MaskedRegMap(Mhpmevent15,csrevent15),
    MaskedRegMap(Mhpmevent16,csrevent16),
    MaskedRegMap(Mhpmevent17,csrevent17),
    MaskedRegMap(Mhpmevent18,csrevent18),
    MaskedRegMap(Mhpmevent19,csrevent19),
    MaskedRegMap(Mhpmevent20,csrevent20),
    MaskedRegMap(Mhpmevent21,csrevent21),
    MaskedRegMap(Mhpmevent22,csrevent22),
    MaskedRegMap(Mhpmevent23,csrevent23),
    MaskedRegMap(Mhpmevent24,csrevent24),
    MaskedRegMap(Mhpmevent25,csrevent25),
    MaskedRegMap(Mhpmevent26,csrevent26),
    MaskedRegMap(Mhpmevent27,csrevent27),
    MaskedRegMap(Mhpmevent28,csrevent28),
    MaskedRegMap(Mhpmevent29,csrevent29),
    MaskedRegMap(Mhpmevent30,csrevent30),
    MaskedRegMap(Mhpmevent31,csrevent31),
  )

  val rdata = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(perfEventMapping, w.bits.addr, rdata, w.valid, w.bits.data)
  io.hpmevent( 0) := csrevent3 
  io.hpmevent( 1) := csrevent4 
  io.hpmevent( 2) := csrevent5 
  io.hpmevent( 3) := csrevent6 
  io.hpmevent( 4) := csrevent7 
  io.hpmevent( 5) := csrevent8 
  io.hpmevent( 6) := csrevent9 
  io.hpmevent( 7) := csrevent10
  io.hpmevent( 8) := csrevent11
  io.hpmevent( 9) := csrevent12
  io.hpmevent(10) := csrevent13
  io.hpmevent(11) := csrevent14
  io.hpmevent(12) := csrevent15
  io.hpmevent(13) := csrevent16
  io.hpmevent(14) := csrevent17
  io.hpmevent(15) := csrevent18
  io.hpmevent(16) := csrevent19
  io.hpmevent(17) := csrevent20
  io.hpmevent(18) := csrevent21
  io.hpmevent(19) := csrevent22
  io.hpmevent(20) := csrevent23
  io.hpmevent(21) := csrevent24
  io.hpmevent(22) := csrevent25
  io.hpmevent(23) := csrevent26
  io.hpmevent(24) := csrevent27
  io.hpmevent(25) := csrevent28
  io.hpmevent(26) := csrevent29
  io.hpmevent(27) := csrevent30
  io.hpmevent(28) := csrevent31
}             

