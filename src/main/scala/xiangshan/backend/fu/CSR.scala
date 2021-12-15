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
import difftest._
import freechips.rocketchip.util._
import utils.MaskedRegMap.WritableMask
import utils._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.fu.util._
import xiangshan.cache._

// Trigger Tdata1 bundles
trait HasTriggerConst {
  def I_Trigger = 0.U
  def S_Trigger = 1.U
  def L_Trigger = 2.U
  def GenESL(triggerType: UInt) = Cat((triggerType === I_Trigger), (triggerType === I_Trigger), (triggerType === I_Trigger))
}

class TdataBundle extends Bundle {
  val hit = Bool()
  val select = Bool()
  val timing = Bool()
//  val size = UInt(4.W) // hardwire to 0
  val action = Bool()
  val chain = Bool()
  val matchType = UInt(2.W)
  val m = Bool()
  val s = Bool()
  val u = Bool()
  val data = UInt(64.W) // tdata2
}

class FpuCsrIO extends Bundle {
  val fflags = Output(Valid(UInt(5.W)))
  val isIllegal = Output(Bool())
  val dirty_fs = Output(Bool())
  val frm = Input(UInt(3.W))
}


class PerfCounterIO(implicit p: Parameters) extends XSBundle {
  val perfEventsFrontend  = Vec(numCSRPCntFrontend, new PerfEvent)
  val perfEventsCtrl      = Vec(numCSRPCntCtrl, new PerfEvent)
  val perfEventsLsu       = Vec(numCSRPCntLsu, new PerfEvent)
  val perfEventsHc        = Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent)
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
  val hartId = Input(UInt(8.W))
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
  // to Fence to disable sfence
  val disableSfence = Output(Bool())
  // Custom microarchiture ctrl signal
  val customCtrl = Output(new CustomCSRCtrlIO)
  // distributed csr write
  val distributedUpdate = Vec(2, Flipped(new DistributedCSRUpdateReq))
}

class CSR(implicit p: Parameters) extends FunctionUnit with HasCSRConst with PMPMethod with PMAMethod with HasTriggerConst
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

    val pad1 = if (XLEN == 64) Output(UInt(25.W)) else null
    val mbe  = if (XLEN == 64) Output(UInt(1.W)) else null
    val sbe  = if (XLEN == 64) Output(UInt(1.W)) else null
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

    def ube = pie.h // a little ugly
    def ube_(r: UInt): Unit = {
      pie.h := r(0)
    }
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

  // Trigger CSRs

  val tdata1_function = Map(
   0.U -> (true, I_Trigger), 1.U -> (false, I_Trigger),
   2.U -> (true, S_Trigger), 3.U -> (false, S_Trigger),
   4.U -> (true, L_Trigger), 5.U -> (false, L_Trigger),
   6.U -> (true, I_Trigger), 7.U -> (false, S_Trigger),
   8.U -> (true, I_Trigger), 9.U -> (false, L_Trigger)
  ).withDefaultValue((false, I_Trigger))
  val tdata1Phy = RegInit(VecInit(List.fill(10) {0.U(64.W).asTypeOf(new TdataBundle)}))
  val tdata2Phy = Reg(Vec(10, UInt(64.W)))
  val tselectPhy = RegInit(0.U(4.W))
  val tDummy = WireInit(0.U(64.W))
  val tControlPhy = RegInit(0.U(64.W))
  def ReadTdata1(rdata: UInt) = {
    val tdata1 = tdata1Phy(tselectPhy)
    Cat(
      2.U(4.W), // type, hardwired
      0.U(1.W), // dmode, hardwired
      0.U(6.W), // maskmax, hardwired to 0 because we don not support
      1.U(2.W), // sizehi, hardwired
      tdata1.hit,
      tdata1.select, // select
      tdata1.timing,
      0.U(2.W), // sizelo
      0.U(3.W), tdata1.action, // action, 0 is breakpoint 1 is enter debug
      tdata1.chain,
      0.U(2.W), tdata1.matchType,
      tdata1.m, false.B, tdata1.s, tdata1.u,
      GenESL(tdata1_function(tselectPhy)._2)
    )
  }
  def WriteTdata1(wdata: UInt) = {
    val tdata1_new = WireInit(tdata1Phy(tselectPhy))
    tdata1_new.hit := wdata(20)
    tdata1_new.select := wdata(19)
    tdata1_new.timing := wdata(18)
    tdata1_new.action := wdata(12)
    tdata1_new.chain := tdata1_function(tselectPhy)._1.B && wdata(11)
    when(wdata(10, 7) === 0.U || wdata(10, 7) === 2.U || wdata(10, 7) === 3.U) {tdata1_new.matchType := wdata(8, 7)}
    tdata1_new.m := wdata(6)
    tdata1_new.s := wdata(4)
    tdata1_new.u := wdata(3)
    tdata1Phy(tselectPhy) := tdata1_new
    0.U
  }

  def ReadTselect(rdata: UInt) = Cat(0.U(60.W), tselectPhy)
  def WriteTselect(wdata: UInt) = {
    when (wdata <= 10.U){
      tselectPhy := wdata(3, 0)
    }
    0.U
  }

  def ReadTdata2(tdata: UInt) = tdata2Phy(tselectPhy)
  def WriteTdata2(wdata: UInt) = {
    tdata2Phy(tselectPhy) := wdata
    0.U
  }

  def ReadTinfo(tdata: UInt) = 2.U(XLEN.W)

  val tcontrolWriteMask = ZeroExt(GenMask(3) | GenMask(7), XLEN)


  def GenTdataDistribute(tdata1: TdataBundle, tdata2: UInt): MatchTriggerIO = {
    val res = Wire(new MatchTriggerIO)
    res.matchType := tdata1.matchType
    res.select := tdata1.select
    res.timing := tdata1.timing
    res.action := tdata1.action
    res.chain := tdata1.chain
    res.tdata2 := tdata2
    res
  }

  csrio.customCtrl.frontend_trigger.t.bits.addr := MuxLookup(tselectPhy, 0.U, Seq(
    0.U -> 0.U,
    1.U -> 1.U,
    6.U -> 2.U,
    8.U -> 3.U
  ))
  csrio.customCtrl.mem_trigger.t.bits.addr := MuxLookup(tselectPhy, 0.U, Seq(
    2.U -> 0.U,
    3.U -> 1.U,
    4.U -> 2.U,
    5.U -> 3.U,
    7.U -> 4.U,
    9.U -> 5.U
  ))
  csrio.customCtrl.frontend_trigger.t.bits.tdata := GenTdataDistribute(tdata1Phy(tselectPhy), tdata2Phy(tselectPhy))
  csrio.customCtrl.mem_trigger.t.bits.tdata := GenTdataDistribute(tdata1Phy(tselectPhy), tdata2Phy(tselectPhy))

  // Machine-Level CSRs
  // mtvec: {BASE (WARL), MODE (WARL)} where mode is 0 or 1
  val mtvecMask = ~(0x2.U(XLEN.W))
  val mtvec = RegInit(UInt(XLEN.W), 0.U)
  val mcounteren = RegInit(UInt(XLEN.W), 0.U)
  val mcause = RegInit(UInt(XLEN.W), 0.U)
  val mtval = RegInit(UInt(XLEN.W), 0.U)
  val mepc = Reg(UInt(XLEN.W))
  // Page 36 in riscv-priv: The low bit of mepc (mepc[0]) is always zero.
  val mepcMask = ~(0x1.U(XLEN.W))

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
  val mconfigptr = RegInit(UInt(XLEN.W), 0.U) // the read-only pointer pointing to the platform config structure, 0 for not supported.
  val mstatus = RegInit("ha00000000".U(XLEN.W))

  // mstatus Value Table
  // | sd   |
  // | pad1 |
  // | sxl  | hardlinked to 10, use 00 to pass xv6 test
  // | uxl  | hardlinked to 10
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

  val mstatusWMask = (~ZeroExt((
    GenMask(XLEN - 2, 36) | // WPRI
    GenMask(35, 32)       | // SXL and UXL cannot be changed
    GenMask(31, 23)       | // WPRI
    GenMask(16, 15)       | // XS is read-only
    GenMask(10, 9)        | // WPRI
    GenMask(6)            | // WPRI
    GenMask(2)              // WPRI
  ), 64)).asUInt()
  val mstatusMask = (~ZeroExt((
    GenMask(XLEN - 2, 36) | // WPRI
    GenMask(31, 23)       | // WPRI
    GenMask(10, 9)        | // WPRI
    GenMask(6)            | // WPRI
    GenMask(2)              // WPRI
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
  // stvec: {BASE (WARL), MODE (WARL)} where mode is 0 or 1
  val stvecMask = ~(0x2.U(XLEN.W))
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
  val satpMask = Cat("h8".U(Satp_Mode_len.W), satp_part_wmask(Satp_Asid_len, AsidLength), satp_part_wmask(Satp_Addr_len, PAddrBits-12))
  val sepc = RegInit(UInt(XLEN.W), 0.U)
  // Page 60 in riscv-priv: The low bit of sepc (sepc[0]) is always zero.
  val sepcMask = ~(0x1.U(XLEN.W))
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
    (EnableLdVioCheckAfterReset.B.asUInt << 4) |
    GenMask(5) // enable soft prefetch by default
  val smblockctl = RegInit(UInt(XLEN.W), smblockctl_init_val)
  csrio.customCtrl.sbuffer_threshold := smblockctl(3, 0)
  // bits 4: enable load load violation check
  csrio.customCtrl.ldld_vio_check_enable := smblockctl(4)
  csrio.customCtrl.soft_prefetch_enable := smblockctl(5)

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

  //val perfEventscounten = List.fill(nrPerfCnts)(RegInit(false(Bool())))
  // Perf Counter
  val nrPerfCnts = 29  // 3...31
  val priviledgeModeOH = UIntToOH(priviledgeMode)
  val perfEventscounten = RegInit(0.U.asTypeOf(Vec(nrPerfCnts, Bool())))
  val perfCnts   = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val perfEvents = List.fill(8)(RegInit("h0000000000".U(XLEN.W))) ++ 
                   List.fill(8)(RegInit("h4010040100".U(XLEN.W))) ++ 
                   List.fill(8)(RegInit("h8020080200".U(XLEN.W))) ++ 
                   List.fill(5)(RegInit("hc0300c0300".U(XLEN.W)))
  for (i <-0 until nrPerfCnts) {
    perfEventscounten(i) := (Cat(perfEvents(i)(62),perfEvents(i)(61),(perfEvents(i)(61,60))) & priviledgeModeOH).orR
  }

  val hpmEvents = Wire(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
  for (i <- 0 until numPCntHc * coreParams.L2NBanks) {
    hpmEvents(i) := csrio.perf.perfEventsHc(i)
  }

  val csrevents = perfEvents.slice(24, 29)
  val hpm_hc = HPerfMonitor(csrevents, hpmEvents)
  val mcountinhibit = RegInit(0.U(XLEN.W))
  val mcycle = RegInit(0.U(XLEN.W))
  mcycle := mcycle + 1.U
  val minstret = RegInit(0.U(XLEN.W))
  val perf_events = csrio.perf.perfEventsFrontend ++
                    csrio.perf.perfEventsCtrl ++
                    csrio.perf.perfEventsLsu ++
                    hpm_hc.getPerf
  minstret := minstret + RegNext(csrio.perf.retiredInstr)
  for(i <- 0 until 29){
    perfCnts(i) := Mux(mcountinhibit(i+3) | !perfEventscounten(i), perfCnts(i), perfCnts(i) + perf_events(i).value)
  }

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
    MaskedRegMap(Stvec, stvec, stvecMask, MaskedRegMap.NoSideEffect, stvecMask),
    MaskedRegMap(Scounteren, scounteren),

    //--- Supervisor Trap Handling ---
    MaskedRegMap(Sscratch, sscratch),
    MaskedRegMap(Sepc, sepc, sepcMask, MaskedRegMap.NoSideEffect, sepcMask),
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
    MaskedRegMap(Mconfigptr, mconfigptr, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    //--- Machine Trap Setup ---
    MaskedRegMap(Mstatus, mstatus, mstatusWMask, mstatusUpdateSideEffect, mstatusMask),
    MaskedRegMap(Misa, misa), // now MXL, EXT is not changeable
    MaskedRegMap(Medeleg, medeleg, "hf3ff".U(XLEN.W)),
    MaskedRegMap(Mideleg, mideleg, "h222".U(XLEN.W)),
    MaskedRegMap(Mie, mie),
    MaskedRegMap(Mtvec, mtvec, mtvecMask, MaskedRegMap.NoSideEffect, mtvecMask),
    MaskedRegMap(Mcounteren, mcounteren),

    //--- Machine Trap Handling ---
    MaskedRegMap(Mscratch, mscratch),
    MaskedRegMap(Mepc, mepc, mepcMask, MaskedRegMap.NoSideEffect, mepcMask),
    MaskedRegMap(Mcause, mcause),
    MaskedRegMap(Mtval, mtval),
    MaskedRegMap(Mip, mip.asUInt, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    //--- Trigger ---
    MaskedRegMap(Tselect, tDummy, WritableMask, WriteTselect, WritableMask, ReadTselect),
    MaskedRegMap(Tdata1, tDummy, WritableMask, WriteTdata1, WritableMask, ReadTdata1),
    MaskedRegMap(Tdata2, tDummy, WritableMask, WriteTdata2, WritableMask, ReadTdata2),
    MaskedRegMap(Tinfo, tDummy, 0.U(XLEN.W), MaskedRegMap.Unwritable, WritableMask, ReadTinfo),
    MaskedRegMap(Tcontrol, tControlPhy, tcontrolWriteMask),

    //--- Debug Mode ---
    MaskedRegMap(Dcsr, dcsr, dcsrMask, dcsrUpdateSideEffect),
    MaskedRegMap(Dpc, dpc),
    MaskedRegMap(Dscratch, dscratch),
    MaskedRegMap(Dscratch1, dscratch1),
    MaskedRegMap(Mcountinhibit, mcountinhibit),
    MaskedRegMap(Mcycle, mcycle),
    MaskedRegMap(Minstret, minstret),
  )

  val perfCntMapping = (0 until 29).map(i => {Map(
    MaskedRegMap(addr = Mhpmevent3 +i,
                 reg  = perfEvents(i),
                 wmask = "hf87fff3fcff3fcff".U(XLEN.W)),
    MaskedRegMap(addr = Mhpmcounter3 +i,
                 reg  = perfCnts(i))
  )}).fold(Map())((a,b) => a ++ b)
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

  val addrInPerfCnt = (addr >= Mcycle.U) && (addr <= Mhpmcounter31.U) ||
    (addr >= Mcountinhibit.U) && (addr <= Mhpmevent31.U)
  csrio.isPerfCnt := addrInPerfCnt && valid && func =/= CSROpType.jmp

  // satp wen check
  val satpLegalMode = (wdata.asTypeOf(new SatpStruct).mode===0.U) || (wdata.asTypeOf(new SatpStruct).mode===8.U)

  // csr access check, special case
  val tvmNotPermit = (priviledgeMode === ModeS && mstatusStruct.tvm.asBool)
  val accessPermitted = !(addr === Satp.U && tvmNotPermit)
  csrio.disableSfence := tvmNotPermit

  // general CSR wen check
  val wen = valid && func =/= CSROpType.jmp && (addr=/=Satp.U || satpLegalMode)
  val dcsrPermitted = dcsrPermissionCheck(addr, false.B, debugMode)
  val triggerPermitted = triggerPermissionCheck(addr, true.B, debugMode) // todo dmode
  val modePermitted = csrAccessPermissionCheck(addr, false.B, priviledgeMode) && dcsrPermitted && triggerPermitted
  val perfcntPermitted = perfcntPermissionCheck(addr, priviledgeMode, mcounteren, scounteren)
  val permitted = Mux(addrInPerfCnt, perfcntPermitted, modePermitted) && accessPermitted

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

  when (RegNext(csrio.fpu.fflags.valid)) {
    fcsr := fflags_wfn(update = true)(RegNext(csrio.fpu.fflags.bits))
  }
  // set fs and sd in mstatus
  when (csrw_dirty_fp_state || RegNext(csrio.fpu.dirty_fs)) {
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.fs := "b11".U
    mstatusNew.sd := true.B
    mstatus := mstatusNew.asUInt()
  }
  csrio.fpu.frm := fcsr.asTypeOf(new FcsrStruct).frm


  // Trigger Ctrl
  csrio.customCtrl.trigger_enable := tdata1Phy.map{tdata1 => tdata1.m && priviledgeMode === ModeM ||
    tdata1.s && priviledgeMode === ModeS || tdata1.u && priviledgeMode === ModeU
  }
  csrio.customCtrl.frontend_trigger.t.valid := RegNext(wen && addr === Tdata1.U && tdata1_function(tselectPhy)._2 === I_Trigger)
  csrio.customCtrl.mem_trigger.t.valid := RegNext(wen && addr === Tdata1.U && tdata1_function(tselectPhy)._2 =/= I_Trigger)


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
  flushPipe := resetSatp || (valid && func === CSROpType.jmp && !isEcall && !isEbreak)

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
    when (mstatusOld.spp =/= ModeM) { mstatusNew.mprv := 0.U }
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
  mipWire.e.s := csrio.externalInterrupt.seip

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
  val hasSingleStep = csrio.exception.bits.uop.ctrl.singleStep && raiseException
  val hasTriggerHit = csrio.exception.bits.uop.cf.trigger.triggerHitVec.orR && raiseException

  val raiseExceptionVec = csrio.exception.bits.uop.cf.exceptionVec
  val regularExceptionNO = ExceptionNO.priorities.foldRight(0.U)((i: Int, sum: UInt) => Mux(raiseExceptionVec(i), i.U, sum))
  val exceptionNO = Mux(hasSingleStep || hasTriggerHit, 3.U, regularExceptionNO)
  val causeNO = (raiseIntr << (XLEN-1)).asUInt() | Mux(raiseIntr, intrNO, exceptionNO)

  val raiseExceptionIntr = csrio.exception.valid

  val raiseDebugExceptionIntr = !debugMode && (hasbreakPoint || raiseDebugIntr || hasSingleStep || hasTriggerHit)
  val ebreakEnterParkLoop = debugMode && hasbreakPoint

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
  // Due to timing reasons of memExceptionVAddr, we delay the write of mtval and stval
  val memExceptionAddr = SignExt(csrio.memExceptionVAddr, XLEN)
  val updateTval = VecInit(Seq(
    hasInstrPageFault,
    hasLoadPageFault,
    hasStorePageFault,
    hasInstrAccessFault,
    hasLoadAccessFault,
    hasStoreAccessFault,
    hasLoadAddrMisaligned,
    hasStoreAddrMisaligned
  )).asUInt.orR
  when (RegNext(RegNext(updateTval))) {
      val tval = RegNext(Mux(
      RegNext(hasInstrPageFault || hasInstrAccessFault),
      RegNext(Mux(
        csrio.exception.bits.uop.cf.crossPageIPFFix,
        SignExt(csrio.exception.bits.uop.cf.pc + 2.U, XLEN),
        SignExt(csrio.exception.bits.uop.cf.pc, XLEN)
      )),
      memExceptionAddr
    ))
    when (RegNext(priviledgeMode === ModeM)) {
      mtval := tval
    }.otherwise {
      stval := tval
    }
  }

  val debugTrapTarget = Mux(!isEbreak && debugMode, 0x38020808.U, 0x38020800.U) // 0x808 is when an exception occurs in debug mode prog buf exec
  val deleg = Mux(raiseIntr, mideleg , medeleg)
  // val delegS = ((deleg & (1 << (causeNO & 0xf))) != 0) && (priviledgeMode < ModeM);
  val delegS = deleg(causeNO(3,0)) && (priviledgeMode < ModeM)
  val clearTval = !updateTval || raiseIntr
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
  val tvec = Mux(delegS, stvec, mtvec)
  val tvecBase = tvec(VAddrBits - 1, 2)
  csrio.trapTarget := Mux(isXRetFlag,
    retTargetReg,
    Mux(raiseDebugExceptionIntr || ebreakEnterParkLoop, debugTrapTarget,
      // When MODE=Vectored, all synchronous exceptions into M/S mode
      // cause the pc to be set to the address in the BASE field, whereas
      // interrupts cause the pc to be set to the address in the BASE field
      // plus four times the interrupt cause number.
      Cat(tvecBase + Mux(tvec(0) && raiseIntr, causeNO(3, 0), 0.U), 0.U(2.W))
  ))

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
      when (clearTval) { stval := 0.U }
    }.otherwise {
      mcause := causeNO
      mepc := SignExt(csrio.exception.bits.uop.cf.pc, XLEN)
      mstatusNew.mpp := priviledgeMode
      mstatusNew.pie.m := mstatusOld.ie.m
      mstatusNew.ie.m := false.B
      priviledgeMode := ModeM
      when (clearTval) { mtval := 0.U }
    }
    mstatus := mstatusNew.asUInt
    debugMode := debugModeNew
  }

  XSDebug(raiseExceptionIntr && delegS, "sepc is writen!!! pc:%x\n", cfIn.pc)

  // Distributed CSR update req
  //
  // For now we use it to implement customized cache op
  // It can be delayed if necessary

  val delayedUpdate0 = DelayN(csrio.distributedUpdate(0), 2)
  val delayedUpdate1 = DelayN(csrio.distributedUpdate(1), 2)
  val distributedUpdateValid = delayedUpdate0.w.valid || delayedUpdate1.w.valid
  val distributedUpdateAddr = Mux(delayedUpdate0.w.valid, 
    delayedUpdate0.w.bits.addr,
    delayedUpdate1.w.bits.addr
  )
  val distributedUpdateData = Mux(delayedUpdate0.w.valid, 
    delayedUpdate0.w.bits.data,
    delayedUpdate1.w.bits.data
  )

  assert(!(delayedUpdate0.w.valid && delayedUpdate1.w.valid))

  when(distributedUpdateValid){
    // cacheopRegs can be distributed updated
    CacheInstrucion.CacheInsRegisterList.map{case (name, attribute) => {
      when((Scachebase + attribute("offset").toInt).U === distributedUpdateAddr){
        cacheopRegs(name) := distributedUpdateData
      }
    }}
  }

  // Implicit add reset values for mepc[0] and sepc[0]
  // TODO: rewrite mepc and sepc using a struct-like style with the LSB always being 0
  when (reset.asBool) {
    mepc := Cat(mepc(XLEN - 1, 1), 0.U(1.W))
    sepc := Cat(sepc(XLEN - 1, 1), 0.U(1.W))
  }

  def readWithScala(addr: Int): UInt = mapping(addr)._1

  val difftestIntrNO = Mux(raiseIntr, causeNO, 0.U)

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = Module(new DifftestArchEvent)
    difftest.io.clock := clock
    difftest.io.coreid := csrio.hartId
    difftest.io.intrNO := RegNext(RegNext(RegNext(difftestIntrNO)))
    difftest.io.cause  := RegNext(RegNext(RegNext(Mux(csrio.exception.valid, causeNO, 0.U))))
    difftest.io.exceptionPC := RegNext(RegNext(RegNext(SignExt(csrio.exception.bits.uop.cf.pc, XLEN))))
    if (env.EnableDifftest) {
      difftest.io.exceptionInst := RegNext(RegNext(RegNext(csrio.exception.bits.uop.cf.instr)))
    }
  }

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = Module(new DifftestCSRState)
    difftest.io.clock := clock
    difftest.io.coreid := csrio.hartId
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

  if(env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = Module(new DifftestDebugMode)
    difftest.io.clock := clock
    difftest.io.coreid := csrio.hartId
    difftest.io.debugMode := debugMode
    difftest.io.dcsr := dcsr
    difftest.io.dpc := dpc
    difftest.io.dscratch0 := dscratch
    difftest.io.dscratch1 := dscratch1
  }
}

class PFEvent(implicit p: Parameters) extends XSModule with HasCSRConst  {
  val io = IO(new Bundle {
    val distribute_csr = Flipped(new DistributedCSRIO())
    val hpmevent = Output(Vec(29, UInt(XLEN.W)))
  })

  val w = io.distribute_csr.w

  val perfEvents = List.fill(8)(RegInit("h0000000000".U(XLEN.W))) ++ 
                   List.fill(8)(RegInit("h4010040100".U(XLEN.W))) ++ 
                   List.fill(8)(RegInit("h8020080200".U(XLEN.W))) ++ 
                   List.fill(5)(RegInit("hc0300c0300".U(XLEN.W)))

  val perfEventMapping = (0 until 29).map(i => {Map(
    MaskedRegMap(addr = Mhpmevent3 +i,
                 reg  = perfEvents(i),
                 wmask = "hf87fff3fcff3fcff".U(XLEN.W))
  )}).fold(Map())((a,b) => a ++ b)

  val rdata = Wire(UInt(XLEN.W))
  MaskedRegMap.generate(perfEventMapping, w.bits.addr, rdata, w.valid, w.bits.data)
  for(i <- 0 until 29){
    io.hpmevent(i) := perfEvents(i)
  }
}             

