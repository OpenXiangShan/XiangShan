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

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.util._
import utility.MaskedRegMap.WritableMask
import utils._
import utility._
import xiangshan.ExceptionNO._
import xiangshan._
import xiangshan.backend.fu.util._
import xiangshan.cache._
import xiangshan.backend.Bundles.{ExceptionInfo, TrapInstInfo}
import xiangshan.backend.fu.NewCSR.CSRNamedConstant.ContextStatus
import utils.MathUtils.{BigIntGenMask, BigIntNot}

class FpuCsrIO extends Bundle {
  val fflags = Output(Valid(UInt(5.W)))
  val isIllegal = Output(Bool())
  val dirty_fs = Output(Bool())
  val frm = Input(UInt(3.W))
}

class VpuCsrIO(implicit p: Parameters) extends XSBundle {
  val vstart = Input(UInt(XLEN.W))
  val vxrm = Input(UInt(2.W))

  val vl = Output(UInt(XLEN.W))

  val set_vstart = Output(Valid(UInt(XLEN.W)))
  val set_vtype = Output(Valid(UInt(XLEN.W)))
  val set_vxsat = Output(Valid(UInt(1.W)))

  val dirty_vs = Output(Bool())
}


class PerfCounterIO(implicit p: Parameters) extends XSBundle {
  val perfEventsFrontend  = Vec(numCSRPCntFrontend, new PerfEvent)
  val perfEventsBackend   = Vec(numCSRPCntCtrl, new PerfEvent)
  val perfEventsLsu       = Vec(numCSRPCntLsu, new PerfEvent)
  val perfEventsHc        = Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent)
  val retiredInstr = UInt(7.W)
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
}

class CSRFileIO(implicit p: Parameters) extends XSBundle {
  val hartId = Input(UInt(hartIdLen.W))
  // output (for func === CSROpType.jmp)
  val perf = Input(new PerfCounterIO)
  val isPerfCnt = Output(Bool())
  // to FPU
  val fpu = Flipped(new FpuCsrIO)
  // to VPU
  val vpu = Flipped(new VpuCsrIO)
  // from rob
  val exception = Flipped(ValidIO(new ExceptionInfo))
  // to ROB
  val isXRet = Output(Bool())
  val trapTarget = Output(UInt(VAddrBits.W))
  val interrupt = Output(Bool())
  val wfi_event = Output(Bool())
  // from LSQ
  val memExceptionVAddr = Input(UInt(VAddrBits.W))
  val memExceptionGPAddr = Input(UInt(GPAddrBits.W))
  // from outside cpu,externalInterrupt
  val externalInterrupt = Input(new ExternalInterruptIO)
  // TLB
  val tlb = Output(new TlbCsrBundle)
  // Debug Mode
  // val singleStep = Output(Bool())
  val debugMode = Output(Bool())
  // Custom microarchiture ctrl signal
  val customCtrl = Output(new CustomCSRCtrlIO)
}

class VtypeStruct(implicit p: Parameters) extends XSBundle {
  val vill = UInt(1.W)
  val reserved = UInt((XLEN - 9).W)
  val vma = UInt(1.W)
  val vta = UInt(1.W)
  val vsew = UInt(3.W)
  val vlmul = UInt(3.W)
}
/*
class CSR(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg)
  with HasCSRConst
  with PMPMethod
  with PMAMethod
  with HasXSParameter
  with SdtrigExt
  with DebugCSR
{
  val csrio = io.csrio.get

  val flushPipe = Wire(Bool())

  val (valid, src1, src2, func) = (
    io.in.valid,
    io.in.bits.data.src(0),
    io.in.bits.data.imm,
    io.in.bits.ctrl.fuOpType
  )

  // CSR define
  val virtMode = RegInit(false.B)
  csrio.customCtrl.virtMode := virtMode

  class Priv extends Bundle {
    val m = Output(Bool())
    val h = Output(Bool()) // unused
    val s = Output(Bool())
    val u = Output(Bool())
  }

  class MstatusStruct extends Bundle {
    val sd = Output(UInt(1.W))

    val pad1 = if (XLEN == 64 && HasHExtension) Output(UInt(23.W)) else if (XLEN == 64) Output(UInt(25.W)) else null
    val mpv  = if (XLEN == 64 && HasHExtension) Output(UInt(1.W)) else null
    val gva  = if (XLEN == 64 && HasHExtension) Output(UInt(1.W)) else null
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
    val vs = Output(UInt(2.W))
    val spp = Output(UInt(1.W))
    val pie = new Priv
    val ie = new Priv
    assert(this.getWidth == XLEN)

    def ube = pie.h // a little ugly
    def ube_(r: UInt): Unit = {
      pie.h := r(0)
    }
  }

  class HstatusStruct extends Bundle {
    val pad4 = if (HSXLEN == 64) Output(UInt(30.W)) else null
    val vsxl = if (HSXLEN == 64) Output(UInt(2.W)) else null
    val pad3 = Output(UInt(9.W))
    val vtsr = Output(UInt(1.W))
    val vtw = Output(UInt(1.W))
    val vtvm = Output(UInt(1.W))
    val pad2 = Output(UInt(2.W))
    val vgein = Output(UInt(6.W))
    val pad1 = Output(UInt(2.W))
    val hu = Output(UInt(1.W))
    val spvp = Output(UInt(1.W))
    val spv = Output(UInt(1.W))
    val gva = Output(UInt(1.W))
    val vsbe = Output(UInt(1.W))
    val pad0 = Output(UInt(5.W))
    assert(this.getWidth == XLEN)
  }

  class Interrupt extends Bundle {
//  val d = Output(Bool())    // Debug
    val e = new Priv
    val t = new Priv
    val s = new Priv
  }

  // Debug CSRs
  val dcsr = RegInit(UInt(32.W), DcsrStruct.init)
  val dpc = Reg(UInt(64.W))
  val dscratch0 = Reg(UInt(64.W))
  val dscratch1 = Reg(UInt(64.W))
  val debugMode = RegInit(false.B)
  val debugIntrEnable = RegInit(true.B) // debug interrupt will be handle only when debugIntrEnable
  csrio.debugMode := debugMode

  val dpcPrev = RegNext(dpc)
  XSDebug(dpcPrev =/= dpc, "Debug Mode: dpc is altered! Current is %x, previous is %x\n", dpc, dpcPrev)

  val dcsrData = Wire(new DcsrStruct)
  dcsrData := dcsr.asTypeOf(new DcsrStruct)
  val dcsrMask = ZeroExt(GenMask(15) | GenMask(13, 11) | GenMask(4) | GenMask(2, 0), XLEN)// Dcsr write mask
  def dcsrUpdateSideEffect(dcsr: UInt): UInt = {
    val dcsrOld = WireInit(dcsr.asTypeOf(new DcsrStruct))
    val dcsrNew = dcsr | (dcsrOld.prv(0) | dcsrOld.prv(1)).asUInt // turn 10 priv into 11
    dcsrNew
  }
  // csrio.singleStep := dcsrData.step
  csrio.customCtrl.singlestep := dcsrData.step && !debugMode

  // Trigger CSRs
  private val tselectPhy = RegInit(0.U(log2Up(TriggerNum).W))

  private val tdata1RegVec = RegInit(VecInit(Seq.fill(TriggerNum)(Tdata1Bundle.default)))
  private val tdata2RegVec = RegInit(VecInit(Seq.fill(TriggerNum)(0.U(64.W))))
  private val tdata1WireVec = tdata1RegVec.map(_.asTypeOf(new Tdata1Bundle))
  private val tdata2WireVec = tdata2RegVec
  private val tdata1Selected = tdata1RegVec(tselectPhy).asTypeOf(new Tdata1Bundle)
  private val tdata2Selected = tdata2RegVec(tselectPhy)
  private val newTriggerChainVec = UIntToOH(tselectPhy, TriggerNum).asBools | tdata1WireVec.map(_.data.asTypeOf(new MControlData).chain)
  private val newTriggerChainIsLegal = TriggerCheckChainLegal(newTriggerChainVec, TriggerChainMaxLength)
  val tinfo = RegInit((BigInt(1) << TrigTypeEnum.MCONTROL.litValue.toInt).U(XLEN.W)) // This value should be 4.U


  def WriteTselect(wdata: UInt) = {
    Mux(wdata < TriggerNum.U, wdata(log2Up(TriggerNum) - 1, 0), tselectPhy)
  }

  def GenTdataDistribute(tdata1: Tdata1Bundle, tdata2: UInt): MatchTriggerIO = {
    val res = Wire(new MatchTriggerIO)
    val mcontrol: MControlData = WireInit(tdata1.data.asTypeOf(new MControlData))
    res.matchType := mcontrol.match_.asUInt
    res.select    := mcontrol.select
    res.timing    := mcontrol.timing
    res.action    := mcontrol.action.asUInt
    res.chain     := mcontrol.chain
    res.execute   := mcontrol.execute
    res.load      := mcontrol.load
    res.store     := mcontrol.store
    res.tdata2    := tdata2
    res
  }

  csrio.customCtrl.frontend_trigger.tUpdate.bits.addr := tselectPhy
  csrio.customCtrl.mem_trigger.tUpdate.bits.addr := tselectPhy
  csrio.customCtrl.frontend_trigger.tUpdate.bits.tdata := GenTdataDistribute(tdata1Selected, tdata2Selected)
  csrio.customCtrl.mem_trigger.tUpdate.bits.tdata := GenTdataDistribute(tdata1Selected, tdata2Selected)

  // Machine-Level CSRs
  // mtvec: {BASE (WARL), MODE (WARL)} where mode is 0 or 1
  val mtvecMask = ~(0x2.U(XLEN.W))
  val mtvec = RegInit(UInt(XLEN.W), 0.U)
  val mcounteren = RegInit(UInt(XLEN.W), 0.U)
  // Currently, XiangShan don't support Unprivileged Counter/Timers CSRs ("Zicntr" and "Zihpm")
  val mcounterenMask = 0.U(XLEN.W)
  val mcause = RegInit(UInt(XLEN.W), 0.U)
  val mtval = RegInit(UInt(XLEN.W), 0.U)
  val mtval2 = RegInit(UInt(XLEN.W), 0.U)
  val mtinst = RegInit(UInt(XLEN.W), 0.U)
  val mepc = RegInit(UInt(XLEN.W), 0.U)
  // Page 36 in riscv-priv: The low bit of mepc (mepc[0]) is always zero.
  val mepcMask = ~(0x1.U(XLEN.W))

  val mie = RegInit(0.U(XLEN.W))
  val mipWire = WireInit(0.U.asTypeOf(new Interrupt))
  val mipReg  = RegInit(0.U(XLEN.W))
  val mipMask = ZeroExt(Array(
    1,  // SSIP
    2,  // VSSIP
    3,  // MSIP
    5,  // STIP
    6,  // VSTIP
    7,  // MTIP
    9,  // SEIP
    10, // VSEIP
    11, // MEIP
    12, // SGEIP
  ).map(GenMask(_)).reduce(_ | _), XLEN)
  val mip = (mipWire.asUInt | mipReg).asTypeOf(new Interrupt)

  val mip_mie_WMask_H = if(HasHExtension){((1 << 2) | (1 << 6) | (1 << 10) | (1 << 12)).U(XLEN.W)}else{0.U(XLEN.W)}
  val vssip_Mask = (1 << 2).U(XLEN.W)

  val mipWMask = vssip_Mask | ((1 << 9) | (1 << 5) | (1 << 1)).U(XLEN.W)
  val mieWMask = mip_mie_WMask_H | "haaa".U(XLEN.W)

  def getMisaMxl(mxl: BigInt): BigInt = mxl << (XLEN - 2)
  def getMisaExt(ext: Char): Long = 1 << (ext.toInt - 'a'.toInt)
  var extList = List('a', 's', 'i', 'u')
  if (HasMExtension) { extList = extList :+ 'm' }
  if (HasCExtension) { extList = extList :+ 'c' }
  if (HasHExtension) { extList = extList :+ 'h' }
  if (HasFPU) { extList = extList ++ List('f', 'd') }
  if (HasVPU) { extList = extList :+ 'v' }
  val misaInitVal = getMisaMxl(2) | extList.foldLeft(0L)((sum, i) => sum | getMisaExt(i)) //"h8000000000141185".U
  val misa = RegInit(UInt(XLEN.W), misaInitVal.U)
  println(s"[CSR] supported isa ext: $extList")

  // MXL = 2          | 0 | EXT = b 00 0000 0100 0001 0001 0000 0101
  // (XLEN-1, XLEN-2) |   |(25, 0)  ZY XWVU TSRQ PONM LKJI HGFE DCBA

  // Machine Configuration
  val menvcfg = RegInit(UInt(XLEN.W), 0.U)

  val mvendorid = RegInit(UInt(XLEN.W), 0.U) // this is a non-commercial implementation
  val marchid = RegInit(UInt(XLEN.W), 25.U) // architecture id for XiangShan is 25; see https://github.com/riscv/riscv-isa-manual/blob/master/marchid.md
  val mimpid = RegInit(UInt(XLEN.W), 0.U) // provides a unique encoding of the version of the processor implementation
  val mhartid = Reg(UInt(XLEN.W)) // the hardware thread running the code
  when (RegNext(RegNext(reset.asBool) && !reset.asBool)) {
    mhartid := csrio.hartId
  }
  val mconfigptr = RegInit(UInt(XLEN.W), 0.U) // the read-only pointer pointing to the platform config structure, 0 for not supported.
  val mstatus = RegInit("ha00002200".U(XLEN.W))

  // mstatus Value Table
  // | sd   | Read Only
  // | pad1 | WPRI
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
  // | fs   | 01 |
  // | mpp  | 00 |
  // | vs   | 01 |
  // | spp  | 0 |
  // | pie  | 0000 | pie.h is used as UBE
  // | ie   | 0000 | uie hardlinked to 0, as N ext is not implemented

  val mstatusStruct = mstatus.asTypeOf(new MstatusStruct)
  def mstatusUpdateSideEffect(mstatus: UInt): UInt = {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    // Cat(sd, other)
    val mstatusNew = Cat(
      mstatusOld.xs === ContextStatus.dirty || mstatusOld.fs === ContextStatus.dirty || mstatusOld.vs === ContextStatus.dirty,
      mstatus(XLEN-2, 0)
    )
    mstatusNew
  }
  def vsstatusUpdateSideEffect(vsstatus: UInt): UInt = {
    val vsstatusOld = WireInit(vsstatus.asTypeOf(new MstatusStruct))
    val vsstatusNew = Cat(vsstatusOld.xs === "b11".U || vsstatusOld.fs === "b11".U, vsstatus(XLEN-2, 0))
    vsstatusNew
  }
  val mstatusWMask = (~ZeroExt((
    GenMask(63)           | // SD is read-only
    (if(HasHExtension)
        GenMask(62, 40)    // WPRI
      else
        GenMask(62, 38)  )| // WPRI
    GenMask(35, 32)       | // SXL and UXL cannot be changed
    GenMask(31, 23)       | // WPRI
    GenMask(16, 15)       | // XS is read-only
    GenMask(6)            | // UBE, always little-endian (0)
    GenMask(4)            | // WPRI
    GenMask(2)            | // WPRI
    GenMask(0)              // WPRI
  ), 64)).asUInt

  val medeleg = RegInit(UInt(XLEN.W), 0.U)
  val midelegInit = if(HasHExtension){((1 << 12) | (1 << 10) | (1 << 6) | (1 << 2)).U}else{0.U}
  val medelegWMask = if(HasHExtension) {
    "hf0b7ff".U(XLEN.W)
  }else {
    "hb3ff".U(XLEN.W)
  }


  val mideleg = RegInit(UInt(XLEN.W), midelegInit)
  val mscratch = RegInit(UInt(XLEN.W), 0.U)

  val midelegWMask = "h222".U(XLEN.W)
  // PMP Mapping
  val pmp = Wire(Vec(NumPMP, new PMPEntry())) // just used for method parameter
  val pma = Wire(Vec(NumPMA, new PMPEntry())) // just used for method parameter
  val pmpMapping = pmp_gen_mapping(pmp_init, NumPMP, PmpcfgBase, PmpaddrBase, pmp)
  val pmaMapping = pmp_gen_mapping(pma_init, NumPMA, PmacfgBase, PmaaddrBase, pma)
  // !WARNNING: pmp and pma CSRs are not checked in difftest.

  // Supervisor-Level CSRs

  val sstatusWNmask: BigInt = (
    BigIntGenMask(63)     | // SD is read-only
    BigIntGenMask(62, 34) | // WPRI
    BigIntGenMask(33, 32) | // UXL is hard-wired to 64(b10)
    BigIntGenMask(31, 20) | // WPRI
    BigIntGenMask(17)     | // WPRI
    BigIntGenMask(16, 15) | // XS is read-only to zero
    BigIntGenMask(12, 11) | // WPRI
    BigIntGenMask(7)      | // WPRI
    BigIntGenMask(6)      | // UBE is always little-endian (0)
    BigIntGenMask(4, 2)   | // WPRI
    BigIntGenMask(0)        // WPRI
  )

  val sstatusWmask = BigIntNot(sstatusWNmask).U(XLEN.W)
  val sstatusRmask = (
    BigIntGenMask(63)     | // SD
    BigIntGenMask(33, 32) | // UXL
    BigIntGenMask(19)     | // MXR
    BigIntGenMask(18)     | // SUM
    BigIntGenMask(16, 15) | // XS
    BigIntGenMask(14, 13) | // FS
    BigIntGenMask(10, 9 ) | // VS
    BigIntGenMask(8)      | // SPP
    BigIntGenMask(6)      | // UBE: hard wired to 0
    BigIntGenMask(5)      | // SPIE
    BigIntGenMask(1)
  ).U(XLEN.W)

  println(s"sstatusWNmask: 0x${sstatusWNmask.toString(16)}")
  println(s"sstatusWmask: 0x${sstatusWmask.litValue.toString(16)}")
  println(s"sstatusRmask: 0x${sstatusRmask.litValue.toString(16)}")

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
  val stval = RegInit(UInt(XLEN.W), 0.U)
  val sscratch = RegInit(UInt(XLEN.W), 0.U)
  val scounteren = RegInit(UInt(XLEN.W), 0.U)
  val senvcfg = RegInit(UInt(XLEN.W), 0.U)  // !WARNING: there is no logic about this CSR.
  // Currently, XiangShan don't support Unprivileged Counter/Timers CSRs ("Zicntr" and "Zihpm")
  val scounterenMask = 0.U(XLEN.W)

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

  // spfctl Bit 0: L1I Cache Prefetcher Enable
  // spfctl Bit 1: L2Cache Prefetcher Enable
  // spfctl Bit 2: L1D Cache Prefetcher Enable
  // spfctl Bit 3: L1D train prefetch on hit
  // spfctl Bit 4: L1D prefetch enable agt
  // spfctl Bit 5: L1D prefetch enable pht
  // spfctl Bit [9:6]: L1D prefetch active page threshold
  // spfctl Bit [15:10]: L1D prefetch active page stride
  // turn off L2 BOP, turn on L1 SMS by default
  val spfctl = RegInit(UInt(XLEN.W), Seq(
    0 << 17,    // L2 pf store only [17] init: false
    1 << 16,    // L1D pf enable stride [16] init: true
    30 << 10,   // L1D active page stride [15:10] init: 30
    12 << 6,    // L1D active page threshold [9:6] init: 12
    1  << 5,    // L1D enable pht [5] init: true
    1  << 4,    // L1D enable agt [4] init: true
    0  << 3,    // L1D train on hit [3] init: false
    1  << 2,    // L1D pf enable [2] init: true
    1  << 1,    // L2 pf enable [1] init: true
    1  << 0,    // L1I pf enable [0] init: true
  ).reduce(_|_).U(XLEN.W))
  csrio.customCtrl.l1I_pf_enable := spfctl(0)
  csrio.customCtrl.l2_pf_enable := spfctl(1)
  csrio.customCtrl.l1D_pf_enable := spfctl(2)
  csrio.customCtrl.l1D_pf_train_on_hit := spfctl(3)
  csrio.customCtrl.l1D_pf_enable_agt := spfctl(4)
  csrio.customCtrl.l1D_pf_enable_pht := spfctl(5)
  csrio.customCtrl.l1D_pf_active_threshold := spfctl(9, 6)
  csrio.customCtrl.l1D_pf_active_stride := spfctl(15, 10)
  csrio.customCtrl.l1D_pf_enable_stride := spfctl(16)
  csrio.customCtrl.l2_pf_store_only := spfctl(17)

  // sfetchctl Bit 0: L1I Cache Parity check enable
  val sfetchctl = RegInit(UInt(XLEN.W), "b0".U)
  csrio.customCtrl.icache_parity_enable := sfetchctl(0)

  // slvpredctl: load violation predict settings
  // Default reset period: 2^16
  // Why this number: reset more frequently while keeping the overhead low
  // Overhead: extra two redirections in every 64K cycles => ~0.1% overhead
  val slvpredctl = Reg(UInt(XLEN.W))
  when(reset.asBool) {
    slvpredctl := Constantin.createRecord("slvpredctl", 0x60)
  }
  csrio.customCtrl.lvpred_disable := slvpredctl(0)
  csrio.customCtrl.no_spec_load := slvpredctl(1)
  csrio.customCtrl.storeset_wait_store := slvpredctl(2)
  csrio.customCtrl.storeset_no_fast_wakeup := slvpredctl(3)
  csrio.customCtrl.lvpred_timeout := slvpredctl(8, 4)

  //  smblockctl: memory block configurations
  //  +------------------------+---+---+---+----+----+-----+--------+
  //  |XLEN-1                10| 9 | 8 | 7 | 6  | 5  |  4  |3      0|
  //  +------------------------+---+---+---+----+----+-----+--------+
  //  |           Reserved     | L | S | O | CE | SP | LVC |   Th   |
  //  +------------------------+---+---+---+----+----+-----+--------+
  //  Description:
  //  Bit 3-0   : Store buffer flush threshold (Th).
  //  Bit 4     : Enable load violation check after reset (LVC).
  //  Bit 5     : Enable soft-prefetch after reset (SP).
  //  Bit 6     : Enable cache error after reset (CE).
  //  Bit 7     : Enable uncache write outstanding (O).
  //  Bit 8     : Enable unaligned store (S).
  //  Bit 9     : Enable unaligned load (L).
  //  Others    : Reserved.

  val smblockctl_init_val =
    (0xf & StoreBufferThreshold) |
    (EnableLdVioCheckAfterReset.toInt << 4) |
    (EnableSoftPrefetchAfterReset.toInt << 5) |
    (EnableCacheErrorAfterReset.toInt << 6) |
    (EnableUncacheWriteOutstanding.toInt << 7) |
    (EnableHardwareStoreMisalign.toInt << 8) |
    (EnableHardwareLoadMisalign.toInt << 9)
  val smblockctl = RegInit(UInt(XLEN.W), smblockctl_init_val.U)
  csrio.customCtrl.sbuffer_threshold := smblockctl(3, 0)
  // bits 4: enable load load violation check
  csrio.customCtrl.ldld_vio_check_enable := smblockctl(4)
  csrio.customCtrl.soft_prefetch_enable := smblockctl(5)
  csrio.customCtrl.cache_error_enable := smblockctl(6)
  csrio.customCtrl.uncache_write_outstanding_enable := smblockctl(7)
  csrio.customCtrl.hd_misalign_st_enable := smblockctl(8)
  csrio.customCtrl.hd_misalign_ld_enable := smblockctl(9)

  println("CSR smblockctl init value:")
  println("  Store buffer replace threshold: " + StoreBufferThreshold)
  println("  Enable ld-ld vio check after reset: " + EnableLdVioCheckAfterReset)
  println("  Enable soft prefetch after reset: " + EnableSoftPrefetchAfterReset)
  println("  Enable cache error after reset: " + EnableCacheErrorAfterReset)
  println("  Enable uncache write outstanding: " + EnableUncacheWriteOutstanding)
  println("  Enable unaligned store: " + EnableHardwareStoreMisalign)
  println("  Enable unaligned load: " + EnableHardwareLoadMisalign)

  val srnctl = RegInit(UInt(XLEN.W), "h7".U)
  csrio.customCtrl.fusion_enable := srnctl(0)
  csrio.customCtrl.wfi_enable := srnctl(2)

  // Hypervisor CSRs
  val hstatusWMask = "h7003c0".U(XLEN.W)
  // hstatus: vtsr, vtw, vtvm, hu, spvp, spv, gva,
  val hstatus = RegInit("h200000000".U(XLEN.W))
  val hstatusStruct = hstatus.asTypeOf(new HstatusStruct)
  val hedeleg = RegInit(UInt(XLEN.W), 0.U)
  val hideleg = RegInit(UInt(XLEN.W), 0.U)
  val hidelegRMask = mideleg
  val hidelegWMask = ((1 << 10) | (1 << 6) | (1 << 2)).U(XLEN.W)
  val hgeie   = RegInit(UInt(XLEN.W), 0.U)
  val htval = RegInit(UInt(XLEN.W), 0.U)
  // hvip hip hie is part of mip or mie
  val hvipMask = ((1 << 10) | (1 << 6) | (1 << 2)).U(XLEN.W)
  val hipRMask = (((1 << 12).U | hvipMask) & mideleg)
  val hipWMask = ((1 << 2).U & mideleg)// vssip
  val hieMask = hipRMask
  val htinst = RegInit(UInt(XLEN.W), 0.U)
  val hgeip = RegInit(UInt(XLEN.W), 0.U)
  val henvcfg = RegInit(UInt(XLEN.W), 0.U)
  val hgatp = RegInit(UInt(XLEN.W), 0.U)
  val hgatpMask = Cat("h8".U(Hgatp_Mode_len.W), satp_part_wmask(Hgatp_Vmid_len, VmidLength), satp_part_wmask(Hgatp_Addr_len, PAddrBits-12))
  // val htimedelta = RegInit(UInt(XLEN.W), 0.U)
  val hcounteren = RegInit(UInt(XLEN.W), 0.U)
  // Currently, XiangShan don't support Unprivileged Counter/Timers CSRs ("Zicntr" and "Zihpm")
  val hcounterenMask = 0.U(XLEN.W)

  val vsstatus = RegInit("h200002000".U(XLEN.W))
  val vsstatusStruct = vsstatus.asTypeOf(new MstatusStruct)
  //vsie vsip
  val vsMask = ((1 << 10) | (1 << 6) | (1 << 2)).U(XLEN.W)
  val vsip_ie_Mask = ZeroExt((hideleg & mideleg & vsMask), XLEN)
  val vsip_WMask = ZeroExt((hideleg & mideleg & vssip_Mask), XLEN)
  val vstvec = RegInit(UInt(XLEN.W), 0.U)
  val vsscratch = RegInit(UInt(XLEN.W), 0.U)
  val vsepc = RegInit(UInt(XLEN.W), 0.U)
  val vscause = RegInit(UInt(XLEN.W), 0.U)
  val vstval = RegInit(UInt(XLEN.W), 0.U)
  val vsatp = RegInit(UInt(XLEN.W), 0.U)
  val tlbBundle = Wire(new TlbCsrBundle)
  tlbBundle.satp.apply(satp)
  tlbBundle.vsatp.apply(vsatp)
  tlbBundle.hgatp.apply(hgatp)
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
    fcsrOld.asUInt
  }
  def frm_rfn(rdata: UInt): UInt = rdata(7,5)

  def fflags_wfn(update: Boolean)(wdata: UInt): UInt = {
    val fcsrOld = fcsr.asTypeOf(new FcsrStruct)
    val fcsrNew = WireInit(fcsrOld)
    if (update) {
      fcsrNew.fflags := wdata(4,0) | fcsrOld.fflags
    } else {
      fcsrNew.fflags := wdata(4,0)
    }
    fcsrNew.asUInt
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

  // Vector extension CSRs
  val vstart = RegInit(0.U(XLEN.W))
  val vcsr = RegInit(0.U(XLEN.W))
  val vl = Reg(UInt(XLEN.W))
  val vtype = Reg(UInt(XLEN.W))
  val vlenb = RegInit(VDataBytes.U(XLEN.W))

  // set mstatus->sd and mstatus->vs when true
  val csrw_dirty_vs_state = WireInit(false.B)

  // vcsr is mapped to vxrm and vxsat
  class VcsrStruct extends Bundle {
    val reserved = UInt((XLEN-3).W)
    val vxrm = UInt(2.W)
    val vxsat = UInt(1.W)
    assert(this.getWidth == XLEN)
  }

  def vxrm_wfn(wdata: UInt): UInt = {
    val vcsrOld = WireInit(vcsr.asTypeOf(new VcsrStruct))
    csrw_dirty_vs_state := true.B
    vcsrOld.vxrm := wdata(1,0)
    vcsrOld.asUInt
  }
  def vxrm_rfn(rdata: UInt): UInt = rdata(2,1)

  def vxsat_wfn(update: Boolean)(wdata: UInt): UInt = {
    val vcsrOld = WireInit(vcsr.asTypeOf(new VcsrStruct))
    val vcsrNew = WireInit(vcsrOld)
    csrw_dirty_vs_state := true.B
    if (update) {
      vcsrNew.vxsat := wdata(0) | vcsrOld.vxsat
    } else {
      vcsrNew.vxsat := wdata(0)
    }
    vcsrNew.asUInt
  }
  def vxsat_rfn(rdata: UInt): UInt = rdata(0)

  def vcsr_wfn(wdata: UInt): UInt = {
    val vcsrOld = WireInit(vcsr.asTypeOf(new VcsrStruct))
    csrw_dirty_vs_state := true.B
    vcsrOld.vxrm := wdata.asTypeOf(vcsrOld).vxrm
    vcsrOld.vxsat := wdata.asTypeOf(vcsrOld).vxsat
    vcsrOld.asUInt
  }

  val vcsrMapping = Map(
    MaskedRegMap(Vstart, vstart),
    MaskedRegMap(Vxrm, vcsr, wfn = vxrm_wfn, rfn = vxrm_rfn),
    MaskedRegMap(Vxsat, vcsr, wfn = vxsat_wfn(false), rfn = vxsat_rfn),
    MaskedRegMap(Vcsr, vcsr, wfn = vcsr_wfn),
    MaskedRegMap(Vl, vl),
    MaskedRegMap(Vtype, vtype),
    MaskedRegMap(Vlenb, vlenb),
  )

  // Hart Privilege Mode
  val privilegeMode = RegInit(UInt(2.W), ModeM)

  //val perfEventscounten = List.fill(nrPerfCnts)(RegInit(false(Bool())))
  // Perf Counter
  val nrPerfCnts = 29  // 3...31
  val privilegeModeOH = UIntToOH(privilegeMode)
  val perfEventscounten = RegInit(0.U.asTypeOf(Vec(nrPerfCnts, Bool())))
  val perfCnts   = List.fill(nrPerfCnts)(RegInit(0.U(XLEN.W)))
  val perfEvents = List.fill(8)(RegInit("h0000000000".U(XLEN.W))) ++
                   List.fill(8)(RegInit("h4010040100".U(XLEN.W))) ++
                   List.fill(8)(RegInit("h8020080200".U(XLEN.W))) ++
                   List.fill(5)(RegInit("hc0300c0300".U(XLEN.W)))
  for (i <-0 until nrPerfCnts) {
    perfEventscounten(i) := (perfEvents(i)(63,60) & privilegeModeOH).orR
  }

  val hpmEvents = Wire(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
  for (i <- 0 until numPCntHc * coreParams.L2NBanks) {
    hpmEvents(i) := csrio.perf.perfEventsHc(i)
  }

  // print perfEvents
  val allPerfEvents = hpmEvents.map(x => (s"Hc", x.value))
  if (printEventCoding) {
    for (((name, inc), i) <- allPerfEvents.zipWithIndex) {
      println("CSR perfEvents Set", name, inc, i)
    }
  }

  val csrevents = perfEvents.slice(24, 29)
  val hpm_hc = HPerfMonitor(csrevents, hpmEvents)
  val mcountinhibit = RegInit(0.U(XLEN.W))
  val mcycle = RegInit(0.U(XLEN.W))
  mcycle := mcycle + 1.U
  val minstret = RegInit(0.U(XLEN.W))
  val perf_events = csrio.perf.perfEventsFrontend ++
                    csrio.perf.perfEventsBackend ++
                    csrio.perf.perfEventsLsu ++
                    hpm_hc.getPerf
  minstret := minstret + RegNext(csrio.perf.retiredInstr)
  for(i <- 0 until 29){
    perfCnts(i) := Mux(mcountinhibit(i+3) | !perfEventscounten(i), perfCnts(i), perfCnts(i) + perf_events(i).value)
  }

  // CSR reg map
  val basicPrivMapping = Map(

    // Unprivileged Floating-Point CSRs
    // Has been mapped above

    // TODO: support Unprivileged Counter/Timers CSRs ("Zicntr" and "Zihpm")
    // Unprivileged Counter/Timers
    MaskedRegMap(Cycle, mcycle),
    // We don't support read time CSR.
    // MaskedRegMap(Time, mtime),
    MaskedRegMap(Instret, minstret),

    //--- Supervisor Trap Setup ---
    MaskedRegMap(Sstatus, mstatus, sstatusWmask, mstatusUpdateSideEffect, sstatusRmask),
    // MaskedRegMap(Sedeleg, Sedeleg),
    // MaskedRegMap(Sideleg, Sideleg),
    MaskedRegMap(Sie, mie, sieMask, MaskedRegMap.NoSideEffect, sieMask),
    MaskedRegMap(Stvec, stvec, stvecMask, MaskedRegMap.NoSideEffect, stvecMask),
    MaskedRegMap(Scounteren, scounteren, scounterenMask),

    //--- Supervisor Configuration ---
    MaskedRegMap(Senvcfg, senvcfg),

    //--- Supervisor Trap Handling ---
    MaskedRegMap(Sscratch, sscratch),
    MaskedRegMap(Sepc, sepc, sepcMask, MaskedRegMap.NoSideEffect, sepcMask),
    MaskedRegMap(Scause, scause),
    MaskedRegMap(Stval, stval),
    MaskedRegMap(Sip, mipReg.asUInt, sipWMask, MaskedRegMap.NoSideEffect, sipMask, x => (mipWire.asUInt | x) & sipMask),

    //--- Supervisor Protection and Translation ---
    MaskedRegMap(Satp, satp, satpMask, MaskedRegMap.NoSideEffect, satpMask),

    //--- Supervisor Custom Read/Write Registers
    MaskedRegMap(Sbpctl, sbpctl),
    MaskedRegMap(Spfctl, spfctl),
    MaskedRegMap(Sfetchctl, sfetchctl),
    MaskedRegMap(Slvpredctl, slvpredctl),
    MaskedRegMap(Smblockctl, smblockctl),
    MaskedRegMap(Srnctl, srnctl),

    //--- Machine Information Registers ---
    MaskedRegMap(Mvendorid, mvendorid, 0.U(XLEN.W), MaskedRegMap.Unwritable),
    MaskedRegMap(Marchid, marchid, 0.U(XLEN.W), MaskedRegMap.Unwritable),
    MaskedRegMap(Mimpid, mimpid, 0.U(XLEN.W), MaskedRegMap.Unwritable),
    MaskedRegMap(Mhartid, mhartid, 0.U(XLEN.W), MaskedRegMap.Unwritable),
    MaskedRegMap(Mconfigptr, mconfigptr, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    //--- Machine Configuration Registers ---
    MaskedRegMap(Menvcfg, menvcfg),

    //--- Machine Trap Setup ---
    MaskedRegMap(Mstatus, mstatus, mstatusWMask, mstatusUpdateSideEffect),
    MaskedRegMap(Misa, misa, 0.U, MaskedRegMap.Unwritable), // now whole misa is unchangeable
    MaskedRegMap(Medeleg, medeleg, medelegWMask),
    MaskedRegMap(Mideleg, mideleg, midelegWMask),
    MaskedRegMap(Mie, mie, mieWMask),
    MaskedRegMap(Mtvec, mtvec, mtvecMask, MaskedRegMap.NoSideEffect, mtvecMask),
    MaskedRegMap(Mcounteren, mcounteren, mcounterenMask),

    //--- Machine Trap Handling ---
    MaskedRegMap(Mscratch, mscratch),
    MaskedRegMap(Mepc, mepc, mepcMask, MaskedRegMap.NoSideEffect, mepcMask),
    MaskedRegMap(Mcause, mcause),
    MaskedRegMap(Mtval, mtval),
    MaskedRegMap(Mip, mipReg.asUInt, mipWMask, MaskedRegMap.NoSideEffect, mipMask, x => (mipWire.asUInt | x) & mipMask),

    //--- Trigger ---
    MaskedRegMap(Tselect, tselectPhy, WritableMask, WriteTselect),
    // Todo: support chain length = 2
    MaskedRegMap(Tdata1, tdata1RegVec(tselectPhy),
      WritableMask,
      x => Tdata1Bundle.Write(x, tdata1RegVec(tselectPhy), newTriggerChainIsLegal, debug_mode = debugMode),
      WritableMask,
      x => Tdata1Bundle.Read(x)),
    MaskedRegMap(Tdata2, tdata2RegVec(tselectPhy)),
    MaskedRegMap(Tinfo, tinfo, 0.U(XLEN.W), MaskedRegMap.Unwritable),

    //--- Debug Mode ---
    MaskedRegMap(Dcsr, dcsr, dcsrMask, dcsrUpdateSideEffect),
    MaskedRegMap(Dpc, dpc),
    MaskedRegMap(Dscratch0, dscratch0),
    MaskedRegMap(Dscratch1, dscratch1),
    MaskedRegMap(Mcountinhibit, mcountinhibit),
    MaskedRegMap(Mcycle, mcycle),
    MaskedRegMap(Minstret, minstret),
  )

  // hypervisor csr map
  val hcsrMapping = Map(
    //--- Hypervisor Trap Setup ---
    MaskedRegMap(Hstatus, hstatus, hstatusWMask),
    MaskedRegMap(Hedeleg, hedeleg),
    MaskedRegMap(Hideleg, hideleg, hidelegWMask, MaskedRegMap.NoSideEffect, hidelegRMask),
    MaskedRegMap(Hie, mie, hieMask, MaskedRegMap.NoSideEffect, hieMask),
    MaskedRegMap(Hcounteren, hcounteren, hcounterenMask),
    MaskedRegMap(Hgeie, hgeie),

    //--- Hypervisor Trap Handling ---
    MaskedRegMap(Htval, htval),
    MaskedRegMap(Hip, mipReg.asUInt, hipWMask, MaskedRegMap.NoSideEffect, hipRMask, x => (mipWire.asUInt | x) & hipRMask),
    MaskedRegMap(Hvip, mipReg.asUInt, hvipMask, MaskedRegMap.NoSideEffect, hvipMask, x => (mipWire.asUInt | x) & hvipMask),
    MaskedRegMap(Htinst, htinst),
    MaskedRegMap(Hgeip, hgeip),

    //--- Hypervisor Configuration ---
    MaskedRegMap(Henvcfg, henvcfg),

    //--- Hypervisor Protection and Translation ---
    MaskedRegMap(Hgatp, hgatp, hgatpMask, MaskedRegMap.NoSideEffect, hgatpMask),

    //--- Hypervisor Counter/Timer Virtualization Registers ---
    // MaskedRegMap(Htimedelta, htimedelta),

    //--- Virtual Supervisor Registers ---
    MaskedRegMap(Vsstatus, vsstatus, rmask = sstatusRmask, wmask = sstatusWmask, wfn = vsstatusUpdateSideEffect),
    MaskedRegMap(Vsie, mie, rmask = vsip_ie_Mask, wmask = vsip_ie_Mask),
    MaskedRegMap(Vstvec, vstvec),
    MaskedRegMap(Vsscratch, vsscratch),
    MaskedRegMap(Vsepc, vsepc),
    MaskedRegMap(Vscause, vscause),
    MaskedRegMap(Vstval, vstval),
    MaskedRegMap(Vsip, mipReg.asUInt, vsip_WMask, MaskedRegMap.NoSideEffect, vsip_ie_Mask, x => mipWire.asUInt | x),
    MaskedRegMap(Vsatp, vsatp, satpMask, MaskedRegMap.NoSideEffect, satpMask),

    //--- Machine Registers ---
    MaskedRegMap(Mtval2, mtval2),
    MaskedRegMap(Mtinst, mtinst),
  )

  val perfCntMapping = (0 until 29).map(i => {Map(
    MaskedRegMap(addr = Mhpmevent3 +i,
                 reg  = perfEvents(i),
                 wmask = "hf87fff3fcff3fcff".U(XLEN.W)),
    MaskedRegMap(addr = Mhpmcounter3 +i,
                 reg = perfCnts(i)),
    MaskedRegMap(addr = Hpmcounter3 + i,
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
                (if (HasVPU) vcsrMapping else Nil) ++
                (if (HasCustomCSRCacheOp) cacheopMapping else Nil) ++
                (if (HasHExtension) hcsrMapping else Nil)


  println("XiangShan CSR Lists")

  for (addr <- mapping.keys.toSeq.sorted) {
    println(f"$addr%#03x ${mapping(addr)._1}")
  }

  val vs_s_csr_map = List(
    Sstatus.U  -> Vsstatus.U,
    Sie.U      -> Vsie.U,
    Stvec.U    -> Vstvec.U,
    Sscratch.U -> Vsscratch.U,
    Sepc.U     -> Vsepc.U,
    Scause.U   -> Vscause.U,
    Stval.U    -> Vstval.U,
    Sip.U      -> Vsip.U,
    Satp.U     -> Vsatp.U
  )
  val addr = Wire(UInt(12.W))
  val vscsr_addr = LookupTreeDefault(src2(11, 0), src2(11, 0), vs_s_csr_map)
  when(virtMode){
    addr := vscsr_addr
  }.otherwise{
    addr := src2(11, 0)
  }
  val csri = ZeroExt(src2(16, 12), XLEN)
  val rdata = Wire(UInt(XLEN.W))
  val rdata_tmp = Wire(UInt(XLEN.W))
  val wdata_tmp = LookupTree(func, List(
    CSROpType.wrt  -> src1,
    CSROpType.set  -> (rdata | src1),
    CSROpType.clr  -> (rdata & (~src1).asUInt),
    CSROpType.wrti -> csri,
    CSROpType.seti -> (rdata | csri),
    CSROpType.clri -> (rdata & (~csri).asUInt)
  ))
  val is_vsip_ie = addr === Vsip.U || addr === Vsie.U
  // for the difftest with NEMU(stay consistent with Spike)
  val is_satp  = addr === Satp.U
  val is_vsatp = addr === Vsatp.U
  val is_hgatp = addr === Hgatp.U
  val check_apt_mode = wdata_tmp(wdata_tmp.getWidth-1, 64-Satp_Mode_len) === 8.U || wdata_tmp(wdata_tmp.getWidth-1, 64-Satp_Mode_len) === 0.U
  val wdata = MuxCase(wdata_tmp, Seq(
    is_vsip_ie -> ZeroExt(wdata_tmp << 1, XLEN),
    (is_satp && !check_apt_mode) -> satp,
    (is_vsatp && !check_apt_mode) -> vsatp,
    (is_hgatp && !check_apt_mode) -> hgatp
  ))
  val addrInPerfCnt = (addr >= Mcycle.U) && (addr <= Mhpmcounter31.U) ||
    (addr >= Mcountinhibit.U) && (addr <= Mhpmevent31.U) ||
    (addr >= Cycle.U) && (addr <= Hpmcounter31.U) ||
    addr === Mip.U
  csrio.isPerfCnt := addrInPerfCnt && valid && func =/= CSROpType.jmp

  // satp wen check
  val satpLegalMode = (wdata.asTypeOf(new SatpStruct).mode===0.U) || (wdata.asTypeOf(new SatpStruct).mode===8.U)

  // csr access check, special case
  val tvmNotPermit = (privilegeMode === ModeS && !virtMode && mstatusStruct.tvm.asBool)
  val accessPermitted = !(addr === Satp.U && tvmNotPermit)
  val vtvmNotPermit = (privilegeMode === ModeS && virtMode && hstatusStruct.vtvm.asBool)
  val vaccessPermitted = !(addr === Vsatp.U && vtvmNotPermit)
//  csrio.disableSfence := (tvmNotPermit || !virtMode && privilegeMode < ModeS) || (vtvmNotPermit || virtMode && privilegeMode < ModeS)
//  csrio.disableHfenceg := !((!virtMode && privilegeMode === ModeS && !mstatusStruct.tvm.asBool) || (privilegeMode === ModeM)) // only valid in HS and mstatus.tvm == 0 or in M
//  csrio.disableHfencev :=  !(privilegeMode === ModeM || (!virtMode && privilegeMode === ModeS))

  // general CSR wen check
  val wen = valid && CSROpType.isCsrAccess(func) && ((addr=/=Satp.U && addr =/= Vsatp.U) || satpLegalMode)
  val dcsrPermitted = dcsrPermissionCheck(addr, false.B, debugMode)
  val triggerPermitted = triggerPermissionCheck(addr, true.B, debugMode) // todo dmode
  val HasH = (HasHExtension == true).asBool
  val csrAccess = csrAccessPermissionCheck(addr, false.B, privilegeMode, virtMode, HasH)
  val modePermitted = csrAccess === 0.U && dcsrPermitted && triggerPermitted
  val perfcntPermitted = perfcntPermissionCheck(addr, privilegeMode, mcounteren, scounteren)
  val permitted = Mux(addrInPerfCnt, perfcntPermitted, modePermitted) && Mux(virtMode, vaccessPermitted, accessPermitted)
  MaskedRegMap.generate(mapping, addr, rdata_tmp, wen && permitted, wdata)
  rdata := Mux(is_vsip_ie, ZeroExt(rdata_tmp >> 1, XLEN), rdata_tmp)
  io.out.bits.res.data := rdata
  io.out.bits.ctrl.flushPipe.get := flushPipe
  connect0LatencyCtrlSingal

  // send distribute csr a w signal
  csrio.customCtrl.distribute_csr.w.valid := wen && permitted
  csrio.customCtrl.distribute_csr.w.bits.data := wdata
  csrio.customCtrl.distribute_csr.w.bits.addr := addr

  when (RegNext(csrio.fpu.fflags.valid)) {
    fcsr := fflags_wfn(update = true)(RegEnable(csrio.fpu.fflags.bits, csrio.fpu.fflags.valid))
  }
  when(RegNext(csrio.vpu.set_vxsat.valid)) {
    vcsr := vxsat_wfn(update = true)(RegEnable(csrio.vpu.set_vxsat.bits, csrio.vpu.set_vxsat.valid))
  }

  // set fs and sd in mstatus
  when (csrw_dirty_fp_state || RegNext(csrio.fpu.dirty_fs)) {
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.fs := "b11".U
    mstatusNew.sd := true.B
    mstatus := mstatusNew.asUInt
    when(virtMode){
      val vsstatusNew = WireInit(vsstatus.asTypeOf(new MstatusStruct))
      vsstatusNew.fs := "b11".U
      vsstatusNew.sd := true.B
      vsstatus := vsstatusNew.asUInt
    }
  }
  csrio.fpu.frm := fcsr.asTypeOf(new FcsrStruct).frm

  when (RegNext(csrio.vpu.set_vstart.valid)) {
    vstart := RegEnable(csrio.vpu.set_vstart.bits, csrio.vpu.set_vstart.valid)
  }
  when (RegNext(csrio.vpu.set_vtype.valid)) {
    vtype := RegEnable(csrio.vpu.set_vtype.bits, csrio.vpu.set_vtype.valid)
  }
  vl := csrio.vpu.vl
  // set vs and sd in mstatus
  when(csrw_dirty_vs_state || RegNext(csrio.vpu.dirty_vs)) {
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    mstatusNew.vs := ContextStatus.dirty
    mstatusNew.sd := true.B
    mstatus := mstatusNew.asUInt
  }

  csrio.vpu.vstart := vstart
  csrio.vpu.vxrm := vcsr.asTypeOf(new VcsrStruct).vxrm

  // Trigger Ctrl
  val triggerEnableVec = tdata1RegVec.map { tdata1 =>
    val mcontrolData = tdata1.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData)
    tdata1.asTypeOf(new Tdata1Bundle).type_.asUInt === TrigTypeEnum.MCONTROL && (
      mcontrolData.m && privilegeMode === ModeM ||
        mcontrolData.s && privilegeMode === ModeS ||
        mcontrolData.u && privilegeMode === ModeU)
  }
  val fetchTriggerEnableVec = triggerEnableVec.zip(tdata1WireVec).map {
    case (tEnable, tdata1) => tEnable && tdata1.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isFetchTrigger
  }
  val memAccTriggerEnableVec = triggerEnableVec.zip(tdata1WireVec).map {
    case (tEnable, tdata1) => tEnable && tdata1.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isMemAccTrigger
  }
  csrio.customCtrl.frontend_trigger.tEnableVec := fetchTriggerEnableVec
  csrio.customCtrl.mem_trigger.tEnableVec := memAccTriggerEnableVec

  val tdata1Update = wen && (addr === Tdata1.U)
  val tdata2Update = wen && (addr === Tdata2.U)
  val triggerUpdate = wen && (addr === Tdata1.U || addr === Tdata2.U)
  val frontendTriggerUpdate =
    tdata1Update && wdata.asTypeOf(new Tdata1Bundle).type_.asUInt === TrigTypeEnum.MCONTROL &&
      wdata.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isFetchTrigger ||
      tdata1Selected.data.asTypeOf(new MControlData).isFetchTrigger && triggerUpdate
  val memTriggerUpdate =
    tdata1Update && wdata.asTypeOf(new Tdata1Bundle).type_.asUInt === TrigTypeEnum.MCONTROL &&
      wdata.asTypeOf(new Tdata1Bundle).data.asTypeOf(new MControlData).isMemAccTrigger ||
      tdata1Selected.data.asTypeOf(new MControlData).isMemAccTrigger && triggerUpdate

  csrio.customCtrl.frontend_trigger.tUpdate.valid := RegNext(RegNext(frontendTriggerUpdate))
  csrio.customCtrl.mem_trigger.tUpdate.valid := RegNext(RegNext(memTriggerUpdate))
  XSDebug(triggerEnableVec.reduce(_ || _), p"Debug Mode: At least 1 trigger is enabled," +
    p"trigger enable is ${Binary(triggerEnableVec.asUInt)}\n")

  // CSR inst decode
  val isEbreak = addr === privEbreak && func === CSROpType.jmp
  val isEcall  = addr === privEcall  && func === CSROpType.jmp
  val isMret   = addr === privMret   && func === CSROpType.jmp
  val isSret   = addr === privSret   && func === CSROpType.jmp
  val isUret   = addr === privUret   && func === CSROpType.jmp
  val isDret   = addr === privDret   && func === CSROpType.jmp
  val isWFI    = func === CSROpType.wfi

  // Illegal privileged operation list
  val illegalMret = valid && isMret && privilegeMode < ModeM
  val illegalSret = valid && isSret && privilegeMode < ModeS
  val illegalSModeSret = valid && isSret && privilegeMode === ModeS && virtMode === false.B && mstatusStruct.tsr.asBool
  // when hstatus.vtsr == 1, if sret is executed in VS-mode, it will cause virtual instruction
  val illegalVSModeSret = valid && isSret && privilegeMode === ModeS && virtMode && hstatusStruct.vtsr.asBool
  // When TW=1, then if WFI is executed in any less-privileged mode,
  // and it does not complete within an implementation-specific, bounded time limit,
  // the WFI instruction causes an illegal instruction exception.
  // The time limit may always be 0, in which case WFI always causes
  // an illegal instruction exception in less-privileged modes when TW=1.
  val illegalWFI = valid && isWFI && (privilegeMode < ModeM && mstatusStruct.tw === 1.U ||  privilegeMode === ModeU && !virtMode)
  val illegalVWFI = valid && isWFI && ((virtMode && privilegeMode === ModeS && hstatusStruct.vtw === 1.U && mstatusStruct.tw === 0.U)||
      (virtMode && privilegeMode === ModeU && mstatusStruct.tw === 0.U))
  // Illegal privileged instruction check
  val isIllegalAddr = valid && CSROpType.isCsrAccess(func) && MaskedRegMap.isIllegalAddr(mapping, addr)
  val isIllegalAccess = !virtMode && wen && !(Mux(addrInPerfCnt, perfcntPermitted, csrAccess === 0.U && dcsrPermitted && triggerPermitted) && accessPermitted)
  val isIllegalPrivOp = illegalMret || illegalSret || illegalSModeSret || illegalWFI

  val isIllegalVAccess = virtMode && wen && (csrAccess === 2.U || !vaccessPermitted)
  val isIllegalVPrivOp = illegalVSModeSret || illegalVWFI
  // expose several csr bits for tlb
  tlbBundle.priv.mxr   := mstatusStruct.mxr.asBool
  tlbBundle.priv.sum   := mstatusStruct.sum.asBool
  tlbBundle.priv.vmxr := vsstatusStruct.mxr.asBool
  tlbBundle.priv.vsum := vsstatusStruct.sum.asBool
  tlbBundle.priv.spvp := hstatusStruct.spvp
  tlbBundle.priv.virt  := Mux(mstatusStruct.mprv.asBool, mstatusStruct.mpv & (mstatusStruct.mpp =/= ModeM), virtMode)
  tlbBundle.priv.imode := privilegeMode
  tlbBundle.priv.dmode := Mux((debugMode && dcsr.asTypeOf(new DcsrStruct).mprven || !debugMode) && mstatusStruct.mprv.asBool, mstatusStruct.mpp, privilegeMode)

  // Branch control
  val retTarget = WireInit(0.U)
  val resetSatp = (addr === Satp.U || addr === Hgatp.U || addr === Vsatp.U) && wen // write to satp will cause the pipeline be flushed
  val writeVstart = addr === Vstart.U && wen // write to vstart will cause the pipeline be flushed
  dontTouch(writeVstart)

  val w_fcsr_change_rm = wen && addr === Fcsr.U && wdata(7, 5) =/= fcsr(7, 5)
  val w_frm_change_rm = wen && addr === Frm.U && wdata(2, 0) =/= fcsr(7, 5)
  val frm_change = w_fcsr_change_rm || w_frm_change_rm
  val isXRet = valid && func === CSROpType.jmp && !isEcall && !isEbreak
  flushPipe := resetSatp || frm_change || isXRet || frontendTriggerUpdate || writeVstart

  private val illegalRetTarget = WireInit(false.B)
  when(valid) {
    when(isDret) {
      retTarget := dpc(VAddrBits - 1, 0)
    }.elsewhen(isMret && !illegalMret) {
      retTarget := mepc(VAddrBits - 1, 0)
    }.elsewhen(isSret && !illegalSret && !illegalSModeSret && !illegalVSModeSret) {
      retTarget := Mux(virtMode, vsepc(VAddrBits - 1, 0), sepc(VAddrBits - 1, 0))
    }.elsewhen(isUret) {
      retTarget := uepc(VAddrBits - 1, 0)
    }.otherwise {
      illegalRetTarget := true.B
    }
  }.otherwise {
    illegalRetTarget := true.B // when illegalRetTarget setted, retTarget should never be used
  }

  // Mux tree for regs
  when(valid) {
    when(isDret) {
      val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
      val debugModeNew = WireInit(debugMode)
      when(dcsr.asTypeOf(new DcsrStruct).prv =/= ModeM) {
        mstatusNew.mprv := 0.U
      } //If the new privilege mode is less privileged than M-mode, MPRV in mstatus is cleared.
      mstatus := mstatusNew.asUInt
      privilegeMode := dcsr.asTypeOf(new DcsrStruct).prv
      debugModeNew := false.B
      debugIntrEnable := true.B
      debugMode := debugModeNew
      XSDebug("Debug Mode: Dret executed, returning to %x.", retTarget)
    }.elsewhen(isMret && !illegalMret) {
      val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
      val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
      mstatusNew.ie.m := mstatusOld.pie.m
      privilegeMode := mstatusOld.mpp
      if (HasHExtension) {
        virtMode := mstatusOld.mpv
        mstatusNew.mpv := 0.U
      }
      mstatusNew.pie.m := true.B
      mstatusNew.mpp := ModeU
      when(mstatusOld.mpp =/= ModeM) {
        mstatusNew.mprv := 0.U
      }
      mstatus := mstatusNew.asUInt
    }.elsewhen(isSret && !illegalSret && !illegalSModeSret && !illegalVSModeSret) {
      val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
      val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
      val hstatusOld = WireInit(hstatus.asTypeOf(new HstatusStruct))
      val hstatusNew = WireInit(hstatus.asTypeOf(new HstatusStruct))
      val vsstatusOld = WireInit(vsstatus.asTypeOf(new MstatusStruct))
      val vsstatusNew = WireInit(vsstatus.asTypeOf(new MstatusStruct))
      when(virtMode === 0.U) {
        virtMode := hstatusOld.spv
        hstatusNew.spv := 0.U
        mstatusNew.ie.s := mstatusOld.pie.s
        privilegeMode := Cat(0.U(1.W), mstatusOld.spp)
        mstatusNew.pie.s := true.B
        mstatusNew.spp := ModeU
        when(mstatusOld.spp =/= ModeM) {
          mstatusNew.mprv := 0.U
        }
        mstatus := mstatusNew.asUInt
        hstatus := hstatusNew.asUInt
      }.otherwise {
        privilegeMode := vsstatusOld.spp
        vsstatusNew.spp := ModeU
        vsstatusNew.ie.s := vsstatusOld.pie.s
        vsstatusNew.pie.s := 1.U
        vsstatus := vsstatusNew.asUInt
      }
    }.elsewhen(isUret) {
      val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
      val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
      // mstatusNew.mpp.m := ModeU //TODO: add mode U
      mstatusNew.ie.u := mstatusOld.pie.u
      privilegeMode := ModeU
      mstatusNew.pie.u := true.B
      mstatus := mstatusNew.asUInt
    }
  }

  io.in.ready := true.B
  io.out.valid := valid

  // In this situation, hart will enter debug mode instead of handling a breakpoint exception simply.
  // Ebreak block instructions backwards, so it's ok to not keep extra info to distinguish between breakpoint
  // exception and enter-debug-mode exception.
  val ebreakEnterDebugMode =
    (privilegeMode === ModeM && dcsrData.ebreakm) ||
    (privilegeMode === ModeS && dcsrData.ebreaks) ||
    (privilegeMode === ModeU && dcsrData.ebreaku)

  // raise a debug exception waiting to enter debug mode, instead of a breakpoint exception
  val raiseDebugException = !debugMode && isEbreak && ebreakEnterDebugMode

  val csrExceptionVec = WireInit(0.U.asTypeOf(ExceptionVec()))
  csrExceptionVec(breakPoint) := io.in.valid && isEbreak
  csrExceptionVec(ecallM) := privilegeMode === ModeM && io.in.valid && isEcall
  csrExceptionVec(ecallVS) := privilegeMode === ModeS && virtMode && io.in.valid && isEcall
  csrExceptionVec(ecallS) := privilegeMode === ModeS && !virtMode && io.in.valid && isEcall
  csrExceptionVec(ecallU) := privilegeMode === ModeU && io.in.valid && isEcall
  // Trigger an illegal instr exception when:
  // * unimplemented csr is being read/written
  // * csr access is illegal
  csrExceptionVec(illegalInstr) := isIllegalAddr || isIllegalAccess || isIllegalPrivOp
  csrExceptionVec(virtualInstr) := isIllegalVAccess || isIllegalVPrivOp
  io.out.bits.ctrl.exceptionVec.get := csrExceptionVec

  XSDebug(io.in.valid, s"Debug Mode: an Ebreak is executed, ebreak cause enter-debug-mode exception ? ${raiseDebugException}\n")

  /**
    * Exception and Intr
    */
  val idelegS =  (mideleg & mip.asUInt)
  val idelegVS = (hideleg & mideleg & mip.asUInt)
  def privilegedEnableDetect(idelegS: Bool, idelegVS: Bool): Bool = Mux(idelegS,
    Mux(idelegVS, (virtMode && privilegeMode === ModeS && vsstatusStruct.ie.s) || (virtMode && privilegeMode < ModeS),
      ((privilegeMode === ModeS) && mstatusStruct.ie.s) || (privilegeMode < ModeS) || virtMode),
    ((privilegeMode === ModeM) && mstatusStruct.ie.m) || (privilegeMode < ModeM))

  val debugIntr = csrio.externalInterrupt.debug & debugIntrEnable
  XSDebug(debugIntr, "Debug Mode: debug interrupt is asserted and valid!")
  // send interrupt information to ROB
  val intrVecEnable = Wire(Vec(13, Bool()))
  val disableInterrupt = debugMode || (dcsrData.step && !dcsrData.stepie)
  intrVecEnable.zip(idelegS.asBools).zip(idelegVS.asBools).map{case((x,y),z) => x := privilegedEnableDetect(y, z) && !disableInterrupt}
  val intrVec = Cat(debugIntr && !debugMode, (mie(11,0) & mip.asUInt & intrVecEnable.asUInt))
  val intrBitSet = intrVec.orR
  csrio.interrupt := intrBitSet
  // Page 45 in RISC-V Privileged Specification
  // The WFI instruction can also be executed when interrupts are disabled. The operation of WFI
  // must be unaffected by the global interrupt bits in mstatus (MIE and SIE) and the delegation
  // register mideleg, but should honor the individual interrupt enables (e.g, MTIE).
  csrio.wfi_event := debugIntr || (mie(11, 0) & mip.asUInt).orR
  mipWire.t.m := csrio.externalInterrupt.mtip
  mipWire.s.m := csrio.externalInterrupt.msip
  mipWire.e.m := csrio.externalInterrupt.meip
  mipWire.e.s := csrio.externalInterrupt.seip

  // interrupts
  val intrNO = IntPriority.foldRight(0.U)((i: Int, sum: UInt) => Mux(intrVec(i), i.U, sum))
  val hasIntr = csrio.exception.valid && csrio.exception.bits.isInterrupt
  val ivmEnable = tlbBundle.priv.imode < ModeM && satp.asTypeOf(new SatpStruct).mode === 8.U
  val iexceptionPC = Mux(ivmEnable, SignExt(csrio.exception.bits.pc, XLEN), csrio.exception.bits.pc)
  val iexceptionGPAddr = Mux(ivmEnable, SignExt(csrio.exception.bits.gpaddr, XLEN), csrio.exception.bits.gpaddr)
  val dvmEnable = tlbBundle.priv.dmode < ModeM && satp.asTypeOf(new SatpStruct).mode === 8.U
  val dexceptionPC = Mux(dvmEnable, SignExt(csrio.exception.bits.pc, XLEN), csrio.exception.bits.pc)
  XSDebug(hasIntr, "interrupt: pc=0x%x, %d\n", dexceptionPC, intrNO)
  val hasDebugIntr = intrNO === IRQ_DEBUG.U && hasIntr

  // exceptions from rob need to handle
  val exceptionVecFromRob    = csrio.exception.bits.exceptionVec
  val hasException           = csrio.exception.valid && !csrio.exception.bits.isInterrupt
  val hasInstrPageFault      = hasException && exceptionVecFromRob(instrPageFault)
  val hasLoadPageFault       = hasException && exceptionVecFromRob(loadPageFault)
  val hasStorePageFault      = hasException && exceptionVecFromRob(storePageFault)
  val hasStoreAddrMisalign   = hasException && exceptionVecFromRob(storeAddrMisaligned)
  val hasLoadAddrMisalign    = hasException && exceptionVecFromRob(loadAddrMisaligned)
  val hasInstrAccessFault    = hasException && exceptionVecFromRob(instrAccessFault)
  val hasLoadAccessFault     = hasException && exceptionVecFromRob(loadAccessFault)
  val hasStoreAccessFault    = hasException && exceptionVecFromRob(storeAccessFault)
  val hasBreakPoint          = hasException && exceptionVecFromRob(breakPoint)
  val hasInstGuestPageFault  = hasException && exceptionVecFromRob(instrGuestPageFault)
  val hasLoadGuestPageFault  = hasException && exceptionVecFromRob(loadGuestPageFault)
  val hasStoreGuestPageFault = hasException && exceptionVecFromRob(storeGuestPageFault)
  val hasSingleStep          = hasException && csrio.exception.bits.singleStep
  val hasTriggerFire         = hasException && csrio.exception.bits.trigger.canFire
  val triggerFrontendHitVec = csrio.exception.bits.trigger.frontendHit
  val triggerMemHitVec = csrio.exception.bits.trigger.backendHit
  val triggerHitVec = triggerFrontendHitVec | triggerMemHitVec // Todo: update mcontrol.hit
  val triggerCanFireVec = csrio.exception.bits.trigger.frontendCanFire | csrio.exception.bits.trigger.backendCanFire
  // More than one triggers can hit at the same time, but only fire one
  // We select the first hit trigger to fire
  val triggerFireOH = PriorityEncoderOH(triggerCanFireVec)
  val triggerFireAction = PriorityMux(triggerFireOH, tdata1WireVec.map(_.getTriggerAction)).asUInt


  XSDebug(hasSingleStep, "Debug Mode: single step exception\n")
  XSDebug(hasTriggerFire, p"Debug Mode: trigger fire, frontend hit vec ${Binary(csrio.exception.bits.trigger.frontendHit.asUInt)} " +
    p"backend hit vec ${Binary(csrio.exception.bits.trigger.backendHit.asUInt)}\n")

  val hasExceptionVec = csrio.exception.bits.exceptionVec
  val regularExceptionNO = ExceptionNO.priorities.foldRight(0.U)((i: Int, sum: UInt) => Mux(hasExceptionVec(i), i.U, sum))
  val exceptionNO = Mux(hasSingleStep || hasTriggerFire, 3.U, regularExceptionNO)
  val causeNO = (hasIntr << (XLEN - 1)).asUInt | Mux(hasIntr, intrNO, exceptionNO)

  val hasExceptionIntr = csrio.exception.valid

  val hasDebugEbreakException = hasBreakPoint && ebreakEnterDebugMode
  val hasDebugTriggerException = hasTriggerFire && triggerFireAction === TrigActionEnum.DEBUG_MODE
  val hasDebugException = hasDebugEbreakException || hasDebugTriggerException || hasSingleStep
  val hasDebugTrap = hasDebugException || hasDebugIntr
  val ebreakEnterParkLoop = debugMode && hasExceptionIntr

  XSDebug(hasExceptionIntr, "int/exc: pc %x int (%d):%x exc: (%d):%x\n",
    dexceptionPC, intrNO, intrVec, exceptionNO, hasExceptionVec.asUInt
  )
  XSDebug(hasExceptionIntr,
    "pc %x mstatus %x mideleg %x medeleg %x mode %x\n",
    dexceptionPC,
    mstatus,
    mideleg,
    medeleg,
    privilegeMode
  )

  // mtval write logic
  // Due to timing reasons of memExceptionVAddr, we delay the write of mtval and stval
  val memExceptionAddr = SignExt(csrio.memExceptionVAddr, XLEN)
  val memExceptionGPAddr = SignExt(csrio.memExceptionGPAddr, XLEN)
  val updateTval = VecInit(Seq(
    hasInstrPageFault,
    hasLoadPageFault,
    hasStorePageFault,
    hasInstrAccessFault,
    hasLoadAccessFault,
    hasStoreAccessFault,
    hasLoadAddrMisalign,
    hasStoreAddrMisalign,
    hasInstGuestPageFault,
    hasLoadGuestPageFault,
    hasStoreGuestPageFault,
    hasBreakPoint,
  )).asUInt.orR
  val updateTval_h = VecInit(Seq(
    hasInstGuestPageFault,
    hasLoadGuestPageFault,
    hasStoreGuestPageFault
  )).asUInt.orR
  when (RegNext(RegNext(updateTval))) {
      val tval = Mux(
        RegNext(RegNext(hasInstrPageFault || hasInstrAccessFault || hasInstGuestPageFault || hasBreakPoint)),
        RegNext(RegNext(Mux(
          csrio.exception.bits.crossPageIPFFix,
          SignExt(csrio.exception.bits.pc + 2.U, XLEN),
          iexceptionPC
        ))),
        memExceptionAddr
    )
    // because we update tval two beats later, we can choose xtval according to the privilegeMode which has been updated
    when (RegNext(privilegeMode === ModeM)) {
      mtval := tval
    }.otherwise {
      when (virtMode){
        vstval := tval
      }.otherwise{
        stval := tval
      }
    }
  }

  when(RegNext(RegNext(updateTval_h))) {
    val tval_tmp = Mux(
      RegNext(RegNext(hasInstGuestPageFault)),
      RegNext(RegNext(Mux(
        csrio.exception.bits.crossPageIPFFix,
        SignExt(csrio.exception.bits.gpaddr + 2.U, XLEN),
        iexceptionGPAddr
      ))),
      memExceptionGPAddr
    )
    val tval = tval_tmp >> 2
    when(RegNext(privilegeMode === ModeM)) {
      mtval2 := tval
    }.otherwise {
      htval := tval
    }
  }

  val debugTrapTarget = Mux(!isEbreak && debugMode, 0x38020808.U, 0x38020800.U) // 0x808 is when an exception occurs in debug mode prog buf exec
  val deleg = Mux(hasIntr, mideleg , medeleg)
  val hdeleg = Mux(hasIntr, hideleg, hedeleg)
  // val delegS = ((deleg & (1 << (causeNO & 0xf))) != 0) && (privilegeMode < ModeM);
  val delegS = deleg(causeNO(7,0)) && (privilegeMode < ModeM)
  val delegVS = virtMode && delegS && hdeleg(causeNO(7, 0)) && (privilegeMode < ModeM)
  val clearTval = !updateTval || hasIntr

  val clearTval_h = !updateTval_h || hasIntr
  val isHyperInst = csrio.exception.bits.isHls
  // ctrl block will use theses later for flush
  val isXRetFlag = RegInit(false.B)
  when (DelayN(io.flush.valid, 5)) {
    isXRetFlag := false.B
  }.elsewhen (isXRet) {
    isXRetFlag := true.B
  }
  csrio.isXRet := isXRetFlag
  private val retTargetReg = RegEnable(retTarget, isXRet && !illegalRetTarget)
  private val illegalXret = RegEnable(illegalMret || illegalSret || illegalSModeSret || illegalVSModeSret, isXRet)

  private val xtvec = Mux(delegS, Mux(delegVS, vstvec, stvec), mtvec)
  private val xtvecBase = xtvec(VAddrBits - 1, 2)
  // When MODE=Vectored, all synchronous exceptions into M/S mode
  // cause the pc to be set to the address in the BASE field, whereas
  // interrupts cause the pc to be set to the address in the BASE field
  // plus four times the interrupt cause number.
  private val pcFromXtvec = Cat(xtvecBase + Mux(xtvec(0) && hasIntr, causeNO(3, 0), 0.U), 0.U(2.W))

  // XRet sends redirect instead of Flush and isXRetFlag is true.B before redirect.valid.
  // ROB sends exception at T0 while CSR receives at T2.
  // We add a RegNext here and trapTarget is valid at T3.
  csrio.trapTarget := RegEnable(
    MuxCase(pcFromXtvec, Seq(
      (isXRetFlag && !illegalXret) -> retTargetReg,
      ((hasDebugTrap && !debugMode) || ebreakEnterParkLoop) -> debugTrapTarget
    )),
    isXRetFlag || csrio.exception.valid)

  when(hasExceptionIntr) {
    val mstatusOld = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val mstatusNew = WireInit(mstatus.asTypeOf(new MstatusStruct))
    val hstatusOld = WireInit(hstatus.asTypeOf(new HstatusStruct))
    val hstatusNew = WireInit(hstatus.asTypeOf(new HstatusStruct))
    val vsstatusOld = WireInit(vsstatus.asTypeOf(new MstatusStruct))
    val vsstatusNew = WireInit(vsstatus.asTypeOf(new MstatusStruct))
    val dcsrNew = WireInit(dcsr.asTypeOf(new DcsrStruct))
    val debugModeNew = WireInit(debugMode)
    when(hasDebugTrap && !debugMode) {
      import DcsrStruct._
      debugModeNew := true.B
      dcsrNew.prv := privilegeMode
      privilegeMode := ModeM
      when(hasDebugIntr) {
        dpc := iexceptionPC
        dcsrNew.cause := CAUSE_HALTREQ
        XSDebug(hasDebugIntr, "Debug Mode: Trap to %x at pc %x\n", debugTrapTarget, dpc)
      }.otherwise { // hasDebugException
        dpc := iexceptionPC // TODO: check it when hasSingleStep
        dcsrNew.cause := MuxCase(0.U, Seq(
          hasTriggerFire -> CAUSE_TRIGGER,
          raiseDebugException -> CAUSE_EBREAK,
          hasBreakPoint -> CAUSE_HALTREQ,
          hasSingleStep -> CAUSE_STEP
        ))
      }
      dcsr := dcsrNew.asUInt
      debugIntrEnable := false.B
    }.elsewhen (debugMode) {
      //do nothing
    }.elsewhen (delegVS) {
      vscause := (hasIntr << (XLEN-1)).asUInt | Mux(hasIntr, intrNO - 1.U, exceptionNO)
      vsepc := Mux(hasInstrPageFault || hasInstrAccessFault, iexceptionPC, dexceptionPC)
      vsstatusNew.spp := privilegeMode
      vsstatusNew.pie.s := vsstatusOld.ie.s
      vsstatusNew.ie.s := false.B
      when (clearTval) {vstval := 0.U}
      virtMode := true.B
      privilegeMode := ModeS
    }.elsewhen (delegS) {
      val virt = Mux(mstatusOld.mprv.asBool, mstatusOld.mpv, virtMode)
      // to do hld st
      hstatusNew.gva := (hasInstGuestPageFault || hasLoadGuestPageFault || hasStoreGuestPageFault ||
                      ((virt.asBool || isHyperInst) && ((hasException && 0.U <= exceptionNO && exceptionNO <= 7.U && exceptionNO =/= 2.U)
                      || hasInstrPageFault || hasLoadPageFault || hasStorePageFault)))
      hstatusNew.spv := virtMode
      when(virtMode){
        hstatusNew.spvp := privilegeMode
      }
      virtMode := false.B
      scause := causeNO
      sepc := Mux(hasInstrPageFault || hasInstrAccessFault, iexceptionPC, dexceptionPC)
      mstatusNew.spp := privilegeMode
      mstatusNew.pie.s := mstatusOld.ie.s
      mstatusNew.ie.s := false.B
      privilegeMode := ModeS
      when (clearTval) { stval := 0.U }
      when (clearTval_h) {htval := 0.U}
    }.otherwise {
      val virt = Mux(mstatusOld.mprv.asBool, mstatusOld.mpv, virtMode)
      // to do hld st
      mstatusNew.gva := (hasInstGuestPageFault || hasLoadGuestPageFault || hasStoreGuestPageFault ||
      ((virt.asBool || isHyperInst) && ((hasException && 0.U <= exceptionNO && exceptionNO <= 7.U && exceptionNO =/= 2.U)
        || hasInstrPageFault || hasLoadPageFault || hasStorePageFault)))
      mstatusNew.mpv := virtMode
      virtMode := false.B
      mcause := causeNO
      mepc := Mux(hasInstrPageFault || hasInstrAccessFault, iexceptionPC, dexceptionPC)
      mstatusNew.mpp := privilegeMode
      mstatusNew.pie.m := mstatusOld.ie.m
      mstatusNew.ie.m := false.B
      privilegeMode := ModeM
      when (clearTval) { mtval := 0.U }
      when (clearTval_h) {mtval2 := 0.U}
    }
    mstatus := mstatusNew.asUInt
    vsstatus := vsstatusNew.asUInt
    hstatus := hstatusNew.asUInt
    debugMode := debugModeNew
  }

  // Cache error debug support
  if(HasCustomCSRCacheOp){
    val cache_error_decoder = Module(new CSRCacheErrorDecoder)
    cache_error_decoder.io.encoded_cache_error := cacheopRegs("CACHE_ERROR")
  }

  // Implicit add reset values for mepc[0] and sepc[0]
  // TODO: rewrite mepc and sepc using a struct-like style with the LSB always being 0
  when (RegNext(RegNext(reset.asBool) && !reset.asBool)) {
    mepc := Cat(mepc(XLEN - 1, 1), 0.U(1.W))
    sepc := Cat(sepc(XLEN - 1, 1), 0.U(1.W))
    vsepc := Cat(vsepc(XLEN - 1, 1), 0.U(1.W))
  }

  def readWithScala(addr: Int): UInt = mapping(addr)._1

  val difftestIntrNO = Mux(hasIntr, causeNO, 0.U)

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = DifftestModule(new DiffArchEvent, delay = 3, dontCare = true)
    difftest.coreid      := csrio.hartId
    difftest.valid       := csrio.exception.valid
    difftest.interrupt   := Mux(hasIntr, causeNO, 0.U)
    difftest.exception   := Mux(hasException, causeNO, 0.U)
    difftest.exceptionPC := dexceptionPC
    if (env.EnableDifftest) {
      difftest.exceptionInst := csrio.exception.bits.instr
    }
  }

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = DifftestModule(new DiffCSRState)
    difftest.coreid := csrio.hartId
    difftest.privilegeMode := privilegeMode
    difftest.mstatus := mstatus
    difftest.sstatus := mstatus & sstatusRmask
    difftest.mepc := mepc
    difftest.sepc := sepc
    difftest.mtval:= mtval
    difftest.stval:= stval
    difftest.mtvec := mtvec
    difftest.stvec := stvec
    difftest.mcause := mcause
    difftest.scause := scause
    difftest.satp := satp
    difftest.mip := mipReg
    difftest.mie := mie
    difftest.mscratch := mscratch
    difftest.sscratch := sscratch
    difftest.mideleg := mideleg
    difftest.medeleg := medeleg
  }

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = DifftestModule(new DiffHCSRState)
    difftest.coreid := csrio.hartId
    difftest.virtMode := virtMode
    difftest.mtval2 := mtval2
    difftest.mtinst := mtinst
    difftest.hstatus := hstatus
    difftest.hideleg := hideleg
    difftest.hedeleg := hedeleg
    difftest.hcounteren := hcounteren
    difftest.htval := htval
    difftest.htinst := htinst
    difftest.hgatp := hgatp
    difftest.vsstatus := vsstatus
    difftest.vstvec := vstvec
    difftest.vsepc := vsepc
    difftest.vscause := vscause
    difftest.vstval := vstval
    difftest.vsatp := vsatp
    difftest.vsscratch := vsscratch
  }

  if(env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = DifftestModule(new DiffDebugMode)
    difftest.coreid := csrio.hartId
    difftest.debugMode := debugMode
    difftest.dcsr := dcsr
    difftest.dpc := dpc
    difftest.dscratch0 := dscratch0
    difftest.dscratch1 := dscratch1
  }

  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    val difftest = DifftestModule(new DiffVecCSRState)
    difftest.coreid := csrio.hartId
    difftest.vstart := vstart
    difftest.vxsat := vcsr.asTypeOf(new VcsrStruct).vxsat
    difftest.vxrm := vcsr.asTypeOf(new VcsrStruct).vxrm
    difftest.vcsr := vcsr
    difftest.vl := vl
    difftest.vtype := vtype
    difftest.vlenb := vlenb
  }
}
*/
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
