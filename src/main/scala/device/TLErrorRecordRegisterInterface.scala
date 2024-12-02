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

package device

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import chisel3.experimental.SourceInfo

object TLErrorRecordConsts {
  def minBanks:   Int = 1
  def minRecords: Int = 1

  //  Error signaling enable field encodings
  def SignalingDisabled:          Int = 0
  def SignalingLowPriority:       Int = 1
  def SignalingHighPriority:      Int = 2
  def SignalingSpecificPriority:  Int = 3

  //  Address-or-information type encodings
  def AddrIsNone: Int = 0
  def AddrIsSPA:  Int = 1
  def AddrIsGPA:  Int = 2
  def AddrIsVA:   Int = 3

  //  Transaction type encodings
  def TxnUnSpecified:   Int = 0
  def TxnCustomUse:     Int = 1
  def TxnExplicitRead:  Int = 4
  def TxnExplicitWrite: Int = 5
  def TxnImplicitRead:  Int = 6
  def TxnImplicitWrite: Int = 7

  //  Error code encodings
  def none: Int                         = 0
  def OtherUnspecifiedError: Int        = 1
  def CorruptedDataAccess: Int          = 2
  def CacheBlockDataErro: Int           = 3
  def CacheScrubbingDetectedError: Int  = 4
  def CacheAddrOrCtrlStateError: Int    = 5
  def CacheUnspecifiedError: Int        = 6
  def SnoopAddrOrCtrlStateError: Int    = 7
  def SnoopUnspecifiedError: Int        = 8
  def PtwCacheDataError: Int            = 9
  def PtwCacheAddrOrCtrlStateError: Int = 10
  def PtwCacheUnspecifiedError: Int     = 11
  def HartStateError: Int               = 12
  def IntrCtrlStateError: Int           = 13
  def InterconnectDataError: Int        = 14
  def InterconnectOtherError: Int       = 15
  def InternalWatchDogError: Int        = 16
  def InternalExuError: Int             = 17
  def SystemMemoryBusError: Int         = 18
  def SystemMemoryUnspecifiedError: Int = 19
  def SystemMemoryDataError: Int        = 20
  def SystemMemoryScrubbingDetectedError: Int = 21
  def ProtocolIllegalIOError: Int       = 22
  def ProtocolIllegalStateError: Int    = 23
  def ProtocolTimeOutError: Int         = 24
  def DeferredErrorPassThroughNotSupported: Int = 25
  def PCIeCXLDetecedErrors: Int         = 26
}

class TLBusErrorBundle extends Bundle {
  val correctable = Bool()
  val deferred    = Bool()
  val priority    = UInt(2.W)
  val txnType     = UInt(3.W)
  val addrType    = UInt(4.W)
  val addrInfo    = UInt(64.W)
  val info        = UInt(64.W)
  val supplInfo   = UInt(64.W)

  def isNone: Bool = correctable && deferred
  def isCE:   Bool = correctable && !deferred
  def isUED:  Bool = !correctable && deferred
  def isUEC:  Bool = !correctable && !deferred
}

class VendorNImpIdBundle extends Bundle {
  val imp_id    = UInt(32.W)
  val vendor_id = UInt(32.W)
}

class BankInfoBundle extends Bundle {
  val version     = UInt(8.W)
  val zero0       = UInt(32.W)
  val layout      = UInt(2.W)
  val n_err_recs  = UInt(6.W)
  val inst_id     = UInt(16.W)
}

class ValidSummaryBundle extends Bundle {
  val valid_bitmap = UInt(63.W)
  val sv           = Bool()
}

class ControlBundle extends Bundle {
  val custom  = UInt(4.W)
  val zero1   = UInt(10.W)
  val srdp    = Bool()
  val sinv    = Bool()
  val eid     = UInt(16.W)
  val zero0   = UInt(24.W)
  val uecs    = UInt(2.W)
  val ueds    = UInt(2.W)
  val ces     = UInt(2.W)
  val cece    = Bool()
  val eLse    = Bool()
}

class StatusBundle extends Bundle {
  val cec   = UInt(16.W)
  val zero2 = UInt(16.W)
  val ec    = UInt(8.W)
  val rdip  = Bool()
  val zero1 = Bool()
  val ceco  = Bool()
  val scrub = Bool()
  val zero0 = UInt(2.W)
  val tsv   = Bool()
  val siv   = Bool()
  val ait   = UInt(4.W)
  val iv    = Bool()
  val tt    = UInt(3.W)
  val c     = Bool()
  val mo    = Bool()
  val pri   = UInt(2.W)
  val uec   = Bool()
  val ued   = Bool()
  val ce    = Bool()
  val v     = Bool()
}

case class TLErrorRecordBankParams(
  numRecords: Int = 1,
  address:    AddressSet,
)

class TLErrorRecordBank(
  bankIndex: Int,
  params:    TLErrorRecordBankParams,
  freqHz:    Int,
  beatBytes: Int  = 8
)(implicit p: Parameters) extends LazyModule
{
  val regWidth = 64
  val numRecords = params.numRecords
  require(bankIndex >= 0, "Bank index must be a unsigned number!")
  require(log2Up(bankIndex) <= 16, "The width of bank index up to 16!")
  require(numRecords <= 63, "A error bank may include up to 63 error records!")
  require(numRecords >= 1, "A minimal implementation one error record only consumes 128 bytes of address space!")
  val device: SimpleDevice = new SimpleDevice(s"error-record-register-bank-${bankIndex}", Seq("XiangShan", s"riscv,reri,bank${bankIndex}"))
  val node: TLRegisterNode = TLRegisterNode(
    address     = Seq(params.address),
    device      = device,
    beatBytes   = beatBytes,
    undefZero   = true,
    concurrency = 1
  )
  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle() {
      val errors = Flipped(Vec(numRecords, ValidIO(new TLBusErrorBundle)))
      val interrupt = Output(Bool())
      val timestamp = Input(UInt(64.W))
      val tick = Input(Bool())
    })

    // Error header
    val vendorNImpIdReg = RegInit(0.U(regWidth.W))
    val reset_bankInfo = ((BigInt(1) << 56) | // version
                          (BigInt(0) << 22) | // layout
                          (BigInt(numRecords) << 16) | // n_err_recs
                           BigInt(bankIndex)) // inst_id
    val bankInfoReg = RegInit(reset_bankInfo.U(regWidth.W))
    val reset_validSummary = ((((1 << numRecords) - 1) << 1) | // valid_bitmap
                              (if (numRecords > 0) 1 else 0)) // sv
    val validSummaryReg = RegInit(reset_validSummary.U(regWidth.W))
    val validSummaryBundle = validSummaryReg.asTypeOf(new ValidSummaryBundle)

    // Error record register
    val recordEnableVec = validSummaryBundle.valid_bitmap.asBools.map(v => v && validSummaryBundle.sv)

    val controlRegs   = Seq.fill(numRecords)(RegInit(0.U.asTypeOf(new ControlBundle)))
    val statusRegs    = Seq.fill(numRecords)(RegInit(0.U.asTypeOf(new StatusBundle)))
    val addrInfoRegs  = Seq.fill(numRecords)(RegInit(0.U(regWidth.W)))
    val infoRegs      = Seq.fill(numRecords)(RegInit(0.U(regWidth.W)))
    val supplInfoRegs = Seq.fill(numRecords)(RegInit(0.U(regWidth.W)))
    val timestampRegs = Seq.fill(numRecords)(RegInit(0.U(regWidth.W)))

    val setStatusRdip = WireInit(VecInit(Seq.fill(numRecords)(false.B)))
    val clearStatusV  = WireInit(VecInit(Seq.fill(numRecords)(false.B)))

    val freq = RegInit(freqHz.U(64.W))
    val cnt = RegInit(0.U(64.W))
    val nextCnt = cnt + 1.U
    val tick = (nextCnt === freq)
    cnt := Mux(nextCnt < freq, nextCnt, 0.U)

    def vendorNImpIdRegDesc() =
      RegFieldDesc(
        name      = "vendor_n_imp_id",
        desc      = "Vendor and implementation ID",
        group     = Some("vendor_n_imp_id"),
        groupDesc = Some("Vendor and implementation ID"),
        reset     = Some(0)
      )

    def bankInfoRegDesc() =
      RegFieldDesc(
        name      = "bank_info",
        desc      = "Error bank information",
        group     = Some("bank_info"),
        groupDesc = Some("Error bank information"),
        reset     = Some(reset_bankInfo)
      )

    def validSummaryRegDesc() =
      RegFieldDesc(
        name      = "valid_summary",
        desc      = "Summary of Valid Error Records",
        group     = Some("valid_summary"),
        groupDesc = Some("Summary of Valid Error Records"),
        reset     = Some(reset_validSummary)
      )

    def controlRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"control_$i",
        desc      = s"Control Register $i",
        group     = Some("control"),
        groupDesc = Some("Control Register"),
        reset     = Some(0)
      )

    def statusRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"status_$i",
        desc      = s"Status Register $i",
        group     = Some("status"),
        groupDesc = Some("Status Register"),
        reset     = Some(0)
      )

    def addrInfoRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"addr_info_$i",
        desc      = s"Address-or-Information Register $i",
        group     = Some("addr_info"),
        groupDesc = Some("Address-or-Information Register"),
        reset     = Some(0)
      )

    def infoRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"info_$i",
        desc      = s"Information Register $i",
        group     = Some("info"),
        groupDesc = Some("Information Register"),
        reset     = Some(0)
      )

    def supplInfoRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"suppl_info_$i",
        desc      = s"Supplemental Information Register $i",
        group     = Some("suppl_info"),
        groupDesc = Some("Supplemental Information Register"),
        reset     = Some(0)
      )

    def timestampRegDesc(i: Int) =
      RegFieldDesc(
        name      = s"timestamp_$i",
        desc      = s"Timestamp Register $i",
        group     = Some("timestamp"),
        groupDesc = Some("Timestamp Register"),
        reset     = Some(0)
      )

    def vendorNImpIdRegField(x: UInt) =
      RegField.r(regWidth, x, vendorNImpIdRegDesc())

    def bankInfoRegField(x: UInt) =
      RegField.r(regWidth, x, bankInfoRegDesc())

    def validSummaryRegField(x: UInt) =
      RegField.r(regWidth, x, validSummaryRegDesc())

    def controlRegField(x: ControlBundle, i: Int, enable: Bool) =
      RegField(regWidth,
        RegReadFn { valid =>
          val readCtrl = WireInit(x)
          readCtrl.srdp := 0.U
          readCtrl.sinv := 0.U
          (true.B, readCtrl.asUInt)
        },
        RegWriteFn { (valid, data) =>
          val wdata = WireInit(data.asTypeOf(new ControlBundle))
          when (valid && enable) {
            setStatusRdip(i) := wdata.srdp
            clearStatusV(i)  := wdata.sinv
            x := wdata
            x.zero1 := 0.U
            x.zero0 := 0.U
          }
          true.B
        },
        controlRegDesc(i)
      )

    def statusRegField(x: StatusBundle, i: Int, enable: Bool) =
      RegField(regWidth,
        RegReadFn { valid =>
          (true.B, x.asUInt)
        },
        RegWriteFn { (valid, data) =>
          val wdata = WireInit(data.asTypeOf(new StatusBundle))
          wdata.zero2 := 0.U
          wdata.zero1 := 0.U
          wdata.zero0 := 0.U
          when (valid && !x.v && enable) { x := wdata }
          true.B
        },
        statusRegDesc(i)
      )

    def addrInfoRegField(x: UInt, i: Int) =
      RegField(regWidth, x, addrInfoRegDesc(i))

    def infoRegField(x: UInt, i: Int) =
      RegField(regWidth, x, infoRegDesc(i))

    def supplInfoRegField(x: UInt, i: Int) =
      RegField(regWidth, x, supplInfoRegDesc(i))

    def timestampRegField(x: UInt, i: Int) =
      RegField(regWidth, x, timestampRegDesc(i))

    def severity(status: StatusBundle) =
      PriorityMux(Seq(
        status.uec -> 2.U,
        status.ued -> 1.U,
        status.ce  -> 0.U
      ))

    def errorType(ctrl: ControlBundle) = {
      val uec = ctrl.uecs =/= TLErrorRecordConsts.SignalingDisabled.U
      val ued = ctrl.ueds =/= TLErrorRecordConsts.SignalingDisabled.U && !uec
      val ce  = ctrl.ces  =/= TLErrorRecordConsts.SignalingDisabled.U && !uec && !ued
      (uec, ued, ce)
    }

    // Header
    val vendorNImpIdRegFields = Seq(
      0x0 -> Seq(vendorNImpIdRegField(vendorNImpIdReg))
    )
    val bankInfoRegFields = Seq(
      0x8 -> Seq(bankInfoRegField(bankInfoReg))
    )
    val validSummaryRegFields = Seq(
      0x10 -> Seq(validSummaryRegField(validSummaryReg))
    )
    val headerMapping = vendorNImpIdRegFields ++
                        bankInfoRegFields ++
                        validSummaryRegFields
    // Recrods
    val controlRegFields = controlRegs.zipWithIndex.map {
      case (reg, i) =>
        (0x40 + 64 * i) -> Seq(controlRegField(reg, i, recordEnableVec(i)))
    }
    val statusRegFields = statusRegs.zipWithIndex.map {
      case (reg, i) =>
        (0x48 + 64 * i) -> Seq(statusRegField(reg, i, recordEnableVec(i)))
    }
    val addrInfoRegFields = addrInfoRegs.zipWithIndex.map {
      case (reg, i) =>
        (0x50 + 64 * i) -> Seq(addrInfoRegField(reg, i))
    }
    val infoRegFields = infoRegs.zipWithIndex.map {
      case (reg, i) =>
        (0x58 + 64 * i) -> Seq(infoRegField(reg, i))
    }
    val supplInfoRegFields = supplInfoRegs.zipWithIndex.map {
      case (reg, i) =>
        (0x60 + 64 * i) -> Seq(supplInfoRegField(reg, i))
    }
    val timestampRegFields = timestampRegs.zipWithIndex.map {
      case (reg, i) =>
        (0x68 + 64 * i) -> Seq(timestampRegField(reg, i))
    }
    val recordsMapping = controlRegFields ++
                         statusRegFields ++
                         addrInfoRegFields ++
                         infoRegFields ++
                         supplInfoRegFields ++
                         timestampRegFields
    node.regmap((headerMapping ++ recordsMapping):_*)

    io.interrupt := VecInit(statusRegs.map(_.asTypeOf(new StatusBundle).v)).asUInt.orR

    //
    io.errors.zip(controlRegs).zip(statusRegs).zip(addrInfoRegs).zip(infoRegs).zip(supplInfoRegs).zip(timestampRegs).zipWithIndex.foreach {
      case (((((((error_i, control_i), status_i), addr_info_i), info_i), suppl_info_i), timestamp_i), i) =>
        val record_i_valid = recordEnableVec(i)

        // a record
        val enable = control_i.eLse && record_i_valid

        // update status.rdip
        when (enable && (status_i.v || clearStatusV(i)) && setStatusRdip(i)) {
          status_i.rdip := 1.U
        }

        // update status.v
        when (enable && status_i.v && (status_i.rdip || setStatusRdip(i)) && clearStatusV(i)) {
          status_i.v := 0.U
        }

        // update control_i.eid
        when (enable && tick && control_i.eid =/= 0.U) {
          control_i.eid := control_i.eid - 1.U
        }

        // new error
        val ceEnable = (!control_i.cece || status_i.ceco)
        val injValid = enable && RegNext(control_i.eid =/= 0.U) && control_i.eid === 0.U

        val extStatus    = WireInit(0.U.asTypeOf(new StatusBundle))
        extStatus.scrub := error_i.bits.isCE
        extStatus.tsv   := 1.U
        extStatus.siv   := 1.U
        extStatus.ait   := error_i.bits.addrType
        extStatus.iv    := 1.U
        extStatus.tt    := error_i.bits.txnType
        extStatus.c     := error_i.bits.isUEC
        extStatus.pri   := error_i.bits.priority
        extStatus.uec   := error_i.bits.isUEC
        extStatus.ued   := error_i.bits.isUED
        extStatus.ce    := error_i.bits.isCE
        extStatus.v     := error_i.valid && (!error_i.bits.isCE || ceEnable)

        val injStatus = WireInit(0.U.asTypeOf(new StatusBundle))
        val (injIsUEC, injIsUED, injIsCE) = errorType(control_i)
        injStatus.scrub := injIsCE
        injStatus.tsv   := 1.U
        injStatus.siv   := 0.U
        injStatus.ait   := TLErrorRecordConsts.AddrIsNone.U
        injStatus.iv    := 0.U
        injStatus.tt    := TLErrorRecordConsts.TxnUnSpecified.U
        injStatus.c     := injIsUEC
        injStatus.pri   := 0.U // lowest priority
        injStatus.uec   := injIsUEC
        injStatus.ued   := injIsUED
        injStatus.ce    := injIsCE
        injStatus.v     := injValid && (!injIsCE || ceEnable)
        val newStatus = Mux(error_i.valid, extStatus, injStatus)

        when (newStatus.v && enable) {
          val overwrite = WireInit(false.B)
          when (status_i.v) {
            val newStatusSeverity = severity(newStatus)
            val oldStatusSeverity = severity(status_i)
            when (newStatusSeverity > oldStatusSeverity) {
              status_i.mo := 0.U
              overwrite := true.B
            } .elsewhen (newStatusSeverity === oldStatusSeverity) {
              status_i.mo := 1.U
              when (newStatus.pri > status_i.pri) {
                overwrite := true.B
              }
            }
            status_i.rdip := 0.U
            status_i.uec  := status_i.uec | newStatus.uec
            status_i.ued  := status_i.ued | newStatus.ued
            status_i.ce   := status_i.ce  | newStatus.ce
          } .otherwise {
            status_i.rdip := 1.U
            status_i.uec  := newStatus.uec
            status_i.ued  := newStatus.ued & ~newStatus.uec
            status_i.ce   := newStatus.ce  & ~newStatus.ued & ~newStatus.uec
            status_i.mo   := 0.U
            overwrite        := true.B
          }

          when (overwrite) {
            status_i.ec     := newStatus.ec
            status_i.scrub  := newStatus.scrub
            status_i.tsv    := newStatus.tsv
            status_i.ait    := newStatus.ait
            status_i.siv    := newStatus.siv
            status_i.iv     := newStatus.iv
            status_i.tt     := newStatus.tt
            status_i.c      := newStatus.c
            status_i.pri    := newStatus.pri
            status_i.v      := 1.U

            // update info
            when (newStatus.ait =/= TLErrorRecordConsts.AddrIsNone.U) {
              addr_info_i   := error_i.bits.addrInfo
            }
            when (newStatus.iv) {
              info_i        := error_i.bits.info
            }
            when (newStatus.siv) {
              suppl_info_i  := error_i.bits.supplInfo
            }
            when (newStatus.tsv) {
              timestamp_i   := io.timestamp
            }
          }
        }

        // update status_i.cec and status_i.ceco
        when ((injValid || error_i.valid) && control_i.cece && newStatus.ce) {
          val newCEC = status_i.cec +& 1.U
          status_i.cec  := newCEC(newCEC.getWidth - 2, 0)
          status_i.ceco := status_i.ceco | newCEC(newCEC.getWidth - 1)
        }
    }
  }
}

case class TLErrorRecordRegisterInterfaceParams (
  bankList:   Seq[TLErrorRecordBankParams],
  beatBytes:  Int     = 8,
  freqHz:     Int     = 100,
)

class TLErrorRecordRegisterInterface(
  params: TLErrorRecordRegisterInterfaceParams
)(implicit p: Parameters) extends LazyModule {
  require(params.bankList.length >= 1, "A minimal implementation with one error bank!")

  val device: SimpleDevice = new SimpleDevice("error-record-register", Seq("XiangShan,reri"))
  val intNode: IntSourceNode = IntSourceNode(IntSourcePortSimple(resources = device.int))

  val node = TLTempNode()
  val xbar = TLXbar()
  xbar := node

  val banks = params.bankList.zipWithIndex.map { case (bankParams, i) =>
      val mod = LazyModule(new TLErrorRecordBank(
        bankIndex = i,
        params    = bankParams,
        beatBytes = params.beatBytes,
        freqHz    = params.freqHz
      ))
      mod.node := xbar
      mod
  }
  val totalRecords = params.bankList.map(_.numRecords).reduce(_+_)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle() {
      val errors = Flipped(Vec(totalRecords, ValidIO(new TLBusErrorBundle)))
      val interrupt = Output(Bool())
    })

    val timestamp = RegInit(0.U(64.W))
    val freqHz = RegInit(params.freqHz.U(64.W))
    val inc = RegInit(1.U(64.W))
    val cnt = RegInit(0.U(64.W))
    val nextCnt = cnt + 1.U
    val tick = (nextCnt === freqHz)
    cnt := Mux(nextCnt < freqHz, nextCnt, 0.U)

    when (tick) { timestamp := timestamp + inc }
    banks.map(_.module.io.timestamp := timestamp)
    banks.map(_.module.io.tick := tick)

    val (intOut, _) = intNode.out(0)
    io.interrupt := VecInit(banks.map(_.module.io.interrupt)).asUInt.orR
    intOut(0) := io.interrupt

    val errors = io.errors
    banks.map(_.module.io.errors).flatten.zip(errors).foreach {
      case (sink, source) =>
        sink <> source
    }
  }
}