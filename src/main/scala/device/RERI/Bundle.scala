/***************************************************************************************
 * Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          https://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 ***************************************************************************************/

package device.RERI

import chisel3._
import xiangshan.backend.fu.NewCSR.{CSRBundle, CSREnum, CSREnumType, ROApply}
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRWARLField => WARL}
import xiangshan.backend.fu.NewCSR.CSRFunc.wNoFilter
import xiangshan.backend.fu.NewCSR.{MvidBankField, MvidOffsetField}

// RERI is exposed as a byte-offset MMIO bank; these offsets are also passed to CSRModule.setAddr.
object RERIAddress {
  final val BaseAddress = 0x38010000L
  final val HeaderBytes = 0x40
  final val RecordBytes = 0x40
  final val VendorAndImplementationId = 0x00
  final val BankInfo = 0x08
  final val ValidSummary = 0x10
  final val Custom = 0x38
  final val Control = 0x00
  final val Status = 0x08
  final val AddressInfo = 0x10
  final val Information = 0x18
  final val SupplementalInformation = 0x20
  final val Timestamp = 0x28
}

object RERIImplementationIdField extends CSREnum with ROApply {
  val RESET = Value(0.U)
  def withParameter(msb: Int, lsb: Int, reset: BigInt): CSREnumType = {
    val field = apply(msb, lsb)
    if (reset == 0) field.withReset(RESET) else field.withNonEnumReset(reset.U)
  }
}

object ReriVersionField extends CSREnum with ROApply {
  (0 to 255).foreach(i => Value(i.U))
}

object ReriLayoutField extends CSREnum with ROApply {
  (0 to 3).foreach(i => Value(i.U))
}

object ReriRecordCountField extends CSREnum with ROApply {
  (1 to 63).foreach(i => Value(i.U))
}

object RERIInstanceIdField extends CSREnum with ROApply {
  val RESET = Value(0.U)
  def withParameter(msb: Int, lsb: Int, reset: BigInt): CSREnumType = {
    val field = apply(msb, lsb)
    if (reset == 0) field.withReset(RESET) else field.withNonEnumReset(reset.U)
  }
}

// Keep the RERI vendor field identical to the CSR Mvendorid reset encoding.
object RERIVendorId {
  final val Reset: BigInt = (MvidBankField.BANK.litValue << 7) | MvidOffsetField.OFFSET.litValue
}

object ErrorRecordType extends ChiselEnum {
  val None = Value /* 0: None */
  val OtherUnspecError = Value /* 1: Other unspecified error occurred */
  val CorruptDataAccess = Value /* 2: Corrupted data access (e.g., attempt to consume poisoned data) error */
  val CacheBlockDataError = Value /* 3: Cache block data (e.g., ECC error on cache data) error */
  val CacheScrubDetectError = Value /* 4: Cache scrubbing detected (e.g., ECC error on cache data) error */
  val CacheAddressOrStateError = Value /* 5: Cache address/control state (e.g., parity error tag or state) error */
  val CacheUnspecError = Value /* 6: Cache unspecified error */
  val SnoopFilterOrDirectoryOrStateError = Value /* 7: Snoop-filter/directory address/control state (e.g., ECC error on tag or state) error */
  val SnoopFilterOrDirectoryUnspecError = Value /* 8: Snoop-filter/directory unspecified error */
  val TLBOrPTWCacheDataError = Value /* 9: TLB/Page-walk cache data (e.g., ECC error on TLB data) error */
  val TLBOrPTWCacheAddressError = Value /* 10: TLB/Page-walk cache address/control state (e.g., ECC error on TLB tag) error */
  val TLBOrPTWCacheUnspecError = Value /* 11: TLB/Page-walk cache unspecified error */
  val HartStateError = Value /* 12: Hart state error (e.g., ECC error on CSRs or x/f/v registers) */
  val InterruptControllerStateError = Value /* 13: Interrupt controller state (e.g., ECC error on interrupt pending/enable state) error */
  val InterconnectDataError = Value /* 14: Interconnect data (e.g., ECC error on data bus) error */
  val InterconnectOther = Value /* 15: Interconnect other (e.g., parity error on address bus) error */
  val InternalWatchdogError = Value /* 16: Internal watchdog error */
  val InternalUnitError = Value /* 17: Internal datapath, memory, or execution units error (e.g, ALU datapath parity) */
  val SystemMemoryCommandOrAddressError = Value /* 18: System memory command/address bus error */
  val SystemMemoryUnspecError = Value /* 19: System memory unspecified error */
  val SystemMemoryDataError = Value /* 20: System memory data (e.g., ECC error in SDRAM or HBM) error */
  val SystemMemoryScrubDetectError = Value /* 21: System Memory scrubbing detected error */
  val ProtocolIlegalIOError = Value /* 22: Protocol Error - illegal input/output error */
  val ProtocolIllegalStateError = Value /* 23: Protocol Error - illegal/unexpected state error */
  val ProtocolTimeoutError = Value /* 24: Protocol Error - timeout error */
  val SystemInternalControllerError = Value /* 25: System internal controller (power management, security, etc.) error */
  val DeferredErrorPassUnSupport = Value /* 26: Deferred error pass-through (e.g., forwarding poisoned data) not supported*/
  val PCIeOrCXLError = Value /* 27: PCIe/CXL detected (e.g., logged into PCIe AER, CXL.mem error log, etc.) errors */
  /* Reserve */

}

// Static capabilities of one record; unsupported severity classes are hardwired off.
case class ErrorRecordInfoParam(
  name: String,
  recordType: ErrorRecordType.Type,
  supportsCE: Boolean = true,
  supportsUED: Boolean = true,
  supportsUEC: Boolean = true,
  // Optional RERI payload storage; disabled means read-zero and write-ignore.
  infoEnable: Boolean = false,
  supplInfoEnable: Boolean = false
) {
  def errorCode: Int = recordType.litValue.toInt
}

case class ErrorRecordInterfaceParam(
  layout: Int = 0,
  version: Int = 1,
  svEnable: Boolean = true,
  recordParams: Seq[ErrorRecordInfoParam] = ErrorRecordInterfaceParam.defaultRecordParams,
  implementationId: Int = 0,
  instanceId: Int = 0
) {
  def numRecordInfo: Int = recordParams.size
  require(numRecordInfo >= 1 && numRecordInfo <= 63)
  require(layout >= 0 && layout < 4)
  require(version >= 0 && version < 256)
}

// Elaboration-time parameters; runtime state lives in CSRModule instances.
object ErrorRecordInterfaceParam {
  val defaultRecordParams: Seq[ErrorRecordInfoParam] = Seq(
    ErrorRecordInfoParam("dcache-data", ErrorRecordType.CacheBlockDataError),
    ErrorRecordInfoParam("dcache-tag", ErrorRecordType.CacheAddressOrStateError),
    ErrorRecordInfoParam("icache-data", ErrorRecordType.CacheBlockDataError),
    ErrorRecordInfoParam("icache-tag", ErrorRecordType.CacheAddressOrStateError),
    ErrorRecordInfoParam("l2cache-data", ErrorRecordType.CacheBlockDataError),
    ErrorRecordInfoParam("l2cache-tag", ErrorRecordType.CacheAddressOrStateError)
  )
}

class VendorImpBundle(implementationId: Int = 0) extends CSRBundle {
  val IMPLEMENTATION_ID = RERIImplementationIdField.withParameter(63, 32, implementationId).withDescription("Vendor implementation identifier.")
  val VENDOR_BANK = MvidBankField(31, 7).withReset(MvidBankField.BANK).withDescription("JEDEC manufacturer bank number.")
  val VENDOR_OFFSET = MvidOffsetField(6, 0).withReset(MvidOffsetField.OFFSET).withDescription("JEDEC manufacturer offset within the bank.")
}

class BankInfoBundle(implicit param: ErrorRecordInterfaceParam) extends CSRBundle {
  val VERSION = ReriVersionField(63, 56).withReset(param.version.U).withDescription("RERI register-layout version.")
  val LAYOUT = ReriLayoutField(23, 22).withReset(param.layout.U).withDescription("Error-bank layout identifier.")
  val N_ERR_RECS = ReriRecordCountField(21, 16).withReset(param.numRecordInfo.U).withDescription("Implemented error-record count.")
  val INST_ID = RERIInstanceIdField.withParameter(15, 0, param.instanceId).withDescription("Vendor-defined component instance identifier.")
}

class ValidSummaryBundle(implicit param: ErrorRecordInterfaceParam) extends CSRBundle {
  val BITMAP = RO(63, 1).withReset(0.U).withDescription("Status.V bitmap when SV is set.")
  val SV = RO(0).withReset(param.svEnable.B).withDescription("Valid-summary availability.")
}

class ControlBundle extends CSRBundle {
  val CUSTOM = WARL(63, 60, wNoFilter).withReset(0.U).withDescription("Vendor-defined control bits.")
  val SRDP = WARL(49, wNoFilter).withReset(false.B).withDescription("Write-one RDIP command.")
  val SINV = WARL(48, wNoFilter).withReset(false.B).withDescription("Write-one invalidate command.")
  val EID = WARL(47, 32, wNoFilter).withReset(0.U).withDescription("Error-injection delay counter.")
  val UECS = WARL(7, 6, wNoFilter).withReset(0.U).withDescription("UEC notification level.")
  val UEDS = WARL(5, 4, wNoFilter).withReset(0.U).withDescription("UED notification level.")
  val CES = WARL(3, 2, wNoFilter).withReset(0.U).withDescription("CE notification level.")
  val CECE = WARL(1, wNoFilter).withReset(false.B).withDescription("Corrected-error counter enable.")
  val ELSE = WARL(0, wNoFilter).withReset(true.B).withDescription("Logging and signaling enable.")
}

class StatusBundle extends CSRBundle {
  val CEC = WARL(63, 48, wNoFilter).withReset(0.U).withDescription("Corrected-error count.")
  val EC = WARL(31, 24, wNoFilter).withReset(0.U).withDescription("Error code.")
  val RDIP = WARL(23, wNoFilter).withReset(false.B).withDescription("Record-data-pending indication.")
  val CECO = WARL(21, wNoFilter).withReset(false.B).withDescription("Corrected-error counter overflow.")
  val SCRUB = WARL(20, wNoFilter).withReset(false.B).withDescription("Corrected data was scrubbed.")
  val TSV = WARL(17, wNoFilter).withReset(false.B).withDescription("Timestamp valid.")
  val SIV = WARL(16, wNoFilter).withReset(false.B).withDescription("Supplemental information valid.")
  val AIT = WARL(15, 12, wNoFilter).withReset(0.U).withDescription("Address-information type.")
  val IV = WARL(11, wNoFilter).withReset(false.B).withDescription("Information valid.")
  val TT = WARL(10, 8, wNoFilter).withReset(0.U).withDescription("Transaction type.")
  val C = WARL(7, wNoFilter).withReset(false.B).withDescription("UEC containment indication.")
  val MO = WARL(6, wNoFilter).withReset(false.B).withDescription("Multiple occurrence indication.")
  val PRI = WARL(5, 4, wNoFilter).withReset(0.U).withDescription("Error priority.")
  val UEC = WARL(3, wNoFilter).withReset(false.B).withDescription("Uncorrected unrecoverable error.")
  val UED = WARL(2, wNoFilter).withReset(false.B).withDescription("Uncorrected deferred error.")
  val CE = WARL(1, wNoFilter).withReset(false.B).withDescription("Corrected error.")
  val V = WARL(0, wNoFilter).withReset(false.B).withDescription("Error-record valid indication.")
}

class AddressInfoBundle extends CSRBundle {
  val ADDR_INFO = WARL(63, 0, wNoFilter).withReset(0.U).withDescription("Address or component-specific error information.")
}

class InformationBundle extends CSRBundle {
  val INFO = WARL(63, 0, wNoFilter).withReset(0.U).withDescription("Additional error information.")
}

class SupplementalInformationBundle extends CSRBundle {
  val SUPPL_INFO = WARL(63, 0, wNoFilter).withReset(0.U).withDescription("Supplemental error information.")
}

class TimestampBundle extends CSRBundle {
  val TIMESTAMP = WARL(63, 0, wNoFilter).withReset(0.U).withDescription("Error timestamp.")
}

// The bus adapter supplies a byte offset within the RERI bank; no TL/AXI type is imposed here.
class RERIRegisterPort extends Bundle {
  val addr = Input(UInt(16.W))
  val readData = Output(UInt(64.W))
  val write = Input(Bool())
  val writeData = Input(UInt(64.W))
}

// Each hardware producer has a dedicated per-record write port.
class RERIErrorEvent extends Bundle {
  val valid = Bool()
  val ce = Bool()
  val ued = Bool()
  val uec = Bool()
  val priority = UInt(2.W)
  val containable = Bool()
  val transactionType = UInt(3.W)
  val addressInfoType = UInt(4.W)
  val address = UInt(64.W)
  val informationValid = Bool()
  val information = UInt(64.W)
  val supplementalInformationValid = Bool()
  val supplementalInformation = UInt(64.W)
  val scrubbed = Bool()
}

// RERIHardwareWritePort deliberately excludes timestamp: RERI supplies it internally.
class RERIHardwareWritePort extends RERIErrorEvent

object SeverityLevel extends ChiselEnum {
  val NONE = Value
  val CE = Value
  val UED = Value
  val UEC = Value
}

class RERINotification extends Bundle {
  val valid = Bool()
  val severity = UInt(SeverityLevel.getWidth.W)
  val level = UInt(2.W)
  val record = UInt(6.W)
}
