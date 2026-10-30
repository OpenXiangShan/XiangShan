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
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSError
import xiangshan.backend.fu.NewCSR.CSRModule

// Status bit positions and software-write masks mirror RERI v1.0.
private object RERIStatus {
  final val Valid = 0
  final val CE = 1
  final val UED = 2
  final val UEC = 3
  final val MultipleOccurred = 6
  final val TransactionType = 8
  final val InformationValid = 11
  final val AddressInfoType = 12
  final val SupplementalInformationValid = 16
  final val TimestampValid = 17
  final val Scrub = 20
  final val CorrectedErrorOverflow = 21
  final val RDIP = 23
  final val CorrectedErrorCountLo = 48
  final val CECE = 1
  final val ControlWritableMask: BigInt =
    (BigInt(0xf) << 60) | (BigInt(0xffff) << 32) | (BigInt(3) << 6) |
      (BigInt(3) << 4) | (BigInt(3) << 2) | (BigInt(1) << 1) | BigInt(1)
  final val StatusWritableMask: BigInt =
    (BigInt(0xffff) << 48) | (BigInt(0xff) << 24) | (BigInt(1) << 23) |
      (BigInt(1) << 21) | (BigInt(1) << 20) | ((BigInt(1) << 18) - 1)

  def severity(ce: Bool, ued: Bool, uec: Bool): UInt =
    Mux(uec, SeverityLevel.UEC.asUInt,
      Mux(ued, SeverityLevel.UED.asUInt, Mux(ce, SeverityLevel.CE.asUInt, SeverityLevel.NONE.asUInt)))
}

/** Header registers are CSRBundle-packed RO values derived from interface parameters. */
class ErrorRecordHead(interfaceParam: ErrorRecordInterfaceParam)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(16.W))
    val readData = Output(UInt(64.W))
    val addressMatch = Output(Bool())
    val recordValid = Input(Vec(interfaceParam.numRecordInfo, Bool()))
  })

  // Header fields are RO: CSRModule receives no software write enable.
  implicit val reriParam: ErrorRecordInterfaceParam = interfaceParam
  val vendorImpCsr = Module(new CSRModule("RERIVendorImp",
    new VendorImpBundle(interfaceParam.implementationId))).setAddr(RERIAddress.VendorAndImplementationId)
  vendorImpCsr.w.wen := false.B
  vendorImpCsr.w.wdata := 0.U
  val bankInfoCsr = Module(new CSRModule("RERIBankInfo", new BankInfoBundle)).setAddr(RERIAddress.BankInfo)
  bankInfoCsr.w.wen := false.B
  bankInfoCsr.w.wdata := 0.U
  val validSummary = Wire(new ValidSummaryBundle)
  val validBitmap = if (interfaceParam.numRecordInfo < 63) {
    Cat(0.U((63 - interfaceParam.numRecordInfo).W), io.recordValid.asUInt)
  } else {
    io.recordValid.asUInt
  }
  validSummary.BITMAP := Mux(interfaceParam.svEnable.B, validBitmap, 0.U(63.W))
  validSummary.SV := interfaceParam.svEnable.B

  // MMIO read is selected by the same CSRModule address metadata used for CSR routing.
  io.addressMatch := io.addr === vendorImpCsr.addr.U || io.addr === bankInfoCsr.addr.U ||
    io.addr === RERIAddress.ValidSummary.U
  // Unimplemented header offsets, including reserved/custom space, return zero.
  io.readData := MuxCase(0.U(64.W), Seq(
    (io.addr === vendorImpCsr.addr.U) -> vendorImpCsr.rdata,
    (io.addr === bankInfoCsr.addr.U) -> bankInfoCsr.rdata,
    (io.addr === RERIAddress.ValidSummary.U) -> validSummary.asUInt
  ))
}

// One record owns six CSRModule registers and matches the full MMIO offset against setAddr.
class ErrorRecordInfo(recordParam: ErrorRecordInfoParam, recordIndex: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(16.W))
    val readData = Output(UInt(64.W))
    val addressMatch = Output(Bool())
    val write = Input(Bool())
    val writeData = Input(UInt(64.W))
    val hwWrite = Input(new RERIHardwareWritePort)
    // One common counter gives all records a comparable timestamp domain.
    val timestampCounter = Input(UInt(64.W))
    val ceNotification = Output(new RERINotification)
    val uedNotification = Output(new RERINotification)
    val uecNotification = Output(new RERINotification)
    val valid = Output(Bool())
  })

  // Each record occupies one 64-byte slot after the 64-byte header.
  val recordBase = RERIAddress.HeaderBytes + recordIndex * RERIAddress.RecordBytes

  val controlCsr = Module(new CSRModule(s"RERIControl_${recordParam.name}", new ControlBundle)).setAddr(recordBase + RERIAddress.Control)
  val eid = RegInit(0.U(16.W))
  // EID is a live countdown, so control_i must expose the private counter rather
  // than the originally programmed CSR value.
  val control = (controlCsr.regOut.asUInt & ~(BigInt(0xffff).U(64.W) << 32)) |
    (eid << 32)

  val statusCsr = Module(new CSRModule(s"RERIStatus_${recordParam.name}", new StatusBundle)).setAddr(recordBase + RERIAddress.Status)
  val status = statusCsr.regOut.asUInt
  // Software commands and hardware events share the status-update arbitration below.
  val statusWrite = WireDefault(false.B)
  val statusWriteData = WireDefault(status)

  val addressInfoCsr = Module(new CSRModule(s"RERIAddressInfo_${recordParam.name}", new AddressInfoBundle)).setAddr(recordBase + RERIAddress.AddressInfo)
  val informationCsr = Module(new CSRModule(s"RERIInformation_${recordParam.name}", new InformationBundle)).setAddr(recordBase + RERIAddress.Information)
  val supplementalInformationCsr = Module(new CSRModule(s"RERISupplementalInformation_${recordParam.name}", new SupplementalInformationBundle)).setAddr(recordBase + RERIAddress.SupplementalInformation)
  val timestampCsr = Module(new CSRModule(s"RERITimestamp_${recordParam.name}", new TimestampBundle)).setAddr(recordBase + RERIAddress.Timestamp)

  // A single MMIO request is broadcast to records; only the record owning this address asserts wen.
  // Example: address 0x48 selects status_0, while the other records see write=false.
  val writeControl = io.write && io.addr === controlCsr.addr.U
  val writeStatus = io.write && io.addr === statusCsr.addr.U
  val writeAddress = io.write && io.addr === addressInfoCsr.addr.U
  val writeInformation = io.write && io.addr === informationCsr.addr.U
  val writeSupplemental = io.write && io.addr === supplementalInformationCsr.addr.U
  val writeTimestamp = io.write && io.addr === timestampCsr.addr.U
  controlCsr.w.wen := writeControl
  controlCsr.w.wdata := io.writeData & RERIStatus.ControlWritableMask.U(64.W)

  val valid = status(RERIStatus.Valid)
  // RERI severity is exclusive: UEC wins over UED, and UED wins over CE.
  // If a producer asserts CE and UEC together, only UEC is recorded.
  // These are elaboration-time enables, not control_i bits; disabled storage is absent logically.
  val infoEnabled = recordParam.infoEnable.B
  val supplInfoEnabled = recordParam.supplInfoEnable.B
  val supportedUEC = io.hwWrite.uec && recordParam.supportsUEC.B
  val supportedUED = io.hwWrite.ued && recordParam.supportsUED.B
  val supportedCE = io.hwWrite.ce && recordParam.supportsCE.B
  val uec = supportedUEC
  val ued = supportedUED && !uec
  val ce = supportedCE && !uec && !ued
  val eventSeverity = RERIStatus.severity(ce, ued, uec)
  val oldSeverity = RERIStatus.severity(status(RERIStatus.CE), status(RERIStatus.UED), status(RERIStatus.UEC))
  val countCE = ce && control(RERIStatus.CECE)
  val oldCec = status(63, 48)
  val nextCec = oldCec + 1.U
  val cecOverflow = countCE && oldCec.andR
  // Payload replacement and state accumulation are independent. A lower-severity
  // event still updates classes, RDIP, MO, and CEC even though it cannot replace the
  // currently recorded payload.
  val canOverwrite = !valid || eventSeverity > oldSeverity ||
    (eventSeverity === oldSeverity && io.hwWrite.priority > status(5, 4))
  val eventCec = Mux(countCE, nextCec, oldCec)
  val nextCeco = cecOverflow || status(RERIStatus.CorrectedErrorOverflow)
  val classMask = ((BigInt(1) << RERIStatus.CE) | (BigInt(1) << RERIStatus.UED) |
    (BigInt(1) << RERIStatus.UEC)).U(64.W)
  val rdipMask = (BigInt(1) << RERIStatus.RDIP).U(64.W)
  val moMask = (BigInt(1) << RERIStatus.MultipleOccurred).U(64.W)
  val cecoMask = (BigInt(1) << RERIStatus.CorrectedErrorOverflow).U(64.W)
  val cecMask = (BigInt(0xffff) << RERIStatus.CorrectedErrorCountLo).U(64.W)
  val validMask = (BigInt(1) << RERIStatus.Valid).U(64.W)
  val eventClasses = Mux(ce, (BigInt(1) << RERIStatus.CE).U(64.W), 0.U(64.W)) |
    Mux(ued, (BigInt(1) << RERIStatus.UED).U(64.W), 0.U(64.W)) |
    Mux(uec, (BigInt(1) << RERIStatus.UEC).U(64.W), 0.U(64.W))
  // Listing 1: a second same-severity error sets MO; a higher severity clears it;
  // a lower severity leaves it unchanged.
  val nextMo = Mux(!valid, false.B,
    Mux(eventSeverity > oldSeverity, false.B,
      Mux(eventSeverity === oldSeverity, true.B, status(RERIStatus.MultipleOccurred))))
  val mergedClasses = Mux(valid, status & classMask, 0.U(64.W)) | eventClasses
  val accumulatedState = (status & ~(classMask | rdipMask | moMask | cecMask | cecoMask)) |
    mergedClasses | Mux(!valid, rdipMask, 0.U(64.W)) | Mux(nextMo, moMask, 0.U(64.W)) |
    (eventCec << RERIStatus.CorrectedErrorCountLo) | Mux(nextCeco, cecoMask, 0.U(64.W))

  val overwriteDetailMask = ((BigInt(0xff) << 24) | (BigInt(1) << RERIStatus.Scrub) |
    (BigInt(1) << RERIStatus.TimestampValid) | (BigInt(1) << RERIStatus.SupplementalInformationValid) |
    (BigInt(0xf) << RERIStatus.AddressInfoType) | (BigInt(1) << RERIStatus.InformationValid) |
    (BigInt(7) << RERIStatus.TransactionType) | (BigInt(1) << 7) |
    (BigInt(3) << 4)).U(64.W)
  val eventDetails = (recordParam.errorCode.U(64.W) << 24) |
    Mux(io.hwWrite.scrubbed, (BigInt(1) << RERIStatus.Scrub).U(64.W), 0.U(64.W)) |
    (BigInt(1) << RERIStatus.TimestampValid).U(64.W) |
    Mux(io.hwWrite.supplementalInformationValid && supplInfoEnabled,
      (BigInt(1) << RERIStatus.SupplementalInformationValid).U(64.W), 0.U(64.W)) |
    (io.hwWrite.addressInfoType << RERIStatus.AddressInfoType) |
    Mux(io.hwWrite.informationValid && infoEnabled,
      (BigInt(1) << RERIStatus.InformationValid).U(64.W), 0.U(64.W)) |
    (io.hwWrite.transactionType << RERIStatus.TransactionType) |
    Mux(io.hwWrite.containable, (BigInt(1) << 7).U(64.W), 0.U(64.W)) |
    (io.hwWrite.priority << 4)
  val eventStatus = ((accumulatedState & ~overwriteDetailMask) | eventDetails | validMask)
  // srdp/sinv are write-one commands. They are not retained in control_i.
  // When both are written, RERI sets rdip and clears v in the same update.
  val sinv = io.writeData(48)
  val srdp = io.writeData(49)
  // EID counts down even if a hardware event arrives; on expiry it makes v visible.
  val eidExpires = !writeControl && eid === 1.U
  val injectionCompletes = eidExpires && !valid
  // Detail writes retain platform priority. Control writes update their own
  // CSR/EID fields independently and must not suppress a hardware error.
  val softwareAction =
    (writeAddress || (writeInformation && infoEnabled) || (writeSupplemental && supplInfoEnabled) || writeTimestamp) && !valid
  // Spec 2.4.2: status/control software writes lose to a same-cycle hardware event.
  // In particular, SINV must not clear V from stale RDIP state after a new event
  // has already cleared RDIP and set MO.
  val logHardwareEvent = !softwareAction && io.hwWrite.valid && control(0)
  val hardwareRecordWrite = logHardwareEvent && canOverwrite

  addressInfoCsr.w.wen := (writeAddress && !valid) ||
    (hardwareRecordWrite && io.hwWrite.addressInfoType =/= 0.U)
  addressInfoCsr.w.wdata := Mux(writeAddress && !valid, io.writeData, io.hwWrite.address)
  informationCsr.w.wen := (writeInformation && !valid && infoEnabled) ||
    (hardwareRecordWrite && io.hwWrite.informationValid)
  informationCsr.w.wdata := Mux(writeInformation && !valid && infoEnabled, io.writeData, io.hwWrite.information)
  supplementalInformationCsr.w.wen := (writeSupplemental && !valid && supplInfoEnabled) ||
    (hardwareRecordWrite && io.hwWrite.supplementalInformationValid)
  supplementalInformationCsr.w.wdata := Mux(writeSupplemental && !valid && supplInfoEnabled, io.writeData, io.hwWrite.supplementalInformation)
  // Timestamp comes from the RERI-owned counter, never from the hardware producer.
  timestampCsr.w.wen := (writeTimestamp && !valid) || hardwareRecordWrite
  timestampCsr.w.wdata := Mux(writeTimestamp && !valid, io.writeData, io.timestampCounter)

  io.addressMatch := io.addr === controlCsr.addr.U || io.addr === statusCsr.addr.U ||
    io.addr === addressInfoCsr.addr.U || io.addr === informationCsr.addr.U ||
    io.addr === supplementalInformationCsr.addr.U || io.addr === timestampCsr.addr.U
  io.readData := MuxCase(0.U(64.W), Seq(
    (io.addr === controlCsr.addr.U) -> control,
    (io.addr === statusCsr.addr.U) -> statusCsr.rdata,
    (io.addr === addressInfoCsr.addr.U) -> addressInfoCsr.rdata,
    (io.addr === informationCsr.addr.U) -> Mux(infoEnabled, informationCsr.rdata, 0.U(64.W)),
    (io.addr === supplementalInformationCsr.addr.U) -> Mux(supplInfoEnabled, supplementalInformationCsr.rdata, 0.U(64.W)),
    (io.addr === timestampCsr.addr.U) -> timestampCsr.rdata
  ))
  io.valid := valid

  when (writeControl) {
    eid := io.writeData(47, 32)
  }

  when (logHardwareEvent) {
    statusWrite := true.B
    statusWriteData := Mux(canOverwrite, eventStatus, accumulatedState)
  }.elsewhen (writeControl) {
    when (sinv && srdp) {
      statusWrite := true.B
      statusWriteData := (status | (1.U(64.W) << RERIStatus.RDIP)) & ~(1.U(64.W) << RERIStatus.Valid)
    }.elsewhen (sinv && status(RERIStatus.RDIP)) {
      statusWrite := true.B
      statusWriteData := status & ~(1.U(64.W) << RERIStatus.Valid)
    }.elsewhen (srdp) {
      statusWrite := true.B
      statusWriteData := status | (1.U(64.W) << RERIStatus.RDIP)
    }
  }.elsewhen (writeStatus && !valid) {
    statusWrite := true.B
    statusWriteData := io.writeData & RERIStatus.StatusWritableMask.U(64.W)
  }.elsewhen ((writeAddress || (writeInformation && infoEnabled) || (writeSupplemental && supplInfoEnabled) || writeTimestamp) && !valid) {
    statusWrite := false.B
  }.elsewhen (injectionCompletes) {
    statusWrite := true.B
    statusWriteData := status | (1.U(64.W) << RERIStatus.Valid) | (1.U(64.W) << RERIStatus.RDIP)
  }

  // Independent severity channels keep an enabled UED or CE visible even with
  // a sticky UEC whose notification is disabled. An informational record also
  // uses CES, as required by the RERI signaling rules.
  val hasCE = status(RERIStatus.CE)
  val hasUED = status(RERIStatus.UED)
  val hasUEC = status(RERIStatus.UEC)
  val informationalEvent = io.hwWrite.valid && !io.hwWrite.ce && !io.hwWrite.ued && !io.hwWrite.uec
  // A new informational event uses CES even when the sticky record already has a
  // higher class. Register it once so the event is visible after the write cycle.
  val informationalNotification = RegNext(logHardwareEvent && informationalEvent, false.B)
  val informational = !hasCE && !hasUED && !hasUEC
  val ceEnabled = (informational || (hasCE && (!control(RERIStatus.CECE) ||
    status(RERIStatus.CorrectedErrorOverflow)))) && control(3, 2) =/= 0.U
  val uedEnabled = hasUED && control(5, 4) =/= 0.U
  val uecEnabled = hasUEC && control(7, 6) =/= 0.U
  // Severity channels are independent: each sticky class can hold its own level.
  io.ceNotification.valid := valid &&
    (ceEnabled || (informationalNotification && control(3, 2) =/= 0.U))
  io.ceNotification.severity := SeverityLevel.CE.asUInt
  io.ceNotification.level := control(3, 2)
  io.ceNotification.record := recordIndex.U
  io.uedNotification.valid := valid && uedEnabled
  io.uedNotification.severity := SeverityLevel.UED.asUInt
  io.uedNotification.level := control(5, 4)
  io.uedNotification.record := recordIndex.U
  io.uecNotification.valid := valid && uecEnabled
  io.uecNotification.severity := SeverityLevel.UEC.asUInt
  io.uecNotification.level := control(7, 6)
  io.uecNotification.record := recordIndex.U
  // When optional storage is disabled, status.iv/siv must remain zero as well.
  val statusWriteFiltered = statusWriteData &
    Mux(infoEnabled, Fill(64, true.B), ~(1.U(64.W) << 11)) &
    Mux(supplInfoEnabled, Fill(64, true.B), ~(1.U(64.W) << 16))
  statusCsr.w.wen := statusWrite
  statusCsr.w.wdata := statusWriteFiltered

  when (!writeControl && eid =/= 0.U) {
    eid := eid - 1.U
  }
}

/** RERI register bank; bus adaptation and interrupt routing are provided by the adapter. */
class ErrorRecordInterface(
  interfaceParam: ErrorRecordInterfaceParam
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val reg = new RERIRegisterPort
    val hwWrite = Input(Vec(interfaceParam.numRecordInfo, new RERIHardwareWritePort))
    val ceNotification = Output(new RERINotification)
    val uedNotification = Output(new RERINotification)
    val uecNotification = Output(new RERINotification)
  })

  val timestampCounter = RegInit(0.U(64.W))
  timestampCounter := timestampCounter + 1.U

  // Every record sees the full address and independently matches its CSRModule addresses.
  val records = interfaceParam.recordParams.zipWithIndex.map { case (recordParam, index) =>
    val record = Module(new ErrorRecordInfo(recordParam, index))
    // Connect the common bus request to every record; its local CSR addresses decide acceptance.
    record.io.addr := io.reg.addr
    record.io.write := io.reg.write
    record.io.writeData := io.reg.writeData
    // hwWrite(index) is private to this record and is never arbitrated with another record.
    record.io.hwWrite := io.hwWrite(index)
    record.io.timestampCounter := timestampCounter
    record
  }

  // Software must only access the header and the configured record slots.
  val registerSpaceEnd = (RERIAddress.HeaderBytes + interfaceParam.numRecordInfo * RERIAddress.RecordBytes).U
  XSError(io.reg.addr >= registerSpaceEnd, "RERI register address exceeds the configured register space")

  val header = Module(new ErrorRecordHead(interfaceParam))
  header.io.addr := io.reg.addr
  header.io.recordValid := VecInit(records.map(_.io.valid))
  // The bus sees one flat MMIO space; header and record matches are mutually exclusive.
  io.reg.readData := MuxCase(0.U(64.W),
    (header.io.addressMatch -> header.io.readData) +:
      records.map(record => record.io.addressMatch -> record.io.readData))

  // Notification routing is local; system/NMI integration remains outside this module.
  def route(getNotification: ErrorRecordInfo => RERINotification, severity: UInt): RERINotification = {
    val result = Wire(new RERINotification)
    result.valid := false.B
    result.severity := severity
    result.level := 0.U
    result.record := 0.U
    records.foreach { record =>
      when (getNotification(record).valid) {
        result := getNotification(record)
      }
    }
    result
  }
  io.ceNotification := route(_.io.ceNotification, SeverityLevel.CE.asUInt)
  io.uedNotification := route(_.io.uedNotification, SeverityLevel.UED.asUInt)
  io.uecNotification := route(_.io.uecNotification, SeverityLevel.UEC.asUInt)
}
