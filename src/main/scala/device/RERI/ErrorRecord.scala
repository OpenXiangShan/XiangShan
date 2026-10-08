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
import xiangshan.backend.fu.NewCSR.CSRModule

// Status bit positions and software-write masks mirror RERI v1.0.
private object RERIStatus {
  final val Valid = 0
  final val CE = 1
  final val UED = 2
  final val UEC = 3
  final val MultipleOccurred = 6
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
    val notification = Output(new RERINotification)
    val valid = Output(Bool())
  })

  // Each record occupies one 64-byte slot after the 64-byte header.
  val recordBase = RERIAddress.HeaderBytes + recordIndex * RERIAddress.RecordBytes

  val controlCsr = Module(new CSRModule(s"RERIControl_${recordParam.name}", new ControlBundle)).setAddr(recordBase + RERIAddress.Control)
  val control = controlCsr.regOut.asUInt

  val statusCsr = Module(new CSRModule(s"RERIStatus_${recordParam.name}", new StatusBundle)).setAddr(recordBase + RERIAddress.Status)
  val status = statusCsr.regOut.asUInt
  // Hardware updates and software writes share status; software commands take priority.
  val statusWrite = WireDefault(false.B)
  val statusWriteData = WireDefault(status)

  val addressInfoCsr = Module(new CSRModule(s"RERIAddressInfo_${recordParam.name}", new AddressInfoBundle)).setAddr(recordBase + RERIAddress.AddressInfo)
  val informationCsr = Module(new CSRModule(s"RERIInformation_${recordParam.name}", new InformationBundle)).setAddr(recordBase + RERIAddress.Information)
  val supplementalInformationCsr = Module(new CSRModule(s"RERISupplementalInformation_${recordParam.name}", new SupplementalInformationBundle)).setAddr(recordBase + RERIAddress.SupplementalInformation)
  val timestampCsr = Module(new CSRModule(s"RERITimestamp_${recordParam.name}", new TimestampBundle)).setAddr(recordBase + RERIAddress.Timestamp)
  val addressInfo = addressInfoCsr.regOut.asUInt
  val information = informationCsr.regOut.asUInt
  val supplementalInformation = supplementalInformationCsr.regOut.asUInt
  val timestamp = timestampCsr.regOut.asUInt
  val eid = RegInit(0.U(16.W))

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
  // Manual rule: an invalid record accepts any class; a valid record accepts a higher
  // severity, or an equal-severity event with a higher priority.
  val canOverwrite = !valid || eventSeverity > oldSeverity ||
    (eventSeverity === oldSeverity && io.hwWrite.priority > status(5, 4))
  val eventCec = Mux(countCE, nextCec, oldCec)
  // New status layout follows status_i: cec, ec, rdip/ceco, metadata,
  // and finally v/ce/ued/uec. The !valid expression sets rdip for a new record.
  val eventStatus = Cat(
    eventCec, 0.U(16.W), recordParam.errorCode.U(8.W), !valid, 0.U(1.W),
    cecOverflow || status(RERIStatus.CorrectedErrorOverflow), io.hwWrite.scrubbed,
    0.U(2.W), true.B, io.hwWrite.supplementalInformationValid && supplInfoEnabled,
    io.hwWrite.addressInfoType, io.hwWrite.informationValid && infoEnabled, io.hwWrite.transactionType,
    io.hwWrite.containable, false.B, io.hwWrite.priority, uec, ued, ce, true.B
  )
  val controlLevel = MuxLookup(eventSeverity, control(3, 2))(Seq(
    SeverityLevel.CE.asUInt -> control(3, 2),
    SeverityLevel.UED.asUInt -> control(5, 4),
    SeverityLevel.UEC.asUInt -> control(7, 6)
  ))
  // srdp/sinv are write-one commands. They are not retained in control_i.
  // When both are written, RERI sets rdip and clears v in the same update.
  val sinv = io.writeData(48)
  val srdp = io.writeData(49)
  // EID counts down even if a hardware event arrives; on expiry it makes v visible.
  val eidExpires = !writeControl && eid === 1.U
  val injectionCompletes = eidExpires && !valid
  val softwareAction = writeControl || (writeStatus && !valid) ||
    ((writeAddress || (writeInformation && infoEnabled) || (writeSupplemental && supplInfoEnabled) || writeTimestamp) && !valid)
  // Auxiliary fields update only when the event actually writes this record.
  val hardwareRecordWrite = !softwareAction && io.hwWrite.valid && control(0) &&
    (!countCE || cecOverflow) && canOverwrite

  addressInfoCsr.w.wen := (writeAddress && !valid) ||
    (hardwareRecordWrite && io.hwWrite.addressInfoType =/= 0.U)
  addressInfoCsr.w.wdata := Mux(writeAddress, io.writeData, io.hwWrite.address)
  informationCsr.w.wen := (writeInformation && !valid && infoEnabled) ||
    (hardwareRecordWrite && io.hwWrite.informationValid)
  informationCsr.w.wdata := Mux(writeInformation, io.writeData, io.hwWrite.information)
  supplementalInformationCsr.w.wen := (writeSupplemental && !valid && supplInfoEnabled) ||
    (hardwareRecordWrite && io.hwWrite.supplementalInformationValid)
  supplementalInformationCsr.w.wdata := Mux(writeSupplemental, io.writeData, io.hwWrite.supplementalInformation)
  // Timestamp comes from the RERI-owned counter, never from the hardware producer.
  timestampCsr.w.wen := (writeTimestamp && !valid) || hardwareRecordWrite
  timestampCsr.w.wdata := Mux(writeTimestamp, io.writeData, io.timestampCounter)

  io.addressMatch := io.addr === controlCsr.addr.U || io.addr === statusCsr.addr.U ||
    io.addr === addressInfoCsr.addr.U || io.addr === informationCsr.addr.U ||
    io.addr === supplementalInformationCsr.addr.U || io.addr === timestampCsr.addr.U
  io.readData := MuxCase(0.U(64.W), Seq(
    (io.addr === controlCsr.addr.U) -> controlCsr.rdata,
    (io.addr === statusCsr.addr.U) -> statusCsr.rdata,
    (io.addr === addressInfoCsr.addr.U) -> addressInfoCsr.rdata,
    (io.addr === informationCsr.addr.U) -> Mux(infoEnabled, informationCsr.rdata, 0.U(64.W)),
    (io.addr === supplementalInformationCsr.addr.U) -> Mux(supplInfoEnabled, supplementalInformationCsr.rdata, 0.U(64.W)),
    (io.addr === timestampCsr.addr.U) -> timestampCsr.rdata
  ))
  io.valid := valid
  io.notification.valid := false.B
  io.notification.severity := eventSeverity
  io.notification.level := controlLevel
  io.notification.record := recordIndex.U

  // Software command writes have priority over a same-cycle hardware event.
  when (writeControl) {
    eid := io.writeData(47, 32)
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
  }.elsewhen (io.hwWrite.valid && control(0)) {
    // CE updates cec without replacing the error record; cec keeps counting while v=0.
    // UED/UEC do not modify cec/ceco according to the RERI manual.
    when (countCE && !cecOverflow) {
      val countedStatus = (status & ~(BigInt(0xffff).U(64.W) << RERIStatus.CorrectedErrorCountLo)) |
        (nextCec << RERIStatus.CorrectedErrorCountLo)
      statusWrite := true.B
      statusWriteData := Mux(injectionCompletes,
        countedStatus | (1.U(64.W) << RERIStatus.Valid) | (1.U(64.W) << RERIStatus.RDIP),
        countedStatus)
    }.elsewhen (canOverwrite) {
      // On replacement, old CE/UED/UEC bits remain sticky; rdip is cleared by eventStatus.
      val stickyClasses = Mux(valid, status & ((1.U(64.W) << RERIStatus.CE) |
        (1.U(64.W) << RERIStatus.UED) | (1.U(64.W) << RERIStatus.UEC)), 0.U(64.W))
      statusWrite := true.B
      statusWriteData := eventStatus | stickyClasses
      io.notification.valid := controlLevel =/= 0.U
    }.elsewhen (eventSeverity === oldSeverity) {
      // Same-severity, non-higher-priority events only set multiple-occurrence (mo).
      statusWrite := true.B
      statusWriteData := status | (1.U(64.W) << RERIStatus.MultipleOccurred)
    }
  }.elsewhen (injectionCompletes) {
    val eidSeverity = RERIStatus.severity(status(RERIStatus.CE), status(RERIStatus.UED), status(RERIStatus.UEC))
    val eidLevel = MuxLookup(eidSeverity, 0.U(2.W))(Seq(
      SeverityLevel.CE.asUInt -> control(3, 2),
      SeverityLevel.UED.asUInt -> control(5, 4),
      SeverityLevel.UEC.asUInt -> control(7, 6)
    ))
    statusWrite := true.B
    statusWriteData := status | (1.U(64.W) << RERIStatus.Valid) | (1.U(64.W) << RERIStatus.RDIP)
    io.notification.valid := eidLevel =/= 0.U
    io.notification.severity := eidSeverity
    io.notification.level := eidLevel
  }
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

/** Standalone block: future integration owns bus adaptation and NMI routing. */
// Standalone bank: a future bus adapter drives the simple register port.
class ErrorRecordInterface(
  interfaceParam: ErrorRecordInterfaceParam = ErrorRecordInterfaceParam()
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

  val header = Module(new ErrorRecordHead(interfaceParam))
  header.io.addr := io.reg.addr
  header.io.recordValid := VecInit(records.map(_.io.valid))
  // The bus sees one flat MMIO space; header and record matches are mutually exclusive.
  io.reg.readData := MuxCase(0.U(64.W),
    (header.io.addressMatch -> header.io.readData) +:
      records.map(record => record.io.addressMatch -> record.io.readData))

  // Notification routing is local; system/NMI integration remains outside this module.
  def route(severity: UInt): RERINotification = {
    val result = Wire(new RERINotification)
    result.valid := false.B
    result.severity := severity
    result.level := 0.U
    result.record := 0.U
    records.foreach { record =>
      when (record.io.notification.valid && record.io.notification.severity === severity) {
        result := record.io.notification
      }
    }
    result
  }
  io.ceNotification := route(SeverityLevel.CE.asUInt)
  io.uedNotification := route(SeverityLevel.UED.asUInt)
  io.uecNotification := route(SeverityLevel.UEC.asUInt)
}
