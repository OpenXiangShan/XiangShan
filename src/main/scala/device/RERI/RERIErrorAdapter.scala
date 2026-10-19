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
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegReadFn, RegWriteFn}
import freechips.rocketchip.tilelink.TLRegisterNode
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSCoreParamsKey

/**
  * Converts cache and uncache error events to RERI hardware records.
  */
class RERIErrorAdapter(implicit p: Parameters) extends Module {
  private val reriParam = p(XSCoreParamsKey).ErrorRecordInterfaceParam
  val io = IO(new Bundle {
    val errors = Input(new XSRERIErrors)
    val reg = new RERIRegisterPort
    val nmi = Output(Bool())
    val extern_interrupt = Output(Bool())
  })

  val reri = Module(new ErrorRecordInterface(reriParam))
  reri.io.reg.addr := io.reg.addr
  reri.io.reg.write := io.reg.write
  reri.io.reg.writeData := io.reg.writeData
  io.reg.readData := reri.io.reg.readData

  for (i <- 0 until reriParam.numRecordInfo) {
    reri.io.hwWrite(i) := 0.U.asTypeOf(reri.io.hwWrite(i))
  }

  def convert(src: RERIErrorInfo, recordName: String, selected: Bool): Unit = {
    val record = reriParam.recordParams.indexWhere(_.name == recordName)
    if (record >= 0) {
      val event = reri.io.hwWrite(record)
      event.valid := src.errorPaddr.valid && selected && src.severity =/= SeverityLevel.NONE
      event.ce := src.severity === SeverityLevel.CE && event.valid
      event.ued := src.severity === SeverityLevel.UED && event.valid
      event.uec := src.severity === SeverityLevel.UEC && event.valid
      event.priority := 0.U
      event.containable := false.B
      event.transactionType := 0.U
      // Cache and uncache events always carry a physical address.
      event.addressInfoType := 1.U
      event.address := src.errorPaddr.bits
      event.informationValid := false.B
      event.information := 0.U
      event.supplementalInformationValid := false.B
      event.supplementalInformation := 0.U
      event.scrubbed := false.B
    }
  }

  // Record names, indices, and source channels all come from the platform
  // parameter.  This keeps the hardware mapping stable when records are
  // reordered or a source is disabled in a configuration.
  convert(io.errors.dcache, "dcache-data", io.errors.dcache.errorSource === ErrorSource.dataError)
  convert(io.errors.dcache, "dcache-tag", io.errors.dcache.errorSource === ErrorSource.addressError)
  convert(io.errors.icache, "icache-data", io.errors.icache.errorSource === ErrorSource.dataError)
  convert(io.errors.icache, "icache-tag", io.errors.icache.errorSource === ErrorSource.addressError)
  convert(io.errors.l2, "l2cache-data", io.errors.l2.errorSource === ErrorSource.dataError)
  convert(io.errors.l2, "l2cache-tag", io.errors.l2.errorSource === ErrorSource.addressError)
  convert(io.errors.uncache, "uncache-denied", io.errors.uncache.errorSource === ErrorSource.addressError)
  convert(io.errors.uncache, "uncache-corrupt", io.errors.uncache.errorSource === ErrorSource.dataError)

  // The XiangShan platform maps UEC to NMI and UED to the legacy
  // UED is exported to the PLIC external-interrupt path. CE remains software-visible through RERI.
  io.nmi := reri.io.uecNotification.valid
  io.extern_interrupt := reri.io.uedNotification.valid
}

/** TileLink MMIO shell for the RERI bank.
  *
  * The register implementation continues to own all field semantics. This
  * wrapper only converts one 64-bit TL register access into its simple offset
  * port.
  */
class TLRERIErrorAdapter(base: BigInt, size: Int = 4096)(implicit p: Parameters) extends LazyModule {
  require(size >= 4096 && (size & (size - 1)) == 0)
  private val reriParam = p(XSCoreParamsKey).ErrorRecordInterfaceParam

  private val device = new SimpleDevice("riscv-error-records", Seq("riscv,reri-1.0"))
  val node = TLRegisterNode(
    address = Seq(AddressSet(base, size - 1)),
    device = device,
    beatBytes = 8
  )

  class TLRERIErrorAdapterImp extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val errors = Input(new XSRERIErrors)
      val nmi = Output(Bool())
      val extern_interrupt = Output(Bool())
    })

    val adapter = Module(new RERIErrorAdapter)
    adapter.io.errors := io.errors
    io.nmi := adapter.io.nmi
    io.extern_interrupt := adapter.io.extern_interrupt

    val regAddr = WireDefault(0.U(16.W))
    val regWrite = WireDefault(false.B)
    val regWriteData = WireDefault(0.U(64.W))
    adapter.io.reg.addr := regAddr
    adapter.io.reg.write := regWrite
    adapter.io.reg.writeData := regWriteData

    def mappedField(offset: Int): RegField = RegField(
      64,
      RegReadFn { ready =>
        when(ready) { regAddr := offset.U }
        (true.B, adapter.io.reg.readData)
      },
      RegWriteFn { (valid, data) =>
        when(valid) {
          regAddr := offset.U
          regWrite := true.B
          regWriteData := data
        }
        true.B
      },
      RegFieldDesc(f"reri_0x$offset%03x", "RERI v1.0 register", volatile = true)
    )

    val headerOffsets = Seq(
      RERIAddress.VendorAndImplementationId,
      RERIAddress.BankInfo,
      RERIAddress.ValidSummary
    )
    val recordOffsets = (0 until reriParam.numRecordInfo).flatMap { record =>
      Seq(
        RERIAddress.Control,
        RERIAddress.Status,
        RERIAddress.AddressInfo,
        RERIAddress.Information,
        RERIAddress.SupplementalInformation,
        RERIAddress.Timestamp
      ).map(RERIAddress.HeaderBytes + record * RERIAddress.RecordBytes + _)
    }
    node.regmap((headerOffsets ++ recordOffsets).map(offset => offset -> Seq(mappedField(offset))): _*)
  }

  lazy val module = new TLRERIErrorAdapterImp
}
