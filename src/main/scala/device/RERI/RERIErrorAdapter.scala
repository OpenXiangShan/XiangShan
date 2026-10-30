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
import chisel3.util.Cat
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
      // Cache and uncache events always carry a physical address.
      event.addressInfoType := 1.U
      event.address := src.errorPaddr.bits
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

  // Records hold these levels while status.v is set. UEC drives NMI; UED and CE
  // share the platform's maskable external RAS interrupt. This preserves a real
  // sink for CES and CECO instead of accepting control bits with no effect.
  io.nmi := reri.io.uecNotification.valid
  io.extern_interrupt := reri.io.uedNotification.valid || reri.io.ceNotification.valid
}

/** TileLink MMIO shell for the RERI bank.
  *
  * The register implementation continues to own all field semantics. This
  * wrapper converts aligned 32-bit or 64-bit TL accesses into its simple
  * offset port.
  */
class TLRERIErrorAdapter(base: BigInt, size: Int = 4096)(implicit p: Parameters) extends LazyModule {
  require(size >= 4096 && (size & (size - 1)) == 0)
  private val reriParam = p(XSCoreParamsKey).ErrorRecordInterfaceParam

  val device = new SimpleDevice("riscv-error-records", Seq("riscv,reri-1.0"))
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

    def mappedFields(offset: Int): Seq[RegField] = {
      val writeByte = WireDefault(VecInit(Seq.fill(8)(false.B)))
      val byteData = WireDefault(VecInit(Seq.fill(8)(0.U(8.W))))
      val fields = (0 until 8).map { byte =>
        RegField(
          8,
          RegReadFn { ready =>
            when(ready) { regAddr := offset.U }
            (true.B, adapter.io.reg.readData(8 * byte + 7, 8 * byte))
          },
          RegWriteFn { (valid, data) =>
            writeByte(byte) := valid
            byteData(byte) := data
            true.B
          },
          RegFieldDesc(f"reri_0x${offset + byte}%03x", "RERI v1.0 register byte", volatile = true)
        )
      }
      when(writeByte.asUInt.orR) {
        regAddr := offset.U
        regWrite := true.B
        // Merge selected byte fields once so RERI still sees one 64-bit write.
        // Unselected bytes retain the current register value and semantics.
        regWriteData := Cat((0 until 8).reverse.map { byte =>
          Mux(writeByte(byte), byteData(byte), adapter.io.reg.readData(8 * byte + 7, 8 * byte))
        })
      }
      fields
    }

    // Keep the complete header aperture decodable. Reserved offsets return
    // zero and ignore writes instead of leaving a request without a D response.
    val headerOffsets = (0 until RERIAddress.HeaderBytes.toInt by 8)
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
    node.regmap((headerOffsets ++ recordOffsets).map(offset => offset -> mappedFields(offset)): _*)
  }

  lazy val module = new TLRERIErrorAdapterImp
}
