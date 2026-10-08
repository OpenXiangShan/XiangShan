package device.RERI

import chisel3._
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegReadFn, RegWriteFn}
import freechips.rocketchip.tilelink.TLRegisterNode
import org.chipsalliance.cde.config.Parameters
import xiangshan.{RERIErrorInfo, XSRERIErrors}

/**
  * Converts cache and uncache error events to RERI hardware records.
  */
class RERIErrorAdapter(implicit p: Parameters) extends Module {
  private val reriParam = ErrorRecordInterfaceParam.defaultRecordParams
  val io = IO(new Bundle {
    val errors = Input(new XSRERIErrors)
    val reg = new RERIRegisterPort
    val nmi = Output(Bool())
  })

  val reri = Module(new ErrorRecordInterface(ErrorRecordInterfaceParam(recordParams = reriParam)))
  reri.io.reg.addr := io.reg.addr
  reri.io.reg.write := io.reg.write
  reri.io.reg.writeData := io.reg.writeData
  io.reg.readData := reri.io.reg.readData

  for (i <- 0 until reriParam.size) {
    reri.io.hwWrite(i) := 0.U.asTypeOf(reri.io.hwWrite(i))
  }

  def convert(src: RERIErrorInfo, record: Int, selected: Bool): Unit = {
    val event = reri.io.hwWrite(record)
    event.valid := src.ecc_error.valid && selected
    event.ce := src.ce && src.ecc_error.valid && selected
    event.uec := src.uec && src.ecc_error.valid && selected
    event.ued := false.B
    event.priority := 0.U
    event.containable := false.B
    event.transactionType := 0.U
    // Cache and uncache events always carry a physical address.
    event.addressInfoType := 1.U
    event.address := src.ecc_error.bits
    event.informationValid := false.B
    event.information := 0.U
    event.supplementalInformationValid := false.B
    event.supplementalInformation := 0.U
    event.scrubbed := false.B
  }

  // Default record layout: dcache-data/tag, icache-data/tag, l2-data/tag,
  // followed by the independent uncache-data record.
  convert(io.errors.dcache, 0, io.errors.dcache.data)
  convert(io.errors.dcache, 1, io.errors.dcache.tag)
  convert(io.errors.icache, 2, io.errors.icache.data)
  convert(io.errors.icache, 3, io.errors.icache.tag)
  convert(io.errors.l2, 4, io.errors.l2.data)
  convert(io.errors.uncache, 6, io.errors.uncache.data)

  // The XiangShan platform maps only the high-severity UEC signal to NMI.
  // CE/UED notifications remain available inside RERI for future low/high
  // priority platform interrupt routing, but must not become an NMI here.
  io.nmi := reri.io.uecNotification.valid
}

/** TileLink MMIO shell for the RERI bank.
  *
  * The register implementation continues to own all field semantics. This
  * wrapper only converts one 64-bit TL register access into its simple offset
  * port, keeping the standalone adapter reusable by focused tests.
  */
class TLRERIErrorAdapter(base: BigInt, size: Int = 4096)(implicit p: Parameters) extends LazyModule {
  require(size >= 4096 && (size & (size - 1)) == 0)

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
    })

    val adapter = Module(new RERIErrorAdapter)
    adapter.io.errors := io.errors
    io.nmi := adapter.io.nmi

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
    val recordOffsets = (0 until ErrorRecordInterfaceParam.defaultRecordParams.size).flatMap { record =>
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
