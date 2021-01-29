// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug.systembusaccess

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.property._
import freechips.rocketchip.devices.debug._

object SystemBusAccessState extends scala.Enumeration {
   type SystemBusAccessState = Value
   val Idle, SBReadRequest, SBWriteRequest, SBReadResponse, SBWriteResponse = Value
} 

object SBErrorCode extends scala.Enumeration {
  type SBErrorCode = Value
  val NoError    = Value(0)
  val Timeout    = Value(1)
  val BadAddr    = Value(2)
  val AlgnError  = Value(3)
  val BadAccess  = Value(4)
  val OtherError = Value(7)
}

object SystemBusAccessModule
{
  def apply(sb2tl: SBToTL, dmactive: Bool, dmAuthenticated: Bool)(implicit p: Parameters):
    (Seq[RegField], Seq[Seq[RegField]], Seq[Seq[RegField]]) =
  {
    import SBErrorCode._

    val cfg = p(DebugModuleKey).get

    val anyAddressWrEn = WireInit(false.B).suggestName("anyAddressWrEn")
    val anyDataRdEn    = WireInit(false.B).suggestName("anyDataRdEn")
    val anyDataWrEn    = WireInit(false.B).suggestName("anyDataWrEn")

    // --- SBCS Status Register ---
    val SBCSFieldsReg = Reg(new SBCSFields()).suggestName("SBCSFieldsReg")

    val SBCSFieldsRegReset = WireInit(0.U.asTypeOf(new SBCSFields()))
    SBCSFieldsRegReset.sbversion   := 1.U(1.W) // This code implements a version of the spec after January 1, 2018
    SBCSFieldsRegReset.sbbusy      := (sb2tl.module.io.sbStateOut =/= SystemBusAccessState.Idle.id.U)
    SBCSFieldsRegReset.sbaccess    := 2.U
    SBCSFieldsRegReset.sbasize     := sb2tl.module.edge.bundle.addressBits.U
    SBCSFieldsRegReset.sbaccess128 := (cfg.maxSupportedSBAccess == 128).B
    SBCSFieldsRegReset.sbaccess64  := (cfg.maxSupportedSBAccess >=  64).B
    SBCSFieldsRegReset.sbaccess32  := (cfg.maxSupportedSBAccess >=  32).B
    SBCSFieldsRegReset.sbaccess16  := (cfg.maxSupportedSBAccess >=  16).B
    SBCSFieldsRegReset.sbaccess8   := (cfg.maxSupportedSBAccess >=   8).B

    val SBCSRdData         = WireInit(0.U.asTypeOf(new SBCSFields())).suggestName("SBCSRdData")

    val SBCSWrDataVal      = WireInit(0.U(32.W))
    val SBCSWrData         = WireInit(SBCSWrDataVal.asTypeOf(new SBCSFields()))
    
    val sberrorWrEn        = WireInit(false.B)
    val sbreadondataWrEn   = WireInit(false.B)
    val sbautoincrementWrEn= WireInit(false.B)
    val sbaccessWrEn       = WireInit(false.B)
    val sbreadonaddrWrEn   = WireInit(false.B)
    val sbbusyerrorWrEn    = WireInit(false.B)

    val sbcsfields = RegFieldGroup("sbcs", Some("system bus access control and status"), Seq(
      RegField.r(1, SBCSRdData.sbaccess8,   RegFieldDesc("sbaccess8",   "8-bit accesses supported",   reset=Some(if (cfg.maxSupportedSBAccess >=   8) 1 else 0))),
      RegField.r(1, SBCSRdData.sbaccess16,  RegFieldDesc("sbaccess16",  "16-bit accesses supported",  reset=Some(if (cfg.maxSupportedSBAccess >=  16) 1 else 0))),
      RegField.r(1, SBCSRdData.sbaccess32,  RegFieldDesc("sbaccess32",  "32-bit accesses supported",  reset=Some(if (cfg.maxSupportedSBAccess >=  32) 1 else 0))),
      RegField.r(1, SBCSRdData.sbaccess64,  RegFieldDesc("sbaccess64",  "64-bit accesses supported",  reset=Some(if (cfg.maxSupportedSBAccess >=  64) 1 else 0))),
      RegField.r(1, SBCSRdData.sbaccess128, RegFieldDesc("sbaccess128", "128-bit accesses supported", reset=Some(if (cfg.maxSupportedSBAccess == 128) 1 else 0))),
      RegField.r(7, SBCSRdData.sbasize,     RegFieldDesc("sbasize",     "bits in address",            reset=Some(sb2tl.module.edge.bundle.addressBits))),
      WNotifyVal(3, SBCSRdData.sberror, SBCSWrData.sberror, sberrorWrEn,
        RegFieldDesc("sberror", "system bus error", reset=Some(0), wrType=Some(RegFieldWrType.ONE_TO_CLEAR))),
      WNotifyVal(1, SBCSRdData.sbreadondata, SBCSWrData.sbreadondata, sbreadondataWrEn,
        RegFieldDesc("sbreadondata", "system bus read on data", reset=Some(0))),
      WNotifyVal(1, SBCSRdData.sbautoincrement, SBCSWrData.sbautoincrement, sbautoincrementWrEn,
        RegFieldDesc("sbautoincrement", "system bus auto-increment address", reset=Some(0))),
      WNotifyVal(3, SBCSRdData.sbaccess, SBCSWrData.sbaccess, sbaccessWrEn,
        RegFieldDesc("sbaccess", "system bus access size", reset=Some(2))),
      WNotifyVal(1, SBCSRdData.sbreadonaddr, SBCSWrData.sbreadonaddr, sbreadonaddrWrEn,
        RegFieldDesc("sbreadonaddr", "system bus read on data", reset=Some(0))),
      RegField.r(1, SBCSRdData.sbbusy, RegFieldDesc("sbbusy", "system bus access is busy", reset=Some(0))),
      WNotifyVal(1, SBCSRdData.sbbusyerror, SBCSWrData.sbbusyerror, sbbusyerrorWrEn,
        RegFieldDesc("sbbusyerror", "system bus busy error", reset=Some(0), wrType=Some(RegFieldWrType.ONE_TO_CLEAR))),
      RegField(6),
      RegField.r(3, SBCSRdData.sbversion, RegFieldDesc("sbversion", "system bus access version", reset=Some(1))),
    ))

    // --- System Bus Address Registers ---
    // ADDR0 Register is required
    // Instantiate ADDR1-3 registers as needed depending on system bus address width
    val hasSBAddr1 = (sb2tl.module.edge.bundle.addressBits >= 33)
    val hasSBAddr2 = (sb2tl.module.edge.bundle.addressBits >= 65)
    val hasSBAddr3 = (sb2tl.module.edge.bundle.addressBits >= 97)
    val hasAddr    = Seq(true, hasSBAddr1, hasSBAddr2, hasSBAddr3)

    val SBADDRESSFieldsReg = Reg(Vec(4, UInt(32.W)))
    SBADDRESSFieldsReg.zipWithIndex.foreach { case(a,i) => a.suggestName("SBADDRESS"+i+"FieldsReg")}
    val SBADDRESSWrData    = WireInit(VecInit(Seq.fill(4) {0.U(32.W)} ))
    val SBADDRESSRdEn      = WireInit(VecInit(Seq.fill(4) {false.B} ))
    val SBADDRESSWrEn      = WireInit(VecInit(Seq.fill(4) {false.B} ))

    val autoIncrementedAddr = WireInit(0.U(128.W))
    autoIncrementedAddr := Cat(SBADDRESSFieldsReg.reverse) + (1.U << SBCSFieldsReg.sbaccess)
    autoIncrementedAddr.suggestName("autoIncrementedAddr")

    val sbaddrfields: Seq[Seq[RegField]] = SBADDRESSFieldsReg.zipWithIndex.map { case(a,i) =>
      if(hasAddr(i)) {
        when (~dmactive || ~dmAuthenticated) {
          a := 0.U(32.W)
        }.otherwise {
          a := Mux(SBADDRESSWrEn(i) && !SBCSRdData.sberror && !SBCSFieldsReg.sbbusy && !SBCSFieldsReg.sbbusyerror, SBADDRESSWrData(i),
               Mux((sb2tl.module.io.rdDone || sb2tl.module.io.wrDone) && SBCSFieldsReg.sbautoincrement, autoIncrementedAddr(32*i+31,32*i), a))
        }

        RegFieldGroup("dmi_sbaddr"+i, Some("SBA Address Register"), Seq(RWNotify(32, a, SBADDRESSWrData(i), SBADDRESSRdEn(i), SBADDRESSWrEn(i),
          Some(RegFieldDesc("dmi_sbaddr"+i, "SBA address register", reset=Some(0), volatile=true)))))
      } else {
        a := DontCare
        Seq.empty[RegField]
      }
    }

    sb2tl.module.io.addrIn := Mux(SBADDRESSWrEn(0),
      Cat(Cat(SBADDRESSFieldsReg.drop(1).reverse), SBADDRESSWrData(0)),
      Cat(SBADDRESSFieldsReg.reverse))
    anyAddressWrEn         := SBADDRESSWrEn.reduce(_ || _)

    // --- System Bus Data Registers ---           
    // DATA0 Register is required
    // DATA1-3 Registers may not be needed depending on implementation
    val hasSBData1     = (cfg.maxSupportedSBAccess >   32)
    val hasSBData2And3 = (cfg.maxSupportedSBAccess == 128)
    val hasData        = Seq(true, hasSBData1, hasSBData2And3, hasSBData2And3)

    val SBDATAFieldsReg = Reg(Vec(4, Vec(4, UInt(8.W))))
    SBDATAFieldsReg.zipWithIndex.foreach { case(d,i) => d.zipWithIndex.foreach { case(d,j) => d.suggestName("SBDATA"+i+"BYTE"+j) }}
    val SBDATARdData    = WireInit(VecInit(Seq.fill(4) {0.U(32.W)} ))
    SBDATARdData.zipWithIndex.foreach { case(d,i) => d.suggestName("SBDATARdData"+i) }
    val SBDATAWrData    = WireInit(VecInit(Seq.fill(4) {0.U(32.W)} ))
    SBDATAWrData.zipWithIndex.foreach { case(d,i) => d.suggestName("SBDATAWrData"+i) }
    val SBDATARdEn      = WireInit(VecInit(Seq.fill(4) {false.B} ))
    val SBDATAWrEn      = WireInit(VecInit(Seq.fill(4) {false.B} ))
    SBDATAWrEn.zipWithIndex.foreach { case(d,i) => d.suggestName("SBDATAWrEn"+i) }

    val sbdatafields: Seq[Seq[RegField]] = SBDATAFieldsReg.zipWithIndex.map { case(d,i) =>
      if(hasData(i)) {
        // For data registers, load enable per-byte
        for (j <- 0 to 3) {
          when (~dmactive || ~dmAuthenticated) {
            d(j) := 0.U(8.W)
          }.otherwise {
            d(j) := Mux(SBDATAWrEn(i) && !SBCSFieldsReg.sbbusy && !SBCSFieldsReg.sbbusyerror && !SBCSRdData.sberror, SBDATAWrData(i)(8*j+7,8*j),
                    Mux(sb2tl.module.io.rdLoad(4*i+j), sb2tl.module.io.dataOut, d(j)))
          }
        }

        SBDATARdData(i) := Cat(d.reverse)

        RegFieldGroup("dmi_sbdata"+i, Some("SBA Data Register"), Seq(RWNotify(32, SBDATARdData(i), SBDATAWrData(i), SBDATARdEn(i), SBDATAWrEn(i),
          Some(RegFieldDesc("dmi_sbdata"+i, "SBA data register", reset=Some(0), volatile=true)))))
      } else {
        for (j <- 0 to 3) { d(j) := DontCare }
        Seq.empty[RegField]
      }
    }

    sb2tl.module.io.dataIn := Mux(sb2tl.module.io.wrEn,Cat(SBDATAWrData.reverse),Cat(SBDATAFieldsReg.flatten.reverse))
    anyDataRdEn            := SBDATARdEn.reduce(_ || _)
    anyDataWrEn            := SBDATAWrEn.reduce(_ || _)

    val tryWrEn = SBDATAWrEn(0)
    val tryRdEn = (SBADDRESSWrEn(0) && SBCSFieldsReg.sbreadonaddr) || (SBDATARdEn(0) && SBCSFieldsReg.sbreadondata)

    val sbAccessError = (SBCSFieldsReg.sbaccess === 0.U) && (SBCSFieldsReg.sbaccess8   =/= 1.U) ||
                        (SBCSFieldsReg.sbaccess === 1.U) && (SBCSFieldsReg.sbaccess16  =/= 1.U) ||
                        (SBCSFieldsReg.sbaccess === 2.U) && (SBCSFieldsReg.sbaccess32  =/= 1.U) ||
                        (SBCSFieldsReg.sbaccess === 3.U) && (SBCSFieldsReg.sbaccess64  =/= 1.U) ||
                        (SBCSFieldsReg.sbaccess === 4.U) && (SBCSFieldsReg.sbaccess128 =/= 1.U) || (SBCSFieldsReg.sbaccess > 4.U)

    val compareAddr = Wire(UInt(32.W)) // Need use written or latched address to detect error case depending on how transaction is initiated
    compareAddr := Mux(SBADDRESSWrEn(0),SBADDRESSWrData(0),SBADDRESSFieldsReg(0))

    val sbAlignmentError = (SBCSFieldsReg.sbaccess === 1.U) && (compareAddr(0)   =/= 0.U) ||
                           (SBCSFieldsReg.sbaccess === 2.U) && (compareAddr(1,0) =/= 0.U) ||
                           (SBCSFieldsReg.sbaccess === 3.U) && (compareAddr(2,0) =/= 0.U) ||
                           (SBCSFieldsReg.sbaccess === 4.U) && (compareAddr(3,0) =/= 0.U)

    sbAccessError.suggestName("sbAccessError")
    sbAlignmentError.suggestName("sbAlignmentError")

    sb2tl.module.io.wrEn     := dmAuthenticated && tryWrEn && !SBCSFieldsReg.sbbusy && !SBCSFieldsReg.sbbusyerror && !SBCSRdData.sberror && !sbAccessError && !sbAlignmentError
    sb2tl.module.io.rdEn     := dmAuthenticated && tryRdEn && !SBCSFieldsReg.sbbusy && !SBCSFieldsReg.sbbusyerror && !SBCSRdData.sberror && !sbAccessError && !sbAlignmentError
    sb2tl.module.io.sizeIn   := SBCSFieldsReg.sbaccess

    val sbBusy = (sb2tl.module.io.sbStateOut =/= SystemBusAccessState.Idle.id.U)

    when (~dmactive || ~dmAuthenticated) {
      SBCSFieldsReg := SBCSFieldsRegReset
    }.otherwise {
      SBCSFieldsReg.sbbusyerror     := Mux(sbbusyerrorWrEn && SBCSWrData.sbbusyerror,     false.B, // W1C
                                       Mux(anyAddressWrEn && sbBusy,               true.B, // Set if a write to SBADDRESS occurs while busy
                                       Mux((anyDataRdEn || anyDataWrEn) && sbBusy, true.B, SBCSFieldsReg.sbbusyerror))) // Set if any access to SBDATA occurs while busy
      SBCSFieldsReg.sbreadonaddr    := Mux(sbreadonaddrWrEn,    SBCSWrData.sbreadonaddr   , SBCSFieldsReg.sbreadonaddr)
      SBCSFieldsReg.sbautoincrement := Mux(sbautoincrementWrEn, SBCSWrData.sbautoincrement, SBCSFieldsReg.sbautoincrement)
      SBCSFieldsReg.sbreadondata    := Mux(sbreadondataWrEn,    SBCSWrData.sbreadondata   , SBCSFieldsReg.sbreadondata)
      SBCSFieldsReg.sbaccess        := Mux(sbaccessWrEn,        SBCSWrData.sbaccess, SBCSFieldsReg.sbaccess)
      SBCSFieldsReg.sbversion       := 1.U(1.W) // This code implements a version of the spec after January 1, 2018
    }

    // sbErrorReg has a per-bit load enable since each bit can be individually cleared by writing a 1 to it
    val sbErrorReg = Reg(Vec(4, UInt(1.W)))
    when(~dmactive || ~dmAuthenticated) {
      for (i <- 0 until 3)
        sbErrorReg(i) := 0.U
    }.otherwise {
      for (i <- 0 until 3)
        sbErrorReg(i) := Mux(sberrorWrEn && SBCSWrData.sberror(i) === 1.U, NoError.id.U(i), // W1C
                         Mux((sb2tl.module.io.wrEn && !sb2tl.module.io.wrLegal) || (sb2tl.module.io.rdEn && !sb2tl.module.io.rdLegal), BadAddr.id.U(i), // Bad address accessed
                         Mux((tryWrEn || tryRdEn) && sbAlignmentError, AlgnError.id.U(i), // Address alignment error
                         Mux((tryWrEn || tryRdEn) && sbAccessError, BadAccess.id.U(i), // Access size error
                         Mux((sb2tl.module.io.rdDone || sb2tl.module.io.wrDone) && sb2tl.module.io.respError, OtherError.id.U(i), sbErrorReg(i)))))) // Response error from TL
    }

    SBCSRdData             := SBCSFieldsReg
    SBCSRdData.sbasize     := sb2tl.module.edge.bundle.addressBits.U
    SBCSRdData.sbaccess128 := (cfg.maxSupportedSBAccess == 128).B
    SBCSRdData.sbaccess64  := (cfg.maxSupportedSBAccess >=  64).B
    SBCSRdData.sbaccess32  := (cfg.maxSupportedSBAccess >=  32).B
    SBCSRdData.sbaccess16  := (cfg.maxSupportedSBAccess >=  16).B
    SBCSRdData.sbaccess8   := (cfg.maxSupportedSBAccess >=   8).B
    SBCSRdData.sbbusy      := sbBusy
    SBCSRdData.sberror     := sbErrorReg.asUInt
    
    when (~dmAuthenticated) {    // Read value must be 0 if not authenticated
      SBCSRdData := 0.U.asTypeOf(new SBCSFields())
    }

    cover(SBCSFieldsReg.sbbusyerror,    "SBCS Cover", "sberror set")
    cover(SBCSFieldsReg.sbbusy === 3.U, "SBCS Cover", "sbbusyerror alignment error")

    cover((sb2tl.module.io.wrEn || sb2tl.module.io.rdEn) && SBCSFieldsReg.sbaccess === 0.U && !sbAccessError && !sbAlignmentError, "SBCS Cover", "8-bit access")
    cover((sb2tl.module.io.wrEn || sb2tl.module.io.rdEn) && SBCSFieldsReg.sbaccess === 1.U && !sbAccessError && !sbAlignmentError, "SBCS Cover", "16-bit access")
    cover((sb2tl.module.io.wrEn || sb2tl.module.io.rdEn) && SBCSFieldsReg.sbaccess === 2.U && !sbAccessError && !sbAlignmentError, "SBCS Cover", "32-bit access")
    cover((sb2tl.module.io.wrEn || sb2tl.module.io.rdEn) && SBCSFieldsReg.sbaccess === 3.U && !sbAccessError && !sbAlignmentError, "SBCS Cover", "64-bit access")
    cover((sb2tl.module.io.wrEn || sb2tl.module.io.rdEn) && SBCSFieldsReg.sbaccess === 4.U && !sbAccessError && !sbAlignmentError, "SBCS Cover", "128-bit access")

    cover(SBCSFieldsReg.sbautoincrement && SBCSFieldsReg.sbbusy,  "SBCS Cover", "Access with autoincrement set")
    cover(!SBCSFieldsReg.sbautoincrement && SBCSFieldsReg.sbbusy, "SBCS Cover", "Access without autoincrement set")

    cover((sb2tl.module.io.wrEn || sb2tl.module.io.rdEn) && SBCSFieldsReg.sbaccess > 4.U, "SBCS Cover", "Invalid sbaccess value")

    (sbcsfields, sbaddrfields, sbdatafields)
  }
}

class SBToTL(implicit p: Parameters) extends LazyModule {

  val cfg = p(DebugModuleKey).get

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1("debug")),
    requestFields = Seq(AMBAProtField()))))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val rdEn         = Input(Bool())
      val wrEn         = Input(Bool())
      val addrIn       = Input(UInt(128.W)) // TODO: Parameterize these widths
      val dataIn       = Input(UInt(128.W))
      val sizeIn       = Input(UInt(3.W))
      val rdLegal      = Output(Bool())
      val wrLegal      = Output(Bool())
      val rdDone       = Output(Bool())
      val wrDone       = Output(Bool())
      val respError    = Output(Bool())
      val dataOut      = Output(UInt(8.W))
      val rdLoad       = Output(Vec(cfg.maxSupportedSBAccess/8, Bool()))
      val sbStateOut   = Output(UInt(log2Ceil(SystemBusAccessState.maxId).W))
    })
    val rf_reset       = IO(Input(Reset()))

    import SystemBusAccessState._
 
    val (tl, edge) = node.out(0)

    val sbState = RegInit(0.U)

    // --- Drive payloads on bus to TileLink ---
    val d = Queue(tl.d, 2) // Add a small buffer since response could arrive on same cycle as request
    d.ready := (sbState === SBReadResponse.id.U) || (sbState === SBWriteResponse.id.U)

    val muxedData = WireInit(0.U(8.W))
    val requestValid = tl.a.valid
    val requestReady = tl.a.ready
    val responseValid = d.valid
    val responseReady = d.ready

    val counter = RegInit(0.U((log2Ceil(cfg.maxSupportedSBAccess/8)+1).W))
    val vecData   = Wire(Vec(cfg.maxSupportedSBAccess/8, UInt(8.W)))
    vecData.zipWithIndex.map { case (vd, i) => vd := io.dataIn(8*i+7,8*i) }
    muxedData := vecData(counter)

    // Need an additional check to determine if address is safe for Get/Put
    val rdLegal_addr = edge.manager.supportsGetSafe(io.addrIn, io.sizeIn, Some(TransferSizes(1,cfg.maxSupportedSBAccess/8)))
    val wrLegal_addr = edge.manager.supportsPutFullSafe(io.addrIn, io.sizeIn, Some(TransferSizes(1,cfg.maxSupportedSBAccess/8)))
    val (_,  gbits) = edge.Get(0.U, io.addrIn, io.sizeIn)
    val (_, pfbits) = edge.Put(0.U, io.addrIn, io.sizeIn, muxedData)

    io.rdLegal := rdLegal_addr
    io.wrLegal := wrLegal_addr

    io.sbStateOut := sbState
    when(sbState === SBReadRequest.id.U) { tl.a.bits :=  gbits  }
    .otherwise                           { tl.a.bits := pfbits  }

    tl.a.bits.user.lift(AMBAProt).foreach { x =>
      x.bufferable := false.B
      x.modifiable := false.B
      x.readalloc  := false.B
      x.writealloc := false.B
      x.privileged := true.B
      x.secure     := true.B
      x.fetch      := false.B
    }

    val respError = d.bits.denied || d.bits.corrupt
    io.respError := respError

    val wrTxValid = sbState === SBWriteRequest.id.U && requestValid && requestReady
    val rdTxValid = sbState === SBReadResponse.id.U && responseValid && responseReady
    val txLast    = counter === ((1.U << io.sizeIn) - 1.U)
    counter := Mux((wrTxValid || rdTxValid) && txLast, 0.U,
               Mux((wrTxValid || rdTxValid)          , counter+1.U, counter))

    for (i <- 0 until (cfg.maxSupportedSBAccess/8)) {
      io.rdLoad(i) := rdTxValid && (counter === i.U)
    }

    // --- State Machine to interface with TileLink ---
    when (sbState === Idle.id.U){
      sbState := Mux(io.rdEn && io.rdLegal, SBReadRequest.id.U,
                 Mux(io.wrEn && io.wrLegal, SBWriteRequest.id.U, sbState))
    }.elsewhen (sbState === SBReadRequest.id.U){
      sbState := Mux(requestValid && requestReady, SBReadResponse.id.U, sbState) 
    }.elsewhen (sbState === SBWriteRequest.id.U){
      sbState := Mux(wrTxValid && txLast, SBWriteResponse.id.U, sbState)
    }.elsewhen (sbState === SBReadResponse.id.U){
      sbState := Mux(rdTxValid && txLast, Idle.id.U, sbState)
    }.elsewhen (sbState === SBWriteResponse.id.U){
      sbState := Mux(responseValid && responseReady, Idle.id.U, sbState)
    }
 
    io.rdDone  := rdTxValid && txLast
    io.wrDone  := (sbState === SBWriteResponse.id.U) && responseValid && responseReady
    io.dataOut := d.bits.data
 
    tl.a.valid := (sbState === SBReadRequest.id.U) || (sbState === SBWriteRequest.id.U)

    // Tie off unused channels
    tl.b.ready := false.B
    tl.c.valid := false.B
    tl.e.valid := false.B

    assert (sbState === Idle.id.U ||
            sbState === SBReadRequest.id.U ||
            sbState === SBWriteRequest.id.U || 
            sbState === SBReadResponse.id.U ||          
            sbState === SBWriteResponse.id.U, "SBA state machine in undefined state")

    cover (sbState === Idle.id.U,            "SBA State Cover", "SBA Access Idle")
    cover (sbState === SBReadRequest.id.U,   "SBA State Cover", "SBA Access Read Req")
    cover (sbState === SBWriteRequest.id.U,  "SBA State Cover", "SBA Access Write Req")
    cover (sbState === SBReadResponse.id.U,  "SBA State Cover", "SBA Access Read Resp")
    cover (sbState === SBWriteResponse.id.U, "SBA State Cover", "SBA Access Write Resp")

    cover (io.rdEn && !io.rdLegal, "SB Legality Cover", "SBA Rd Address Illegal")
    cover (io.wrEn && !io.wrLegal, "SB Legality Cover", "SBA Wr Address Illegal")
 
  }
}
