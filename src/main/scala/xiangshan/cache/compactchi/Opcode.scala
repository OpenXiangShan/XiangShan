package oceanus.compactchi

import chisel3._
import chisel3.util._
import oceanus.compactchi.CCHIChannelType._

/*
* CCHI Opcode container.
* 
* @param channel    Specified CCHI channel
* @param opcode     Opcode value
* @param name       Opcode name
*/
class CCHIOpcode(val channel : CCHIChannelType,
                val opcode  : Int,
                val name    : String) {

  require(opcode < (1 << channel.maxOpcodeWidth), "opcode range overflow")
  
  /*
  * Convert to literal UInt instance with the width of originally specified
  * channel Opcode.
  *
  * @return Literal UInt instance of opcode
  */
  def asUInt: UInt = opcode.U(channel.maxOpcodeWidth.W)
  def U = asUInt

  /*
  * Software pre-elaboration time comparsion of opcode. 
  * 
  * @param opcode Integer value of opcode
  */
  def is(opcode: Int): Boolean = opcode == this.opcode

  /* 
  * Hardware comparsion of opcode. 
  * 
  * @param opcode Hardware instance of opcode source, with X-state propagation compatiblity
  * @param valid  Hardware instance of valid signal
  */
  def is(opcode: UInt, valid: Bool = true.B): Bool 
    // with X-state propagation compatibility
    = Mux(valid, opcode === asUInt, dontTouch(WireInit(false.B)))
}

object CCHIOpcode {

  def apply(channel : CCHIChannelType,
            opcode  : Int, 
            name    : String)
    = new CCHIOpcode(channel, opcode, name)

  // EVT channel
  val Evict         = CCHIOpcode(EVT, 0x00, "Evict")
  val WriteBackFull = CCHIOpcode(EVT, 0x01, "WriteBackFull")

  // REQ channel
  val StashShared       = CCHIOpcode(REQ, 0x00, "StashShared")
  val StashUnique       = CCHIOpcode(REQ, 0x01, "StashUnique")
  val ReadNoSnp         = CCHIOpcode(REQ, 0x02, "ReadNoSnp")
  val ReadOnce          = CCHIOpcode(REQ, 0x03, "ReadOnce")
  val ReadShared        = CCHIOpcode(REQ, 0x04, "ReadShared")
//                                        0x05
//                                        0x06
//                                        0x07
  val WriteNoSnpPtl     = CCHIOpcode(REQ, 0x08, "WriteNoSnpPtl")
  val WriteNoSnpFull    = CCHIOpcode(REQ, 0x09, "WriteNoSnpFull")
  val WriteUniquePtl    = CCHIOpcode(REQ, 0x0A, "WriteUniquePtl")
  val WriteUniqueFull   = CCHIOpcode(REQ, 0x0B, "WriteUniqueFull")
  val CleanShared       = CCHIOpcode(REQ, 0x0C, "CleanShared")
  val CleanInvalid      = CCHIOpcode(REQ, 0x0D, "CleanInvalid")
  val MakeInvalid       = CCHIOpcode(REQ, 0x0E, "MakeInvalid")
//                                        0x0F
  val ReadUnique        = CCHIOpcode(REQ, 0x10, "ReadUnique")
//val CleanUnique       = CCHIOpcode(REQ, 0x11, "CleanUnique")
  val MakeUnique        = CCHIOpcode(REQ, 0x12, "MakeUnique")
//                                        0x13
//                                        ...
//                                        0x1D
  val EvictBack         = CCHIOpcode(REQ, 0x1E, "EvictBack")
//                                        0x1F
  val AtomicLoad_ADD    = CCHIOpcode(REQ, 0x20, "AtomicLoad.ADD")
  val AtomicLoad_CLR    = CCHIOpcode(REQ, 0x21, "AtomicLoad.CLR")
  val AtomicLoad_EOR    = CCHIOpcode(REQ, 0x22, "AtomicLoad.EOR")
  val AtomicLoad_SET    = CCHIOpcode(REQ, 0x23, "AtomicLoad.SET")
  val AtomicLoad_SMAX   = CCHIOpcode(REQ, 0x24, "AtomicLoad.SMAX")
  val AtomicLoad_SMIN   = CCHIOpcode(REQ, 0x25, "AtomicLoad.SMIN")
  val AtomicLoad_UMAX   = CCHIOpcode(REQ, 0x26, "AtomicLoad.UMAX")
  val AtomicLoad_UMIN   = CCHIOpcode(REQ, 0x27, "AtomicLoad.UMIN")
  val AtomicStore_ADD   = CCHIOpcode(REQ, 0x28, "AtomicStore.ADD")
  val AtomicStore_CLR   = CCHIOpcode(REQ, 0x29, "AtomicStore.CLR")
  val AtomicStore_EOR   = CCHIOpcode(REQ, 0x2A, "AtomicStore.EOR")
  val AtomicStore_SET   = CCHIOpcode(REQ, 0x2B, "AtomicStore.SET")
  val AtomicStore_SMAX  = CCHIOpcode(REQ, 0x2C, "AtomicStore.SMAX")
  val AtomicStore_SMIN  = CCHIOpcode(REQ, 0x2D, "AtomicStore.SMIN")
  val AtomicStore_UMAX  = CCHIOpcode(REQ, 0x2E, "AtomicStore.UMAX")
  val AtomicStore_UMIN  = CCHIOpcode(REQ, 0x2F, "AtomicStore.UMIN")
  val AtomicSwap        = CCHIOpcode(REQ, 0x30, "AtomicSwap")
  val AtomicCompare     = CCHIOpcode(REQ, 0x31, "AtomicCompare")
//                                        0x32
//                                        ...
//                                        0x3F

  // SNP channel
  val SnpMakeInvalid    = CCHIOpcode(SNP, 0x00, "SnpMakeInvalid")
  val SnpToInvalid      = CCHIOpcode(SNP, 0x01, "SnpToInvalid")
  val SnpToShared       = CCHIOpcode(SNP, 0x02, "SnpToShared")
  val SnpToClean        = CCHIOpcode(SNP, 0x03, "SnpToClean")

  // DnRSP channel
  val CompStash         = CCHIOpcode(DnRSP, 0x00, "CompStash")
  val Comp              = CCHIOpcode(DnRSP, 0x01, "Comp")
  val DBIDResp          = CCHIOpcode(DnRSP, 0x02, "DBIDResp")
  val CompDBIDResp      = CCHIOpcode(DnRSP, 0x03, "CompDBIDResp")
  val CompCMO           = CCHIOpcode(DnRSP, 0x04, "CompCMO")
//val DBIDRespOrd       = CCHIOpcode(DnRSP, 0x05, "DBIDRespOrd")
//                                          0x06
//                                          0x07

  // UpRSP channel
  val CompAck           = CCHIOpcode(UpRSP, 0x00, "CompAck")
  val SnpResp           = CCHIOpcode(UpRSP, 0x01, "SnpResp")

  // DnDAT channel
  val CompData          = CCHIOpcode(DnDAT, 0x00, "CompData")

  // UpDAT channel
  val NonCopyBackWrData = CCHIOpcode(UpDAT, 0x00, "NonCopyBackWrData")
//val NCBWrDataCompAck  = CCHIOpcode(UpDAT, 0x01, "NCBWrDataCompAck")
  val CopyBackWrData    = CCHIOpcode(UpDAT, 0x02, "CopyBackWrData")
  val SnpRespData       = CCHIOpcode(UpDAT, 0x03, "SnpRespData")
}


/*
* CCHI Opcode Decoder
* 
* @param paramChannel   Specify the targeted channel of CHI for this decoder.
* 
* @param paramOpcodeSupported   Specify all supported CHI Opcodes, all supported if empty.
* 
* @param paramOpcodeAll         Specify all CHI Opcodes.
* 
* @param paramEnableUnsupportedCheck    Whether enable assertions for unsupported CHI Opcodes.
*                                       Unsupported CHI Opcodes: CHI Opcodes exist in paramOpcodeAll
*                                                                but absent in paramOpcodeSupported.
*/
abstract class CCHIOpcodeDecoder(val paramChannel                    : CCHIChannelType,
                                 val paramOpcodeSupported            : Seq[CCHIOpcode],
                                 val paramOpcodeAll                  : Seq[CCHIOpcode],
                                 val paramEnableUnsupportedCheck     : Boolean           = false)
    extends Module {

  protected val paramOpcodeWidth = paramChannel.maxOpcodeWidth
  protected val paramDecodedWidth = 1 << paramChannel.maxOpcodeWidth

  /*
  * Module I/O:
  *
  * @io input     valid       : CHI Opcode Input Valid.
  * @io input     opcode      : CHI Opcode Input.
  * @io output    decoded     : CHI Decoded Onehot Output.
  */
  val io = IO(new Bundle {
      // opcode input
      val valid       = Input(Bool())
      val opcode      = Input(UInt(paramOpcodeWidth.W))

      // decoded output
      val decoded     = Output(Vec(paramDecodedWidth, Bool()))
  })


  // default value and logic wires for opcode decoding
  protected val seqLogicDecoded   = Seq.fill(io.decoded.length)(Wire(Bool()))

  (0 until paramDecodedWidth).foreach(i => {
    seqLogicDecoded(i)  := false.B
  })

  // decoding supported CHI Opcodes
  paramOpcodeSupported.foreach(u => {

    seqLogicDecoded(u.opcode)   := u.is(io.opcode, io.valid)

    dontTouch(
      seqLogicDecoded(u.opcode).suggestName(s"decoded_${u.name}"))
  })

  // decoding (unsupported / all) CHI Opcodes
  protected var seqLogicUnsupported  = Seq[Bool]()

  paramOpcodeAll.foreach(u => {

    if (!paramOpcodeSupported.isEmpty)
    {
      if (!paramOpcodeSupported.contains(u))
      {
        seqLogicDecoded(u.opcode) := u.is(io.opcode, io.valid)

        if (paramEnableUnsupportedCheck)
          assert(!seqLogicDecoded(u.opcode), s"Unsupported CCHI Opcode: ${u.name} (0x${u.opcode.toHexString})")

        dontTouch(seqLogicDecoded(u.opcode).suggestName(s"decoded_${u.name}_UNSUPPORTED"))

        seqLogicUnsupported = seqLogicUnsupported :+ seqLogicDecoded(u.opcode)
      }
    }
    else
    {
      seqLogicDecoded(u.opcode) := u.is(io.opcode, io.valid)

      dontTouch(seqLogicDecoded(u.opcode).suggestName(s"decoded_${u.name}"))
    }
  })

  // decoding unknown CHI Opcodes
  protected var seqLogicUnknown   = Seq[Bool]()

  (0 until seqLogicDecoded.length).foreach(i => {
    if (!paramOpcodeAll.map(u => u.is(i)).reduce(_ || _))
    {
      seqLogicDecoded(i) := 
        // with X-state propagation compatibility
        Mux(io.valid, io.opcode === i.U, dontTouch(WireInit(false.B)))

      assert(!seqLogicDecoded(i), s"Unknown CCHI Opcode: 0x${i.toHexString}")

      dontTouch(seqLogicDecoded(i).suggestName(s"decoded_${i.toHexString}_UNKNOWN"))

      seqLogicUnknown = seqLogicUnknown :+ seqLogicDecoded(i)
    }
  })

    
  // decoded output
  seqLogicDecoded.zipWithIndex.foreach(u => {
    io.decoded(u._2)    := u._1
  })

    
  // utility functions
  def is(opcode: CCHIOpcode): Bool = io.decoded(opcode.opcode)

  def is(opcode0: CCHIOpcode, opcodes: CCHIOpcode*): Bool = {
    VecInit((opcodes :+ opcode0).map(opcode => {io.decoded(opcode.opcode)})).asUInt.orR
  }
}


/* 
* Decoder for CCHI Opcodes of EVT channel 
*/
class CCHIEVTOpcodeDecoder(paramOpcodeSupported         : Seq[CCHIOpcode] = Seq(),
                           paramEnableUnsupportedCheck  : Boolean         = false)
    extends CCHIOpcodeDecoder(EVT, paramOpcodeSupported, Seq(
      CCHIOpcode.Evict,
      CCHIOpcode.WriteBackFull
    ), paramEnableUnsupportedCheck)

/* 
* Decoder for CCHI Opcodes of REQ channel 
*/
class CCHIREQOpcodeDecoder(paramOpcodeSupported         : Seq[CCHIOpcode] = Seq(),
                           paramEnableUnsupportedCheck  : Boolean         = false)
    extends CCHIOpcodeDecoder(REQ, paramOpcodeSupported, Seq(
      CCHIOpcode.StashShared,
      CCHIOpcode.StashUnique,
      CCHIOpcode.ReadNoSnp,
      CCHIOpcode.ReadOnce,
      CCHIOpcode.ReadShared,
      CCHIOpcode.WriteNoSnpPtl,
      CCHIOpcode.WriteNoSnpFull,
      CCHIOpcode.WriteUniquePtl,
      CCHIOpcode.WriteUniqueFull,
      CCHIOpcode.CleanShared,
      CCHIOpcode.CleanInvalid,
      CCHIOpcode.MakeInvalid,
      CCHIOpcode.ReadUnique,
      CCHIOpcode.MakeUnique,
      CCHIOpcode.EvictBack,
      CCHIOpcode.AtomicLoad_ADD,
      CCHIOpcode.AtomicLoad_CLR,
      CCHIOpcode.AtomicLoad_EOR,
      CCHIOpcode.AtomicLoad_SET,
      CCHIOpcode.AtomicLoad_SMAX,
      CCHIOpcode.AtomicLoad_SMIN,
      CCHIOpcode.AtomicLoad_UMAX,
      CCHIOpcode.AtomicLoad_UMIN,
      CCHIOpcode.AtomicStore_ADD,
      CCHIOpcode.AtomicStore_CLR,
      CCHIOpcode.AtomicStore_EOR,
      CCHIOpcode.AtomicStore_SET,
      CCHIOpcode.AtomicStore_SMAX,
      CCHIOpcode.AtomicStore_SMIN,
      CCHIOpcode.AtomicStore_UMAX,
      CCHIOpcode.AtomicStore_UMIN,
      CCHIOpcode.AtomicSwap,
      CCHIOpcode.AtomicCompare
    ), paramEnableUnsupportedCheck)

/* 
* Decoder for CCHI Opcodes of SNP channel 
*/
class CCHISNPOpcodeDecoder(paramOpcodeSupported         : Seq[CCHIOpcode] = Seq(),
                           paramEnableUnsupportedCheck  : Boolean         = false)
    extends CCHIOpcodeDecoder(SNP, paramOpcodeSupported, Seq(
      CCHIOpcode.SnpMakeInvalid,
      CCHIOpcode.SnpToInvalid,
      CCHIOpcode.SnpToShared,
      CCHIOpcode.SnpToClean
    ), paramEnableUnsupportedCheck)

/* 
* Decoder for CCHI Opcodes of DnRSP channel 
*/
class CCHIDnRSPOpcodeDecoder(paramOpcodeSupported         : Seq[CCHIOpcode] = Seq(),
                             paramEnableUnsupportedCheck  : Boolean         = false)
    extends CCHIOpcodeDecoder(DnRSP, paramOpcodeSupported, Seq(
      CCHIOpcode.CompStash,
      CCHIOpcode.Comp,
      CCHIOpcode.DBIDResp,
      CCHIOpcode.CompDBIDResp,
      CCHIOpcode.CompCMO
    ), paramEnableUnsupportedCheck)

/* 
* Decoder for CCHI Opcodes of UpRSP channel 
*/
class CCHIUpRSPOpcodeDecoder(paramOpcodeSupported         : Seq[CCHIOpcode] = Seq(),
                             paramEnableUnsupportedCheck  : Boolean         = false)
    extends CCHIOpcodeDecoder(UpRSP, paramOpcodeSupported, Seq(
      CCHIOpcode.CompAck,
      CCHIOpcode.SnpResp
    ), paramEnableUnsupportedCheck)

/* 
* Decoder for CCHI Opcodes of DnDAT channel
*/
class CCHIDnDATOpcodeDecoder(paramOpcodeSupported         : Seq[CCHIOpcode] = Seq(),
                             paramEnableUnsupportedCheck  : Boolean         = false)
    extends CCHIOpcodeDecoder(DnDAT, paramOpcodeSupported, Seq(
      CCHIOpcode.CompData
    ), paramEnableUnsupportedCheck)

/* 
* Decoder for CCHI Opcodes of UpDAT channel
*/
class CCHIUpDATOpcodeDecoder(paramOpcodeSupported         : Seq[CCHIOpcode] = Seq(),
                             paramEnableUnsupportedCheck  : Boolean         = false)
    extends CCHIOpcodeDecoder(UpDAT, paramOpcodeSupported, Seq(
      CCHIOpcode.NonCopyBackWrData,
      CCHIOpcode.CopyBackWrData,
      CCHIOpcode.SnpRespData
    ), paramEnableUnsupportedCheck)
