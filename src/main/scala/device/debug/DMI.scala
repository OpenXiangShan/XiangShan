// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

/** Constant values used by both Debug Bus Response & Request
  */

object DMIConsts{

  def dmiDataSize = 32

  def dmiOpSize = 2
  def dmi_OP_NONE            = "b00".U
  def dmi_OP_READ            = "b01".U
  def dmi_OP_WRITE           = "b10".U

  def dmiRespSize = 2
  def dmi_RESP_SUCCESS     = "b00".U
  def dmi_RESP_FAILURE     = "b01".U
  def dmi_RESP_HW_FAILURE  = "b10".U
  // This is used outside this block
  // to indicate 'busy'.
  def dmi_RESP_RESERVED    = "b11".U
}

// *****************************************
// Module Interfaces
// 
// *****************************************

/** Structure to define the contents of a Debug Bus Request
  */
class DMIReq(addrBits : Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt(DMIConsts.dmiDataSize.W)
  val op   = UInt(DMIConsts.dmiOpSize.W)

  override def cloneType = new DMIReq(addrBits).asInstanceOf[this.type]
}

/** Structure to define the contents of a Debug Bus Response
  */
class DMIResp( ) extends Bundle {
  val data = UInt(DMIConsts.dmiDataSize.W)
  val resp = UInt(DMIConsts.dmiRespSize.W)
}

/** Structure to define the top-level DMI interface 
  *  of DebugModule.
  *  DebugModule is the consumer of this interface.
  *  Therefore it has the 'flipped' version of this.
  */
class DMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val req = new  DecoupledIO(new DMIReq(p(DebugModuleKey).get.nDMIAddrSize))
  val resp = Flipped(new DecoupledIO(new DMIResp))
}

/** This includes the clock and reset as these are passed through the
  *  hierarchy until the Debug Module is actually instantiated. 
  *  
  */

class ClockedDMIIO(implicit val p: Parameters) extends ParameterizedBundle()(p){
  val dmi      = new DMIIO()(p)
  val dmiClock = Output(Clock())
  val dmiReset = Output(Reset())
}

/** Convert DMI to TL. Avoids using special DMI synchronizers and register accesses
  *  
  */

class DMIToTL(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(Seq(TLMasterPortParameters.v2(Seq(TLMasterParameters.v2(
    name = "debug",
    emits = TLMasterToSlaveTransferSizes(
      get = TransferSizes(4,4),
      putFull = TransferSizes(4,4)))))))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val dmi = Flipped(new DMIIO()(p))
    })

    val (tl, edge) = node.out(0)

    val src  = WireInit(0.U)
    val addr = WireInit(io.dmi.req.bits.addr << 2)
    val size = (log2Ceil(DMIConsts.dmiDataSize / 8)).U

    val (_,  gbits) = edge.Get(src, addr, size)
    val (_, pfbits) = edge.Put(src, addr, size, io.dmi.req.bits.data)

    // We force DMI NOPs to write to read-only HARTINFO register because
    // Inner  may be in reset / not have a clock,
    // so we force address to be the one that goes to Outer.
    // Therefore for a NOP we don't really need to pay the penalty to go
    // across the CDC.

    val (_, nbits)  = edge.Put(src, toAddress = (DMI_RegAddrs.DMI_HARTINFO << 2).U, size, data=0.U)

    when (io.dmi.req.bits.op === DMIConsts.dmi_OP_WRITE)       { tl.a.bits := pfbits
    }.elsewhen  (io.dmi.req.bits.op === DMIConsts.dmi_OP_READ) { tl.a.bits := gbits
    }.otherwise {                                                tl.a.bits := nbits
    }

    tl.a.valid       := io.dmi.req.valid
    io.dmi.req.ready := tl.a.ready

    io.dmi.resp.valid      := tl.d.valid
    tl.d.ready             := io.dmi.resp.ready
    io.dmi.resp.bits.resp  := Mux(tl.d.bits.corrupt || tl.d.bits.denied, DMIConsts.dmi_RESP_FAILURE, DMIConsts.dmi_RESP_SUCCESS)
    io.dmi.resp.bits.data  := tl.d.bits.data

    // Tie off unused channels
    tl.b.ready := false.B
    tl.c.valid := false.B
    tl.e.valid := false.B

  }
}
