package xiangshan.backend.fu.util

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import CSRConst.ModeM

trait DebugCSR {
  implicit val p: Parameters
  class DcsrStruct extends Bundle {
    val debugver  = Output(UInt(4.W)) // [28:31]
    val pad1      = Output(UInt(10.W))// [27:18]
    val ebreakvs  = Output(Bool())    // [17] reserved for Hypervisor debug
    val ebreakvu  = Output(Bool())    // [16] reserved for Hypervisor debug
    val ebreakm   = Output(Bool())    // [15]
    val pad0      = Output(Bool())    // [14] ebreakh has been removed
    val ebreaks   = Output(Bool())    // [13]
    val ebreaku   = Output(Bool())    // [12]
    val stepie    = Output(Bool())    // [11]
    val stopcount = Output(Bool())    // [10]
    val stoptime  = Output(Bool())    // [9]
    val cause     = Output(UInt(3.W)) // [8:6]
    val v         = Output(Bool())    // [5]
    val mprven    = Output(Bool())    // [4]
    val nmip      = Output(Bool())    // [3]
    val step      = Output(Bool())    // [2]
    val prv       = Output(UInt(2.W)) // [1:0]
    require(this.getWidth == 32)
  }

  object DcsrStruct extends DcsrStruct {
    def DEBUGVER_NONE   = 0.U
    def DEBUGVER_SPEC   = 4.U
    def DEBUGVER_CUSTOM = 15.U
    def CAUSE_EBREAK        = 1.U
    def CAUSE_TRIGGER       = 2.U
    def CAUSE_HALTREQ       = 3.U
    def CAUSE_STEP          = 4.U
    def CAUSE_RESETHALTREQ  = 5.U
    private def debugver_offset   = 28
    private def stopcount_offset  = 10
    private def stoptime_offset   = 9
    private def mprven_offset     = 5
    private def prv_offset        = 0
    def init: UInt = (
      (DEBUGVER_SPEC.litValue << debugver_offset) | /* Debug implementation as it described in 0.13 draft */
      (0L << stopcount_offset) |                    /* Stop count updating has not been supported */
      (0L << stoptime_offset) |                     /* Stop time updating has not been supported */
      (0L << mprven_offset) |                       /* Whether use mstatus.perven as mprven */
      (ModeM.litValue << prv_offset)
    ).U
  }
}
