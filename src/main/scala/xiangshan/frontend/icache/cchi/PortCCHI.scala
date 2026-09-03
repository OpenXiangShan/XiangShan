package xiangshan.frontend.icache

import chisel3._
import chisel3.util._
import oceanus.compactchi._
import xiangshan.cache.DCacheCCHI

/*
 * Compact CHI Type 4 (read-only non-coherent) upstream port, ICache view.
 *
 * Active channels: TXREQ (ReadOnce) + RXDAT (CompData).
 */
class CCHIType4Port extends Bundle {
  val txreq = DecoupledIO(new FlitREQ)
  val rxdat = Flipped(DecoupledIO(new FlitDnDAT))
}

object ICacheCCHI {
  object Params {
    val srcId: UInt = L1CCHINodeId.ICacheSrcId
    val tgtId: UInt = L1CCHINodeId.L2TgtId
    val memAttr: UInt = DCacheCCHI.Params.memAttr
    val size64: UInt = DCacheCCHI.Params.size64
  }

  object Tx {
    def missReq(req: FlitREQ, txnId: UInt, addr: UInt, alias: UInt): Unit = {
      req.TxnID := txnId
      req.SrcID := Params.srcId
      req.TgtID := Params.tgtId
      req.Opcode := CCHIOpcode.ReadOnce.U
      req.Size := Params.size64
      req.Addr := addr(47, 0)
      req.alias := alias(1, 0)
      req.NS := false.B
      req.Order := 0.U
      req.MemAttr := Params.memAttr
      req.Excl := false.B
      req.ExpCompData := true.B
      req.WayValid := false.B
      req.Way := 0.U
      req.TraceTag := 0.U(1.W)
    }
  }

  object Rx {
    def denied(respErr: UInt): Bool = DCacheCCHI.Rx.denied(respErr)
    def corrupt(respErr: UInt): Bool = DCacheCCHI.Rx.corrupt(respErr)
  }
}
