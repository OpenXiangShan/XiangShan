package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLPermissions
import oceanus.compactchi._

/*
 * Compact CHI Type 1 (fully coherent) upstream port, DCache view.
 *
 * Each channel is Decoupled (valid/ready). No P-Credit, Retry, or QoS.
 *
 * Ready (wired in later DCache steps, not here):
 *   TX*: ready from downstream; arbiter if several sources share a TX channel
 *   RXSNP: ready = free ProbeEntry; must not stall on TXREQ (spec §9.1)
 *   RXRSP/RXDAT: always ready
 */
class CCHIType1Port extends Bundle {
  // TX (DCache -> L2)
  val txevt = DecoupledIO(new FlitEVT)
  val txreq = DecoupledIO(new FlitREQ)
  val txrsp = DecoupledIO(new FlitUpRSP)
  val txdat = DecoupledIO(new FlitUpDAT)
  // RX (L2 -> DCache)
  val rxsnp = Flipped(DecoupledIO(new FlitSNP))
  val rxrsp = Flipped(DecoupledIO(new FlitDnRSP))
  val rxdat = Flipped(DecoupledIO(new FlitDnDAT))
}

/*
 * DCache-side Compact CHI helpers: phase-1 pinned params, TX builders, RX decoders.
 */
object DCacheCCHI {
  object Params {
    val srcId: UInt = 0.U
    val tgtId: UInt = 0.U
    // CHI MemAttr[3:0] = {Allocate, Cacheable, Device, EWA}; cacheable DCache: 0b1101
    val memAttr: UInt = "b1101".U(4.W)
    val size64: UInt = CCHISize.B64.U
  }

  object Tx {
    private def fillReq(req: FlitREQ, expCompData: Bool): Unit = {
      req.SrcID := Params.srcId
      req.TgtID := Params.tgtId
      req.Size := Params.size64
      req.NS := false.B
      req.Order := 0.U
      req.MemAttr := Params.memAttr
      req.Excl := false.B
      req.ExpCompData := expCompData
      req.WayValid := false.B
      req.Way := 0.U
      req.TraceTag := 0.U(1.W)
    }

    def fillEvt(evt: FlitEVT): Unit = {
      evt.SrcID := Params.srcId
      evt.TgtID := Params.tgtId
      evt.NS := false.B
      evt.WayValid := false.B
      evt.Way := 0.U
      evt.TraceTag := 0.U(1.W)
    }

    private def fillUpRsp(rsp: FlitUpRSP, traceTag: UInt = 0.U(1.W)): Unit = {
      rsp.SrcID := Params.srcId
      rsp.TgtID := Params.tgtId
      rsp.RespErr := 0.U
      rsp.TraceTag := traceTag
    }

    def fillUpDat(dat: FlitUpDAT, traceTag: UInt = 0.U(1.W)): Unit = {
      dat.SrcID := Params.srcId
      dat.TgtID := Params.tgtId
      dat.RespErr := 0.U
      dat.TraceTag := traceTag
    }

    def missReq(req: FlitREQ, txnId: UInt, addr: UInt, alias: UInt, growParam: UInt, fullOverwrite: Bool): Unit = {
      fillReq(req, expCompData = !fullOverwrite)
      req.TxnID := txnId
      req.Addr := addr(47, 0)
      req.alias := alias(1, 0)
      // fullOverwrite: whole-line store miss → MakeUnique (NtoT/BtoT grow unused)
      // growParam NtoB: load miss from Invalid → ReadShared; else NtoT/BtoT → ReadUnique
      req.Opcode := Mux(fullOverwrite, CCHIOpcode.MakeUnique.U,
        Mux(growParam === TLPermissions.NtoB, CCHIOpcode.ReadShared.U, CCHIOpcode.ReadUnique.U))
    }

    def cmoReq(req: FlitREQ, txnId: UInt, addr: UInt, cmoOpcode: UInt): Unit = {
      fillReq(req, expCompData = false.B)
      req.TxnID := txnId
      req.Addr := addr(47, 0)
      req.alias := 0.U(2.W)
      req.Opcode := Mux(cmoOpcode === 1.U, CCHIOpcode.CleanInvalid.U,
        Mux(cmoOpcode === 2.U, CCHIOpcode.MakeInvalid.U, CCHIOpcode.CleanShared.U))
    }

    def compAck(rsp: FlitUpRSP, dbid: UInt): Unit = {
      fillUpRsp(rsp)
      rsp.Opcode := CCHIOpcode.CompAck.U
      rsp.TxnID := dbid
      rsp.Resp := 0.U(3.W)
    }

    // TL shrink param (toN/toB/toT) + dirty → CHI SnpResp/SnpRespData Resp
    def probeResp(tlParam: UInt, dirty: Bool): UInt = {
      val base = MuxLookup(tlParam, CCHIResp.I.U)(Seq(
        TLPermissions.toN -> CCHIResp.I.U,
        TLPermissions.toB -> CCHIResp.SC.U,
        TLPermissions.toT -> CCHIResp.UC.U
      ))
      Mux(dirty, base | 0b100.U(3.W), base)
    }

    def evtEvict(evt: FlitEVT, txnId: UInt, addr: UInt): Unit = {
      fillEvt(evt)
      evt.Opcode := CCHIOpcode.Evict.U
      evt.TxnID := txnId
      evt.Addr := addr(47, 0)
    }

    def evtWriteBackFull(evt: FlitEVT, txnId: UInt, addr: UInt): Unit = {
      fillEvt(evt)
      evt.Opcode := CCHIOpcode.WriteBackFull.U
      evt.TxnID := txnId
      evt.Addr := addr(47, 0)
    }

    def snpResp(rsp: FlitUpRSP, txnId: UInt, tlParam: UInt, dirty: Bool, traceTag: UInt): Unit = {
      fillUpRsp(rsp, traceTag)
      rsp.Opcode := CCHIOpcode.SnpResp.U
      rsp.TxnID := txnId
      rsp.Resp := probeResp(tlParam, dirty)
    }

    def snpRespData(dat: FlitUpDAT, txnId: UInt, tlParam: UInt, dirty: Bool, dataId: UInt,
      beatData: UInt, corrupt: Bool, traceTag: UInt): Unit = {
      fillUpDat(dat, traceTag)
      dat.Opcode := CCHIOpcode.SnpRespData.U
      dat.TxnID := txnId
      dat.Resp := probeResp(tlParam, dirty)
      dat.DataID := dataId
      dat.Data := beatData
      dat.BE := Mux(corrupt, 0.U, ~0.U(32.W))
    }

    def copyBackWrData(dat: FlitUpDAT, dbid: UInt, dataId: UInt, beatData: UInt, corrupt: Bool,
      traceTag: UInt = 0.U(1.W)): Unit = {
      fillUpDat(dat, traceTag)
      dat.Opcode := CCHIOpcode.CopyBackWrData.U
      dat.TxnID := dbid
      dat.Resp := 0.U(3.W)
      dat.DataID := dataId
      dat.Data := beatData
      dat.BE := Mux(corrupt, 0.U, ~0.U(32.W))
    }
  }

  object Rx {
    // UC*→toT, SC*→toB, I*→toN; *_PD → dirty
    def grantParam(resp: UInt): UInt = {
      MuxLookup(resp(1, 0), TLPermissions.toN)(Seq(
        2.U(2.W) -> TLPermissions.toT, // UC
        1.U(2.W) -> TLPermissions.toB, // SC
        0.U(2.W) -> TLPermissions.toN  // I
      ))
    }
    def dirty(resp: UInt): Bool = CCHIResp.isPD(resp)
    def denied(respErr: UInt): Bool = respErr === "b11".U // NDERR
    def corrupt(respErr: UInt): Bool = respErr === "b10".U || respErr === "b11".U // DERR | NDERR
  }
}
