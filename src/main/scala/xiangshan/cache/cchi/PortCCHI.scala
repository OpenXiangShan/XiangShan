package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLPermissions
import oceanus.compactchi._
// CCHIParametersKey lives in xscache.oceanus.compactchi (see that package's Flit.scala)
import xscache.oceanus.compactchi.CCHIParametersKey
import org.chipsalliance.cde.config.Parameters

/*
 * Compact CHI Type 1 (fully coherent) upstream port, DCache view.
 *
 * Each channel is Decoupled (valid/ready). No P-Credit, Retry, or QoS.
 */
class CCHIType1Port(implicit p: Parameters) extends Bundle {
  // up (DCache -> L2)
  val upEVT = DecoupledIO(new FlitEVT)
  val upREQ = DecoupledIO(new FlitREQ)
  val upRSP = DecoupledIO(new FlitUpRSP)
  val upDAT = DecoupledIO(new FlitUpDAT)
  // dn (L2 -> DCache)
  val dnSNP = Flipped(DecoupledIO(new FlitSNP))
  val dnRSP = Flipped(DecoupledIO(new FlitDnRSP))
  val dnDAT = Flipped(DecoupledIO(new FlitDnDAT))
}

/*
 * Compact CHI Type 4 (read-only non-coherent) upstream port.
 *
 * Active channels: upREQ (ReadOnce) + dnDAT (CompData).
 * Used by ICache and PTW (L2TLB).
 */
class CCHIType4Port(implicit p: Parameters) extends Bundle {
  // up (ICache/PTW -> L2)
  val upREQ = DecoupledIO(new FlitREQ)
  // dn (L2 -> ICache/PTW)
  val dnDAT = Flipped(DecoupledIO(new FlitDnDAT))
}

/*
 * DCache-side Compact CHI helpers: phase-1 pinned params, TX builders, RX decoders.
 */
object DCacheCCHI {
  /** Width of a downstream (L2) node ID: the SrcID on Comp/CompData/DBIDResp/Snp,
    * echoed back as TgtID on CompAck/SnpResp/SnpRespData/CopyBackWrData. Read from
    * the compactchi parameter behind Flit*Dn.SrcID / Flit*Up.TgtID so these IDs can
    * never drift from the flit fields they mirror (a mismatch truncates silently).
    */
  def dnNodeIdWidth(implicit p: Parameters): Int = p(CCHIParametersKey).DownstreamNodeID_Width

  /** Width of an L2-issued transaction ID: DBID on FlitDnRSP/FlitDnDAT and TxnID on
    * FlitSNP, echoed back as TxnID on FlitUpRSP/FlitUpDAT. All of those flit fields
    * take DBID_Width (the L1's own REQ TxnID is the separate TxnID_Width).
    */
  def dnTxnIdWidth(implicit p: Parameters): Int = p(CCHIParametersKey).DBID_Width

  /** Width of FlitREQ.TagAlias (TagAlias_Width). */
  def tagAliasWidth(implicit p: Parameters): Int = p(CCHIParametersKey).TagAlias_Width

  object Params {
    // REQ/EVT TgtID is rewritten by L2 eSAM, so the placeholder below is fine for them.
    // Upstream RSP/DAT (CompAck/SnpResp/SnpRespData/CopyBackWrData) is NOT rewritten:
    // L2 routes UpRSP/UpDAT to slices by TgtID (L2Top postSAM), so those helpers take
    // the responding node's SrcID as an explicit tgtId argument instead of this value.
    val tgtId: UInt = 0.U
    // CHI MemAttr[3:0] = {Allocate, Cacheable, Device, EWA}; cacheable DCache: 0b1101
    val memAttr: UInt = "b1101".U(4.W)
    val size64: UInt = CCHISize.B64.U
  }

  object Tx {
    private def fillReq(req: FlitREQ, expCompData: Bool, srcId: UInt): Unit = {
      req.SrcID := srcId
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

    def fillEvt(evt: FlitEVT, srcId: UInt): Unit = {
      evt.SrcID := srcId
      evt.TgtID := Params.tgtId
      evt.NS := false.B
      evt.MemAttr := false.B
      evt.WayValid := false.B
      evt.Way := 0.U
      evt.TraceTag := 0.U(1.W)
    }

    // tgtId: SrcID of the flit being answered (grant / DBIDResp / Snp) — L2 routes by it.
    private def fillUpRsp(rsp: FlitUpRSP, tgtId: UInt, srcId: UInt, traceTag: UInt = 0.U(1.W)): Unit = {
      rsp.SrcID := srcId
      rsp.TgtID := tgtId
      rsp.RespErr := 0.U
      rsp.TraceTag := traceTag
    }

    def fillUpDat(dat: FlitUpDAT, tgtId: UInt, srcId: UInt, traceTag: UInt = 0.U(1.W)): Unit = {
      dat.SrcID := srcId
      dat.TgtID := tgtId
      dat.RespErr := 0.U
      dat.TraceTag := traceTag
    }

    def missReq(req: FlitREQ, txnId: UInt, addr: UInt, alias: UInt, growParam: UInt, fullOverwrite: Bool, srcId: UInt): Unit = {
      // alias is at most TagAlias_Width bits; a wider one would truncate silently below
      require(alias.getWidth <= req.paramCCHI.TagAlias_Width,
        s"alias is ${alias.getWidth}b but FlitREQ.TagAlias is ${req.paramCCHI.TagAlias_Width}b")
      fillReq(req, expCompData = !fullOverwrite, srcId)
      req.TxnID := txnId
      req.Addr := addr(47, 0)
      req.TagAlias := alias
      // fullOverwrite: whole-line store miss → MakeUnique (NtoT/BtoT grow unused)
      // growParam NtoB: load miss from Invalid → ReadShared; else NtoT/BtoT → ReadUnique
      req.Opcode := Mux(fullOverwrite, CCHIOpcode.MakeUnique.U,
        Mux(growParam === TLPermissions.NtoB, CCHIOpcode.ReadShared.U, CCHIOpcode.ReadUnique.U))
    }

    def cmoReq(req: FlitREQ, txnId: UInt, addr: UInt, cmoOpcode: UInt, srcId: UInt): Unit = {
      fillReq(req, expCompData = false.B, srcId)
      req.TxnID := txnId
      req.Addr := addr(47, 0)
      req.TagAlias := 0.U(req.paramCCHI.TagAlias_Width.W)
      req.Opcode := Mux(cmoOpcode === 1.U, CCHIOpcode.CleanInvalid.U,
        Mux(cmoOpcode === 2.U, CCHIOpcode.MakeInvalid.U, CCHIOpcode.CleanShared.U))
    }

    // tgtId = grant/Comp SrcID, dbid = grant/Comp DBID (both echoed back).
    def compAck(rsp: FlitUpRSP, dbid: UInt, tgtId: UInt, srcId: UInt): Unit = {
      fillUpRsp(rsp, tgtId, srcId)
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

    def evtEvict(evt: FlitEVT, txnId: UInt, addr: UInt, srcId: UInt): Unit = {
      fillEvt(evt, srcId)
      evt.Opcode := CCHIOpcode.Evict.U
      evt.TxnID := txnId
      evt.Addr := addr(47, 0)
    }

    def evtWriteBackFull(evt: FlitEVT, txnId: UInt, addr: UInt, srcId: UInt): Unit = {
      fillEvt(evt, srcId)
      evt.Opcode := CCHIOpcode.WriteBackFull.U
      evt.TxnID := txnId
      evt.Addr := addr(47, 0)
    }

    // tgtId = Snp SrcID, txnId = Snp TxnID (both echoed back).
    def snpResp(rsp: FlitUpRSP, txnId: UInt, tgtId: UInt, tlParam: UInt, dirty: Bool, traceTag: UInt, srcId: UInt): Unit = {
      fillUpRsp(rsp, tgtId, srcId, traceTag)
      rsp.Opcode := CCHIOpcode.SnpResp.U
      rsp.TxnID := txnId
      rsp.Resp := probeResp(tlParam, dirty)
    }

    def snpRespData(dat: FlitUpDAT, txnId: UInt, tgtId: UInt, tlParam: UInt, dirty: Bool, dataId: UInt,
      beatData: UInt, corrupt: Bool, traceTag: UInt, srcId: UInt): Unit = {
      fillUpDat(dat, tgtId, srcId, traceTag)
      dat.Opcode := CCHIOpcode.SnpRespData.U
      dat.TxnID := txnId
      dat.Resp := probeResp(tlParam, dirty)
      dat.DataID := dataId
      dat.Data := beatData
      // FlitUpDAT.BE is one bit per byte of a Data_Width beat
      dat.BE := Mux(corrupt, 0.U, ~0.U((dat.paramCCHI.Data_Width / 8).W))
    }

    // tgtId = DBIDResp/CompDBIDResp SrcID, dbid = its DBID (both echoed back).
    def copyBackWrData(dat: FlitUpDAT, dbid: UInt, tgtId: UInt, dataId: UInt, beatData: UInt, corrupt: Bool,
      srcId: UInt, traceTag: UInt = 0.U(1.W)): Unit = {
      fillUpDat(dat, tgtId, srcId, traceTag)
      dat.Opcode := CCHIOpcode.CopyBackWrData.U
      dat.TxnID := dbid
      dat.Resp := 0.U(3.W)
      dat.DataID := dataId
      dat.Data := beatData
      // FlitUpDAT.BE is one bit per byte of a Data_Width beat
      dat.BE := Mux(corrupt, 0.U, ~0.U((dat.paramCCHI.Data_Width / 8).W))
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

object ICacheCCHI {
  object Params {
    val tgtId: UInt = DCacheCCHI.Params.tgtId
    val memAttr: UInt = DCacheCCHI.Params.memAttr
    val size64: UInt = DCacheCCHI.Params.size64
  }

  object Tx {
    def missReq(req: FlitREQ, txnId: UInt, addr: UInt, alias: UInt, srcId: UInt): Unit = {
      // alias is at most TagAlias_Width bits; a wider one would truncate silently below
      require(alias.getWidth <= req.paramCCHI.TagAlias_Width,
        s"alias is ${alias.getWidth}b but FlitREQ.TagAlias is ${req.paramCCHI.TagAlias_Width}b")
      req.TxnID := txnId
      req.SrcID := srcId
      req.TgtID := Params.tgtId
      req.Opcode := CCHIOpcode.ReadOnce.U
      req.Size := Params.size64
      req.Addr := addr(47, 0)
      req.TagAlias := alias
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

object PtwCCHI {
  object Params {
    val tgtId: UInt = DCacheCCHI.Params.tgtId
    val memAttr: UInt = DCacheCCHI.Params.memAttr
    val size64: UInt = DCacheCCHI.Params.size64
  }

  object Tx {
    def readReq(req: FlitREQ, txnId: UInt, addr: UInt, srcId: UInt): Unit = {
      req.TxnID := txnId
      req.SrcID := srcId
      req.TgtID := Params.tgtId
      req.Opcode := CCHIOpcode.ReadOnce.U
      req.Size := Params.size64
      req.Addr := addr(47, 0)
      req.TagAlias := 0.U(req.paramCCHI.TagAlias_Width.W)
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

object CCHIConnect {
  def type1(l2: CCHIInterfaceType1, l1: CCHIType1Port): Unit = {
    l2.UpEVT <> l1.upEVT
    l2.UpREQ <> l1.upREQ
    l1.dnSNP <> l2.DnSNP
    l2.UpRSP <> l1.upRSP
    l2.UpDAT <> l1.upDAT
    l1.dnRSP <> l2.DnRSP
    l1.dnDAT <> l2.DnDAT
  }

  def type4(l2: CCHIInterfaceType4, l1: CCHIType4Port): Unit = {
    l2.UpREQ <> l1.upREQ
    l1.dnDAT <> l2.DnDAT
  }
}
