package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

import scala.math.min

class SCReq extends TageReq

class SCResp(val ctrBits: Int = 6) extends TageBundle {
  val ctr = Vec(2, SInt(ctrBits.W))
}

class SCUpdate(val ctrBits: Int = 6) extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val fetchIdx = UInt(log2Up(TageBanks).W)
  val hist = UInt(HistoryLength.W)
  val mask = Vec(TageBanks, Bool())
  val oldCtr = SInt(ctrBits.W)
  val tagePred = Bool()
  val taken = Bool()
}

class SCTableIO extends TageBundle {
  val req = Input(Valid(new SCReq))
  val resp = Output(Vec(TageBanks, new SCResp))
  val update = Input(new SCUpdate)
}

abstract class BaseSCTable(val r: Int = 1024, val cb: Int = 6, val h: Int = 0) extends TageModule {
  val io = IO(new SCTableIO)
  def getCenteredValue(ctr: SInt): SInt = (ctr << 1) + 1.S
}

class FakeSCTable extends BaseSCTable {
  io.resp := Wire(0.U.asTypeOf(Vec(TageBanks, new SCResp)))
}

class SCTable(val nRows: Int, val ctrBits: Int, val histLen: Int) extends BaseSCTable(nRows, ctrBits, histLen) {

  val table = List.fill(TageBanks) {
    List.fill(2) {
      Module(new SRAMTemplate(SInt(ctrBits.W), set=nRows, shouldReset=false, holdRead=true, singlePort=false))
    }
  }

  def compute_folded_hist(hist: UInt, l: Int) = {
    if (histLen > 0) {
      val nChunks = (histLen + l - 1) / l
      val hist_chunks = (0 until nChunks) map {i =>
        hist(min((i+1)*l, histLen)-1, i*l)
      }
      hist_chunks.reduce(_^_)
    }
    else 0.U
  }

  def getIdx(hist: UInt, pc: UInt) = {
    (compute_folded_hist(hist, log2Ceil(nRows)) ^ (pc >> 1.U))(log2Ceil(nRows)-1,0)
  }

  def ctrUpdate(ctr: SInt, cond: Bool): SInt = signedSatUpdate(ctr, ctrBits, cond)
  
  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }

  val idx = getIdx(io.req.bits.hist, io.req.bits.pc)
  val idxLatch = RegEnable(idx, enable=io.req.valid)

  val table_r = WireInit(0.U.asTypeOf(Vec(TageBanks,Vec(2, SInt(ctrBits.W)))))

  val baseBank = io.req.bits.pc(log2Up(TageBanks), 1)
  val baseBankLatch = RegEnable(baseBank, enable=io.req.valid)

  val bankIdxInOrder = VecInit((0 until TageBanks).map(b => (baseBankLatch +& b.U)(log2Up(TageBanks)-1, 0)))
  val realMask = circularShiftLeft(io.req.bits.mask, TageBanks, baseBank)
  val maskLatch = RegEnable(io.req.bits.mask, enable=io.req.valid)

  val update_idx = getIdx(io.update.hist, io.update.pc - (io.update.fetchIdx << 1))
  val update_wdata = ctrUpdate(io.update.oldCtr, io.update.taken)


  for (b <- 0 until TageBanks) {
    for (i <- 0 to 1) {
      table(b)(i).reset := reset.asBool
      table(b)(i).io.r.req.valid := io.req.valid && realMask(b)
      table(b)(i).io.r.req.bits.setIdx := idx

      table_r(b)(i) := table(b)(i).io.r.resp.data(0)

      table(b)(i).io.w.req.valid := (io.update.mask(b) && i.U === io.update.tagePred.asUInt) || doing_reset
      table(b)(i).io.w.req.bits.setIdx := Mux(doing_reset, reset_idx, update_idx)
      table(b)(i).io.w.req.bits.data := Mux(doing_reset, 0.S, update_wdata)
    }
    
  }

  (0 until TageBanks).map(b => {
    io.resp(b).ctr := table_r(bankIdxInOrder(b))
  })

}

class SCThreshold(val ctrBits: Int = 5, val initVal: Int = 5) extends TageBundle {
  val thres = UInt(ctrBits.W)
  def update(cause: Bool): SCThreshold = {
    val res = Wire(new SCThreshold(this.ctrBits))
    res.thres := satUpdate(this.thres, this.ctrBits, cause)
    res
  }
}

object SCThreshold {
  def apply(bits: Int) = {
    val t = Wire(new SCThreshold(ctrBits=bits))
    t
  }
}