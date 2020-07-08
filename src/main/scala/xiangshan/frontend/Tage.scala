package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

trait HasTageParameter {
  //                   Sets  Hist   Tag
  val TableInfo = Seq(( 128,    2,    7),
                      ( 128,    4,    7),
                      ( 256,    8,    8),
                      ( 256,   16,    8),
                      ( 128,   32,    9),
                      ( 128,   64,    9))
  val TageNTables = TableInfo.size
  val UBitPeriod = 2048
  val BankWidth = FetchWidth // 8
}

abstract class TageBundle extends XSBundle with HasTageParameter
abstract class TageModule extends XSModule with HasTageParameter

class TageReq extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
}

class TageResp extends TageBundle {
  val ctr = UInt(3.W)
  val u = UInt(2.W)
}

class TageUpdate extends TageBundle {
  val pc = UInt(VAddrBits.W)
  val hist = UInt(HistoryLength.W)
  // update tag and ctr
  val mask = Vec(BankWidth, Bool())
  val taken = Vec(BankWidth, Bool())
  val alloc = Vec(BankWidth, Bool())
  val oldCtr = Vec(BankWidth, UInt(3.W))
  // update u
  val uMask = Vec(BankWidth, Bool())
  val u = Vec(BankWidth, UInt(2.W))
}

class TageTable extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val resp = Output(Vec(BankWidth, Valid(new TageResp)))
    val update = Input(new TageUpdate)
  })

  // bypass entries for tage update
  val wrBypassEntries = 4

}

class Tage extends TageModule {
  val io = IO(new Bundle() {
    val req = Input(Valid(new TageReq))
    val out = new Bundle {
      val hits = Output(UInt(FetchWidth.W))
      val takens = Output(Vec(FetchWidth, Bool()))
    }
    val meta = Output(Vec(FetchWidth, (new TageMeta)))
    val redirectInfo = Flipped(new RedirectInfo)
  })

}