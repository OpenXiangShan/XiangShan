package cache.TLCTest

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import chipsalliance.rocketchip.config.Parameters

import scala.util.Random
import chisel3.util._

import scala.math.pow

class TLULMasterAgent(ID: Int, val maxSink: Int, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)]
                      , scoreboard: mutable.Map[BigInt, BigInt])
                     (implicit p: Parameters)
  extends TLCAgent(ID, addrStateMap, serialList, scoreboard) {
  val outerGet: mutable.Map[BigInt, GetCallerTrans] = mutable.Map[BigInt, GetCallerTrans]()
  val outerPut: mutable.Map[BigInt, PutCallerTrans] = mutable.Map[BigInt, PutCallerTrans]()

  var tmpA = new TLCScalaA()
  var a_cnt = 0
  var a_cnt_end = 0
  var tmpD = new TLCScalaD()
  var d_cnt = 0
  var d_cnt_end = 0

  def fireD(inD: TLCScalaD): Unit = {
    if (inD.opcode == AccessAckData) {
      d_cnt_end = {
        if (inD.size.pow(2) <= l2params.beatBytes)
          1
        else
          (inD.size.pow(2) / l2params.beatBytes).toInt
      }
      if (d_cnt == 0) { //start burst
        tmpD = inD.copy()
        d_cnt += 1
      }
      else {
        tmpD.data = dataConcatBeat(tmpD.data, inD.data, d_cnt)
        d_cnt += 1
      }
      if (d_cnt == d_cnt_end) {
        d_cnt = 0
        val getT = outerGet(inD.source)
        getT.pairAccessAckData(tmpD)
        insertMaskedRead(getT.a.get.address, tmpD.data, getT.a.get.mask)
        outerGet.remove(inD.source)
      }
    }
    else if (inD.opcode == AccessAck) {
      val putT = outerPut(inD.source)
      insertMaskedWrite(putT.a.get.address, putT.a.get.data, putT.a.get.mask)
      outerPut.remove(inD.source)
    }
    else {
      assert(false, f"UL unhandled opcode: ${inD.opcode}")
    }
  }

  def fireA(inA: TLCScalaA): Unit = {
    if (inA.opcode == Get) {
      handleA(inA)
    }
    else if (inA.opcode == PutFullData || inA.opcode == PutPartialData) {
      if (a_cnt == 0) { //start burst
        a_cnt_end = {
          if (inA.size.pow(2) <= l2params.beatBytes)
            1
          else
            (inA.size.pow(2) / l2params.beatBytes).toInt
        }
        tmpA = inA.copy()
        a_cnt += 1
      }
      else {
        tmpA.mask = maskConcatBeat(tmpA.mask, inA.mask, a_cnt)
        tmpA.data = dataConcatBeat(tmpA.data, inA.data, a_cnt)
        a_cnt += 1
      }
      if (a_cnt == a_cnt_end) {
        a_cnt = 0
        handleA(tmpA)
      }
    }
    else {
      assert(false, f"UL unhandled opcode: ${inA.opcode}")
    }
  }

  def handleA(inA: TLCScalaA): Unit = {
    if (inA.opcode == Get) {
      val getT = new GetCallerTrans()
      if (inA.size.pow(2) > l2params.beatBytes) { //fill n mask if there are n beats
        inA.mask = (0 until (inA.size.pow(2) / l2params.beatBytes).toInt).foldLeft(BigInt(0))(
          (cmask, i) => maskConcatBeat(cmask, inA.mask, i))
      }
      getT.pairGet(inA)
      outerGet(inA.source) = getT
    }
    else if (inA.opcode == PutFullData || inA.opcode == PutPartialData) {
      val putT = new PutCallerTrans()
      putT.pairPut(inA)
      outerPut(inA.source) = putT
    }
  }

}