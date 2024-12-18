package xiangshan.frontend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils._
import utility._
import xiangshan._
import chiseltest.fork

abstract class LvpBundle(implicit p: Parameters) extends XSBundle with HasLvpConst

abstract class LvpModule(implicit p: Parameters) extends XSModule with HasLvpConst

trait HasLvpConst extends HasXSParameter {
    val LvpTagLen = 14
    val ThresHold = 7
    val EntryNum = 256
}

class LvpEntry(implicit p: Parameters) extends LvpBundle{
    val tag = UInt(LvpTagLen.W)
    val value = UInt(XLEN.W)
    val confidence = UInt(3.W)
}

class LoadToLvp(implicit p: Parameters) extends LvpBundle{
    val loadvalid = Input(Bool())
    val pc = Input(UInt(VAddrBits.W))
    val loadvalue = Input(UInt(XLEN.W))
}

class LvpIO(implicit p: Parameters) extends LvpBundle{
    val fromload = Vec(backendParams.LduCnt, new LoadToLvp)
    val PredictValue = Vec(backendParams.LduCnt,Output(UInt(XLEN.W)))
    val flush = Output(Bool())
    val Predict = Vec(backendParams.LduCnt, Output(Bool()))
}
class Lvp(implicit p: Parameters) extends LvpModule{
    val io = IO(new LvpIO)

    val LvpTable = RegInit(VecInit.fill(EntryNum)(0.U.asTypeOf((new LvpEntry))))
    val LvpTableNext = WireInit(LvpTable)
    //init 
    val PredictValue = RegInit(VecInit(Seq.fill(backendParams.LduCnt)(0.U(XLEN.W))))
    io.flush := false.B
    val Predict = RegInit(VecInit(Seq.fill(backendParams.LduCnt)(false.B)))
    // VPE
    // temporarily don`t care same tag different load instruction
    io.fromload.zipWithIndex.foreach{ case (load, i) =>
        val index = load.pc(log2Ceil(EntryNum)-1, 0)
        val tagMatch = LvpTable(index).tag === load.pc(LvpTagLen-1, 0)
        val valueMatch = load.loadvalue === LvpTable(index).value
        val confidentEnough = LvpTable(index).confidence >= ThresHold.U

        when (load.loadvalid) {
            // write
            when (!tagMatch) {
                // if don`t match, create new entry
                LvpTableNext(index).tag := load.pc(LvpTagLen-1, 0)
                LvpTableNext(index).value := load.loadvalue
                LvpTableNext(index).confidence := 0.U
            }.otherwise {
                // if match, update confidence
                LvpTableNext(index).confidence := Mux(valueMatch, (Mux(confidentEnough, LvpTable(index).confidence, LvpTable(index).confidence + 1.U)), 0.U)
                LvpTableNext(index).value := Mux(!valueMatch, load.loadvalue, LvpTable(index).value)
            }
            Predict(i) := tagMatch && valueMatch && confidentEnough
            PredictValue(i) := Mux(tagMatch && valueMatch && confidentEnough, LvpTable(index).value, 0.U)
        }.otherwise {
            Predict(i) := false.B
            PredictValue(i) := 0.U
        }
        // delay 2 cycle to avoid writeback signal clear predict table
        io.Predict(i) := RegNext(Predict(i))
        io.PredictValue(i) := RegNext(PredictValue(i))
    }
    LvpTable := LvpTableNext
}

