/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/
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
    val flush = Valid(new Redirect)
    val Predict = Vec(backendParams.LduCnt, Output(Bool()))
}
class Lvp(implicit p: Parameters) extends LvpModule{
    val io = IO(new LvpIO)

    val LvpTable = RegInit(VecInit.fill(EntryNum)(0.U.asTypeOf((new LvpEntry))))
    val LvpTableNext = WireInit(LvpTable)
    //init 
    val PredictValue = RegInit(VecInit(Seq.fill(backendParams.LduCnt)(0.U(XLEN.W))))
    val Predict = RegInit(VecInit(Seq.fill(backendParams.LduCnt)(false.B)))
    val misPredict = RegInit(Predict)
    io.flush := DontCare
    // VPE
    // temporarily don`t care same tag different load instruction
    io.fromload.zipWithIndex.foreach{ case (load, i) =>
        val index = load.pc(log2Ceil(EntryNum)-1, 0)
        val tagMatch = LvpTable(index).tag === load.pc(LvpTagLen-1, 0)
        val valueMatch = load.loadvalue === LvpTable(index).value
        val confidentEnough = LvpTable(index).confidence >= ThresHold.U

        when (load.loadvalid) {
            // write
            when (!tagMatch) {4
                // if don`t match, create new entry
                LvpTableNext(index).tag := load.pc(LvpTagLen-1, 0)
                LvpTableNext(index).value := load.loadvalue
                LvpTableNext(index).confidence := 0.U
            }.otherwise {
                // if match, update confidence
                // todo: flush when value mismatch
                LvpTableNext(index).confidence := Mux(valueMatch, (Mux(confidentEnough, LvpTable(index).confidence, LvpTable(index).confidence + 1.U)), 0.U)
                LvpTableNext(index).value := Mux(!valueMatch, load.loadvalue, LvpTable(index).value)
            }
            Predict(i) := tagMatch && valueMatch && confidentEnough
            PredictValue(i) := Mux(tagMatch && valueMatch && confidentEnough, LvpTable(index).value, 0.U)
            misPredict(i) := tagMatch && confidentEnough && !valueMatch
        }.otherwise {
            Predict(i) := false.B
            PredictValue(i) := 0.U
        }
        // delay 2 cycle to avoid writeback signal clear predict table
        io.Predict(i) := Predict(i)
        io.PredictValue(i) := PredictValue(i)
    }
    LvpTable := LvpTableNext
    XSPerfAccumulate("predict",             io.Predict.reduce(_ || _))
    XSPerfAccumulate("mispredict",          misPredict.reduce(_ || _))
}

