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
    val ThresHold = 63
    val EntryNum = 256
}

class LvpEntry(implicit p: Parameters) extends LvpBundle{
    val tag = UInt(LvpTagLen.W)
    val value = UInt(XLEN.W)
    val confidence = UInt(log2Ceil(ThresHold).W)
}

class LoadToLvp(implicit p: Parameters) extends LvpBundle{
    val loadvalid = Input(Bool())
    val pc = Input(UInt(VAddrBits.W))
    val loadvalue = Input(UInt(XLEN.W))
}

class DecodeToLvp(implicit p: Parameters) extends LvpBundle{
    val valid = Input(Bool())
    val pc = Input(UInt(VAddrBits.W))
    val pred = Output(Bool())
    val predValue = Output(UInt(XLEN.W))
}

class LvpIO(implicit p: Parameters) extends LvpBundle{
    val readPorts = Vec(DecodeWidth, new DecodeToLvp)
    val fromload = Vec(backendParams.LduCnt, new LoadToLvp)
}
class Lvp(implicit p: Parameters) extends LvpModule{
    val io = IO(new LvpIO)

    val LvpTable = RegInit(VecInit.fill(EntryNum)(0.U.asTypeOf((new LvpEntry))))
    val LvpTableNext = WireInit(LvpTable)
    // VPE
    // temporarily don`t care same tag different load instruction

    // load instr read pvt in decode stage
    io.readPorts.zipWithIndex.foreach{ case (read, i) =>
        when (read.valid) {
            val readIdx = read.pc(log2Ceil(EntryNum)-1, 0)
            when (LvpTable(readIdx).tag === read.pc(LvpTagLen-1, 0)) {
                read.pred := LvpTable(readIdx).confidence >= ThresHold.U
                read.predValue := LvpTable(readIdx).value
            }.otherwise {
                read.pred := false.B
                read.predValue := 0.U
            }
        }.otherwise {
            read.pred := false.B
            read.predValue := 0.U
        }
    }

    io.fromload.zipWithIndex.foreach { case (load, i) =>
        val index = load.pc(log2Ceil(EntryNum) - 1, 0)
        val tagMatch = LvpTable(index).tag === load.pc(LvpTagLen - 1, 0)
        val valueMatch = load.loadvalue === LvpTable(index).value
        val confidentEnough = LvpTable(index).confidence >= ThresHold.U

        when(load.loadvalid) {
            // write
            when(!tagMatch) {
                // if don`t match, create new entry
                LvpTableNext(index).tag := load.pc(LvpTagLen - 1, 0)
                LvpTableNext(index).value := load.loadvalue
                LvpTableNext(index).confidence := 0.U
            }.otherwise {
                // if match, update confidence
                LvpTableNext(index).confidence := Mux(valueMatch, (Mux(confidentEnough, LvpTable(index).confidence, LvpTable(index).confidence + 1.U)), 0.U)
                LvpTableNext(index).value := Mux(!valueMatch, load.loadvalue, LvpTable(index).value)
            }
        }
    }
    LvpTable := LvpTableNext
}

