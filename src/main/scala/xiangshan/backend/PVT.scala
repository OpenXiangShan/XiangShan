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
package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import org.codehaus.plexus.classworlds.strategy.ParentFirstStrategy
import xiangshan.backend.datapath.DataConfig.IntData

class PvtReadPort(implicit p: Parameters) extends PvtBundle {
    val valid = Input(Bool())
    val addr = Input(UInt(PvtTagLen.W))
    val data = Output(UInt(XLEN.W))
}

class PvtWritePort(implicit p: Parameters) extends PvtBundle {
    val wen = Input(Bool())
    val addr = Input(UInt(PvtTagLen.W))
    val data = Input(UInt(XLEN.W))
}

abstract class PvtBundle(implicit p: Parameters) extends XSBundle with HasPvtConst

abstract class PvtModule(implicit p: Parameters) extends XSModule with HasPvtConst

trait HasPvtConst extends HasXSParameter {
    val PvtTagLen = PhyRegIdxWidth
    val EntryNum = MaxPhyRegs
    val ReadPortNum = 8
    val WritePortNum = RenameWidth
}

class PvtEntry(implicit p: Parameters) extends PvtBundle{
    val valid = Bool()
    val tag = UInt(PvtTagLen.W)
    val value = UInt(XLEN.W)
}

class PvtIO(implicit p: Parameters) extends PvtBundle{
    val readPorts = Vec(ReadPortNum, new PvtReadPort)
    val writePorts = Vec(WritePortNum, new PvtWritePort)
    val flush = Input(Bool())
    val full = Output(Bool())
    val writeFail = Vec(WritePortNum, Output(Bool()))
    val pvtUpdate = Vec(backendParams.numPregWb(IntData()), Input(UInt(PhyRegIdxWidth.W)))
    val pvtUpdateFrmRename = Vec(RenameWidth, Input(UInt(PhyRegIdxWidth.W)))
}

class Pvt(implicit p: Parameters) extends PvtModule{
    val io = IO(new PvtIO)

    val PvtTable = RegInit(VecInit.fill(EntryNum)(0.U.asTypeOf(new PvtEntry)))
    val PvtTableNext = WireInit(PvtTable)
    io.writeFail := VecInit.fill(WritePortNum)(false.B)

    //write
    io.writePorts.zipWithIndex.foreach{ case (w, i) =>
        val windex = w.addr(log2Ceil(EntryNum)-1, 0)
        //todo: same tag different ld
        when (w.wen){
            // can not find match entry, create a new entry
            PvtTableNext(windex).value := w.data
            PvtTableNext(windex).tag := w.addr
            PvtTableNext(windex).valid := true.B
        }
    }

    // pvt wb update
    io.pvtUpdate.zipWithIndex.foreach{ case (pvtUpdate, i) =>
        when (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).valid &&
          (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).tag === pvtUpdate)) {
            PvtTableNext(pvtUpdate(log2Ceil(EntryNum)-1, 0)) := 0.U.asTypeOf(new PvtEntry)
        }
    }
    io.pvtUpdateFrmRename.zipWithIndex.foreach{ case (pvtUpdate, i) =>
        when (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).valid &&
          (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).tag === pvtUpdate)) {
            PvtTableNext(pvtUpdate(log2Ceil(EntryNum)-1, 0)) := 0.U.asTypeOf(new PvtEntry)
        }
    }

    PvtTable := PvtTableNext

    // pvt read
    for ((r, i) <- io.readPorts.zipWithIndex) {
        val rindex = r.addr(log2Ceil(EntryNum)-1, 0)
        r.data := PvtTable(rindex).value
        when (r.valid) {
            assert(PvtTable(rindex).valid && PvtTable(rindex).tag === r.addr, "Pvt readports $i read a invalid entry")
        }
    }

    when (io.flush) {
        PvtTable := 0.U.asTypeOf(Vec(EntryNum, new PvtEntry))
    }

    io.full := PvtTable.map(_.valid).reduce(_ && _)
}
