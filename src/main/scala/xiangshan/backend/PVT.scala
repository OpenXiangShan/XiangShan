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
import xiangshan.backend.rename.RatPredPort
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.LoadToPvt

class PvtReadPort(implicit p: Parameters) extends PvtBundle {
    val valid = Input(Bool())
    val addr = Input(UInt(PvtTagLen.W))
    val data = Output(UInt(XLEN.W))
}

class PvtWritePort(implicit p: Parameters) extends PvtBundle {
    val wen = Input(Bool())
    val addr = Input(UInt(PvtTagLen.W))
    val data = Input(UInt(XLEN.W))
    val robIdx = Input(new RobPtr)
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
    val robIdx = new RobPtr
    val used = Bool()
}

class PvtIO(implicit p: Parameters) extends PvtBundle{
    val readPorts = Vec(ReadPortNum, new PvtReadPort)
    val writePorts = Vec(WritePortNum, new PvtWritePort)
    val flush = Flipped(ValidIO(new Redirect))
    val full = Output(Bool())
    val writeFail = Vec(WritePortNum, Output(Bool()))
    val pvtUpdate = Vec(backendParams.numPregWb(IntData()), Input(UInt(PhyRegIdxWidth.W)))
    val pvtUpdateFrmRename = Vec(RenameWidth, Input(UInt(PhyRegIdxWidth.W)))
    val fromLoad = Vec(backendParams.LduCnt, Flipped(Valid(new LoadToPvt)))
    val misPred = Vec(backendParams.LduCnt, ValidIO(new Redirect))
    val intSrcPred = Vec(ReadPortNum*3, new RatPredPort)
    val fpSrcPred = Vec(ReadPortNum*3, new RatPredPort)
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
            PvtTableNext(windex).robIdx := w.robIdx
            PvtTableNext(windex).used := false.B
        }
    }

    // pvt wb update
    io.pvtUpdate.zipWithIndex.foreach{ case (pvtUpdate, i) =>
        when (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).valid &&
          (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).tag === pvtUpdate)) {
            PvtTableNext(pvtUpdate(log2Ceil(EntryNum)-1, 0)).valid := false.B
        }
    }
    io.pvtUpdateFrmRename.zipWithIndex.foreach{ case (pvtUpdate, i) =>
        when (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).valid &&
          (PvtTable(pvtUpdate(log2Ceil(EntryNum)-1, 0)).tag === pvtUpdate)) {
            PvtTableNext(pvtUpdate(log2Ceil(EntryNum)-1, 0)).valid := false.B
        }
    }

    // pvt read
    for ((r, i) <- io.readPorts.zipWithIndex) {
        val rindex = r.addr(log2Ceil(EntryNum)-1, 0)
        r.data := PvtTable(rindex).value
        when (r.valid) {
            PvtTableNext(rindex).used := true.B
            assert(PvtTable(rindex).valid && PvtTable(rindex).tag === r.addr, "Pvt readports $i read a invalid entry")
        }
    }

    for ((pred, i) <- io.intSrcPred.zipWithIndex) {
        val rindex = pred.addr(log2Ceil(EntryNum)-1, 0)
        pred.pred := PvtTable(rindex).valid
    }
    io.fpSrcPred.foreach{_.pred := DontCare}

    //load verify
    for ((veri, i) <- io.fromLoad.zipWithIndex) {
        val index = veri.bits.pdest(log2Ceil(EntryNum)-1, 0)
        val entryMatch = PvtTable(index).robIdx === veri.bits.misPredict.robIdx && PvtTable(index).tag === veri.bits.pdest
        val misMatch = PvtTable(index).value =/= veri.bits.loadvalue && entryMatch
        when (veri.valid) {
            val needCancel = WireInit(VecInit((0 until EntryNum).map(i => {
                PvtTable(i).robIdx.needFlush(veri.bits.misPredict)
            })))
            // flush pvt selectively, used means that has influence to after instr
            when (misMatch && PvtTable(index).used) {
                PvtTableNext.zip(needCancel).foreach { case (entry, flush) =>
                    when (flush) {entry := 0.U.asTypeOf(new PvtEntry)}
                }
            }.elsewhen (entryMatch) { PvtTableNext(index) := 0.U.asTypeOf(new PvtEntry) }
        }
        io.misPred(i).valid := misMatch && PvtTable(index).used && veri.valid
        io.misPred(i).bits := veri.bits.misPredict
    }
    // other redirect, flush after
    when (io.flush.valid) {
        val needCancel = WireInit(VecInit((0 until EntryNum).map(i => {
            PvtTable(i).robIdx.needFlush(io.flush)
        })))
        PvtTableNext.zip(needCancel).foreach { case (entry, flush) =>
            when (flush) {entry := 0.U.asTypeOf(new PvtEntry)}
        }
    }
    PvtTable := PvtTableNext
    io.full := PvtTable.map(_.valid).reduce(_ && _)
}
