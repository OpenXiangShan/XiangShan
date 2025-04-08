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
import xiangshan.backend.datapath.DataConfig.{FpData, IntData}
import xiangshan.backend.rename.RatPredPort
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.LoadToPvt

class PvtReadPort(implicit p: Parameters) extends PvtBundle {
    val valid = Input(Bool())
    val addr = Input(UInt(PvtTagLen.W))
    val data = Output(UInt(XLEN.W))
    val fail = Output(Bool()) // for verify and read at the same time
}

class PvtWritePort(implicit p: Parameters) extends PvtBundle {
    val wen = Input(Bool())
    val rfWen = Input(Bool())
    val fpWen = Input(Bool())
    val addr = Input(UInt(PvtTagLen.W))
    val data = Input(UInt(XLEN.W))
    val robIdx = Input(new RobPtr)
}

abstract class PvtBundle(implicit p: Parameters) extends XSBundle with HasPvtConst

abstract class PvtModule(implicit p: Parameters) extends XSModule with HasPvtConst

trait HasPvtConst extends HasXSParameter {
    val PvtTagLen = PhyRegIdxWidth
    val IntEntryNum = IntPhyRegs
    val FpEntryNum = FpPhyRegs
    val IntReadPortNum = 8
    val FpReadPortNum = 10
    val WritePortNum = RenameWidth
}

class PvtEntry(implicit p: Parameters) extends PvtBundle{
    val valid = Bool()
    val tag = UInt(PvtTagLen.W)
    val value = UInt(XLEN.W)
    val robIdx = new RobPtr
    val used = Bool()
}

class UpdateFrmRename(implicit p: Parameters) extends PvtBundle{
    val rfWen = Input(Bool())
    val fpWen = Input(Bool())
    val addr = Input(UInt(PhyRegIdxWidth.W))
}

class PvtIO(implicit p: Parameters) extends PvtBundle{
    val intReadPorts = Vec(IntReadPortNum, new PvtReadPort)
    val fpReadPorts = Vec(FpReadPortNum, new PvtReadPort)
    val writePorts = Vec(WritePortNum, new PvtWritePort)
    val flush = Flipped(ValidIO(new Redirect))
    val full = Output(Bool())
    val intPvtUpdate = Vec(backendParams.numPregWb(IntData()), Input(UInt(PhyRegIdxWidth.W)))
    val fpPvtUpdate = Vec(backendParams.numPregWb(FpData()), Input(UInt(PhyRegIdxWidth.W)))
    val pvtUpdateFrmRename = Vec(RenameWidth, new UpdateFrmRename)
    val fromLoad = Vec(backendParams.LduCnt, Flipped(Valid(new LoadToPvt)))
    val misPred = Vec(backendParams.LduCnt, ValidIO(new Redirect))
    val intSrcPred = Vec(IntReadPortNum*3, new RatPredPort)
    val fpSrcPred = Vec(FpReadPortNum*3, new RatPredPort)
}

class Pvt(implicit p: Parameters) extends PvtModule{
    val io = IO(new PvtIO)

    val IntPvtTable = RegInit(VecInit.fill(IntEntryNum)(0.U.asTypeOf(new PvtEntry)))
    val IntPvtTableNext = WireInit(IntPvtTable)
    val FpPvtTable = RegInit(VecInit.fill(FpEntryNum)(0.U.asTypeOf(new PvtEntry)))
    val FpPvtTableNext = WireInit(FpPvtTable)

    //write
    io.writePorts.zipWithIndex.foreach{ case (w, i) =>
        //todo: same tag different ld
        when (w.wen && w.rfWen){
            // can not find match entry, create a new entry
            val windex = w.addr(log2Ceil(IntEntryNum)-1, 0)
            IntPvtTableNext(windex).value := w.data
            IntPvtTableNext(windex).tag := w.addr
            IntPvtTableNext(windex).valid := true.B
            IntPvtTableNext(windex).robIdx := w.robIdx
            IntPvtTableNext(windex).used := false.B
        }.elsewhen (w.wen && w.fpWen){
            val windex = w.addr(log2Ceil(FpEntryNum)-1, 0)
            FpPvtTableNext(windex).value := w.data
            FpPvtTableNext(windex).tag := w.addr
            FpPvtTableNext(windex).valid := true.B
            FpPvtTableNext(windex).robIdx := w.robIdx
            FpPvtTableNext(windex).used := false.B
        }
    }

    // pvt wb update
    io.intPvtUpdate.zipWithIndex.foreach{ case (pvtUpdate, i) =>
        when (IntPvtTable(pvtUpdate(log2Ceil(IntEntryNum)-1, 0)).valid &&
          (IntPvtTable(pvtUpdate(log2Ceil(IntEntryNum)-1, 0)).tag === pvtUpdate)) {
            IntPvtTableNext(pvtUpdate(log2Ceil(IntEntryNum)-1, 0)).valid := false.B
        }
    }
    io.fpPvtUpdate.zipWithIndex.foreach{ case (pvtUpdate, i) =>
        when (FpPvtTable(pvtUpdate(log2Ceil(IntEntryNum)-1, 0)).valid &&
          (FpPvtTable(pvtUpdate(log2Ceil(IntEntryNum)-1, 0)).tag === pvtUpdate)) {
            FpPvtTableNext(pvtUpdate(log2Ceil(IntEntryNum)-1, 0)).valid := false.B
        }
    }

    io.pvtUpdateFrmRename.foreach{ update =>
        when (update.rfWen) {
            when (IntPvtTable(update.addr(log2Ceil(IntEntryNum)-1, 0)).valid &&
              (IntPvtTable(update.addr(log2Ceil(IntEntryNum)-1, 0)).tag === update.addr)) {
                IntPvtTableNext(update.addr(log2Ceil(IntEntryNum)-1, 0)).valid := false.B
            }
        }.elsewhen (update.fpWen) {
            when (FpPvtTable(update.addr(log2Ceil(FpEntryNum)-1, 0)).valid &&
              (FpPvtTable(update.addr(log2Ceil(FpEntryNum)-1, 0)).tag === update.addr)) {
                FpPvtTableNext(update.addr(log2Ceil(FpEntryNum)-1, 0)).valid := false.B
            }
        }
    }

    // pvt read
    for ((r, i) <- io.intReadPorts.zipWithIndex) {
        val rindex = r.addr(log2Ceil(IntEntryNum)-1, 0)
        r.data := IntPvtTable(rindex).value
        val readFail = io.fromLoad.map{ veri =>
            val veriIdx = veri.bits.pdest(log2Ceil(IntEntryNum)-1, 0)
            val readConflict = veri.bits.pdest === r.addr && r.valid && veri.valid
            val misPred = IntPvtTable(veriIdx).robIdx === veri.bits.misPredict.robIdx && veri.valid &&
              IntPvtTable(veriIdx).tag === veri.bits.pdest && IntPvtTable(veriIdx).value =/= veri.bits.loadvalue
            readConflict && misPred
        }.reduce(_ || _)
        r.fail := readFail
        when (r.valid) {
            IntPvtTableNext(rindex).used := true.B
            assert(IntPvtTable(rindex).valid && IntPvtTable(rindex).tag === r.addr, "Pvt intReadPorts $i read a invalid entry")
        }
    }
    for ((r, i) <- io.fpReadPorts.zipWithIndex) {
        val rindex = r.addr(log2Ceil(FpEntryNum)-1, 0)
        r.data := FpPvtTable(rindex).value
        val readFail = io.fromLoad.map{ veri =>
            val veriIdx = veri.bits.pdest(log2Ceil(FpEntryNum)-1, 0)
            val readConflict = veri.bits.pdest === r.addr && r.valid && veri.valid
            val misPred = FpPvtTable(veriIdx).robIdx === veri.bits.misPredict.robIdx && veri.valid &&
              FpPvtTable(veriIdx).tag === veri.bits.pdest && FpPvtTable(veriIdx).value =/= veri.bits.loadvalue
            readConflict && misPred
        }.reduce(_ || _)
        r.fail := readFail
        when (r.valid) {
            FpPvtTableNext(rindex).used := true.B
            assert(FpPvtTable(rindex).valid && FpPvtTable(rindex).tag === r.addr, "Pvt fpReadPorts $i read a invalid entry")
        }
    }

    for ((pred, i) <- io.intSrcPred.zipWithIndex) {
        val rindex = pred.addr(log2Ceil(IntEntryNum)-1, 0)
        pred.pred := IntPvtTable(rindex).valid
    }
    io.fpSrcPred.foreach{ pred =>
        val rindex = pred.addr(log2Ceil(FpEntryNum)-1, 0)
        pred.pred := FpPvtTable(rindex).valid
    }

    //load verify
    for ((veri, i) <- io.fromLoad.zipWithIndex) {
        io.misPred(i).valid := false.B
        io.misPred(i).bits := DontCare
        when (veri.bits.rfWen) {
            val index = veri.bits.pdest(log2Ceil(IntEntryNum)-1, 0)
            val entryMatch = IntPvtTable(index).robIdx === veri.bits.misPredict.robIdx && IntPvtTable(index).tag === veri.bits.pdest
            val misMatch = IntPvtTable(index).value =/= veri.bits.loadvalue && entryMatch
            when (veri.valid) {
                val needCancel = WireInit(VecInit((0 until IntEntryNum).map(i => {
                    IntPvtTable(i).robIdx.needFlush(veri.bits.misPredict)
                })))
                // flush pvt selectively, used means that has influence to after instr
                when (misMatch && IntPvtTable(index).used) {
                    io.misPred(i).valid := true.B
                    io.misPred(i).bits := veri.bits.misPredict
                    IntPvtTableNext.zip(needCancel).foreach { case (entry, flush) =>
                        when (flush) {entry := 0.U.asTypeOf(new PvtEntry)}
                    }
                }.elsewhen (entryMatch) {
                    IntPvtTableNext(index) := 0.U.asTypeOf(new PvtEntry)
                    io.misPred(i).valid := false.B
                    io.misPred(i).bits := 0.U.asTypeOf(new Redirect)
                }
            }
        }.elsewhen (veri.bits.fpWen) {
            val index = veri.bits.pdest(log2Ceil(FpEntryNum)-1, 0)
            val entryMatch = FpPvtTable(index).robIdx === veri.bits.misPredict.robIdx && FpPvtTable(index).tag === veri.bits.pdest
            val misMatch = FpPvtTable(index).value =/= veri.bits.loadvalue && entryMatch
            when (veri.valid) {
                val needCancel = WireInit(VecInit((0 until FpEntryNum).map(i => {
                    FpPvtTable(i).robIdx.needFlush(veri.bits.misPredict)
                })))
                // flush pvt selectively, used means that has influence to after instr
                when (misMatch && FpPvtTable(index).used) {
                    io.misPred(i).valid := true.B
                    io.misPred(i).bits := veri.bits.misPredict
                    FpPvtTableNext.zip(needCancel).foreach { case (entry, flush) =>
                        when (flush) {entry := 0.U.asTypeOf(new PvtEntry)}
                    }
                }.elsewhen (entryMatch) {
                    FpPvtTableNext(index) := 0.U.asTypeOf(new PvtEntry)
                    io.misPred(i).valid := false.B
                    io.misPred(i).bits := 0.U.asTypeOf(new Redirect)
                }
            }
        }
    }
    // other redirect, flush after
    when (io.flush.valid) {
        val intNeedCancel = WireInit(VecInit((0 until IntEntryNum).map(i => {
            IntPvtTable(i).robIdx.needFlush(io.flush)
        })))
        val fpNeedCancel = WireInit(VecInit((0 until FpEntryNum).map(i => {
            FpPvtTable(i).robIdx.needFlush(io.flush)
        })))
        IntPvtTableNext.zip(intNeedCancel).foreach { case (entry, flush) =>
            when (flush) {entry := 0.U.asTypeOf(new PvtEntry)}
        }
        FpPvtTableNext.zip(fpNeedCancel).foreach { case (entry, flush) =>
            when (flush) {entry := 0.U.asTypeOf(new PvtEntry)}
        }
    }
    IntPvtTable := IntPvtTableNext
    FpPvtTable := FpPvtTableNext
    io.full := IntPvtTable.map(_.valid).reduce(_ && _) || FpPvtTable.map(_.valid).reduce(_ && _)
}
