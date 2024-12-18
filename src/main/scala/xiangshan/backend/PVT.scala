package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import org.codehaus.plexus.classworlds.strategy.ParentFirstStrategy

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
    val PvtTagLen = log2Up(IntLogicRegs) max log2Up(FpLogicRegs) max log2Up(VecLogicRegs)
    val EntryNum = 32
    val ReadPortNum = 16
    val WritePortNum = backendParams.LduCnt
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
    val pvtLdestUpdate = Vec(RenameWidth, Input(UInt(LogicRegsWidth.W)))
}

class Pvt(implicit p: Parameters) extends PvtModule{
    val io = IO(new PvtIO)

    val PvtTable = RegInit(VecInit.fill(EntryNum)(0.U.asTypeOf(new PvtEntry)))
    val PvtTableNext = WireInit(PvtTable)
    io.writeFail := VecInit.fill(WritePortNum)(false.B)
    io.readPorts.foreach(_.data := 0.U)
    
    //write
    io.writePorts.zipWithIndex.foreach{ case (w, i) =>
        val windex = w.addr(log2Ceil(EntryNum)-1, 0)
        //todo: same tag different ld?
        when (!PvtTable(windex).valid && w.wen){
            PvtTableNext(windex).value := w.data
            PvtTableNext(windex).tag := w.addr
            PvtTableNext(windex).valid := true.B
        }.elsewhen(PvtTable(windex).valid && w.wen){
            io.writeFail(i) := true.B
        }
    }

    // pvt wb update
    for ((update, i) <- io.pvtLdestUpdate.zipWithIndex) {
        when (PvtTable(update(log2Ceil(EntryNum)-1, 0)).valid && 
            (PvtTable(update(log2Ceil(EntryNum)-1, 0)).tag === update)) {
            PvtTableNext(update(log2Ceil(EntryNum)-1, 0)) := 0.U.asTypeOf(new PvtEntry)
        }
    }

    PvtTable := PvtTableNext

    // pvt read
    for ((r, i) <- io.readPorts.zipWithIndex) {
        val rindex = r.addr(log2Ceil(EntryNum)-1, 0)
        when (PvtTable(rindex).valid && PvtTable(rindex).tag === r.addr && r.valid) {
            r.data := PvtTable(rindex).value
        }
    }

    when (io.flush) {
        PvtTable := 0.U.asTypeOf(Vec(EntryNum, new PvtEntry))
    }

    io.full := PvtTable.map(_.valid).reduce(_ && _)
    
}