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

package xiangshan.backend.regfile

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._

class RfReadPort(numEntries: Int, dataWidth: Int)(implicit p: Parameters) extends Bundle {
  val addr = Input(UInt(log2Ceil(numEntries).W))
  val data = Output(UInt(dataWidth.W))
}

class RfWritePort(numEntries: Int, dataWidth: Int)(implicit p: Parameters) extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(log2Ceil(numEntries).W))
  val data = Input(UInt(dataWidth.W))
}

abstract class BaseRegfile (
  numReadPorts: Int,
  numWritePorts: Int,
  hasZero: Boolean,
  numEntries: Int,
  dataWidth: Int
)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RfReadPort(numEntries, dataWidth))
    val writePorts = Vec(numWritePorts, new RfWritePort(numEntries, dataWidth))
    val debug_ports = Vec(32, new RfReadPort(numEntries, dataWidth))
  })
  println(s"REGFILE: $numEntries entries, $numReadPorts read ports, $numWritePorts write ports")
}

class SimRegfile (
  numReadPorts: Int,
  numWritePorts: Int,
  hasZero: Boolean,
  numEntries: Int,
  dataWidth: Int
)(implicit p: Parameters) extends BaseRegfile(numReadPorts, numWritePorts, hasZero, numEntries, dataWidth) {

  val mem = Mem(NRPhyRegs, UInt(dataWidth.W))
  for (r <- io.readPorts) {
    val rdata = if (hasZero) Mux(r.addr === 0.U, 0.U, mem(r.addr)) else mem(r.addr)
    r.data := RegNext(rdata)
  }
  for (w <- io.writePorts) {
    when(w.wen) {
      mem(w.addr) := w.data
    }
  }

  for (port <- io.debug_ports) {
    val zero_rdata = Mux(port.addr === 0.U, 0.U, mem(port.addr))
    port.data := (if (hasZero) zero_rdata else mem(port.addr))
  }
}

/**
 * Synthesizable regfile for ASIC flows.
 * The data width is limited to 32 and the number of entries is limited to 64.
 */
class SynRegfileSlice (
  numReadPorts: Int,
  numWritePorts: Int,
  hasZero: Boolean,
  numEntries: Int,
  dataWidth: Int
)(implicit p: Parameters) extends BaseRegfile(numReadPorts, numWritePorts, hasZero, numEntries, dataWidth) {
  val entry = Reg(Vec(numEntries, UInt(dataWidth.W)))
  val entry_next = Wire(Vec(numEntries, UInt(dataWidth.W)))
  entry := entry_next

  // READ: addr at T0, data at T1 (no forwarding for WRITE in T0)
  val raddr_dec_reg = io.readPorts.map(port => RegNext(UIntToOH(port.addr)))
  for ((addr_dec, rdata) <- raddr_dec_reg.zip(io.readPorts.map(_.data))) {
    rdata := Mux1H(addr_dec, entry)
  }

  // WRITE: addr and data at T0. Actually take effect at T1.
  val write_en_reg = RegNext(VecInit(io.writePorts.map(_.wen)))
  val write_addr_reg = RegNext(VecInit(io.writePorts.map(_.addr)))
  val write_data_reg = RegNext(VecInit(io.writePorts.map(_.data)))
  val waddr_dec = write_addr_reg.map(addr => UIntToOH(addr)(numEntries - 1, 0))
  for ((next, i) <- entry_next.zipWithIndex) {
    next := entry(i)
    if (hasZero && i == 0) {
      next := 0.U
    }
    else {
      val write_en = write_en_reg.zip(waddr_dec).map(w => w._1 && w._2(i))
      when (VecInit(write_en).asUInt.orR) {
        next := Mux1H(write_en, write_data_reg)
      }
    }
  }

  // DEBUG: READ with bypass from WRITE
  for (port <- io.debug_ports) {
    port.data := entry_next(port.addr)
  }
}

class SynRegfile (
  numReadPorts: Int,
  numWritePorts: Int,
  hasZero: Boolean,
  numEntries: Int,
  dataWidth: Int
)(implicit p: Parameters) extends BaseRegfile(numReadPorts, numWritePorts, hasZero, numEntries, dataWidth) {
  val (rfEntries, rfDataBits) = (64, 32)
  require(dataWidth % rfDataBits == 0, s"dataWidth $dataWidth should div $rfDataBits for now")
  require(numEntries % rfEntries == 0, s"numEntries $numEntries should div $rfEntries for now")

  val (numEntryDiv, numDataDiv) = (numEntries / rfEntries, dataWidth / rfDataBits)
  def entryBankIndex(addr: UInt): UInt = {
    if (numEntryDiv == 1) 0.U else addr(addr.getWidth - 1, log2Ceil(rfEntries))
  }
  def entryBankData(addr: UInt, data: Seq[UInt], regNext: Boolean = false): UInt = {
    if (numEntryDiv == 1) {
      require(data.length == 1)
      data.head
    }
    else {
      val bankIndex = entryBankIndex(addr)
      VecInit(data)(if (regNext) RegNext(bankIndex) else bankIndex)
    }
  }

  val regfile = (0 until numEntryDiv).map(entry_bank_i => {
    // only the first bank requires zero reg_0
    val rfHasZero = hasZero && entry_bank_i == 0
    Seq.fill(numDataDiv)(Module(new SynRegfileSlice(numReadPorts, numWritePorts, rfHasZero, rfEntries, rfDataBits)))
  })

  regfile.zipWithIndex.foreach { case (data_banks, bank_i) =>
    data_banks.zipWithIndex.foreach { case (bank, data_i) =>
      bank.io.writePorts.zip(io.writePorts).foreach { case (bank_port, port) =>
        bank_port.wen := port.wen && entryBankIndex(port.addr) === bank_i.U
        bank_port.addr := port.addr
        bank_port.data := port.data.asTypeOf(Vec(numDataDiv, UInt(rfDataBits.W)))(data_i)
      }
      bank.io.readPorts.zip(io.readPorts).foreach { case (bank_port, port) =>
        bank_port.addr := port.addr
      }
      bank.io.debug_ports.zip(io.debug_ports).foreach { case (bank_port, port) =>
        bank_port.addr := port.addr
      }
    }
  }

  // READ data
  for ((port, i) <- io.readPorts.zipWithIndex) {
    val rdata = regfile.map(rf => VecInit(rf.map(_.io.readPorts(i).data)).asUInt)
    port.data := entryBankData(port.addr, rdata, regNext = true)
  }

  // DEBUG data
  for ((port, i) <- io.debug_ports.zipWithIndex) {
    val rdata = regfile.map(rf => VecInit(rf.map(_.io.debug_ports(i).data)).asUInt)
    port.data := entryBankData(port.addr, rdata)
  }
}

object Regfile {
  def apply(
    numReadPorts: Int,
    numWritePorts: Int,
    hasZero: Boolean,
    numEntries: Int,
    dataWidth: Int,
    fastSim: Boolean
  )(implicit p: Parameters): BaseRegfile = {
    if (fastSim) {
      Module(new SimRegfile(numReadPorts, numWritePorts, hasZero, numEntries, dataWidth))
    }
    else {
      Module(new SynRegfile(numReadPorts, numWritePorts, hasZero, numEntries, dataWidth))
    }
  }

  def apply(
    numEntries: Int,
    raddr: Seq[UInt],
    wen: Seq[Bool],
    waddr: Seq[UInt],
    wdata: Seq[UInt],
    hasZero: Boolean,
    withReset: Boolean = false,
    debugRead: Option[Seq[UInt]] = None,
    fastSim: Boolean = false
  )(implicit p: Parameters): Seq[UInt] = {
    val numReadPorts = raddr.length
    val numWritePorts = wen.length
    require(wen.length == waddr.length)
    require(wen.length == wdata.length)
    val dataBits = wdata.map(_.getWidth).min
    require(wdata.map(_.getWidth).min == wdata.map(_.getWidth).max, s"dataBits != $dataBits")
    val regfile = Regfile(numReadPorts, numWritePorts, hasZero, numEntries, dataBits, fastSim)
    val rdata = regfile.io.readPorts.zip(raddr).map { case (rport, addr) =>
      rport.addr := addr
      rport.data
    }
    regfile.io.writePorts.zip(wen).zip(waddr).zip(wdata).foreach{ case (((wport, en), addr), data) =>
      wport.wen := en
      wport.addr := addr
      wport.data := data
    }
    if (withReset) {
      val numResetCycles = math.ceil(numEntries / numWritePorts).toInt
      val resetCounter = RegInit(numResetCycles.U)
      val resetWaddr = RegInit(VecInit((0 until numWritePorts).map(_.U(log2Up(numEntries + 1).W))))
      val inReset = resetCounter =/= 0.U
      when (inReset) {
        resetCounter := resetCounter - 1.U
        resetWaddr := VecInit(resetWaddr.map(_ + numWritePorts.U))
      }
      when (!inReset) {
        resetWaddr.map(_ := 0.U)
      }
      for ((wport, i) <- regfile.io.writePorts.zipWithIndex) {
        wport.wen := inReset || wen(i)
        wport.addr := Mux(inReset, resetWaddr(i), waddr(i))
        wport.data := wdata(i)
      }
    }
    regfile.io.debug_ports := DontCare
    val debug_rdata = regfile.io.debug_ports.zip(debugRead.getOrElse(Seq())).map { case (port, addr) =>
      port.addr := addr
      port.data
    }
    rdata ++ debug_rdata
  }
}
