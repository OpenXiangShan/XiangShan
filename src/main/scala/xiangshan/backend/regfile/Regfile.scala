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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._

class RfReadPort(dataWidth: Int, addrWidth: Int)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(addrWidth.W))
  val data = Output(UInt(dataWidth.W))
}

class RfWritePort(dataWidth: Int, addrWidth: Int)(implicit p: Parameters) extends XSBundle {
  val wen = Input(Bool())
  val addr = Input(UInt(addrWidth.W))
  val data = Input(UInt(dataWidth.W))
}

class Regfile
(
  name: String,
  numReadPorts: Int,
  numWritePorts: Int,
  hasZero: Boolean,
  len: Int,
  width: Int,
)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec(numReadPorts, new RfReadPort(len, width))
    val writePorts = Vec(numWritePorts, new RfWritePort(len, width))
    val debug_rports = Vec(64, new RfReadPort(len, width))
  })

  println(name + ": size:" + NRPhyRegs + " read: " + numReadPorts + " write: " + numWritePorts)

  val mem = Reg(Vec(NRPhyRegs, UInt(len.W)))
  for (r <- io.readPorts) {
    val rdata = if (hasZero) Mux(r.addr === 0.U, 0.U, mem(r.addr)) else mem(r.addr)
    r.data := rdata
  }
  for (w <- io.writePorts) {
    when(w.wen) {
      mem(w.addr) := w.data
    }
  }

  for (rport <- io.debug_rports) {
    val zero_rdata = Mux(rport.addr === 0.U, 0.U, mem(rport.addr))
    rport.data := (if (hasZero) zero_rdata else mem(rport.addr))
  }
}

object Regfile {
  def apply(
    name         : String,
    numEntries   : Int,
    raddr        : Seq[UInt],
    wen          : Seq[Bool],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    hasZero      : Boolean,
    withReset    : Boolean = false,
    debugReadAddr: Option[Seq[UInt]] = None,
  )(implicit p: Parameters): Seq[UInt] = {
    val numReadPorts = raddr.length
    val numWritePorts = wen.length
    require(wen.length == waddr.length)
    require(wen.length == wdata.length)
    val dataBits = wdata.map(_.getWidth).min
    require(wdata.map(_.getWidth).min == wdata.map(_.getWidth).max, s"dataBits != $dataBits")
    val addrBits = waddr.map(_.getWidth).min
    require(waddr.map(_.getWidth).min == waddr.map(_.getWidth).max, s"addrBits != $addrBits")

    val regfile = Module(new Regfile(name, numReadPorts, numWritePorts, hasZero, dataBits, addrBits))
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
    regfile.io.debug_rports := DontCare
    val debug_rdata = regfile.io.debug_rports.zip(debugReadAddr.getOrElse(Seq())).map { case (rport, addr) =>
      rport.addr := addr
      rport.data
    }
    rdata ++ debug_rdata
  }
}

object IntRegFile {
  def apply(
    name         : String,
    numEntries   : Int,
    raddr        : Seq[UInt],
    wen          : Seq[Bool],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    withReset    : Boolean = false,
    debugReadAddr: Option[Seq[UInt]] = None,
  )(implicit p: Parameters): Seq[UInt] = {
    Regfile(
      name, numEntries, raddr, wen, waddr, wdata,
      hasZero = true, withReset, debugReadAddr)
  }
}

object VfRegFile {
  def apply(
    name         : String,
    numEntries   : Int,
    splitNum     : Int,
    raddr        : Seq[UInt],
    wen          : Seq[Seq[Bool]],
    waddr        : Seq[UInt],
    wdata        : Seq[UInt],
    withReset    : Boolean = false,
    debugReadAddr: Option[Seq[UInt]] = None,
  )(implicit p: Parameters) : Seq[UInt] = {
    require(splitNum >= 1, "splitNum should be no less than 1")
    require(splitNum == wen.length, "splitNum should be equal to length of wen vec")
    if (splitNum == 1) {
      Regfile(name, numEntries, raddr, wen.head, waddr, wdata,
        hasZero = false, withReset, debugReadAddr)
    } else {
      val dataWidth = 64
      val numReadPorts = raddr.length + debugReadAddr.getOrElse(Seq()).length
      require(splitNum > 1 && wdata.head.getWidth == dataWidth * splitNum)
      val wdataVec = Wire(Vec(splitNum, Vec(wdata.length, UInt(dataWidth.W))))
      var rdataVec = Wire(Vec(splitNum, Vec(numReadPorts, UInt(dataWidth.W))))
      for (i <- 0 until splitNum) {
        wdataVec(i) := wdata.map(_((i + 1) * dataWidth - 1, i * dataWidth))
        rdataVec(i) := Regfile(name+s"Part${i}", numEntries, raddr, wen(i), waddr, wdataVec(i),
          hasZero = false, withReset, debugReadAddr)
      }
      val rdata = Wire(Vec(numReadPorts, UInt(wdata.head.getWidth.W)))
      for (i <- 0 until rdata.length) {
        rdata(i) := Cat(rdataVec.map(_(i)).reverse)
      }
      rdata
    }
  }
}
