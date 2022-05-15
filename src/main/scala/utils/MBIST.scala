/***************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2022 Peng Cheng Laboratory
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

package utils

import chisel3._
import chisel3.experimental.{DataMirror, ExtModule}
import chisel3.util._

// A sharebus implementation for MBIST
class MBISTBus(arrayWidth: Int, addrWidth: Int, singlePort: Boolean) extends Bundle {
  // MBIST mode request
  val req = Input(Bool())
  // Array Selector
  val array = if (arrayWidth > 0) Some(Input(UInt(arrayWidth.W))) else None
  // MBIST Logical Address, right-justified
  val addr = Input(UInt(addrWidth.W))
  // MBIST Read Port Logical Address, right-justified
  // The read address for two port RAM
  val addr_rd = if (singlePort) None else Some(Input(addr.cloneType))
  // Data In
  val indata = Input(UInt(256.W))
  // Write Control
  val writeen = Input(Bool())
  // Read Control
  val readen = Input(Bool())
  // Byte and Bit Control
  val be = Input(UInt(256.W))
  // Power Simulation Mode
  val all = Input(Bool())
  // Data Out
  val outdata = Output(indata.cloneType)
  // MBIST Mode Ready
  val ack = Output(Bool())
  // Use the DFTRAMHOLD signal to disable RAM macros during scan test shifting
  val dftramhold = Input(Bool())
  // MBIST reset
  // val nmbistreset = Input(Bool())
}

trait HasMBISTSlave { this: Module =>
  val mbistArrayWidth: Int
  val mbistAddrWidth: Int
  val mbistSinglePort: Boolean
  val mbistNumBeat: Int
  val mbistDataWidth: Int = 256
  lazy val mbistIsEmpty: Boolean = false

  lazy val mbist: Option[MBISTBus] = {
    if (mbistIsEmpty)
      None
    else
      Some(IO(new MBISTBus(mbistArrayWidth, mbistAddrWidth, mbistSinglePort)))
  }
}

class MBIST2SRAM[T <: Data](
  gen: T,
  set: Int,
  way: Int,
  override val mbistSinglePort: Boolean
) extends Module with HasMBISTSlave {

  private val (addrWidth, dataWidth, numBeat) = {
    val elementWidth = gen.getWidth
    val totalWidth = way * elementWidth
    // Allocate only one array address if the totalWidth is within the MBIST data bus width
    if (totalWidth <= 256) {
      (log2Ceil(set), totalWidth, 1)
    }
    else {
      require(isPow2(way), s"cannot detect MBIST params for $elementWidth $set $way")
      val numBeat = 1 << log2Ceil((totalWidth + 256 - 1) / 256)
      require((way % numBeat == 0) || (numBeat % way == 0) , s"cannot divide $elementWidth $set $way to $numBeat")
      val dataWidth = totalWidth / numBeat
      (log2Ceil(set), dataWidth, numBeat)
    }
  }
  override val mbistAddrWidth: Int = addrWidth
  override val mbistDataWidth: Int = dataWidth
  override val mbistNumBeat: Int = numBeat
  override val mbistArrayWidth: Int = log2Ceil(mbistNumBeat)

  val sram = IO(Flipped(new SRAMRawIO(gen, set, way)))
  // TODO fixme
  // FIXME
  for (elem <- mbist.get.elements.values) {
    if (DataMirror.directionOf(elem) == ActualDirection.Output) {
      elem := LFSR64()
    }
  }

  sram := DontCare
  dontTouch(mbist.get)
  dontTouch(sram)
}

// This trait resolves the parameters for the MBIST interface.
trait HasMBISTInterface extends HasMBISTSlave { this: Module =>
  val mbistSlaves: Seq[HasMBISTSlave]

  override lazy val mbistArrayWidth: Int = {
    def reduceArrayWidth(width: Seq[Int]): Int = {
      // Find the smallest duplicate width and combine the duplicated slaves
      val duplicate = width.distinct.filter(w => width.count(_ == w) > 1)
      if (duplicate.nonEmpty) {
        val minDuplicated = duplicate.min
        val minCount = width.count(_ == minDuplicated)
        val newWidth = minDuplicated + log2Ceil(minCount)
        reduceArrayWidth(width.filter(_ != minDuplicated) :+ newWidth)
      }
      else {
        if (width.length > 1) width.max + 1 else width.max
      }
    }
    reduceArrayWidth(mbistSlaves.map(_.mbistArrayWidth) :+ 0)
  }
  override lazy val mbistAddrWidth: Int = (mbistSlaves.map(_.mbistAddrWidth) :+ 0).max
  override lazy val mbistSinglePort: Boolean = (mbistSlaves.map(_.mbistSinglePort) :+ true).reduce(_ && _)
  override lazy val mbistNumBeat: Int = mbistSlaves.map(_.mbistNumBeat).sum
  override lazy val mbistIsEmpty: Boolean = mbistSlaves.isEmpty

  def connectMBIST(master: MBISTBus, slave: Seq[MBISTBus]): Unit = {
    // TODO: implement mbist_sel
    val mbist_sel = Wire(Vec(slave.length, Bool()))
    // FIXME: mbist_sel
    mbist_sel := UIntToOH(Cat(LFSR64()(5, 0), master.addr)).asBools.take(slave.length)
    for ((o, i) <- slave.zipWithIndex) {
      // default connection
      connectMBIST(master, o, false)
      // override previous assignment
      o.writeen := master.writeen && mbist_sel(i)
      o.readen := master.readen && mbist_sel(i)
    }
    master.ack := VecInit(slave.map(_.ack)).asUInt.andR
    master.outdata := Mux1H(mbist_sel, slave.map(_.outdata))
  }

  def connectMBIST(master: MBISTBus, slave: MBISTBus, regNext: Boolean): Unit = {
    for ((m, s) <- master.elements.values.zip(slave.elements.values)) {
      if (DataMirror.directionOf(m) == ActualDirection.Input) {
        s := (if (regNext) RegNext(m) else m)
      }
      else {
        m := (if (regNext) RegNext(s) else s)
      }
    }
  }

  def connectMBIST(regNext: Boolean = false): Unit = {
    if (mbist.isDefined) {
      if (mbistSlaves.length > 1) {
        val interface = Module(new MBISTInterface(this, regNext))
        for ((s, s_if) <- mbistSlaves.zip(interface.slave)) {
          s_if <> s.mbist.get
        }
        mbist.get <> interface.mbist
      }
      else {
        connectMBIST(mbist.get, mbistSlaves.head.mbist.get, regNext)
      }
    }
    else {
      println(s"[WARNING] MBIST for ${this.name} is not connected.")
    }
  }
}

// This Module instantiates the hardware interface.
class MBISTInterface(mod: HasMBISTInterface, regNext: Boolean = false) extends Module {
  val slave = mod.mbistSlaves.map(s => IO(Flipped(s.mbist.get.cloneType)))
  val mbist = IO(mod.mbist.get.cloneType)

  if (regNext) {
    val mbist_REG = Wire(mbist.cloneType)
    mod.connectMBIST(mbist, mbist_REG, true)
    mod.connectMBIST(mbist_REG, slave)
  }
  else {
    mod.connectMBIST(mbist, slave)
  }
}

// This trait resolves the parameters for the MBIST controller.
trait HasMBISTController { this: Module =>
  val mbistSlave: Seq[HasMBISTSlave]

  def instantiateMBISTController(): Seq[MBISTControllerIO] = {
    mbistSlave.map { slave =>
      val adapter = Module(new MBISTControllerAdapter(slave))
      val controller = Module(new MBISTController())
      adapter.mbist_bus <> slave.mbist.get
      adapter.controller <> controller.io
      controller.io
    }
  }
}

class MBISTControllerIO extends Bundle {
  val dummy = Output(Bool())
}

class MBISTControllerAdapter(slave: HasMBISTSlave) extends Module {
  val mbist_bus = IO(Flipped(slave.mbist.get.cloneType))
  val controller = IO(Flipped(new MBISTControllerIO))

  for (elem <- mbist_bus.elements.values) {
    if (DataMirror.directionOf(elem) == ActualDirection.Output) {
      elem := LFSR64()
    }
  }

  controller := DontCare
  dontTouch(mbist_bus)
  dontTouch(controller)
}

// TODO: controller is a blackbox
class MBISTController() extends Module {
  val io = IO(new MBISTControllerIO)

  io := DontCare
  dontTouch(io)
}
