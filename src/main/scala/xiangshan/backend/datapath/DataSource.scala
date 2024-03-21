package xiangshan.backend.datapath

import chisel3._

class DataSource extends Bundle {
  val value = UInt(3.W)

  def readReg: Bool = value(2)

  def readRegOH: Bool = value === DataSource.reg

  def readAnotherReg: Bool = value === DataSource.anotherReg

  def readZero: Bool = value === DataSource.zero

  def readForward: Bool = value === DataSource.forward

  def readBypass: Bool = value === DataSource.bypass

  def readImm: Bool = value === DataSource.imm

}

object DataSource {
  def apply() = new DataSource

  def reg: UInt = "b100".U

  def anotherReg: UInt = "b101".U

  // read int preg addr is 0
  def zero: UInt = "b000".U

  def forward: UInt = "b001".U

  def bypass: UInt = "b010".U

  def imm: UInt = "b011".U

}

