package xiangshan.backend.datapath

import chisel3._

class DataSource extends Bundle {
  val value = UInt(4.W)

  def readReg: Bool = value(3)

  def readRegOH: Bool = value === DataSource.reg

  def readRegCache: Bool = value === DataSource.regcache

  def readV0: Bool = value === DataSource.v0

  def readZero: Bool = value === DataSource.zero

  def readForward: Bool = value === DataSource.forward

  def readBypass: Bool = value === DataSource.bypass

  def readBypass2: Bool = value === DataSource.bypass2

  def readImm: Bool = value === DataSource.imm

}

object DataSource {
  def apply() = new DataSource

  def reg: UInt = "b1000".U

  def regcache: UInt = "b0110".U

  def v0: UInt = "b0101".U

  // read int preg addr is 0
  def zero: UInt = "b0000".U

  def forward: UInt = "b0001".U

  def bypass: UInt = "b0010".U

  def bypass2: UInt = "b0011".U

  def imm: UInt = "b0100".U

}

