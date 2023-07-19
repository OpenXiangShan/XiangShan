package xiangshan.backend.datapath

import chisel3._

class DataSource extends Bundle {
  val value = UInt(2.W)

  def readReg: Bool = value === DataSource.reg

  def readBypass: Bool = value(1)

  def readForward: Bool = value(0)
}

object DataSource {
  def apply() = new DataSource

  // no need to read any data
  def none: UInt = "b00".U

  def reg: UInt = "b00".U

  def forward: UInt = "b01".U

  def bypass: UInt = "b10".U

}

