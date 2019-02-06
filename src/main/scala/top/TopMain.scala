package top

import core._
import core.ALU

import chisel3.Driver

object TopMain extends App {
  Driver.execute(args, () => new ALU)
}
