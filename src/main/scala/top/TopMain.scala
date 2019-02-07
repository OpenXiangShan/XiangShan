package top

import core._

import chisel3.Driver

object TopMain extends App {
  Driver.execute(args, () => new NOOP)
}
