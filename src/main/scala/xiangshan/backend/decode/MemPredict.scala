package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

trait MemPredParameters {
  val ResetTimeMax2Pow = 20 //1078576
  val ResetTimeMin2Pow = 10 //1024
  val MemPredPCWidth = log2Up(WaitTableSize)
  
  // wait table parameters
  val WaitTableSize = 1024

  // store set parameters
  val SSITSize = WaitTableSize
  val LFSTSize = 32
  val SSIDWidth = log2Up(LFSTSize)
}
