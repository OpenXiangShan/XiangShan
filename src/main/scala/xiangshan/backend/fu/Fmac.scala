package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._

import xiangshan.backend.fu.FunctionUnit._

class Fmac extends FunctionUnit(fmacCfg){
  override def toString: String = "Fmac"
}
