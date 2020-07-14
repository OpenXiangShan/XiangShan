package xiangshan.backend.fu

import chisel3._
import chisel3.util._

import xiangshan._
import utils._

import FunctionUnit._

/*
    XiangShan Function Unit
    A Exu can have one or more function units
 */

case class FuConfig
(
  fuType: UInt,
  numIntSrc: Int,
  numFpSrc: Int,
  writeIntRf: Boolean,
  writeFpRf: Boolean,
  hasRedirect: Boolean
)

class FunctionUnitIO extends XSBundle {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(XLEN.W))
    val src2 = Output(UInt(XLEN.W))
    val src3 = Output(UInt(XLEN.W))
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(XLEN.W)))
}

abstract class FunctionUnit(cfg: FuConfig) extends XSModule

object FunctionUnit {

  val csrCfg =
    FuConfig(FuType.csr, 1, 0, writeIntRf = true, writeFpRf = false, hasRedirect = false)

  val jmpCfg =
    FuConfig(FuType.jmp, 1, 0, writeIntRf = true, writeFpRf = false, hasRedirect = true)

  val i2fCfg =
    FuConfig(FuType.i2f, 1, 0, writeIntRf = false, writeFpRf = true, hasRedirect = false)

  val aluCfg =
    FuConfig(FuType.alu, 2, 0, writeIntRf = true, writeFpRf = false, hasRedirect = true)

  val mulCfg =
    FuConfig(FuType.mul, 2, 0, writeIntRf = true, writeFpRf = false, hasRedirect = false)

  val divCfg =
    FuConfig(FuType.div, 2, 0, writeIntRf = true, writeFpRf = false, hasRedirect = false)

  val lsuCfg =
    FuConfig(FuType.ldu, 2, 1, writeIntRf = true, writeFpRf = true, hasRedirect = false)

  val fmacCfg =
    FuConfig(FuType.fmac, 0, 3, writeIntRf = false, writeFpRf = true, hasRedirect = false)

  val fmiscCfg =
    FuConfig(FuType.fmisc, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false)

  val fDivSqrtCfg =
    FuConfig(FuType.fDivSqrt, 0, 2, writeIntRf = false, writeFpRf = true, hasRedirect = false)
}