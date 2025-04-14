package xiangshan.backend

import chisel3.util.log2Up
import chisel3.{Bundle, Module}
import org.chipsalliance.cde.config.{Field, Parameters => P}
import utils.NamedUInt

package object vector {
  case object XSVectorParamKey extends Field[XSVectorParameters]

  /**
   *
   * @param nVecPhyReg number of vector physical registers
   */
  case class XSVectorParameters(
    nVecPhyReg: Int,
  )

  trait VecModule extends Module with HasVectorParam {
    implicit val p: P
  }

  trait HasVectorSettings {
    def VLEN = 128
    // non-configurable parameters
    def nVecLogReg = 32
    def maxSrc = 5
    // The max of number of uop that IQ/ROB will hold
    def maxSplitUopNum = 8

    def maxLMUL = 8

    def uopBufferLength = 7
  }

  trait HasVectorParam extends HasVectorSettings {
    implicit val p: P

    def nVecPhyReg: Int = p(XSVectorParamKey).nVecPhyReg

    val vecPtagWidth = log2Up(nVecPhyReg)
    val vecLregWidth = log2Up(nVecLogReg)
    val uopNumWidth = log2Up(maxSplitUopNum)

    object Lreg extends NamedUInt(maxSrc)
  }

  trait VecBundle extends Bundle with HasVectorParam {
    implicit val p: P
  }


}
