package xiangshan.backend.vector

import chisel3._
import chisel3.util.log2Up
import xiangshan.FuOpType
import xiangshan.backend.fu.FuType
import xiangshan.frontend.ftq.FtqPtr
import xiangshan._

import org.chipsalliance.cde.config.{Parameters => P}

package object Decoder {
  class FuInfo extends Bundle {
    val fuType          = FuType()
    val fuOpType        = FuOpType()
  }

  class DecoderCtrlInfo extends Bundle {
    val waitForward     = Bool() // no speculate execution
    val blockBackward   = Bool()
    val flushPipe       = Bool() // This inst will flush all the pipe when commit, like exception but can commit
    val canRobCompress  = Bool()
    val illegalInst     = Bool()
    val virtualInst     = Bool()
    val isXSTrap        = Bool()
    val commitType      = CommitType()
  }

  class FtqInfo(implicit p: P) extends XSBundle {
    val ftqPtr          = new FtqPtr
    val ftqOffset       = UInt(FetchBlockInstOffsetWidth.W)
  }
}
