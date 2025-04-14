package xiangshan.backend.vector.Decoder.Uop

import chisel3._

class UopInfoIssue extends Bundle {
  // exu used
  val src1Sign = Bool()
  val src2Sign = Bool()
  val src1Widen = Bool()
  val src2Widen = Bool()
  val src1VecScala = Bool()

  val readV0AsMask = Bool()
  val readV0AsSrc = Bool()

  val destSign = Bool()
  val destWiden = Bool()
  val destVecScala = Bool()
}
