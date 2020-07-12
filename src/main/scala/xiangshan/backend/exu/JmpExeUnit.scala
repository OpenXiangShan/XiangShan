package xiangshan.backend.exu

import chisel3._
import xiangshan.backend.fu.Jump

// NOTE: BRUOpType is at backend/package.scala

// TODO: add csr
class JmpExeUnit extends Exu(Exu.jmpExeUnitCfg) {

  val jmp = Module(new Jump)

  io <> jmp.io

}