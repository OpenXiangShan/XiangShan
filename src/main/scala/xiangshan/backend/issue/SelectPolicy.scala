package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class SelectPolicy(config: RSConfig)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    // select for enqueue
    val validVec = Input(UInt(config.numEntries.W))
    val allocate = Vec(config.numEnq, DecoupledIO(UInt(config.numEntries.W)))
    // select for issue
    val request = Input(UInt(config.numEntries.W))
    val grant = Vec(config.numDeq, DecoupledIO(UInt(config.numEntries.W))) //TODO: optimize it
  })

  // TODO optimize timing
  var maskedEmptyVec = VecInit(io.validVec.asBools.map(v => !v))
  for (i <- 0 until config.numEnq) {
    io.allocate(i).valid := maskedEmptyVec.asUInt.orR
    io.allocate(i).bits := PriorityEncoderOH(maskedEmptyVec.asUInt)
    maskedEmptyVec = VecInit(maskedEmptyVec.zip(io.allocate(i).bits.asBools).map{ case (m, s) => m && !s })

    XSError(io.allocate(i).valid && PopCount(io.allocate(i).bits) =/= 1.U,
      p"allocate vec ${Binary(io.allocate(i).bits)} is not onehot")
    XSDebug(io.allocate(i).fire(), p"select for allocation: ${Binary(io.allocate(i).bits)}\n")
  }

  // TODO optimize timing
  // naive selection
  // var maskedRequest = VecInit(io.request.asBools)
  // for (i <- 0 until config.numDeq) {
  //   io.grant(i).valid := maskedRequest.asUInt.orR
  //   io.grant(i).bits := PriorityEncoderOH(maskedRequest.asUInt)
  //   maskedRequest = VecInit(maskedRequest.zip(io.grant(i).bits.asBools).map{ case(m, s) => m && !s })

  //   XSError(io.grant(i).valid && PopCount(io.grant(i).bits.asBools) =/= 1.U,
  //     p"grant vec ${Binary(io.grant(i).bits)} is not onehot")
  //   XSDebug(io.grant(i).valid, p"select for issue request: ${Binary(io.grant(i).bits)}\n")
  // }

  // a better one: select from both directions
  val request = io.request.asBools
  // +1 is needed when numDeq is an odd number
  val req_forward = new NaiveSelectOne(request, (config.numDeq + 1) / 2)
  val req_backward = new NaiveSelectOne(request.reverse, config.numDeq / 2)
  for (i <- 0 until config.numDeq) {
    io.grant(i).valid := OnesMoreThan(request, i + 1)
    val sel_index = (i / 2) + 1
    if (i % 2 == 0) {
      io.grant(i).bits := req_forward.getNthOH(sel_index).asUInt
    }
    else {
      io.grant(i).bits := VecInit(req_backward.getNthOH(sel_index).reverse).asUInt
    }

    XSError(io.grant(i).valid && PopCount(io.grant(i).bits.asBools) =/= 1.U,
      p"grant vec ${Binary(io.grant(i).bits)} is not onehot")
    XSDebug(io.grant(i).valid, p"select for issue request: ${Binary(io.grant(i).bits)}\n")
  }

}
