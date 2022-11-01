package utils

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.interrupts.IntAdapterNode
import freechips.rocketchip.util.AsyncResetSynchronizerShiftReg

class IntBuffer(implicit p: Parameters) extends LazyModule {

  val node = IntAdapterNode()

  lazy val module = new LazyModuleImp(this){
    for(((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)){
      out := AsyncResetSynchronizerShiftReg(in, 3)
    }
  }

}

object IntBuffer {
  def apply()(implicit p: Parameters) = {
    val intBuffer = LazyModule(new IntBuffer)
    intBuffer.node
  }
}
