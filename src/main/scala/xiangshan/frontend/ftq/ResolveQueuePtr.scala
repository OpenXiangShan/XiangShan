package xiangshan.frontend.ftq

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import xiangshan.XSCoreParamsKey

class ResolveQueuePtr(entries: Int) extends CircularQueuePtr[ResolveQueuePtr](entries) {
  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).frontendParameters.ftqParameters.ResolveQueueSize)
}

object ResolveQueuePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): ResolveQueuePtr = {
    val ptr = Wire(new ResolveQueuePtr())
    ptr.flag  := f
    ptr.value := v
    ptr
  }
}
