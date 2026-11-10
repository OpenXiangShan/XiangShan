package xiangshan.frontend.ftq

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import xiangshan.XSCoreParamsKey

class PrefetchQueuePtr(entries: Int) extends CircularQueuePtr[PrefetchQueuePtr](entries) {
  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).frontendParameters.ftqParameters.pqParameters.Size)
}

object PrefetchQueuePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): PrefetchQueuePtr = {
    val ptr = Wire(new PrefetchQueuePtr())
    ptr.flag  := f
    ptr.value := v
    ptr
  }
}
