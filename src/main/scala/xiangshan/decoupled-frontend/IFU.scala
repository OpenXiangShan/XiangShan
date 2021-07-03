package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class FtqInterface(implicit p: Parameters) extends XSBundle {
  val startAddr = UInt(64.W)  
  val target = UInt(64.W)
  val hit = Bool()
}

class ICacheMeta

class IFUIO(implicit p: Parameters) extends XSBundle {
  val FtqInterface 
  val ICacheInterface
  val toIbuffer
  
}

@chiselName
class IFU(implicit p: Parameters) extends XSModule
{
  val io = IO(new IFUIO)
  val toFtq    = io.FtqInterface.toFtq
  val fromtFtq = io.FtqInterface.fromFtq

}
