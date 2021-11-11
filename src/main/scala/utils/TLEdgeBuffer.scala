package utils

import chisel3._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._


class TLEdgeBuffer
(
  edgeFilter: Int => Boolean,
  nameOpt: Option[String],
  a: BufferParams = BufferParams.default,
  b: BufferParams = BufferParams.default,
  c: BufferParams = BufferParams.default,
  d: BufferParams = BufferParams.default,
  e: BufferParams = BufferParams.default,
)(implicit p: Parameters) extends LazyModule {

  val node = new TLBufferNode(a, b, c, d, e)

  lazy val module = new LazyModuleImp(this){
    for((((in, edgeIn), (out, edgeOut)), i) <- node.in.zip(node.out).zipWithIndex){
      val buffer = edgeFilter(i)
      if(buffer){
        out <> TLBufferModule(edgeIn, edgeOut, in, name = nameOpt.map(s => s"${s}_edge_$i"))
      } else {
        out <> in
      }
    }
  }

}

object TLEdgeBuffer {
  def apply
  (
    edgeFilter: Int => Boolean,
    nameOpt: Option[String] = None
  )(implicit p: Parameters) = {
    val buffer = LazyModule(new TLEdgeBuffer(edgeFilter, nameOpt))
    buffer.suggestName(nameOpt)
    buffer.node
  }
}

class TLBufferModule
(
  edge: TLEdge,
  bce: Boolean = true,
  a: BufferParams = BufferParams.default,
  b: BufferParams = BufferParams.default,
  c: BufferParams = BufferParams.default,
  d: BufferParams = BufferParams.default,
  e: BufferParams = BufferParams.default
) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(TLBundle(edge.bundle))
    val out = TLBundle(edge.bundle)
  })

  io.out.a <> a(io.in.a)
  io.in.d <> d(io.out.d)
  if(bce){
    io.out.c <> c(io.in .c)
    io.out.e <> e(io.in .e)
    io.in.b <> b(io.out.b)
  } else {
    io.in.b.valid := false.B
    io.in.c.ready := true.B
    io.in.e.ready := true.B
    io.out.b.ready := true.B
    io.out.c.valid := false.B
    io.out.e.valid := false.B
  }

}

object TLBufferModule {
  def apply
  (
    edgeIn: TLEdgeIn,
    edgeOut: TLEdgeOut,
    in: TLBundle,
    a: BufferParams = BufferParams.default,
    b: BufferParams = BufferParams.default,
    c: BufferParams = BufferParams.default,
    d: BufferParams = BufferParams.default,
    e: BufferParams = BufferParams.default,
    name: Option[String] = None
  ): TLBundle = {
    val bce = edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe
    val mod = Module(new TLBufferModule(edgeIn, bce, a, b, c, d, e))
    name.map(mod.suggestName(_))
    mod.io.in <> in
    mod.io.out
  }
}
