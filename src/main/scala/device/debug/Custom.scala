// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import chisel3._
import chisel3.util._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, NexusNode, RenderedEdge,
  SimpleNodeImp, SinkNode, SourceNode, ValName}
import freechips.rocketchip.config.Parameters

case class DebugCustomParams(
  addrs: List[Int],
  width: Int
) {
  require (width % 8 == 0, s"Currently only support custom debug widths which are multiples of 8, not ${width}")
}

case class DebugCustomNull()

class DebugCustomBundle(val p: DebugCustomParams) extends Bundle {
  val addr = Input(UInt(log2Up(p.addrs.foldLeft(0){_ max _}).W))
  val data = Output(UInt(p.width.W))
  val ready = Output(Bool())
  val valid = Input(Bool())
}

class DebugCustomImp extends SimpleNodeImp[DebugCustomParams, DebugCustomNull, DebugCustomParams, DebugCustomBundle] {

  def edge(pd: DebugCustomParams, pu: DebugCustomNull, p: Parameters, sourceInfo: SourceInfo) = pd
  def bundle(e: DebugCustomParams) = new DebugCustomBundle(e)
  def render(e: DebugCustomParams) = RenderedEdge(colour = "#20B2AA" /* lightseagreen */)

}

// The Sink is the Debug Module
case class DebugCustomSink()(implicit valName: ValName) extends SinkNode(new DebugCustomImp)(pi = Seq(DebugCustomNull()))

// The Source is the sources of data you want to snoop
case class DebugCustomSource(snoopable: DebugCustomParams)(implicit valName: ValName) extends SourceNode(new DebugCustomImp)(Seq(snoopable))

// Crossbar is used to connect different sources, if desired.
case class DebugCustomNexusNode(
  sourceFn: Seq[DebugCustomParams] => DebugCustomParams,
  sinkFn  : Seq[DebugCustomNull] => DebugCustomNull,
  inputRequiresOutput: Boolean = true,
  outputRequiresInput: Boolean = true
)( implicit valName: ValName) extends NexusNode(new DebugCustomImp) (sourceFn, sinkFn, inputRequiresOutput, outputRequiresInput)

class DebugCustomXbar(
  inputRequiresOutput: Boolean = true,
  outputRequiresInput: Boolean = true
)(implicit p: Parameters) extends LazyModule {
  val node = DebugCustomNexusNode(
    sourceFn = { seq =>
      if (seq.size == 0) { //Allow no sources
        DebugCustomParams(Nil, 0)
      } else {
        val all_addrs = seq.map{_.addrs}.flatten
        require(all_addrs.size == all_addrs.distinct.size, "Different Custom sources can't use the same addresses.")
        val max_width = seq.foldLeft(0){(result, current) => result max current.width}
        DebugCustomParams(all_addrs.toList, max_width)
      }
    },
    sinkFn = { seq => new DebugCustomNull()},
    inputRequiresOutput,
    outputRequiresInput
  )

  lazy val module = new LazyModuleImp(this) {
    // require only one sink
    require(node.out.size == 1, "Must have exactly one sink node, not ${node.out.size}")
    // send address to all sources
    val (sink, sinkParam) = node.out.head
    val (sources, sourceParams) = node.in.unzip
    val decoded = sourceParams.map { x => x.addrs.foldLeft(false.B) { (result, current) => result || current.U === sink.addr}}
    sources.zipWithIndex.foreach { case (source, i) =>
      source.addr := sink.addr
      // decode the 'valid' signal based on address
      source.valid := sink.valid & decoded(i)
    }
    // mux correct 'ready' and 'data' based on address
    sink.ready := (decoded zip sources).foldLeft(false.B){case (result, (d, i)) => result || (d & i.ready)}
    sink.data := (decoded zip sources).foldLeft(0.U){ case (result, (d, i)) => result | Mux(i.ready, i.data, 0.U)}

  }
}
