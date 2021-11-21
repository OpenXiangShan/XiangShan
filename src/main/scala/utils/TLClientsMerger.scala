package utils

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.TLAdapterNode

class TLClientsMerger(debug: Boolean)(implicit p: Parameters) extends LazyModule {

  val node = TLAdapterNode(
    clientFn = s => {
      val sourceIds = s.masters.map(_.sourceId)
      val minId = sourceIds.map(_.start).min
      val maxId = sourceIds.map(_.end).max
      val merged = s.v1copy(
        clients = Seq(s.masters.head.v1copy(
          sourceId = IdRange(minId, maxId),
          visibility = s.masters.flatMap(_.visibility)
        ))
      )
      if(debug){
        println("TLClientsMerger: Merging clients:")
        for(c <- s.masters){
          println(c)
        }
        println("Merged params:")
        println(merged.masters)
      }
      for(c <- s.masters.tail){
        // ensure all masters have same params except `sourceId` and `visiblity`
        val head = s.masters.head
        for(i <- (0 until head.productArity)
          .filterNot(x => x == 1 || x == 3)) // skip `sourceId` and `visibility`
        {
          require(head.productElement(i) == c.productElement(i))
        }
      }
      merged
    }
  )

  lazy val module = new LazyModuleImp(this){
    require(node.in.size == 1)
    for((in, out) <- node.in.map(_._1).zip(node.out.map(_._1))){
      out <> in
    }
    for(((in, edgeIn), (out, edgeOut)) <- node.in.zip(node.out)){
      out <> in
      // handle b channel carefully
      val banks = edgeIn.master.masters.size
      if(banks != 1){
        val blockBytes = edgeIn.master.masters.head.visibility.head.alignment
        val bankBits = log2Up(banks)
        val bankIdx = (out.b.bits.address >> log2Up(blockBytes))(bankBits - 1, 0)
        val startIds = VecInit(edgeIn.master.masters.map(_.sourceId.start.U))
        in.b.bits.source := startIds(bankIdx)
      }
    }
  }

}

object TLClientsMerger {
  def apply(debug: Boolean = true)(implicit p: Parameters) = {
    val merger = LazyModule(new TLClientsMerger(debug))
    merger.node
  }
}
