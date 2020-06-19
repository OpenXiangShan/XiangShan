package fpu.fma

import chisel3._
import chisel3.util._

class LzaIO(len: Int) extends Bundle {
  val a, b = Input(UInt(len.W))
  val out = Output(UInt(log2Up(len+1).W))
  override def cloneType: LzaIO.this.type = new LzaIO(len).asInstanceOf[this.type]
}

// Leading Zero Anticipator
class LZA(len: Int) extends Module {
  val io = IO(new LzaIO(len))
  /**  msb             lsb
    *  0    1    2 ... n-1
    */
  val (a, b) = (io.a.asBools().reverse, io.b.asBools().reverse)
  //
  val g, s, e, f = Wire(Vec(len, Bool()))

  for(i <- 0 until len){
    g(i) := a(i) & !b(i)
    s(i) := !a(i) & b(i)
    e(i) := a(i) === b(i)
  }

  f(0) := (s(0) & !s(1)) | (g(0) & !g(1))
  f(len-1) := false.B

  for(i <- 1 until len-1){
    f(i) :=  (e(i-1) & g(i) & !s(i+1)) |
      (!e(i-1) & s(i) & !s(i+1)) |
      (e(i-1) & s(i) & !g(i+1)) |
      (!e(i-1) & g(i) & !g(i+1))
  }

  val res = PriorityEncoder(f)

  val p, n, z = Wire(Vec(len, Bool()))
  p(0) := g(0)
  n(0) := s(0)
  p(1) := g(1)
  n(1) := s(1)
  for(i <- 2 until len){
    p(i) := (e(i-1) | e(i-2) & g(i-1) | !e(i-2) & s(i-1)) & g(i)
    n(i) := (e(i-1) | e(i-2) & s(i-1) | !e(i-2) & g(i-1)) & s(i)
  }
  for(i <- 0 until len){
    z(i) := !(p(i) | n(i))
  }

  class TreeNode extends Bundle {
    val Z, P, N = Bool()
  }

  def buildOneLevel(nodes: Seq[TreeNode]): Seq[TreeNode] = {
    nodes match {
      case Seq(_) => nodes
      case Seq(_, _) => nodes
      case Seq(left, mid, right) =>
        val next_l, next_r = Wire(new TreeNode)
        next_l.P := left.P | left.Z & mid.P
        next_l.N := left.N | left.Z & mid.N
        next_l.Z := left.Z & mid.Z
        next_r.P := !left.Z & mid.P | right.P & (left.Z | mid.Z)
        next_r.N := !left.Z & mid.N | right.N & (left.Z | mid.Z)
        next_r.Z := right.Z & (left.Z | mid.Z)
        Seq(next_l, next_r)
      case _ =>
        buildOneLevel(nodes.take(3)) ++ buildOneLevel(nodes.drop(3))
    }
  }

  def detectionTree(nodes: Seq[TreeNode]): Bool = {
    assert(nodes.size >= 2)
    nodes match {
      case Seq(left, right) =>
        left.P & right.N | left.N & right.P
      case _ =>
        val nextLevel = buildOneLevel(nodes)
        detectionTree(nextLevel)
    }
  }

  val nodes = (0 until len).map(i => {
    val treeNode = Wire(new TreeNode)
    treeNode.P := p(i)
    treeNode.N := n(i)
    treeNode.Z := z(i)
    treeNode
  })

  val error = detectionTree(nodes)

  io.out := res + error
}
