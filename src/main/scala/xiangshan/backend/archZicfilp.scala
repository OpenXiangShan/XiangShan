package xiangshan.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

// LPAD 阅读：接口来自 ROB 的实际提交。commitValid 不包含 ROB walk/回滚的条目。
class ArchZicfilpIO(implicit p: Parameters) extends XSBundle {
  val commitValid = Input(Vec(CommitWidth, Bool()))
  val commitJalr = Input(Vec(CommitWidth, Bool()))
  val commitLPAD = Input(Vec(CommitWidth, Bool()))
  val enable = Input(Bool())
  val trap = Input(Bool())
  val xret = Flipped(ValidIO(Bool()))
  val archELP = Output(Bool())
}

class ArchZicfilp(implicit p: Parameters) extends XSModule {
  val io = IO(new ArchZicfilpIO)

  // 真正的架构 ELP 寄存器，描述“已提交程序”是否仍在等待目标 LPAD。
  // 例：JALR 已提交，目标 LPAD 已在 Decode 接收但未提交：specELP=0，而本寄存器=1。
  // 中断/trap 要保存的是这个提交边界的 1，不能保存更超前的推测状态。
  val archELP = RegInit(false.B)
  val hasCommit = io.commitValid.asUInt.orR
  // foldLeft 生成按提交位置从老到年轻串接的组合选择器，不是逐周期循环。
  // 受保护 JALR 置 1，合法 LPAD 清 0，普通指令/无效位置保持传入值。
  // 同拍 [JALR,LPAD,ADD] 得 0；[LPAD,JALR] 得 1，不能只看最后一条是否 JALR。
  val commitELP = io.commitValid.zip(io.commitJalr).zip(io.commitLPAD).foldLeft(archELP) {
    case (elp, ((valid, jalr), lpad)) =>
      Mux(io.enable && valid && jalr, true.B, Mux(io.enable && valid && lpad, false.B, elp))
  }

  // 这些赋值在时钟沿更新寄存器。trap 优先清零供 handler 使用；旧值已由 ROB 快照保存。
  // xRET 恢复对应返回事务的 ELP；标签错误的 LPAD 不会提交，因此不会在提交链提前清零。
  when(io.trap) {
    archELP := false.B
  }.elsewhen(io.xret.valid) {
    archELP := io.xret.bits
  }.elsewhen(!io.enable) {
    archELP := false.B
  }.elsewhen(hasCommit) {
    archELP := commitELP
  }

  io.archELP := archELP && io.enable
}
