package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{XSBundle, XSModule}

// LPAD 阅读：本模块只跟踪“下一条架构指令是否必须为 LPAD”，不读取 x7，也不比较 20 位标签。
// enable 是当前模式的运行时 LPE；HasZicfilp 则在 DecodeStage 中决定是否生成本模块。
class SpecZicfilpIO(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  // bits 是要恢复的 ELP 值。它与 redirect.valid 一起使用，不是跳转目标地址。
  val redirect = Flipped(ValidIO(Bool()))
  // 每个 i 对应本拍的一个输出位置，按从老到年轻排列；不是第 i 个时钟周期。
  // DecodeStage 已将 valid 限定为“有效且 firstUop”，fire 还要求下游 ready。
  val valid = Input(Vec(DecodeWidth, Bool()))
  val fire = Input(Vec(DecodeWidth, Bool()))
  // isJalr 仅包含 rs1 不为 x1/x5/x7 的受保护 JALR。
  // isLPAD 表示 AUIPC rd=x0 且 PC 对齐；标签是否匹配要到 JumpUnit 才知道。
  val isJalr = Input(Vec(DecodeWidth, Bool()))
  val isLPAD = Input(Vec(DecodeWidth, Bool()))
  // exception：期待 LPAD 却没有合法 LPAD，随当前 uop 标记 software-check。
  // lpadValid：当前 LPAD 需要检查标签，随 uop 传到执行阶段，不表示检查已经成功。
  val exception = Output(Vec(DecodeWidth, Bool()))
  val lpadValid = Output(Vec(DecodeWidth, Bool()))
}

class SpecZicfilp(implicit p: Parameters) extends XSModule {
  val io = IO(new SpecZicfilpIO)

  // 本模块唯一跨周期保存的 ELP 寄存器：0=无需期待，1=下一架构指令必须是 LPAD。
  // Decode 已经接收 LPAD、但它还在 Issue 等 x7 时，这个推测状态就可以清零。
  // trap 要保存的提交边界状态另由 ArchZicfilp 维护，不能拿本寄存器代替。
  val speculativeELP = RegInit(false.B)
  // 两组 Wire 都是组合连线，不是两组寄存器。N 个位置有 N+1 个前后边界。
  // outputELP(i)：按本组前面 valid 的 first uop 推演，位置 i 之前应看到的 ELP。
  // nextELP(i)：只按前面 fire 的 first uop 推演，实际接收后应保存的 ELP。
  val outputELP = Wire(Vec(DecodeWidth + 1, Bool()))
  val nextELP = Wire(Vec(DecodeWidth + 1, Bool()))
  outputELP.head := speculativeELP
  nextELP.head := speculativeELP

  // Scala for 在 elaboration 时展开为各位置的组合选择器，不会每循环一次增加一拍。
  // 以下示例均假设 enable=1，期间没有 redirect。
  // 例：旧 ELP=0，输出 [JALR-link, 同条 jr, LPAD, ADD]，全部接收时：
  // valid/fire=[1,0,1,1]，outputELP/nextELP=[0,1,1,0,0]，lpadValid=[0,0,1,0]。
  // link 设置期待，jr 不是 firstUop 因而跳过，LPAD 在同一拍看到这个期待。
  for (i <- 0 until DecodeWidth) {
    val checkLandingPad = io.valid(i) && outputELP(i)
    io.exception(i) := io.enable && checkLandingPad && !io.isLPAD(i)
    io.lpadValid(i) := io.enable && checkLandingPad && io.isLPAD(i)
    // 任意 valid 的架构指令都会决定后续期待：受保护 JALR 置 1，其他指令置 0。
    // 非 LPAD 消费了期待时，上面已为它标记异常；推测链继续前进，ROB 负责精确 trap。
    outputELP(i + 1) := Mux(io.valid(i), io.isJalr(i), outputELP(i))
    // ready=0 时 fire=0，选择旧值。本拍可输出检查标志，但不能提前改变持久状态。
    nextELP(i + 1) := Mux(io.fire(i), io.isJalr(i), nextELP(i))
  }

  // 阻塞例：旧 ELP=1，输出 [jr,LPAD,ADD,ADD]，valid=[0,1,1,1]、fire 全 0：
  // outputELP=[1,1,0,0,0]，nextELP=[1,1,1,1,1]；时钟沿后寄存器仍为 1。
  // 恢复接收的沿，LPADValid=1 被下一级锁存，寄存器才按 nextELP.last 清 0。
  // 以后 JumpUnit 使用该 uop 携带的检查标志，不再读取当时的实时 speculativeELP。
  // 下列 when 驱动 Reg 的 D 端，在时钟沿生效；优先级为恢复 > 关闭 > 正常推进。
  when(io.redirect.valid) {
    speculativeELP := io.redirect.bits
  }.elsewhen(!io.enable) {
    speculativeELP := false.B
  }.otherwise {
    speculativeELP := nextELP.last
  }
}
