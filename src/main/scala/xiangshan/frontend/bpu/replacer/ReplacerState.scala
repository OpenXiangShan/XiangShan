// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu.replacer

import chisel3._
import chisel3.util._

// Independent replacer state management enables finer-grained clock gating
class ReplacerState(
    NumSets:           Int,
    StateBits:         Int,
    NumExtraReadPort:  Int = 0,
    NumExtraWritePort: Int = 0
) extends Module {
  class ReplacerStateIO extends Bundle {
    class Read extends Bundle {
      val setIdx: UInt = Input(UInt(SetIdxBits.W))
      val state:  UInt = Output(UInt(StateBits.W))
    }

    class Write extends Bundle { // direction included in Valid() call
      val setIdx: UInt = UInt(SetIdxBits.W)
      val state:  UInt = UInt(StateBits.W)
    }

    // magic number 2: we need at least 2 ports for predict/train read/write
    val read:  Vec[Read]         = Vec(2 + NumExtraReadPort, new Read)
    val write: Vec[Valid[Write]] = Vec(2 + NumExtraWritePort, Flipped(Valid(new Write)))

    def predictRead: Read      = read.head
    def trainRead:   Read      = read.last
    def extraRead:   Seq[Read] = read.init.tail

    def predictWrite: Valid[Write]      = write.head
    def trainWrite:   Valid[Write]      = write.last
    def extraWrite:   Seq[Valid[Write]] = write.init.tail
  }

  def SetIdxBits: Int = log2Ceil(NumSets)

  require(NumExtraReadPort >= 0 && NumExtraWritePort >= 0, "NumExtraPort cannot be negative")

  val io: ReplacerStateIO = IO(new ReplacerStateIO)

  private val states = RegInit(VecInit(Seq.fill(NumSets)(0.U.asTypeOf(UInt(StateBits.W)))))

  /* *** write *** */
  // NOTE: when write-write conflict (write same setIdx), port with higher physical idx will take effect,
  //       therefore, with the default parameter (2 ports), trainWrite has higher priority than predictWrite
  // NOTE: we don't have to explicitly check if there's a conflict, chisel will handle this,
  //       i.e. for { when(valid(i)) { state := ... } } will become a `if (valid_0) ... else if (valid_1) ...` chain
  io.write.foreach { port =>
    when(port.valid) {
      states(port.bits.setIdx) := port.bits.state
    }
  }

  /* *** read *** */
  io.read.foreach(port => port.state := states(port.setIdx))
}
