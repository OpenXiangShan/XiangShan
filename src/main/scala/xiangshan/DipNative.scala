/***************************************************************************************
* Copyright (c) 2024-2026 Institute of Computing Technology, Chinese Academy of Sciences
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan

import chisel3._
import chisel3.util.HasBlackBoxInline

private[xiangshan] object DipNative {
  val implementation: String = {
    val source = scala.io.Source.fromResource("tea/dip_sampler.h")
    try source.mkString finally source.close()
  }
  def cpp(name: String, rob: Int, rename: Int, commit: Int): String = {
    val words = 4 + rename * 8 + (2 * commit + 3) * 14
    s"""
       |$implementation
       |extern "C" void $name(unsigned char reset, unsigned char phase,
       |  const uint32_t *in, uint32_t *out) {
       |  static DipNativeSampler<$rob, $rename, $commit> sampler;
       |  alignas(8) uint64_t outputs[$words];
       |  sampler.evaluate(reset, phase, reinterpret_cast<const uint64_t *>(in), outputs);
       |  std::memcpy(out, outputs, sizeof(outputs));
       |}
       |""".stripMargin
  }
}

private[xiangshan] class DipNative(rob: Int, rename: Int, commit: Int, hart: Int, inWords: Int, outWords: Int)
  extends BlackBox with HasBlackBoxInline {
  override def desiredName: String = s"DipNative_${rob}_${rename}_${commit}_$hart"
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val phase = Input(Bool())
    val in = Input(UInt((inWords * 64).W))
    val out = Output(UInt((outWords * 64).W))
  })
  private val function = s"${desiredName}_eval"
  setInline(s"$desiredName.sv", s"""
    |module $desiredName(input reset, input phase,
    |  input [${64 * inWords - 1}:0] in, output reg [${64 * outWords - 1}:0] out);
    |  import "DPI-C" function void $function(input bit reset, input bit phase,
    |    input bit [${64 * inWords - 1}:0] in, output bit [${64 * outWords - 1}:0] out);
    |  always @(*) $function(reset, phase, in, out);
    |endmodule
    |""".stripMargin)
  difftest.DifftestModule.createCppDPICModule(function, DipNative.cpp(function, rob, rename, commit))
  difftest.DifftestModule.createCppExtModule(desiredName, s"""
    |${DipNative.implementation}
    |void $desiredName(uint8_t reset, uint8_t phase,
    |  unsigned _BitInt(${64 * inWords}) in, unsigned _BitInt(${64 * outWords}) &out) {
    |  static DipNativeSampler<$rob, $rename, $commit> sampler;
    |  alignas(8) uint64_t outputs[$outWords];
    |  sampler.evaluate(reset, phase, reinterpret_cast<const uint64_t *>(&in), outputs);
    |  std::memcpy(&out, outputs, sizeof(outputs));
    |}
    |""".stripMargin)
}
