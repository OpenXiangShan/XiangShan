/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

//package gpu
//
//import chisel3._
//import chisel3.util._
//
//import bus.axi4._
//import device.AXI4SlaveModule
//import utils._
//import utility._
//
//class PixelBundle extends Bundle {
//  val a = UInt(8.W)
//  val r = UInt(8.W)
//  val g = UInt(8.W)
//  val b = UInt(8.W)
//}
//
///* struct texture {
// *   uint32_t pixels[TextureW * TextureH];
// * } __attribute__((packed));
// */
//class TextureLineBundle extends Bundle {
//  val pixels = Vec(8, new PixelBundle)
//}
//
///* struct sprite {
// *   uint16_t texture, x, y;
// *   uint32_t display : 4;
// *   uint32_t z : 12;
// * } __attribute__((packed));
// */
//class SpriteBundle extends Bundle {
//  val z = UInt(12.W)
//  val display = UInt(4.W)
//  val y = UInt(16.W)
//  val x = UInt(16.W)
//  val texture = UInt(16.W)
//}
//
//trait GPUConst {
//  val BeatBytes = 4
//
//  val TextureW = 8
//  val TextureH = 8
//  val ColorBytes = 4
//
//  val TextureLineBytes = TextureW * ColorBytes
//  val TextureLineBeats = TextureLineBytes / BeatBytes
//  val TextureLineShift = log2Up(TextureLineBytes)
//  val TextureBytes = TextureLineBytes * TextureH
//  val TextureShift = log2Up(TextureBytes)
//  val TextureMaxNum = 65536 // 0 indicate the end
//  val TextureIdBits = log2Up(TextureMaxNum)
//  val TextureArrayBytes = TextureMaxNum * TextureBytes
//  val TextureBase = 0x60000000L - TextureArrayBytes * 2
//
//  def textureLineAddr(idx: UInt, line: UInt): UInt = TextureBase.U |
//    (idx(TextureIdBits - 1, 0) << TextureShift.U) |
//    (line(2, 0) << TextureLineShift.U)
//
//  val SpriteBase = TextureBase + TextureArrayBytes
//  val SpriteBytes = 8
//  val SpriteBeats = SpriteBytes / BeatBytes
//  val SpriteShift = log2Up(SpriteBytes)
//  def spriteAddr(idx: UInt): UInt = SpriteBase.U | (idx << SpriteShift.U)
//
//  val ScreenW = 400
//  val ScreenH = 300
//  val FrameBufBase = 0x40000000L
//  def fbAddr(x: UInt, y: UInt): UInt = {
//    assert(x < ScreenW.U && y < ScreenH.U)
//    FrameBufBase.U + ((y * ScreenW.U + x) << 2)
//  }
//}
//
//class GPUOutBundle extends Bundle {
//  // can use 32 bit after implementing burst
//  val metaData = new AXI4
//  val fb = new AXI4
//}
//
//class AXI4GPU extends AXI4SlaveModule(new AXI4Lite, new GPUOutBundle) with GPUConst {
//  val out = io.extra.get
//
//  // control registers
//  def index(addr: UInt) = (addr & 0xf.U) >> 2
//  val statIdx = 0
//  val ctrlIdx = 1
//
//  val statReg = Reg(UInt(32.W))
//  val ctrlReg = Reg(UInt(32.W))
//
//  def readReg(addr: UInt) = LookupTree(index(addr), List(
//    statIdx.U -> statReg,
//    ctrlIdx.U -> ctrlReg
//  ))
//  in.r.bits.data := RegEnable(readReg(in.ar.bits.addr), in.ar.fire)
//
//  val wIdx = index(in.aw.bits.addr)
//  val wdata = genWdata(readReg(in.aw.bits.addr))
//  when (in.aw.fire) {
//    when (wIdx === ctrlIdx.U) { ctrlReg := wdata }
//  }
//
//  val startCmd = ctrlReg(0) && !RegNext(ctrlReg(0))
//
//  val s_idle :: s_sprite_read :: s_texture_read :: s_render_line :: s_render_bwait :: Nil = Enum(5)
//  val state = RegInit(s_idle)
//  statReg := (state =/= s_idle)
//
//  out := DontCare
//  out.metaData.ar.bits.prot  := AXI4Parameters.PROT_PRIVILEDGED
//  out.metaData.ar.bits.id    := 0.U
//  out.metaData.ar.bits.size := "b10".U // 32 bit
//  out.metaData.ar.bits.burst := AXI4Parameters.BURST_INCR
//  out.metaData.ar.bits.lock  := false.B
//  out.metaData.ar.bits.cache := 0.U
//  out.metaData.ar.bits.qos   := 0.U
//  out.metaData.ar.bits.user  := 0.U
//  out.fb.w.bits.last   := false.B
//  out.fb.aw.bits := out.metaData.ar.bits
//
//  out.metaData.r.ready := false.B
//  val metaDataRwait = RegInit(false.B)
//
//  val spriteIdx = Counter(65536)
//  when (state === s_idle && startCmd) {
//    printf("GPU start!!!!\n");
//    state := s_sprite_read
//    spriteIdx.value := 0.U
//  }
//
//  val textureLineCnt = Counter(TextureH)
//
//  val spriteBufReg = Reg(Vec(SpriteBeats, UInt(32.W)))
//  val spriteBuf = spriteBufReg.asTypeOf(new SpriteBundle)
//  val spriteReadCnt = Counter(SpriteBeats)
//  when (state === s_sprite_read) {
//    out.metaData.ar.bits.addr := spriteAddr(spriteIdx.value)
//    out.metaData.ar.bits.len := (SpriteBeats - 1).U  // 2 beats
//    out.metaData.r.ready := true.B
//    when (out.metaData.ar.fire) { metaDataRwait := true.B }
//
//    when (out.metaData.r.fire) {
//      spriteBufReg(spriteReadCnt.value) := out.metaData.r.bits.data
//      when (spriteReadCnt.inc()) {
//        metaDataRwait := false.B
//        textureLineCnt.value := 0.U
//        // since textureId is read at the first beat before,
//        // we can use a valid textureId here
//        val isEnd = spriteBuf.texture === 0.U
//        state := Mux(isEnd, s_idle, s_texture_read)
//      }
//    }
//  }
//
//  val textureLineBuf = Reg(Vec(TextureLineBeats, UInt(32.W)))
//  val textureLineReadCnt = Counter(TextureLineBeats)
//  when (state === s_texture_read) {
//    out.metaData.ar.bits.addr := textureLineAddr(spriteBuf.texture, textureLineCnt.value)
//    out.metaData.ar.bits.len := (TextureLineBeats - 1).U  // 8 beats
//    out.metaData.r.ready := true.B
//    when (out.metaData.ar.fire) { metaDataRwait := true.B }
//
//    when (out.metaData.r.fire) {
//      textureLineBuf(textureLineReadCnt.value) := out.metaData.r.bits.data
//      when (textureLineReadCnt.inc()) {
//        metaDataRwait := false.B
//        state := s_render_line
//      }
//    }
//  }
//
//  val textureLineWriteCnt = Counter(TextureLineBeats)
//  val wSend = Wire(Bool())
//  out.fb.aw.bits.addr := fbAddr(x = spriteBuf.x, y = spriteBuf.y + textureLineCnt.value)
//  out.fb.aw.bits.len := (TextureLineBeats - 1).U  // 8 beats
//  out.fb.w.bits.data := textureLineBuf(textureLineWriteCnt.value)
//  out.fb.w.bits.strb := 0xf.U
//  out.fb.w.bits.last := textureLineWriteCnt.value === (TextureLineBeats - 1).U
//  when (state === s_render_line) {
//    // FIXME: check the result of renderLineMask
//    //val renderLineMask = Cat(textureLineBuf.asTypeOf(new TextureLineBundle).pixels.map(
//    //  c => Mux(c.a === 0.U, 0.U(4.W), 0xf.U(4.W))))
//
//    when (out.fb.w.fire) { textureLineWriteCnt.inc() }
//    when (wSend) { state := s_render_bwait }
//  }
//
//  when (state === s_render_bwait) {
//    when (out.fb.b.fire) {
//      val finishOneTexture = textureLineCnt.inc()
//      when (finishOneTexture) { spriteIdx.inc() }
//      state := Mux(finishOneTexture, s_sprite_read, s_texture_read)
//    }
//  }
//
//  out.metaData.ar.valid := BoolStopWatch(
//    (state === s_sprite_read || state === s_texture_read) && !metaDataRwait, out.metaData.ar.fire)
//  out.metaData.aw.valid := false.B
//  out.metaData.w.valid := false.B
//  out.metaData.b.ready := true.B
//
//  val awAck = BoolStopWatch(out.fb.aw.fire, wSend)
//  val wAck = BoolStopWatch(out.fb.w.fire && out.fb.w.bits.last, wSend)
//  wSend := (out.fb.aw.fire && out.fb.w.fire && out.fb.w.bits.last) || (awAck && wAck)
//
//  out.fb.aw.valid := (state === s_render_line) && !awAck
//  out.fb.w .valid := (state === s_render_line) && !wAck
//  out.fb.b.ready := BoolStopWatch(wSend, out.fb.b.fire)
//  out.fb.ar.valid := false.B
//  out.fb.r.ready := true.B
//}
