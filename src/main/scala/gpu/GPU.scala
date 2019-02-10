package gpu

import chisel3._
import chisel3.util._

import noop.MemIO

class PixelBundle extends Bundle {
  val a = UInt(8.W)
  val r = UInt(8.W)
  val g = UInt(8.W)
  val b = UInt(8.W)
}

/* struct texture {
 *   uint32_t pixels[TextureW * TextureH];
 * } __attribute__((packed));
 */
class TextureLineBundle extends Bundle {
  val pixels = Vec(8, new PixelBundle)
}

/* struct sprite {
 *   uint16_t texture, x, y;
 *   uint32_t display : 4;
 *   uint32_t z : 12;
 * } __attribute__((packed));
 */
class SpriteBundle extends Bundle {
  val z = UInt(12.W)
  val display = UInt(4.W)
  val y = UInt(16.W)
  val x = UInt(16.W)
  val texture = UInt(16.W)
}

trait GPUConst {
  val TextureW = 8
  val TextureH = 8
  val ColorBytes = 4

  val TextureLineBytes = TextureW * ColorBytes
  val TextureLineShift = log2Up(TextureLineBytes)
  val TextureBytes = TextureLineBytes * TextureH
  val TextureShift = log2Up(TextureBytes)
  val TextureMaxNum = 65536 // 0 indicate the end
  val TextureIdBits = log2Up(TextureMaxNum)
  val TextureArrayBytes = TextureMaxNum * TextureBytes
  val TextureBase = 0x88000000L - TextureArrayBytes * 2

  def textureLineAddr(idx: UInt, line: UInt): UInt = TextureBase.U |
    (idx(TextureIdBits - 1, 0) << TextureShift.U) |
    (line(2, 0) << TextureLineShift.U)

  val SpriteBase = TextureBase + TextureArrayBytes
  val SpriteBytes = 8
  val SpriteShift = log2Up(SpriteBytes)
  def spriteAddr(idx: UInt): UInt = SpriteBase.U | (idx << SpriteShift.U)

  val ScreenW = 400
  val ScreenH = 320
  val FrameBufBase = 0x80040000L
  def fbAddr(x: UInt, y: UInt): UInt = {
    assert(x < ScreenW.U && y < ScreenH.U)
    FrameBufBase.U + ((y * ScreenW.U + x) << 2)
  }
}

class GPU extends Module with GPUConst {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val out = new MemIO(256)
  })

  val startCmd = io.start && !RegNext(io.start)

  val s_idle :: s_sprite_read :: s_texture_read :: s_render_line :: s_sync :: s_render_align :: s_render_unalign :: Nil = Enum(7)
  val state = RegInit(s_idle)

  io.out := DontCare

  val spriteIdx = Counter(65536)
  when (state === s_idle && startCmd) {
    printf("GPU start!!!!\n");
    state := s_sprite_read
    spriteIdx.value := 0.U
  }

  val spriteBuf = Reg(new SpriteBundle)
  val textureLineCnt = Counter(TextureH)
  when (state === s_sprite_read) {
    io.out.a.bits.addr := spriteAddr(spriteIdx.value)
    io.out.a.bits.size := log2Up(SpriteBytes).U

    // assume no read delay
    val rdata = io.out.r.bits.data.asTypeOf(new SpriteBundle)
    spriteBuf := rdata
    textureLineCnt.value := 0.U

    val isEnd = rdata.texture === 0.U
    state := Mux(isEnd, s_sync, s_texture_read)
  }

  val textureLineBuf = Reg(UInt((TextureLineBytes * 8).W))
  when (state === s_texture_read) {
    io.out.a.bits.addr := textureLineAddr(spriteBuf.texture, textureLineCnt.value)
    io.out.a.bits.size := log2Up(TextureLineBytes).U

    // assume no read delay
    textureLineBuf := io.out.r.bits.data
    state := s_render_line
  }

  when (state === s_render_line) {
    val renderAddr = fbAddr(x = spriteBuf.x, y = spriteBuf.y + textureLineCnt.value)
    val renderLineMask = Cat(textureLineBuf.asTypeOf(new TextureLineBundle).pixels.map(
      c => Mux(c.a === 0.U, 0.U(4.W), 0xf.U(4.W))))

    // should handle sprite accross a tile
    assert((renderAddr & (TextureLineBytes - 1).U) === 0.U)

    io.out.a.bits.addr := renderAddr
    io.out.a.bits.size := log2Up(TextureLineBytes).U
    io.out.w.bits.data := textureLineBuf
//    io.out.wmask := renderLineMask

    val finishOneTexture = textureLineCnt.inc()
    when (finishOneTexture) { spriteIdx.inc() }
    state := Mux(finishOneTexture, s_sprite_read, s_texture_read)
  }

  when (state === s_sync) {
    io.out.a.bits.addr := 0x4104.U
    io.out.a.bits.size := 0x2.U
    io.out.w.bits.data := 1.U

    state := s_idle
  }

  io.out.a.valid := (state === s_sprite_read || state === s_texture_read || state === s_render_line || state === s_sync)
  io.out.w.valid := (state === s_render_line || state === s_sync)
}
