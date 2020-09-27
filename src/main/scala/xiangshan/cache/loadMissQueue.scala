package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._

class LoadMissEntry extends DCacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val lsu         = Flipped(new DCacheLineIO)

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)

    val data_req  = DecoupledIO(new L1DataReadReq)
    val data_resp = Input(Vec(nWays, Vec(blockRows, Bits(encRowBits.W))))

    val idx = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))
  })

  val s_invalid :: s_miss_req :: s_miss_resp :: s_miss_finish :: s_data_read_req :: s_data_read_resp :: s_resp :: Nil = Enum(7)
  val state = RegInit(s_invalid)

  val req     = Reg(new DCacheLineReq)
  val resp    = Reg(new DCacheLineResp)

  val req_idx = get_idx(req.addr)
  val req_tag = get_tag(req.addr)
  val req_block_addr = get_block_addr(req.addr)
  val reg_miss_resp = Reg(new MissResp)

  // assign default values to output signals
  io.lsu.req.ready     := state === s_invalid
  io.lsu.resp.valid    := false.B
  io.lsu.resp.bits     := DontCare

  io.miss_req.valid      := false.B
  io.miss_req.bits       := DontCare
  io.miss_finish.valid   := false.B
  io.miss_finish.bits    := DontCare

  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.idx.bits := req_idx
  io.tag.bits := req_tag


  XSDebug("entry: %d state: %d\n", io.id, state)
  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    when (io.lsu.req.fire()) {
      assert(io.lsu.req.bits.cmd === M_XRD)
      assert(!io.lsu.req.bits.meta.replay)
      req       := io.lsu.req.bits
      resp.meta := io.lsu.req.bits.meta
      resp.miss := false.B
      resp.nack := false.B
      state := s_miss_req
    }
  }

  // --------------------------------------------
  when (state === s_miss_req) {
    io.miss_req.valid          := true.B
    io.miss_req.bits.cmd       := req.cmd
    io.miss_req.bits.addr      := req_block_addr
    io.miss_req.bits.client_id := io.id

    when (io.miss_req.fire()) {
      state := s_miss_resp
    }
  }

  when (state === s_miss_resp) {
    when (io.miss_resp.fire()) {
      reg_miss_resp := io.miss_resp.bits
      resp.data     := io.miss_resp.bits.data
      when (io.miss_resp.bits.has_data) {
        state := s_resp
      } .otherwise {
        // miss queue says that data is already in dcache
        // so we need to read it
        state := s_data_read_req
      }
    }
  }

  val dataArrayLatency = 2
  val data_array_ctr  = Reg(UInt(log2Up(dataArrayLatency).W))

  when (state === s_data_read_req) {
    // Data read for new requests
    io.data_req.valid       := true.B
    io.data_req.bits.addr   := req_block_addr
    io.data_req.bits.way_en := reg_miss_resp.way_en
    io.data_req.bits.rmask  := ~0.U(blockRows.W)

    when (io.data_req.fire()) {
      state := s_data_read_resp
      data_array_ctr := 0.U
    }
  }

  when (state === s_data_read_resp) {
    data_array_ctr := data_array_ctr + 1.U
    when (data_array_ctr === (dataArrayLatency - 1).U) {
      val way_idx = OHToUInt(reg_miss_resp.way_en)
      resp.data := Cat((0 until blockRows).reverse map { i =>
        val row = io.data_resp(way_idx)(i)
        // decode each word in this row
        val row_decoded = Cat((0 until rowWords).reverse map { w =>
          val data_word = row(encWordBits * (w + 1) - 1, encWordBits * w)
          val decoded = cacheParams.dataCode.decode(data_word)
          val data_word_decoded = decoded.corrected
          assert(!decoded.uncorrectable)
          data_word_decoded
        })
      row_decoded
      })
      state := s_resp
    }
  }


  // --------------------------------------------
  when (state === s_resp) {
    io.lsu.resp.valid := true.B
    io.lsu.resp.bits  := resp

    when (io.lsu.resp.fire()) {
      state := s_miss_finish
    }
  }

  when (state === s_miss_finish) {
    io.miss_finish.valid          := true.B
    io.miss_finish.bits.client_id := io.id
    io.miss_finish.bits.entry_id  := reg_miss_resp.entry_id
    when (io.miss_finish.fire()) {
      state := s_invalid
    }
  }
}


class LoadMissQueue extends DCacheModule
{
  val io = IO(new Bundle {
    val lsu         = Flipped(new DCacheLineIO)

    val miss_req    = DecoupledIO(new MissReq)
    val miss_resp   = Flipped(ValidIO(new MissResp))
    val miss_finish = DecoupledIO(new MissFinish)

    val data_req    = DecoupledIO(new L1DataReadReq)
    val data_resp   = Input(Vec(nWays, Vec(blockRows, Bits(encRowBits.W))))

  })

  val miss_req_arb    = Module(new Arbiter(new MissReq,        cfg.nLoadMissEntries))
  val miss_finish_arb = Module(new Arbiter(new MissFinish,     cfg.nLoadMissEntries))
  val data_req_arb    = Module(new Arbiter(new L1DataReadReq,  cfg.nLoadMissEntries))
  val resp_arb        = Module(new Arbiter(new DCacheLineResp, cfg.nLoadMissEntries))

  val idx_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))
  val tag_matches = Wire(Vec(cfg.nLoadMissEntries, Bool()))

  val tag_match   = Mux1H(idx_matches, tag_matches)
  val idx_match   = idx_matches.reduce(_||_)


  val req             = io.lsu.req
  val entry_alloc_idx = Wire(UInt())
  val pri_rdy         = WireInit(false.B)
  val pri_val         = req.valid && !idx_match

  val entry_id_MSB = reqIdWidth - 1
  val entry_id_LSB = reqIdWidth - loadMissQueueEntryIdWidth

  val entries = (0 until cfg.nLoadMissEntries) map { i =>
    val entry = Module(new LoadMissEntry)

    entry.io.id := i.U(loadMissQueueEntryIdWidth.W)

    idx_matches(i) := entry.io.idx.valid && entry.io.idx.bits === get_idx(req.bits.addr)
    tag_matches(i) := entry.io.tag.valid && entry.io.tag.bits === get_tag(req.bits.addr)

    // lsu req and resp
    val entry_lsu = entry.io.lsu
    entry_lsu.req.valid := (i.U === entry_alloc_idx) && pri_val
    when (i.U === entry_alloc_idx) {
      pri_rdy := entry_lsu.req.ready
    }
    entry_lsu.req.bits  := req.bits

    resp_arb.io.in(i)   <> entry_lsu.resp

    miss_req_arb.io.in(i)  <> entry.io.miss_req
    data_req_arb.io.in(i)  <> entry.io.data_req
    entry.io.miss_resp.valid := (i.U === io.miss_resp.bits.client_id) && io.miss_resp.valid
    entry.io.miss_resp.bits  := io.miss_resp.bits
    entry.io.data_resp       := io.data_resp

    miss_finish_arb.io.in(i) <> entry.io.miss_finish
    entry
  }

  entry_alloc_idx    := PriorityEncoder(entries.map(m=>m.io.lsu.req.ready))

  // whenever index matches, do not let it in
  req.ready      := pri_rdy && !idx_match
  io.lsu.resp    <> resp_arb.io.out
  io.miss_req    <> miss_req_arb.io.out
  io.data_req    <> data_req_arb.io.out
  io.miss_finish <> miss_finish_arb.io.out

  // debug output
  when (req.fire()) {
    XSDebug(s"req cmd: %x addr: %x data: %x mask: %x id: %d replay: %b\n",
      req.bits.cmd, req.bits.addr, req.bits.data, req.bits.mask, req.bits.meta.id, req.bits.meta.replay)
  }

  val resp = io.lsu.resp
  when (resp.fire()) {
    XSDebug(s"resp: data: %x id: %d replay: %b miss: %b nack: %b\n",
      resp.bits.data, resp.bits.meta.id, resp.bits.meta.replay, resp.bits.miss, resp.bits.nack)
  }

  val miss_req = io.miss_req
  XSDebug(miss_req.fire(), "miss_req cmd: %x addr: %x client_id: %d\n",
    miss_req.bits.cmd, miss_req.bits.addr, miss_req.bits.client_id)

  val miss_resp = io.miss_resp
  XSDebug(miss_resp.fire(), "miss_resp client_id: %d entry_id: %d\n",
    miss_resp.bits.client_id, miss_resp.bits.entry_id)

  val miss_finish = io.miss_finish
  XSDebug(miss_finish.fire(), "miss_finish client_id: %d entry_id: %d\n",
    miss_finish.bits.client_id, miss_finish.bits.entry_id)
}
