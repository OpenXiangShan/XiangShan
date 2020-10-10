package xiangshan.cache

import chisel3._
import chisel3.util._
import utils.XSDebug
import freechips.rocketchip.tilelink.{TLBundleC, TLEdgeOut, TLPermissions}

class WritebackReq(sourceBits: Int) extends DCacheBundle {
  val tag = Bits(tagBits.W)
  val idx = Bits(idxBits.W)
  val source = UInt(sourceBits.W)
  val param = UInt(TLPermissions.cWidth.W) 
  val way_en = Bits(nWays.W)
  val voluntary = Bool()

  override def cloneType: WritebackReq.this.type = new WritebackReq(sourceBits).asInstanceOf[this.type]
}

class WritebackUnit(edge: TLEdgeOut) extends DCacheModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new WritebackReq(edge.bundle.sourceBits)))
    val resp = Output(Bool())
    val data_req = DecoupledIO(new L1DataReadReq)
    val data_resp = Input(Vec(nWays, Vec(blockRows, Bits(encRowBits.W))))
    val release = DecoupledIO(new TLBundleC(edge.bundle))
    val mem_grant = Input(Bool())
    val inflight_addr = Output(Valid(UInt()))
  })

  val req = Reg(new WritebackReq(edge.bundle.sourceBits))
  val s_invalid :: s_data_read_req :: s_data_read_resp :: s_active :: s_grant :: s_resp :: Nil = Enum(6)
  val state = RegInit(s_invalid)

  val data_req_cnt = RegInit(0.U(log2Up(refillCycles+1).W))

  val (_, last_beat, all_beats_done, beat_count) = edge.count(io.release)

  val wb_buffer = Reg(Vec(refillCycles, UInt(beatBits.W)))
  val acked = RegInit(false.B)

  // assign default value to signals
  io.req.ready       := false.B
  io.resp            := false.B

  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare

  io.release.valid   := false.B
  io.release.bits    := DontCare

  io.inflight_addr.valid := state =/= s_invalid
  io.inflight_addr.bits  := req.idx << blockOffBits

  XSDebug("state: %d\n", state)

  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire()) {
      // for report types: TtoT, BtoB, NtoN, we do nothing
      import freechips.rocketchip.tilelink.TLPermissions._
      def do_nothing(x: UInt) = x > BtoN
      when (do_nothing(io.req.bits.param)) {
        state := s_resp
      } .otherwise {
        state := s_data_read_req
        data_req_cnt := 0.U
        req := io.req.bits
        acked := false.B
      }
    }
  }

  val dataArrayLatency = 2
  val data_array_ctr  = Reg(UInt(log2Up(dataArrayLatency).W))

  when (state === s_data_read_req) {
    // Data read for new requests
    io.data_req.valid       := true.B
    io.data_req.bits.addr   := req.idx << blockOffBits
    io.data_req.bits.way_en := req.way_en
    io.data_req.bits.rmask  := ~0.U(blockRows.W)

    when (io.data_req.fire()) {
      state := s_data_read_resp
      data_array_ctr := 0.U
    }
  }

  when (state === s_data_read_resp) {
    data_array_ctr := data_array_ctr + 1.U
    when (data_array_ctr === (dataArrayLatency - 1).U) {
      val way_idx = OHToUInt(req.way_en)
      for (i <- 0 until refillCycles) {
        wb_buffer(i) := Cat((0 until beatRows).reverse map { j =>
          val idx = i * beatRows + j
          val row = io.data_resp(way_idx)(idx)
          // encode each word in this row
          val row_decoded = Cat((0 until rowWords).reverse map { w =>
            val data_word = row(encWordBits * (w + 1) - 1, encWordBits * w)
            val decoded = cacheParams.dataCode.decode(data_word)
            val data_word_decoded = decoded.corrected
            assert(!decoded.uncorrectable)
            data_word_decoded
          })
        row_decoded
        })
      }

      state := s_active
    }
  }

  // release
  val r_address = (Cat(req.tag, req.idx) << blockOffBits).asUInt()
  val id = cfg.nMissEntries

  val probeResponse = edge.ProbeAck(
    fromSource = id.U,
    toAddress = r_address,
    lgSize = log2Ceil(cfg.blockBytes).U,
    reportPermissions = req.param,
    data = wb_buffer(data_req_cnt)
  )

  val voluntaryRelease = edge.Release(
    fromSource = id.U,
    toAddress = r_address,
    lgSize = log2Ceil(cfg.blockBytes).U,
    shrinkPermissions = req.param,
    data = wb_buffer(data_req_cnt)
  )._2

  when (state === s_active) {
    io.release.valid := data_req_cnt < refillCycles.U
    io.release.bits  := Mux(req.voluntary, voluntaryRelease, probeResponse)

    when (io.mem_grant) {
      acked := true.B
    }

    when (io.release.fire()) {
      data_req_cnt := data_req_cnt + 1.U

      when (data_req_cnt === (refillCycles-1).U) {
        state := Mux(req.voluntary, s_grant, s_resp)
      }
    }
  }
  
  when (state === s_grant) {
    when (io.mem_grant) {
      acked := true.B
    }
    when (acked) {
      state := s_resp
    }
  }

  when (state === s_resp) {
    io.resp := true.B
    state := s_invalid
  }

  // print all input/output requests for debug purpose
  // print req
  val io_req = io.req.bits
  XSDebug(io.req.fire(), "req tag: %x idx: %x source: %d param: %x way_en: %x voluntary: %b\n",
    io_req.tag, io_req.idx, io_req.source, io_req.param, io_req.way_en, io_req.voluntary)

  // print data req
  val io_data_req = io.data_req.bits
  XSDebug(io.data_req.fire(), "data_req addr: %x way_en: %x\n", io_data_req.addr, io_data_req.way_en)

  // print release
  // XSDebug.exec(io.release.fire(), io.release.bits.dump)

  // print mem_grant
  XSDebug(io.mem_grant, "mem_grant\n")
}
