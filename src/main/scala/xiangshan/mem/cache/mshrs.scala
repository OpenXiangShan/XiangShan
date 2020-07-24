//******************************************************************************
// Ported from Rocket-Chip
// See LICENSE.Berkeley and LICENSE.SiFive in Rocket-Chip for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package xiangshan.mem.cache

import chisel3._
import chisel3.util._

import xiangshan.mem.DCacheReq
import bus.tilelink._

class DCacheReqInternal extends DCacheReq
  with HasDCacheParameters
{
  // miss info
  val tag_match = Bool()
  val old_meta  = new L1Metadata
  val way_en    = UInt(nWays.W)

  // Used in the MSHRs
  val sdq_id    = UInt(log2Up(cfg.nSDQ).W)
}


// 原来的MSHR太复杂了，涉及到的case太多，看都看不懂
// 暂时先
class MSHR extends DCacheModule
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    // 这个是input，为了简单起见，我们暂时只支持处理一个请求
    // 不支持同时处理多个请求
    val req_pri_val = Input(Bool())
    val req_pri_rdy = Output(Bool())
    val req         = Input(new DCacheReqInternal)

    // 这个是用来告诉外面，它现在选的哪一路，是用来匹配，阻塞用的。
    // 其实假如是同一个set，并且way不重合的话，其实也是可以跑的吧。
    // 这边的index，way，tag都是被Valid给包着的
    val idx = Output(Valid(UInt()))
    val way = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))

    val mem_acquire = Decoupled(new TLBundleA(cfg.busParams))
    val mem_grant   = Flipped(Decoupled(new TLBundleD(cfg.busParams)))
    val mem_finish  = Decoupled(new TLBundleE(cfg.busParams))

    val refill      = Decoupled(new L1DataWriteReq)

    val meta_write  = Decoupled(new L1MetaWriteReq)

    val wb_req      = Decoupled(new WritebackReq)
    // Writeback unit tells us when it is done processing our wb
    val wb_resp     = Input(Bool())

    // Replays go through the cache pipeline again
    val replay      = Decoupled(new DCacheReqInternal)
  })

  // TODO: Optimize this. We don't want to mess with cache during speculation
  // s_refill_req      : Make a request for a new cache line
  // s_refill_resp     : Store the refill response into our buffer
  // s_drain_rpq_loads : Drain out loads from the rpq
  //                   : If miss was misspeculated, go to s_invalid
  // s_wb_req          : Write back the evicted cache line
  // s_wb_resp         : Finish writing back the evicted cache line
  // s_meta_write_req  : Write the metadata for new cache lne
  // s_meta_write_resp :

  // 这边搞了这么多状态实在是太复杂了啊，估计它在做的时候，得考虑好多问题？
  // 比如这里似乎还要考虑prefetch的特殊处理？要支持branch错的时候，及时取消之前的请求啥的？
  val s_invalid :: s_refill_req :: s_refill_resp :: s_wb_req :: s_wb_resp :: s_drain_rpq :: s_meta_write_req :: s_mem_finish :: Nil = Enum(8)
  val state = RegInit(s_invalid)

  val req     = Reg(new DCacheReqInternal)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  val req_tag = req.addr >> untagBits
  // 这边算的是request的block的地址，就是把block内部的地址给丢掉了
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits

  // 这个应该是apply之后的新的coherence？
  // 除了init时是new coh，最后又怎么恢复呢？
  val new_coh = RegInit(ClientMetadata.onReset)
  // M_FLUSH是个啥啊？
  // 老的块上来，我们一定是要把它给清理掉的吧？
  // 这里还有更多更复杂的同步问题呢？假如在处理的时候，有人要写这个块呢？
  // 我们应该怎样阻止它呢？
  // 原先的这个块肯定要被flush掉的
  // 这个old meta，应该就是它miss后，替换算法选的那个way原来的coh
  val (_, shrink_param, coh_on_clear) = req.old_meta.coh.onCacheControl(M_FLUSH)
  // 看新的coh在遭遇到这个操作后，是否要升级权限
  // 这个pram其实给的是：当前权限是new coh，为了能执行mem_cmd，我需要进行什么样的权限升级
  val grow_param = new_coh.onAccess(req.cmd)._2
  // 这个是new coh在收到grant后，它的新状态到底是多少？
  // 问题：会不会有可能说收到grant请求后，权限不是我们期望的权限？
  // 例如给的是exclusive，那我们什么时候把它变成dirty呢？
  val coh_on_grant = new_coh.onGrant(req.cmd, io.mem_grant.bits.param)

  val (_, _, refill_done, refill_address_inc) = TLUtilities.addr_inc(io.mem_grant)

  // use ldq到底是啥？
  // branch killable意思是，branch kill可以一路杀到这个里面来，这可真是个难题啊。
  // 估计也是的，当branch kill的时候，这些请求都不需要返回回去了啊，当然应该被kill了啊。
  // use ldq到底是啥呢？
  val rpq = Module(new Queue(new DCacheReqInternal, cfg.nRPQ))

  // 为啥prefetch指令不能进来呢？why？
  rpq.io.enq.valid := io.req_pri_val && io.req_pri_rdy
  rpq.io.enq.bits  := io.req
  // 这个为啥是false呢？
  // 估计是默认给个false，拉低吧。
  rpq.io.deq.ready := false.B


  // 这个是对grant的ack
  val grantack = Reg(Valid(new TLBundleE(cfg.busParams)))
  val refill_ctr  = Reg(UInt(log2Up(cacheDataBeats).W))
  // grant分为只给权限，不给数据的grant，和又给权限，又给数据的grant

  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.way.valid := state =/= s_invalid
  io.idx.bits := req_idx
  io.tag.bits := req_tag
  io.way.bits := req.way_en

  // 给输出信号赋值
  // 所有信号默认全部拉低
  io.req_pri_rdy         := false.B

  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare

  io.mem_grant.ready     := false.B

  io.mem_finish.valid    := false.B
  io.mem_finish.bits     := DontCare

  io.refill.valid        := false.B
  io.refill.bits         := DontCare

  io.meta_write.valid    := false.B
  io.meta_write.bits     := DontCare

  io.wb_req.valid        := false.B
  io.wb_req.bits         := DontCare

  io.replay.valid        := false.B
  io.replay.bits         := DontCare

  def handle_pri_req(old_state: UInt): UInt = {
    // 收到primary请求后，把原来的一些状态变量给重新初始化
    val new_state = WireInit(old_state)
    // 这个是啥？估计只能算是初始化？
    grantack.valid := false.B
    refill_ctr := 0.U
    assert(rpq.io.enq.ready)
    // 记录req
    req := io.req
    val old_coh   = io.req.old_meta.coh
    // 我们要把替换算法挑中的这个块给flush掉，那是否需要 write back呢？
    val needs_wb = old_coh.onCacheControl(M_FLUSH)._1 // does the line we are evicting need to be written back
    // 这个又是啥呢？
    // tag match又是啥啊？
    // tag match应该是说，replacement替换的那一块就是这一块儿，这个是有可能的
    // 可能的因素有：
    // 块是invalid，然后我们是not hit，只不过正好被替换算法，挑了这同一块儿给我们。
    // 块是valid，我们是hit，只不过我们是权限miss
    // 例如权限升级之类的
    // 所以看来DCache的s2在处理的时候，还是得考虑这方面的内容的啊。
    when (io.req.tag_match) {
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(io.req.cmd)
      // 这个hit唯一有可能的就是从exclusive变成dirty，详见tilelink/metadata里面的内容
      when (is_hit) { // set dirty bit
        // 只有这个是不需要发acquire请求的，所以它就直接更新new coh了
        // 其他的都是把new coh给设置成一个合理的初始值，然后再申请权限
        // 根据grant的结果，来作为新的权限
        assert(isWrite(io.req.cmd))
        // new_coh似乎就是操作之后的coh？
        new_coh     := coh_on_hit
        // 所以我们这里连权限升级请求都不需要发，直接replay就可以了。
        // 问题：那write meta data是在什么时候写的呢？
        // 可以直接更新meta data了
        new_state   := s_drain_rpq
      } .otherwise { // upgrade permissions
        // 这边的new coh为啥存放的就是old coh呢？why？
        // 这边的tag match存在若干种可能性：
        // 1. 是shared状态，我们要升级权限，这个必须得要向下acquire，但是是不需要带数据的
        // 2. 是nothing状态，我们要获取数据块同时获得权限，向下acquire是带数据的
        // 但是not hit，那就先用老的meta data
        // 等grant回复了，我们才知道我们拿到了什么权限。
        // 我猜测说，当我们想要拿exclusive权限的时候，它只给我们一个shared权限？
        new_coh     := old_coh
        new_state   := s_refill_req
      }
    } .otherwise { // refill and writeback if necessary
      // 我们拿到的块不是要访问的
      // 我们得先把别人写回？
      // 这边的new_coh怎么看也不对啊？
      // tag不match，所以coh用的是默认的reset
      new_coh     := ClientMetadata.onReset
      when (needs_wb) {
        // 需要写回
        new_state   := s_wb_req
      } .otherwise {
        // 不需要写回，直接refill就可以了
        new_state   := s_refill_req
      }
    }
    new_state
  }

  // --------------------------------------------
  // 接受请求
  // s_invalid: 接受请求 + 初始化一些内部数据结构
  when (state === s_invalid) {
    // 依然可以接受primary miss请求
    io.req_pri_rdy := true.B

    // 收到primary miss请求
    // 进行处理
    when (io.req_pri_val && io.req_pri_rdy) {
      state := handle_pri_req(state)
    }
  } 
  
  // --------------------------------------------
  // write back
  when (state === s_wb_req) {
    io.wb_req.valid          := true.B

    io.wb_req.bits.tag       := req.old_meta.tag
    io.wb_req.bits.idx       := req_idx
    io.wb_req.bits.param     := shrink_param
    io.wb_req.bits.way_en    := req.way_en
    io.wb_req.bits.source    := io.id
    io.wb_req.bits.voluntary := true.B
    when (io.wb_req.fire()) {
      state := s_wb_resp
    }
  }
  
  // write back完成后就要进行refill了
  when (state === s_wb_resp) {
    when (io.wb_resp) {
      state := s_refill_req
    }
  }

  // --------------------------------------------
  // refill
  // 发出acquire请求进行refill
  when (state === s_refill_req) {
    // 既然立马就开始回填，那车上应该就只有一个人啊，所以最最简洁的实现应该就是在回填的时候，
    // 这个这就阻塞，不允许拼车不就好了嘛，感觉实现其实没必要搞得那么复杂啊。
    // 看来这个的确是一种写法啊，我感觉很好啊。对于wire，并不一定得把所有的连线都写在最下面啊，那种太繁琐了啊
    io.mem_acquire.valid := true.B
    // TODO: Use AcquirePerm if just doing permissions acquire
    io.mem_acquire.bits  := TLMasterUtilities.AcquireBlock(
      params = cfg.busParams,
      fromSource      = io.id,
      toAddress       = Cat(req_tag, req_idx) << blockOffBits,
      lgSize          = (log2Up(cfg.blockBytes)).U,
      growPermissions = grow_param)._2
    when (io.mem_acquire.fire()) {
      // 请求发出，等待response
      state := s_refill_resp
    }
  }
  
  when (state === s_refill_resp) {
    // 这边的line buffer应该就是用来存放acquire的结果的
    // 假如有bits的话，那就要等line buffer全部写入了，才能ready
    // 假如没有data的，那grant直接就可以ready了
    // 如果有data，那得等line buffer成功写入了，才算OK
    when (TLUtilities.hasData(io.mem_grant.bits)) {
      // 直接将grant的内容写入dcache
      io.mem_grant.ready      := io.refill.ready
      io.refill.valid         := io.mem_grant.valid
      io.refill.bits.addr     := req_block_addr | (refill_ctr << rowOffBits)
      io.refill.bits.way_en   := req.way_en
      io.refill.bits.wmask    := ~(0.U(rowWords.W))
      io.refill.bits.data     := io.mem_grant.bits.data
    } .otherwise {
      // 如果没哟data，那就是直接ready
      io.mem_grant.ready      := true.B
    }

    when (refill_done) {
      // 这个是啥意思？啥叫gratn is Request？
      // 我就不懂了，grant还能不是request吗？
      grantack.valid := TLUtilities.isRequest(io.mem_grant.bits)
      grantack.bits := TLMasterUtilities.GrantAck(io.mem_grant.bits)
      // 为什么grant has data就去drain rpq loads了？
      // 另外就是感觉这里并没有充分地实现MESI啊。
      // 根据MESI，假如读的数据将来要写的话，那读的时候就先申请exclusive。
      // 为什么是先drain load呢？
      // 如果grant有data，那说明这个块原来就不存在？
      state := s_mem_finish
      // 这个肯定是不对的，就是request要写回的话，那后续要执行，那肯定要load的，grant肯定是有data的。
      // 现在终于可以把new coh给更新上去了
      // 现在的问题是，什么时候channel E上给个response呢？
      new_coh := coh_on_grant
    }
  }
  
  when (state === s_mem_finish) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits

    when (io.mem_finish.fire()) {
      grantack.valid := false.B
      state := s_drain_rpq
    }
  }

  // --------------------------------------------
  // meta write
  when (state === s_meta_write_req) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := new_coh
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en

    when (io.meta_write.fire()) {
      state := s_invalid
    }
  }

  // --------------------------------------------
  // replay
  when (state === s_drain_rpq) {
    io.replay <> rpq.io.deq
    io.replay.bits.way_en    := req.way_en
    io.replay.bits.addr := Cat(req_tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0))
    when (io.replay.fire() && isWrite(rpq.io.deq.bits.cmd)) {
      // Set dirty bit
      val (is_hit, _, coh_on_hit) = new_coh.onAccess(rpq.io.deq.bits.cmd)
      assert(is_hit, "We still don't have permissions for this store")
      new_coh := coh_on_hit
    }
    when (rpq.io.count === 0.U) {
      state := s_meta_write_req
    }
  }
}

/*
class BoomIOMSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req  = Flipped(Decoupled(new BoomDCacheReq))
    val resp = Decoupled(new BoomDCacheResp)
    val mem_access = Decoupled(new TLBundleA(edge.bundle))
    val mem_ack    = Flipped(Valid(new TLBundleD(edge.bundle)))

    // We don't need brupdate in here because uncacheable operations are guaranteed non-speculative
  })

  def beatOffset(addr: UInt) = addr.extract(beatOffBits-1, wordOffBits)

  def wordFromBeat(addr: UInt, dat: UInt) = {
    val shift = Cat(beatOffset(addr), 0.U((wordOffBits+log2Ceil(wordBytes)).W))
    (dat >> shift)(wordBits-1, 0)
  }

  val req = Reg(new BoomDCacheReq)
  val grant_word = Reg(UInt(wordBits.W))

  val s_idle :: s_mem_access :: s_mem_ack :: s_resp :: Nil = Enum(4)

  val state = RegInit(s_idle)
  io.req.ready := state === s_idle

  val loadgen = new LoadGen(req.uop.mem_size, req.uop.mem_signed, req.addr, grant_word, false.B, wordBytes)

  val a_source  = id.U
  val a_address = req.addr
  val a_size    = req.uop.mem_size
  val a_data    = Fill(beatWords, req.data)

  val get      = edge.Get(a_source, a_address, a_size)._2
  val put      = edge.Put(a_source, a_address, a_size, a_data)._2
  val atomics  = if (edge.manager.anySupportLogical) {
    MuxLookup(req.cmd, (0.U).asTypeOf(new TLBundleA(edge.bundle)), Array(
      M_XA_SWAP -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.SWAP)._2,
      M_XA_XOR  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.XOR) ._2,
      M_XA_OR   -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.OR)  ._2,
      M_XA_AND  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.AND) ._2,
      M_XA_ADD  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.ADD)._2,
      M_XA_MIN  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MIN)._2,
      M_XA_MAX  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAX)._2,
      M_XA_MINU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MINU)._2,
      M_XA_MAXU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAXU)._2))
  } else {
    // If no managers support atomics, assert fail if processor asks for them
    assert(state === s_idle || !isAMO(req.cmd))
    Wire(new TLBundleA(edge.bundle))
  }
  assert(state === s_idle || req.cmd =/= M_XSC)

  io.mem_access.valid := state === s_mem_access
  io.mem_access.bits  := Mux(isAMO(req.cmd), atomics, Mux(isRead(req.cmd), get, put))

  val send_resp = isRead(req.cmd)

  io.resp.valid     := (state === s_resp) && send_resp
  io.resp.bits.uop  := req.uop
  io.resp.bits.data := loadgen.data

  when (io.req.fire()) {
    req   := io.req.bits
    state := s_mem_access
  }
  when (io.mem_access.fire()) {
    state := s_mem_ack
  }
  when (state === s_mem_ack && io.mem_ack.valid) {
    state := s_resp
    when (isRead(req.cmd)) {
      grant_word := wordFromBeat(req.addr, io.mem_ack.bits.data)
    }
  }
  when (state === s_resp) {
    when (!send_resp || io.resp.fire()) {
      state := s_idle
    }
  }
}

// 看来line buffer有很多项啊
// 似乎是先回填进lb的？
// 这个line buffer是不是就相当于是victim cache？
class LineBufferReadReq(implicit p: Parameters) extends BoomBundle()(p)
  with HasL1HellaCacheParameters
{
  val id      = UInt(log2Ceil(nLBEntries).W)
  val offset  = UInt(log2Ceil(cacheDataBeats).W)
  def lb_addr = Cat(id, offset)
}

// 而且读写的单位似乎是word级别？
// 而且在line buffer里面也是被enc的？
class LineBufferWriteReq(implicit p: Parameters) extends LineBufferReadReq()(p)
{
  val data   = UInt(encRowBits.W)
}

// line buffer还有meta data？
// 这个或许就是victim cache？还是stream cache？
class LineBufferMetaWriteReq(implicit p: Parameters) extends BoomBundle()(p)
{
  val id   = UInt(log2Ceil(nLBEntries).W)
  val coh  = new ClientMetadata
  val addr = UInt(coreMaxAddrBits.W)
}

class LineBufferMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasL1HellaCacheParameters
{
  val coh  = new ClientMetadata
  val addr = UInt(coreMaxAddrBits.W)
}
*/

class MSHRFile extends DCacheModule
{
  val io = IO(new Bundle {
    // 卧槽，这边复杂就复杂在，同时可以有两条指令失效啊，怪不得搞得复杂啊！
    val req  = Flipped(Vec(memWidth, Decoupled(new DCacheReqInternal))) // Req from s2 of DCache pipe
    val block_hit = Output(Vec(memWidth, Bool()))

    val mem_acquire  = Decoupled(new TLBundleA(cfg.busParams))
    val mem_grant    = Flipped(Decoupled(new TLBundleD(cfg.busParams)))
    val mem_finish   = Decoupled(new TLBundleE(cfg.busParams))

    val refill     = Decoupled(new L1DataWriteReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay     = Decoupled(new DCacheReqInternal)
    val wb_req     = Decoupled(new WritebackReq)
    val wb_resp   = Input(Bool())
  })

  // OHToUInt是要求只能有一个bit是high的
  // 所以这里并不能同时处理两条失效请求，只能处理一条失效请求
  val req_idx = OHToUInt(io.req.map(_.valid))
  val req     = io.req(req_idx)

  for (w <- 0 until memWidth)
    io.req(w).ready := false.B


  // 这是啥？？？？
  // val cacheable = edge.manager.supportsAcquireBFast(req.bits.addr, lgCacheBlockBytes.U)
  val cacheable = true.B

  // --------------------
  // The MSHR SDQ
  // 所以这边为了节省空间。每个MSHR的queue里面并没有保存store data，这是保存的基本的meta
  // 然后这个sdq是共用的！
  val sdq_val      = RegInit(0.U(cfg.nSDQ.W))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(cfg.nSDQ-1,0))
  val sdq_rdy      = !sdq_val.andR
  // 只有write 才进sdq
  val sdq_enq      = req.fire() && cacheable && isWrite(req.bits.cmd)
  val sdq          = Mem(cfg.nSDQ, UInt(wordBits.W))

  when (sdq_enq) {
    sdq(sdq_alloc_id) := req.bits.data
  }

  // --------------------
  // The LineBuffer Data
  // Holds refilling lines, prefetched lines
  // 所以line buffer里面装的是回填还有prefetch的数据？
  // 所以它这个prefetch并没有prefetch进L1 cache？
  // 所以line buffer本质上就是一个ram，它的长度是entry * beat
  // 然后每一个数据是一个word

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

  // 把进来的index，tag，way和每一个mshr的内容进行比较
  val idx_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val tag_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val way_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))

  // tag match和way match的前提条件是要index match
  val tag_match   = widthMap(w => Mux1H(idx_matches(w), tag_matches(w)))
  val idx_match   = widthMap(w => idx_matches(w).reduce(_||_))
  val way_match   = widthMap(w => Mux1H(idx_matches(w), way_matches(w)))

  // 这是啥？
  val wb_tag_list = Wire(Vec(cfg.nMSHRs, UInt(tagBits.W)))

  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq           , cfg.nMSHRs))
  val wb_req_arb     = Module(new Arbiter(new WritebackReq, cfg.nMSHRs))
  val replay_arb     = Module(new Arbiter(new DCacheReqInternal    , cfg.nMSHRs))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq           , cfg.nMSHRs))

  io.mem_grant.ready := false.B

  // 开始分配MSHR

  val mshr_alloc_idx = Wire(UInt())
  val pri_rdy = WireInit(false.B)
  // primary ready需要的是idx not match，所以可以理解成是按照set来阻塞的。
  // 当一个set在被处理的时候，别人不能再处理它了。
  val pri_val = req.valid && sdq_rdy && cacheable && !idx_match(req_idx)
  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new MSHR)
    mshr.io.id := i.U(log2Up(cfg.nMSHRs).W)

    for (w <- 0 until memWidth) {
      idx_matches(w)(i) := mshr.io.idx.valid && mshr.io.idx.bits === io.req(w).bits.addr(untagBits-1,blockOffBits)
      tag_matches(w)(i) := mshr.io.tag.valid && mshr.io.tag.bits === io.req(w).bits.addr >> untagBits
      way_matches(w)(i) := mshr.io.way.valid && mshr.io.way.bits === io.req(w).bits.way_en
    }
    wb_tag_list(i) := mshr.io.wb_req.bits.tag



    // 假如指定就是它，那就看好不好分配？
    mshr.io.req_pri_val  := (i.U === mshr_alloc_idx) && pri_val
    when (i.U === mshr_alloc_idx) {
      pri_rdy := mshr.io.req_pri_rdy
    }

    mshr.io.req          := req.bits
    mshr.io.req.sdq_id   := sdq_alloc_id

    // 这么看来这个wb resp确实不需要加id，因为wbu是阻塞的，所以同一时刻只能有一个人在用它。
    // 也只会有一个人在等待它的结果
    mshr.io.wb_resp      := io.wb_resp

    meta_write_arb.io.in(i) <> mshr.io.meta_write
    wb_req_arb.io.in(i)     <> mshr.io.wb_req
    replay_arb.io.in(i)     <> mshr.io.replay
    refill_arb.io.in(i)     <> mshr.io.refill

    mshr.io.mem_grant.valid := false.B
    mshr.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      mshr.io.mem_grant <> io.mem_grant
    }

    // 只要有任何一个mshr是busy，那就not ready
    mshr
  }

  // Try to round-robin the MSHRs
  // 这个是啥？why？为啥要对MSHR进行round robin呢？
  // 算了，不管了，暂时就先这么搞吧？
  // 先用普通的PriorityEncoder吧？
  mshr_alloc_idx    := RegNext(PriorityEncoder(mshrs.map(m=>m.io.req_pri_rdy)))

  io.meta_write <> meta_write_arb.io.out
  io.wb_req     <> wb_req_arb.io.out

  TLArbiter.lowestFromSeq(io.mem_acquire, mshrs.map(_.io.mem_acquire))
  TLArbiter.lowestFromSeq(io.mem_finish,  mshrs.map(_.io.mem_finish))

  // 为什么response还有个queue呢？这是为啥呢？
  val mmio_rdy = true.B

  for (w <- 0 until memWidth) {
    io.req(w).ready      := (w.U === req_idx) &&
      Mux(!cacheable, mmio_rdy, sdq_rdy && pri_rdy)
    // idx_match应该就是进了同一个set
    // 同时tag也match？这是啥？
    io.block_hit(w)      := idx_match(w) && tag_match(w)
  }
  io.refill         <> refill_arb.io.out

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.cmd)

  io.replay <> replay_arb.io.out
  io.replay.bits.data := sdq(replay_arb.io.out.bits.sdq_id)

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(cfg.nSDQ, free_sdq)) |
      PriorityEncoderOH(~sdq_val(cfg.nSDQ-1,0)) & Fill(cfg.nSDQ, sdq_enq)
  }
}
