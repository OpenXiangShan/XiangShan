package device

import chisel3.{BlackBox, IO, _}
import chisel3.util.{HasBlackBoxResource, _}
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter
import coupledL2.tl2chi

case class CDBParams(
                      // 对应 SV 模块的所有参数，默认值与 SV 中一致
                      CDB_REQFIFO_DEPTH_ICN: Int = 8,
                      CDB_RSPFIFO_DEPTH_ICN: Int = 8,
                      CDB_DATFIFO_DEPTH_ICN: Int = 8,
                      CDB_SNPFIFO_DEPTH_ICN: Int = 8,
                      CDB_REQFIFO_DEPTH_DEV: Int = 8,
                      CDB_RSPFIFO_DEPTH_DEV: Int = 8,
                      CDB_DATFIFO_DEPTH_DEV: Int = 8,
                      CDB_SNPFIFO_DEPTH_DEV: Int = 8,
                      CDB_REQBP_FIFO_DEPTH_ICN: Int = 4,
                      CDB_RSPBP_FIFO_DEPTH_ICN: Int = 4,
                      CDB_DATBP_FIFO_DEPTH_ICN: Int = 4,
                      CDB_SNPBP_FIFO_DEPTH_ICN: Int = 4,
                      CDB_REQBP_FIFO_DEPTH_DEV: Int = 4,
                      CDB_RSPBP_FIFO_DEPTH_DEV: Int = 4,
                      CDB_DATBP_FIFO_DEPTH_DEV: Int = 4,
                      CDB_SNPBP_FIFO_DEPTH_DEV: Int = 4,
                      CDB_ICN2DEV_REQ_BYPASS_EN: Int = 1,
                      CDB_ICN2DEV_RSP_BYPASS_EN: Int = 1,
                      CDB_ICN2DEV_SNP_BYPASS_EN: Int = 1,
                      CDB_ICN2DEV_DAT_BYPASS_EN: Int = 1,
                      CDB_DEV2ICN_REQ_BYPASS_EN: Int = 1,
                      CDB_DEV2ICN_RSP_BYPASS_EN: Int = 1,
                      CDB_DEV2ICN_SNP_BYPASS_EN: Int = 1,
                      CDB_DEV2ICN_DAT_BYPASS_EN: Int = 1,
                      CDB_ICN2DEV_REQ_EN: Int = 1,
                      CDB_ICN2DEV_RSP_EN: Int = 1,
                      CDB_ICN2DEV_SNP_EN: Int = 1,
                      CDB_ICN2DEV_DAT_EN: Int = 1,
                      CDB_DEV2ICN_REQ_EN: Int = 1,
                      CDB_DEV2ICN_RSP_EN: Int = 1,
                      CDB_DEV2ICN_SNP_EN: Int = 1,
                      CDB_DEV2ICN_DAT_EN: Int = 1,
                      CDB_REQFLIT_WIDTH: Int = 162, // 示例值，需替换为你的 MESH_REQ_FLIT_WIDTH 实际值
                      CDB_RSPFLIT_WIDTH: Int = 73, // 示例值，需替换为你的 MESH_RSP_FLIT_WIDTH 实际值
                      CDB_SNPFLIT_WIDTH: Int = 115, // 示例值，需替换为你的 MESH_SNP_FLIT_WIDTH 实际值
                      CDB_DATFLIT_WIDTH: Int = 422 // 示例值，需替换为你的 MESH_DAT_FLIT_WIDTH 实际值
                    )
//async interfaces between two half bridges.
class CHIAsyncIODSU(params: CDBParams) extends Bundle {
  // FIFO 接口：REQ 通道
  val cdb_fifo_data_icn2dev_req = Output(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_ICN).W))
  val rptr_r_dev2icn_req        = Input(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))
  val wptr_r_icn2dev_req        = Output(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))
  val cdb_fifo_data_dev2icn_req = Input(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_ICN).W))
  val rptr_r_icn2dev_req        = Output(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))
  val wptr_r_dev2icn_req        = Input(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))

  // FIFO 接口：RSP 通道
  val cdb_fifo_data_icn2dev_rsp = Output(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_ICN).W))
  val rptr_r_dev2icn_rsp        = Input(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
  val wptr_r_icn2dev_rsp        = Output(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
  val cdb_fifo_data_dev2icn_rsp = Input(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_ICN).W))
  val rptr_r_icn2dev_rsp        = Output(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
  val wptr_r_dev2icn_rsp        = Input(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))

  // FIFO 接口：SNP 通道
  val cdb_fifo_data_icn2dev_snp = Output(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_ICN).W))
  val rptr_r_dev2icn_snp        = Input(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
  val wptr_r_icn2dev_snp        = Output(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
  val cdb_fifo_data_dev2icn_snp = Input(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_ICN).W))
  val rptr_r_icn2dev_snp        = Output(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
  val wptr_r_dev2icn_snp        = Input(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))

  // FIFO 接口：DAT 通道
  val cdb_fifo_data_icn2dev_dat = Output(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_ICN).W))
  val rptr_r_dev2icn_dat        = Input(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
  val wptr_r_icn2dev_dat        = Output(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
  val cdb_fifo_data_dev2icn_dat = Input(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_ICN).W))
  val rptr_r_icn2dev_dat        = Output(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
  val wptr_r_dev2icn_dat        = Input(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
}

class CHIAsyncICNDSU(params: CDBParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cdb = new CHIAsyncIODSU(params)
    val chi = new PortIO
  })
  val cdbicn = Module(new cdb_icn(params))

  // connect io

  cdbicn.io.clk_icn := clock
  cdbicn.io.rstn_icn := (!reset.asBool).asAsyncReset

  io.chi.syscoreq := false.B //cdbdev.io.SYSCOAC
  // ICN 侧链路信号
  cdbicn.io.rxsactive_icn := io.chi.rxsactive
  io.chi.txsactive := cdbicn.io.txsactive_icn
  cdbicn.io.rxlinkactivereq_icn := io.chi.rx.linkactivereq
  io.chi.rx.linkactiveack := cdbicn.io.rxlinkactiveack_icn
  io.chi.tx.linkactivereq := cdbicn.io.txlinkactivereq_icn
  cdbicn.io.txlinkactiveack_icn := io.chi.tx.linkactiveack

  // ICN 侧 REQ 通道
  cdbicn.io.rxreq_flitpend_icn := 0.U //io.chi.rx.req.flitpend
  cdbicn.io.rxreq_flitv_icn := 0.U //io.chi.rx.req.flitv
  cdbicn.io.rxreq_flit_icn := 0.U //io.chi.rx.req.flit
  //       io.chi.rx.req.lcrdv             := cdbicn.io.rxreq_crdv_icn
  io.chi.tx.req.flitpend := cdbicn.io.txreq_flitpend_icn
  io.chi.tx.req.flitv := cdbicn.io.txreq_flitv_icn
  io.chi.tx.req.flit := cdbicn.io.txreq_flit_icn
  cdbicn.io.txreq_crdv_icn := io.chi.tx.req.lcrdv

  // ICN 侧 RSP 通道
  cdbicn.io.rxrsp_flitpend_icn := io.chi.rx.rsp.flitpend
  cdbicn.io.rxrsp_flitv_icn := io.chi.rx.rsp.flitv
  cdbicn.io.rxrsp_flit_icn := io.chi.rx.rsp.flit
  io.chi.rx.rsp.lcrdv := cdbicn.io.rxrsp_crdv_icn
  io.chi.tx.rsp.flitpend := cdbicn.io.txrsp_flitpend_icn
  io.chi.tx.rsp.flitv := cdbicn.io.txrsp_flitv_icn
  io.chi.tx.rsp.flit := cdbicn.io.txrsp_flit_icn
  cdbicn.io.txrsp_crdv_icn := io.chi.tx.rsp.lcrdv

  // ICN 侧 SNP 通道
  cdbicn.io.rxsnp_flitpend_icn := io.chi.rx.snp.flitpend
  cdbicn.io.rxsnp_flitv_icn := io.chi.rx.snp.flitv
  cdbicn.io.rxsnp_flit_icn := io.chi.rx.snp.flit
  io.chi.rx.snp.lcrdv := cdbicn.io.rxsnp_crdv_icn
  //       io.chi.tx.snp.flitpend         := cdbicn.io.txsnp_flitpend_icn
  //       io.chi.tx.snp.flitv            := cdbicn.io.txsnp_flitv_icn
  //       io.chi.tx.snp.flit             := cdbicn.io.txsnp_flit_icn
  cdbicn.io.txsnp_crdv_icn := 0.U // io.chi.tx.snp.crdv_icn

  // ICN 侧 DAT 通道
  cdbicn.io.rxdat_flitpend_icn := io.chi.rx.dat.flitpend
  cdbicn.io.rxdat_flitv_icn := io.chi.rx.dat.flitv
  cdbicn.io.rxdat_flit_icn := io.chi.rx.dat.flit
  io.chi.rx.dat.lcrdv := cdbicn.io.rxdat_crdv_icn
  io.chi.tx.dat.flitpend := cdbicn.io.txdat_flitpend_icn
  io.chi.tx.dat.flitv := cdbicn.io.txdat_flitv_icn
  io.chi.tx.dat.flit := cdbicn.io.txdat_flit_icn
  cdbicn.io.txdat_crdv_icn := io.chi.tx.dat.lcrdv

  // FIFO 接口：REQ 通道
  io.cdb.cdb_fifo_data_icn2dev_req := cdbicn.io.cdb_fifo_data_icn2dev_req
  cdbicn.io.rptr_r_dev2icn_req := io.cdb.rptr_r_dev2icn_req
  io.cdb.wptr_r_icn2dev_req := cdbicn.io.wptr_r_icn2dev_req
  cdbicn.io.cdb_fifo_data_dev2icn_req := io.cdb.cdb_fifo_data_dev2icn_req
  io.cdb.rptr_r_icn2dev_req := cdbicn.io.rptr_r_icn2dev_req
  cdbicn.io.wptr_r_dev2icn_req := io.cdb.wptr_r_dev2icn_req

  // FIFO 接口：RSP 通道
  io.cdb.cdb_fifo_data_icn2dev_rsp := cdbicn.io.cdb_fifo_data_icn2dev_rsp
  cdbicn.io.rptr_r_dev2icn_rsp := io.cdb.rptr_r_dev2icn_rsp
  io.cdb.wptr_r_icn2dev_rsp := cdbicn.io.wptr_r_icn2dev_rsp
  cdbicn.io.cdb_fifo_data_dev2icn_rsp := io.cdb.cdb_fifo_data_dev2icn_rsp
  io.cdb.rptr_r_icn2dev_rsp := cdbicn.io.rptr_r_icn2dev_rsp
  cdbicn.io.wptr_r_dev2icn_rsp := io.cdb.wptr_r_dev2icn_rsp

  // FIFO 接口：SNP 通道
  io.cdb.cdb_fifo_data_icn2dev_snp := cdbicn.io.cdb_fifo_data_icn2dev_snp
  cdbicn.io.rptr_r_dev2icn_snp := io.cdb.rptr_r_dev2icn_snp
  io.cdb.wptr_r_icn2dev_snp := cdbicn.io.wptr_r_icn2dev_snp
  cdbicn.io.cdb_fifo_data_dev2icn_snp := io.cdb.cdb_fifo_data_dev2icn_snp
  io.cdb.rptr_r_icn2dev_snp := cdbicn.io.rptr_r_icn2dev_snp
  cdbicn.io.wptr_r_dev2icn_snp := io.cdb.wptr_r_dev2icn_snp

  // FIFO 接口：DAT 通道
  io.cdb.cdb_fifo_data_icn2dev_dat := cdbicn.io.cdb_fifo_data_icn2dev_dat
  cdbicn.io.rptr_r_dev2icn_dat := io.cdb.rptr_r_dev2icn_dat
  io.cdb.wptr_r_icn2dev_dat := cdbicn.io.wptr_r_icn2dev_dat
  cdbicn.io.cdb_fifo_data_dev2icn_dat := io.cdb.cdb_fifo_data_dev2icn_dat
  io.cdb.rptr_r_icn2dev_dat := cdbicn.io.rptr_r_icn2dev_dat
  cdbicn.io.wptr_r_dev2icn_dat := io.cdb.wptr_r_dev2icn_dat
}

// 声明 CDB_ICN 的 BlackBox，严格匹配 SV 模块的参数和端口
class cdb_icn(params: CDBParams) extends BlackBox(Map(
  // 对应 SV 模块的所有参数，默认值与 SV 中一致
  "CDB_REQFIFO_DEPTH_ICN    " -> params.CDB_REQFIFO_DEPTH_ICN,
  "CDB_RSPFIFO_DEPTH_ICN    " -> params.CDB_RSPFIFO_DEPTH_ICN,
  "CDB_DATFIFO_DEPTH_ICN    " -> params.CDB_DATFIFO_DEPTH_ICN,
  "CDB_SNPFIFO_DEPTH_ICN    " -> params.CDB_SNPFIFO_DEPTH_ICN,
  "CDB_REQFIFO_DEPTH_DEV    " -> params.CDB_REQFIFO_DEPTH_DEV,
  "CDB_RSPFIFO_DEPTH_DEV    " -> params.CDB_RSPFIFO_DEPTH_DEV,
  "CDB_DATFIFO_DEPTH_DEV    " -> params.CDB_DATFIFO_DEPTH_DEV,
  "CDB_SNPFIFO_DEPTH_DEV    " -> params.CDB_SNPFIFO_DEPTH_DEV,
  "CDB_REQBP_FIFO_DEPTH_ICN " -> params.CDB_REQBP_FIFO_DEPTH_ICN,
  "CDB_RSPBP_FIFO_DEPTH_ICN " -> params.CDB_RSPBP_FIFO_DEPTH_ICN,
  "CDB_DATBP_FIFO_DEPTH_ICN " -> params.CDB_DATBP_FIFO_DEPTH_ICN,
  "CDB_SNPBP_FIFO_DEPTH_ICN " -> params.CDB_SNPBP_FIFO_DEPTH_ICN,
  "CDB_REQBP_FIFO_DEPTH_DEV " -> params.CDB_REQBP_FIFO_DEPTH_DEV,
  "CDB_RSPBP_FIFO_DEPTH_DEV " -> params.CDB_RSPBP_FIFO_DEPTH_DEV,
  "CDB_DATBP_FIFO_DEPTH_DEV " -> params.CDB_DATBP_FIFO_DEPTH_DEV,
  "CDB_SNPBP_FIFO_DEPTH_DEV " -> params.CDB_SNPBP_FIFO_DEPTH_DEV,
  "CDB_ICN2DEV_REQ_BYPASS_EN" -> params.CDB_ICN2DEV_REQ_BYPASS_EN,
  "CDB_ICN2DEV_RSP_BYPASS_EN" -> params.CDB_ICN2DEV_RSP_BYPASS_EN,
  "CDB_ICN2DEV_SNP_BYPASS_EN" -> params.CDB_ICN2DEV_SNP_BYPASS_EN,
  "CDB_ICN2DEV_DAT_BYPASS_EN" -> params.CDB_ICN2DEV_DAT_BYPASS_EN,
  "CDB_DEV2ICN_REQ_BYPASS_EN" -> params.CDB_DEV2ICN_REQ_BYPASS_EN,
  "CDB_DEV2ICN_RSP_BYPASS_EN" -> params.CDB_DEV2ICN_RSP_BYPASS_EN,
  "CDB_DEV2ICN_SNP_BYPASS_EN" -> params.CDB_DEV2ICN_SNP_BYPASS_EN,
  "CDB_DEV2ICN_DAT_BYPASS_EN" -> params.CDB_DEV2ICN_DAT_BYPASS_EN,
  "CDB_ICN2DEV_REQ_EN       " -> params.CDB_ICN2DEV_REQ_EN,
  "CDB_ICN2DEV_RSP_EN       " -> params.CDB_ICN2DEV_RSP_EN,
  "CDB_ICN2DEV_SNP_EN       " -> params.CDB_ICN2DEV_SNP_EN,
  "CDB_ICN2DEV_DAT_EN       " -> params.CDB_ICN2DEV_DAT_EN,
  "CDB_DEV2ICN_REQ_EN       " -> params.CDB_DEV2ICN_REQ_EN,
  "CDB_DEV2ICN_RSP_EN       " -> params.CDB_DEV2ICN_RSP_EN,
  "CDB_DEV2ICN_SNP_EN       " -> params.CDB_DEV2ICN_SNP_EN,
  "CDB_DEV2ICN_DAT_EN       " -> params.CDB_DEV2ICN_DAT_EN,
  "CDB_REQFLIT_WIDTH        " -> params.CDB_REQFLIT_WIDTH,
  "CDB_RSPFLIT_WIDTH        " -> params.CDB_RSPFLIT_WIDTH,
  "CDB_SNPFLIT_WIDTH        " -> params.CDB_SNPFLIT_WIDTH,
  "CDB_DATFLIT_WIDTH        " -> params.CDB_DATFLIT_WIDTH
)) with HasBlackBoxResource {
  // 定义端口，严格匹配 SV 模块的输入输出
  val io = IO(new Bundle {
    // 时钟与复位
    val clk_icn = Input(Clock())
    val rstn_icn = Input(Reset())

    // ICN 侧链路信号
    val rxsactive_icn = Input(Bool())
    val txsactive_icn = Output(Bool())
    val rxlinkactivereq_icn = Input(Bool())
    val rxlinkactiveack_icn = Output(Bool())
    val txlinkactivereq_icn = Output(Bool())
    val txlinkactiveack_icn = Input(Bool())

    // ICN 侧 REQ 通道
    val rxreq_flitpend_icn = Input(Bool())
    val rxreq_flitv_icn = Input(Bool())
    val rxreq_flit_icn = Input(UInt(params.CDB_REQFLIT_WIDTH.W))
    val rxreq_crdv_icn = Output(Bool())
    val txreq_flitpend_icn = Output(Bool())
    val txreq_flitv_icn = Output(Bool())
    val txreq_flit_icn = Output(UInt(params.CDB_REQFLIT_WIDTH.W))
    val txreq_crdv_icn = Input(Bool())

    // ICN 侧 RSP 通道
    val rxrsp_flitpend_icn = Input(Bool())
    val rxrsp_flitv_icn = Input(Bool())
    val rxrsp_flit_icn = Input(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val rxrsp_crdv_icn = Output(Bool())
    val txrsp_flitpend_icn = Output(Bool())
    val txrsp_flitv_icn = Output(Bool())
    val txrsp_flit_icn = Output(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val txrsp_crdv_icn = Input(Bool())

    // ICN 侧 SNP 通道
    val rxsnp_flitpend_icn = Input(Bool())
    val rxsnp_flitv_icn = Input(Bool())
    val rxsnp_flit_icn = Input(UInt(params.CDB_SNPFLIT_WIDTH.W))
    val rxsnp_crdv_icn = Output(Bool())
    val txsnp_flitpend_icn = Output(Bool())
    val txsnp_flitv_icn = Output(Bool())
    val txsnp_flit_icn = Output(UInt(params.CDB_SNPFLIT_WIDTH.W))
    val txsnp_crdv_icn = Input(Bool())

    // ICN 侧 DAT 通道
    val rxdat_flitpend_icn = Input(Bool())
    val rxdat_flitv_icn = Input(Bool())
    val rxdat_flit_icn = Input(UInt(params.CDB_DATFLIT_WIDTH.W))
    val rxdat_crdv_icn = Output(Bool())
    val txdat_flitpend_icn = Output(Bool())
    val txdat_flitv_icn = Output(Bool())
    val txdat_flit_icn = Output(UInt(params.CDB_DATFLIT_WIDTH.W))
    val txdat_crdv_icn = Input(Bool())

    // FIFO 接口：REQ 通道
    val cdb_fifo_data_icn2dev_req = Output(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_ICN).W))
    val rptr_r_dev2icn_req = Input(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))
    val wptr_r_icn2dev_req = Output(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))
    val cdb_fifo_data_dev2icn_req = Input(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_ICN).W))
    val rptr_r_icn2dev_req = Output(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))
    val wptr_r_dev2icn_req = Input(UInt(params.CDB_REQFIFO_DEPTH_ICN.W))

    // FIFO 接口：RSP 通道
    val cdb_fifo_data_icn2dev_rsp = Output(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_ICN).W))
    val rptr_r_dev2icn_rsp = Input(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
    val wptr_r_icn2dev_rsp = Output(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
    val cdb_fifo_data_dev2icn_rsp = Input(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_ICN).W))
    val rptr_r_icn2dev_rsp = Output(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
    val wptr_r_dev2icn_rsp = Input(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))

    // FIFO 接口：SNP 通道
    val cdb_fifo_data_icn2dev_snp = Output(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_ICN).W))
    val rptr_r_dev2icn_snp = Input(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
    val wptr_r_icn2dev_snp = Output(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
    val cdb_fifo_data_dev2icn_snp = Input(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_ICN).W))
    val rptr_r_icn2dev_snp = Output(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
    val wptr_r_dev2icn_snp = Input(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))

    // FIFO 接口：DAT 通道
    val cdb_fifo_data_icn2dev_dat = Output(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_ICN).W))
    val rptr_r_dev2icn_dat = Input(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
    val wptr_r_icn2dev_dat = Output(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
    val cdb_fifo_data_dev2icn_dat = Input(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_ICN).W))
    val rptr_r_icn2dev_dat = Output(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
    val wptr_r_dev2icn_dat = Input(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
  })
  //指定 SV 文件路径（如果放在 src/main/resources 下可省略，否则需完整路径）
  addResource("/dsu_cdb/defines/dsu_chie_defines.sv")
  addResource("/dsu_cdb/defines/common_defines.sv")
  addResource("/dsu_cdb/defines/router_define.sv")
  addResource("/dsu_cdb/rlink_lainit.v")
  addResource("/dsu_cdb/fifo.sv")
  addResource("/dsu_cdb/credit_manager.sv")
  addResource("/dsu_cdb/sync_dff.sv")
  addResource("/dsu_cdb/rstn_dff.sv")
  addResource("/dsu_cdb/clock_gate.sv")
  addResource("/dsu_cdb/reset_sync.sv")
  addResource("/dsu_cdb/cdb_ingress_channel.sv")
  addResource("/dsu_cdb/cdb_egress_channel.sv")
  addResource("/dsu_cdb/cdb_icn.sv")
}

// 声明 CDB_DEV 的 BlackBox，严格匹配 SV 模块的参数和端口
class cdb_dev(params: CDBParams) extends BlackBox(Map(
  // 对应 SV 模块的所有参数，默认值与 SV 中一致
  "CDB_REQFIFO_DEPTH_ICN    " -> params.CDB_REQFIFO_DEPTH_ICN,
  "CDB_RSPFIFO_DEPTH_ICN    " -> params.CDB_RSPFIFO_DEPTH_ICN,
  "CDB_DATFIFO_DEPTH_ICN    " -> params.CDB_DATFIFO_DEPTH_ICN,
  "CDB_SNPFIFO_DEPTH_ICN    " -> params.CDB_SNPFIFO_DEPTH_ICN,
  "CDB_REQFIFO_DEPTH_DEV    " -> params.CDB_REQFIFO_DEPTH_DEV,
  "CDB_RSPFIFO_DEPTH_DEV    " -> params.CDB_RSPFIFO_DEPTH_DEV,
  "CDB_DATFIFO_DEPTH_DEV    " -> params.CDB_DATFIFO_DEPTH_DEV,
  "CDB_SNPFIFO_DEPTH_DEV    " -> params.CDB_SNPFIFO_DEPTH_DEV,
  "CDB_REQBP_FIFO_DEPTH_ICN " -> params.CDB_REQBP_FIFO_DEPTH_ICN,
  "CDB_RSPBP_FIFO_DEPTH_ICN " -> params.CDB_RSPBP_FIFO_DEPTH_ICN,
  "CDB_DATBP_FIFO_DEPTH_ICN " -> params.CDB_DATBP_FIFO_DEPTH_ICN,
  "CDB_SNPBP_FIFO_DEPTH_ICN " -> params.CDB_SNPBP_FIFO_DEPTH_ICN,
  "CDB_REQBP_FIFO_DEPTH_DEV " -> params.CDB_REQBP_FIFO_DEPTH_DEV,
  "CDB_RSPBP_FIFO_DEPTH_DEV " -> params.CDB_RSPBP_FIFO_DEPTH_DEV,
  "CDB_DATBP_FIFO_DEPTH_DEV " -> params.CDB_DATBP_FIFO_DEPTH_DEV,
  "CDB_SNPBP_FIFO_DEPTH_DEV " -> params.CDB_SNPBP_FIFO_DEPTH_DEV,
  "CDB_ICN2DEV_REQ_BYPASS_EN" -> params.CDB_ICN2DEV_REQ_BYPASS_EN,
  "CDB_ICN2DEV_RSP_BYPASS_EN" -> params.CDB_ICN2DEV_RSP_BYPASS_EN,
  "CDB_ICN2DEV_SNP_BYPASS_EN" -> params.CDB_ICN2DEV_SNP_BYPASS_EN,
  "CDB_ICN2DEV_DAT_BYPASS_EN" -> params.CDB_ICN2DEV_DAT_BYPASS_EN,
  "CDB_DEV2ICN_REQ_BYPASS_EN" -> params.CDB_DEV2ICN_REQ_BYPASS_EN,
  "CDB_DEV2ICN_RSP_BYPASS_EN" -> params.CDB_DEV2ICN_RSP_BYPASS_EN,
  "CDB_DEV2ICN_SNP_BYPASS_EN" -> params.CDB_DEV2ICN_SNP_BYPASS_EN,
  "CDB_DEV2ICN_DAT_BYPASS_EN" -> params.CDB_DEV2ICN_DAT_BYPASS_EN,
  "CDB_ICN2DEV_REQ_EN       " -> params.CDB_ICN2DEV_REQ_EN,
  "CDB_ICN2DEV_RSP_EN       " -> params.CDB_ICN2DEV_RSP_EN,
  "CDB_ICN2DEV_SNP_EN       " -> params.CDB_ICN2DEV_SNP_EN,
  "CDB_ICN2DEV_DAT_EN       " -> params.CDB_ICN2DEV_DAT_EN,
  "CDB_DEV2ICN_REQ_EN       " -> params.CDB_DEV2ICN_REQ_EN,
  "CDB_DEV2ICN_RSP_EN       " -> params.CDB_DEV2ICN_RSP_EN,
  "CDB_DEV2ICN_SNP_EN       " -> params.CDB_DEV2ICN_SNP_EN,
  "CDB_DEV2ICN_DAT_EN       " -> params.CDB_DEV2ICN_DAT_EN,
  "CDB_REQFLIT_WIDTH        " -> params.CDB_REQFLIT_WIDTH,
  "CDB_RSPFLIT_WIDTH        " -> params.CDB_RSPFLIT_WIDTH,
  "CDB_SNPFLIT_WIDTH        " -> params.CDB_SNPFLIT_WIDTH,
  "CDB_DATFLIT_WIDTH        " -> params.CDB_DATFLIT_WIDTH
)) with HasBlackBoxResource {

  // 定义端口，严格匹配 SV 模块的输入输出
  val io = IO(new Bundle {
    // 时钟与复位
    val clk_dev = Input(Clock())
    val rstn_dev = Input(Reset())

    // DEV 侧链路信号
    val rxsactive_dev = Input(Bool())
    val txsactive_dev = Output(Bool())
    val rxlinkactivereq_dev = Input(Bool())
    val rxlinkactiveack_dev = Output(Bool())
    val txlinkactivereq_dev = Output(Bool())
    val txlinkactiveack_dev = Input(Bool())

    // DEV 侧 REQ 通道
    val rxreq_flitpend_dev = Input(Bool())
    val rxreq_flitv_dev = Input(Bool())
    val rxreq_flit_dev = Input(UInt(params.CDB_REQFLIT_WIDTH.W))
    val rxreq_crdv_dev = Output(Bool())
    val txreq_flitpend_dev = Output(Bool())
    val txreq_flitv_dev = Output(Bool())
    val txreq_flit_dev = Output(UInt(params.CDB_REQFLIT_WIDTH.W))
    val txreq_crdv_dev = Input(Bool())

    // DEV 侧 RSP 通道
    val rxrsp_flitpend_dev = Input(Bool())
    val rxrsp_flitv_dev = Input(Bool())
    val rxrsp_flit_dev = Input(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val rxrsp_crdv_dev = Output(Bool())
    val txrsp_flitpend_dev = Output(Bool())
    val txrsp_flitv_dev = Output(Bool())
    val txrsp_flit_dev = Output(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val txrsp_crdv_dev = Input(Bool())

    // DEV 侧 SNP 通道
    val rxsnp_flitpend_dev = Input(Bool())
    val rxsnp_flitv_dev = Input(Bool())
    val rxsnp_flit_dev = Input(UInt(params.CDB_SNPFLIT_WIDTH.W))
    val rxsnp_crdv_dev = Output(Bool())
    val txsnp_flitpend_dev = Output(Bool())
    val txsnp_flitv_dev = Output(Bool())
    val txsnp_flit_dev = Output(UInt(params.CDB_SNPFLIT_WIDTH.W))
    val txsnp_crdv_dev = Input(Bool())

    // DEV 侧 DAT 通道
    val rxdat_flitpend_dev = Input(Bool())
    val rxdat_flitv_dev = Input(Bool())
    val rxdat_flit_dev = Input(UInt(params.CDB_DATFLIT_WIDTH.W))
    val rxdat_crdv_dev = Output(Bool())
    val txdat_flitpend_dev = Output(Bool())
    val txdat_flitv_dev = Output(Bool())
    val txdat_flit_dev = Output(UInt(params.CDB_DATFLIT_WIDTH.W))
    val txdat_crdv_dev = Input(Bool())

    // FIFO 接口：REQ 通道
    val cdb_fifo_data_dev2icn_req = Output(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_DEV).W))
    val rptr_r_icn2dev_req = Input(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
    val wptr_r_dev2icn_req = Output(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
    val cdb_fifo_data_icn2dev_req = Input(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_DEV).W))
    val rptr_r_dev2icn_req = Output(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
    val wptr_r_icn2dev_req = Input(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))

    // FIFO 接口：RSP 通道
    val cdb_fifo_data_dev2icn_rsp = Output(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_DEV).W))
    val rptr_r_icn2dev_rsp = Input(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
    val wptr_r_dev2icn_rsp = Output(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
    val cdb_fifo_data_icn2dev_rsp = Input(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_DEV).W))
    val rptr_r_dev2icn_rsp = Output(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
    val wptr_r_icn2dev_rsp = Input(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))

    // FIFO 接口：SNP 通道
    val cdb_fifo_data_dev2icn_snp = Output(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_DEV).W))
    val rptr_r_icn2dev_snp = Input(UInt(params.CDB_SNPFIFO_DEPTH_DEV.W))
    val wptr_r_dev2icn_snp = Output(UInt(params.CDB_SNPFIFO_DEPTH_DEV.W))
    val cdb_fifo_data_icn2dev_snp = Input(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_DEV).W))
    val rptr_r_dev2icn_snp = Output(UInt(params.CDB_SNPFIFO_DEPTH_DEV.W))
    val wptr_r_icn2dev_snp = Input(UInt(params.CDB_SNPFIFO_DEPTH_DEV.W))

    // FIFO 接口：DAT 通道
    val cdb_fifo_data_dev2icn_dat = Output(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_DEV).W))
    val rptr_r_icn2dev_dat = Input(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
    val wptr_r_dev2icn_dat = Output(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
    val cdb_fifo_data_icn2dev_dat = Input(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_DEV).W))
    val rptr_r_dev2icn_dat = Output(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
    val wptr_r_icn2dev_dat = Input(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
  })

  // 指定 SV 文件路径（如果放在 src/main/resources 下可省略，否则需完整路径）
  addResource("/dsu_cdb/defines/dsu_chie_defines.sv")
  addResource("/dsu_cdb/defines/common_defines.sv")
  addResource("/dsu_cdb/defines/router_define.sv")
  addResource("/dsu_cdb/rlink_lainit.v")
  addResource("/dsu_cdb/fifo.sv")
  addResource("/dsu_cdb/credit_manager.sv")
  addResource("/dsu_cdb/sync_dff.sv")
  addResource("/dsu_cdb/rstn_dff.sv")
  addResource("/dsu_cdb/clock_gate.sv")
  addResource("/dsu_cdb/reset_sync.sv")
  addResource("/dsu_cdb/cdb_ingress_channel.sv")
  addResource("/dsu_cdb/cdb_egress_channel.sv")
  addResource("/dsu_cdb/cdb_dev.sv")
}

class CHIAsyncDEVDSU(params: CDBParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cdb = Flipped(new CHIAsyncIODSU(params))
    val chi = Flipped(new PortIO)
  }
  )
  //---instance cdb bridge ---
  val cdbdev = Module(new cdb_dev(params))

  //===clock reset connect =====
  cdbdev.io.rstn_dev := (!reset.asBool).asAsyncReset
  cdbdev.io.clk_dev := clock
  //=== chi connect===
   // cdbdev.io.SYSCOREQ := io.chi.syscoreq
  io.chi.syscoack := true.B //cdbdev.io.SYSCOACK// DEV 侧链路信号
  cdbdev.io.rxsactive_dev := io.chi.txsactive
  io.chi.rxsactive := cdbdev.io.txsactive_dev
  cdbdev.io.rxlinkactivereq_dev := io.chi.tx.linkactivereq
  io.chi.tx.linkactiveack := cdbdev.io.rxlinkactiveack_dev
  io.chi.rx.linkactivereq := cdbdev.io.txlinkactivereq_dev
  cdbdev.io.txlinkactiveack_dev := io.chi.rx.linkactiveack

  // DEV 侧 REQ 通道
  cdbdev.io.rxreq_flitpend_dev := io.chi.tx.req.flitpend
  cdbdev.io.rxreq_flitv_dev := io.chi.tx.req.flitv
  cdbdev.io.rxreq_flit_dev := io.chi.tx.req.flit
  io.chi.tx.req.lcrdv := cdbdev.io.rxreq_crdv_dev
  //     io.chi.rx.req.flitpend          := cdbdev.io.txreq_flitpend_dev
  //     io.chi.rx.req.flitv         := cdbdev.io.txreq_flitv_dev
  //     io.chi.rx.req.flit          := cdbdev.io.txreq_flit_dev
  cdbdev.io.txreq_crdv_dev := 0.U // io.chi.rx.req.lcrdv

  // DEV 侧 RSP 通道
  cdbdev.io.rxrsp_flitpend_dev := io.chi.tx.rsp.flitpend
  cdbdev.io.rxrsp_flitv_dev := io.chi.tx.rsp.flitv
  cdbdev.io.rxrsp_flit_dev := io.chi.tx.rsp.flit
  io.chi.tx.rsp.lcrdv := cdbdev.io.rxrsp_crdv_dev
  io.chi.rx.rsp.flitpend := cdbdev.io.txrsp_flitpend_dev
  io.chi.rx.rsp.flitv := cdbdev.io.txrsp_flitv_dev
  io.chi.rx.rsp.flit := cdbdev.io.txrsp_flit_dev
  cdbdev.io.txrsp_crdv_dev := io.chi.rx.rsp.lcrdv

  // DEV 侧 SNP 通道
  cdbdev.io.rxsnp_flitpend_dev := 0.U // io.chi.tx.snp.flitpend
  cdbdev.io.rxsnp_flitv_dev := 0.U //  io.chi.tx.snp.flitv
  cdbdev.io.rxsnp_flit_dev := 0.U //  io.chi.tx.snp.flit
  //     io.chi.tx.snp.lcrdv              := cdbdev.io.rxsnp_crdv_dev
  io.chi.rx.snp.flitpend := cdbdev.io.txsnp_flitpend_dev
  io.chi.rx.snp.flitv := cdbdev.io.txsnp_flitv_dev
  io.chi.rx.snp.flit := cdbdev.io.txsnp_flit_dev
  cdbdev.io.txsnp_crdv_dev := io.chi.rx.snp.lcrdv

  // DEV 侧 DAT 通道
  cdbdev.io.rxdat_flitpend_dev := io.chi.tx.dat.flitpend
  cdbdev.io.rxdat_flitv_dev := io.chi.tx.dat.flitv
  cdbdev.io.rxdat_flit_dev := io.chi.tx.dat.flit
  io.chi.tx.dat.lcrdv := cdbdev.io.rxdat_crdv_dev
  io.chi.rx.dat.flitpend := cdbdev.io.txdat_flitpend_dev
  io.chi.rx.dat.flitv := cdbdev.io.txdat_flitv_dev
  io.chi.rx.dat.flit := cdbdev.io.txdat_flit_dev
  cdbdev.io.txdat_crdv_dev := io.chi.rx.dat.lcrdv

  // FIFO 接口：REQ 通道
  io.cdb.cdb_fifo_data_dev2icn_req := cdbdev.io.cdb_fifo_data_dev2icn_req
  cdbdev.io.rptr_r_icn2dev_req := io.cdb.rptr_r_icn2dev_req
  io.cdb.wptr_r_dev2icn_req := cdbdev.io.wptr_r_dev2icn_req
  cdbdev.io.cdb_fifo_data_icn2dev_req := io.cdb.cdb_fifo_data_icn2dev_req
  io.cdb.rptr_r_dev2icn_req := cdbdev.io.rptr_r_dev2icn_req
  cdbdev.io.wptr_r_icn2dev_req := io.cdb.wptr_r_icn2dev_req

  // FIFO 接口：RSP 通道
  io.cdb.cdb_fifo_data_dev2icn_rsp := cdbdev.io.cdb_fifo_data_dev2icn_rsp
  cdbdev.io.rptr_r_icn2dev_rsp := io.cdb.rptr_r_icn2dev_rsp
  io.cdb.wptr_r_dev2icn_rsp := cdbdev.io.wptr_r_dev2icn_rsp
  cdbdev.io.cdb_fifo_data_icn2dev_rsp := io.cdb.cdb_fifo_data_icn2dev_rsp
  io.cdb.rptr_r_dev2icn_rsp := cdbdev.io.rptr_r_dev2icn_rsp
  cdbdev.io.wptr_r_icn2dev_rsp := io.cdb.wptr_r_icn2dev_rsp

  // FIFO 接口：SNP 通道
  io.cdb.cdb_fifo_data_dev2icn_snp := cdbdev.io.cdb_fifo_data_dev2icn_snp
  cdbdev.io.rptr_r_icn2dev_snp := io.cdb.rptr_r_icn2dev_snp
  io.cdb.wptr_r_dev2icn_snp := cdbdev.io.wptr_r_dev2icn_snp
  cdbdev.io.cdb_fifo_data_icn2dev_snp := io.cdb.cdb_fifo_data_icn2dev_snp
  io.cdb.rptr_r_dev2icn_snp := cdbdev.io.rptr_r_dev2icn_snp
  cdbdev.io.wptr_r_icn2dev_snp := io.cdb.wptr_r_icn2dev_snp

  // FIFO 接口：DAT 通道
  io.cdb.cdb_fifo_data_dev2icn_dat := cdbdev.io.cdb_fifo_data_dev2icn_dat
  cdbdev.io.rptr_r_icn2dev_dat := io.cdb.rptr_r_icn2dev_dat
  io.cdb.wptr_r_dev2icn_dat := cdbdev.io.wptr_r_dev2icn_dat
  cdbdev.io.cdb_fifo_data_icn2dev_dat := io.cdb.cdb_fifo_data_icn2dev_dat
  io.cdb.rptr_r_dev2icn_dat := cdbdev.io.rptr_r_dev2icn_dat
  cdbdev.io.wptr_r_icn2dev_dat := io.cdb.wptr_r_icn2dev_dat
}
