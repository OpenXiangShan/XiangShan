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
                     CDB_REQFIFO_DEPTH_DEV_PARAM     : Int = 8,
                     CDB_RSPFIFO_DEPTH_DEV_PARAM     : Int = 8,
                     CDB_DATFIFO_DEPTH_DEV_PARAM     : Int = 8,
                     CDB_SNPFIFO_DEPTH_DEV_PARAM     : Int = 8,
                     CDB_REQFIFO_DEPTH_ICN_PARAM     : Int = 8,
                     CDB_RSPFIFO_DEPTH_ICN_PARAM     : Int = 8,
                     CDB_DATFIFO_DEPTH_ICN_PARAM     : Int = 8,
                     CDB_SNPFIFO_DEPTH_ICN_PARAM     : Int = 8,
                     CDB_REQLCRD_MAX_COUNT_DEV_PARAM : Int = 8,
                     CDB_RSPLCRD_MAX_COUNT_DEV_PARAM : Int = 8,
                     CDB_DATLCRD_MAX_COUNT_DEV_PARAM : Int = 8,
                     CDB_SNPLCRD_MAX_COUNT_DEV_PARAM : Int = 8,
                     CDB_REQLCRD_MAX_COUNT_ICN_PARAM : Int = 8,
                     CDB_RSPLCRD_MAX_COUNT_ICN_PARAM : Int = 8,
                     CDB_DATLCRD_MAX_COUNT_ICN_PARAM : Int = 8,
                     CDB_SNPLCRD_MAX_COUNT_ICN_PARAM : Int = 8,
                     CDB_REQFLIT_WIDTH_PARAM         : Int = 162,
                     CDB_RSPFLIT_WIDTH_PARAM         : Int = 73 ,
                     CDB_DATFLIT_WIDTH_PARAM         : Int = 422,
                     CDB_SNPFLIT_WIDTH_PARAM         : Int = 115,
                     CDB_REQFLIT_OPCODE_LSB_PARAM    : Int = 62,
                     CDB_REQFLIT_OPCODE_MSB_PARAM    : Int = 68,
                     CDB_RSPFLIT_OPCODE_LSB_PARAM    : Int = 38,
                     CDB_RSPFLIT_OPCODE_MSB_PARAM    : Int = 42,
                     CDB_DATFLIT_OPCODE_LSB_PARAM    : Int = 49,
                     CDB_DATFLIT_OPCODE_MSB_PARAM    : Int = 52,
                     CDB_SNPFLIT_OPCODE_LSB_PARAM    : Int = 50,
                     CDB_SNPFLIT_OPCODE_MSB_PARAM    : Int = 54
                    ) {
  def CDB_REQFIFO_DEPTH_DEV: Int = CDB_REQFIFO_DEPTH_DEV_PARAM
  def CDB_RSPFIFO_DEPTH_DEV: Int = CDB_RSPFIFO_DEPTH_DEV_PARAM
  def CDB_DATFIFO_DEPTH_DEV: Int = CDB_DATFIFO_DEPTH_DEV_PARAM
  def CDB_SNPFIFO_DEPTH_DEV: Int = CDB_SNPFIFO_DEPTH_DEV_PARAM
  def CDB_REQFIFO_DEPTH_ICN: Int = CDB_REQFIFO_DEPTH_ICN_PARAM
  def CDB_RSPFIFO_DEPTH_ICN: Int = CDB_RSPFIFO_DEPTH_ICN_PARAM
  def CDB_DATFIFO_DEPTH_ICN: Int = CDB_DATFIFO_DEPTH_ICN_PARAM
  def CDB_SNPFIFO_DEPTH_ICN: Int = CDB_SNPFIFO_DEPTH_ICN_PARAM
  def CDB_REQFLIT_WIDTH: Int = CDB_REQFLIT_WIDTH_PARAM
  def CDB_RSPFLIT_WIDTH: Int = CDB_RSPFLIT_WIDTH_PARAM
  def CDB_DATFLIT_WIDTH: Int = CDB_DATFLIT_WIDTH_PARAM
  def CDB_SNPFLIT_WIDTH: Int = CDB_SNPFLIT_WIDTH_PARAM
}
//async interfaces between two half bridges.
class CHIAsyncIODSU(params: CDBParams) extends Bundle {
  val devtoicn_req_wptr_async = Input(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
  val devtoicn_req_fifo_data_mcp = Input(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_DEV).W))
  val icntodev_req_rptr_async = Output(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
  val devtoicn_rsp_wptr_async = Input(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
  val devtoicn_rsp_fifo_data_mcp = Input(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_DEV).W))
  val icntodev_rsp_rptr_async = Output(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
  val devtoicn_dat_wptr_async = Input(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
  val devtoicn_dat_fifo_data_mcp = Input(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_DEV).W))
  val icntodev_dat_rptr_async = Output(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
  val icntodev_snp_wptr_async = Output(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
  val icntodev_snp_fifo_data_mcp = Output(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_ICN).W))
  val devtoicn_snp_rptr_async = Input(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
  val icntodev_rsp_wptr_async = Output(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
  val icntodev_rsp_fifo_data_mcp = Output(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_ICN).W))
  val devtoicn_rsp_rptr_async = Input(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
  val icntodev_dat_wptr_async = Output(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
  val icntodev_dat_fifo_data_mcp = Output(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_ICN).W))
  val devtoicn_dat_rptr_async = Input(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
  val devtoicn_pwr_handshake_async = Input(Bool())
  val icntodev_SACTIVE_async = Output(Bool())
  val devtoicn_SACTIVE_async = Input(Bool())
  val icntodev_txfifo_qactive_async = Output(Bool())
  val icntodev_rxfifo_qactive_async = Output(Bool())
  val devtoicn_txfifo_qactive_async = Input(Bool())
  val devtoicn_rxfifo_qactive_async = Input(Bool())
  val devtoicn_pwr_qreqn_async = Input(Bool())
  val icntodev_pwr_qacceptn_async = Output(Bool())
  val icntodev_pwr_qdeny_async = Output(Bool())
  val devtoicn_ptr_reset_req_async = Input(Bool())
  val icntodev_ptr_reset_ack_async = Output(Bool())
  val devtoicn_syscoreq_async = Input(Bool())
  val icntodev_syscoack_async = Output(Bool())
}

class CHIAsyncICNDSU(params: CDBParams)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cdb = new CHIAsyncIODSU(params)
    val chi = new PortIO
  })
  val cdbicn = Module(new cdb_rnf_icn(params))

  cdbicn.io.clk := clock
  cdbicn.io.RESETN := (!reset.asBool).asAsyncReset
  cdbicn.io.DFTRSTDISABLE := false.B //i.dft.scan_enable
  cdbicn.io.DFTCGEN := false.B //i.dft.icg_scan_en

  cdbicn.io.RXSACTIVE_local := io.chi.rxsactive
  io.chi.txsactive := cdbicn.io.TXSACTIVE_local
  cdbicn.io.RXLINKACTIVEREQ := io.chi.rx.linkactivereq
  io.chi.rx.linkactiveack := cdbicn.io.RXLINKACTIVEACK
  io.chi.tx.linkactivereq := cdbicn.io.TXLINKACTIVEREQ
  cdbicn.io.TXLINKACTIVEACK := io.chi.tx.linkactiveack
  io.chi.syscoreq := cdbicn.io.SYSCOREQ
  cdbicn.io.SYSCOACK := io.chi.syscoack

  io.chi.tx.req.flitpend := cdbicn.io.TXREQFLITPEND
  io.chi.tx.req.flitv := cdbicn.io.TXREQFLITV
  io.chi.tx.req.flit := cdbicn.io.TXREQFLIT
  cdbicn.io.TXREQLCRDV := io.chi.tx.req.lcrdv

  io.chi.tx.rsp.flitpend := cdbicn.io.TXRSPFLITPEND
  io.chi.tx.rsp.flitv := cdbicn.io.TXRSPFLITV
  io.chi.tx.rsp.flit := cdbicn.io.TXRSPFLIT
  cdbicn.io.TXRSPLCRDV := io.chi.tx.rsp.lcrdv
  cdbicn.io.RXRSPFLITPEND := io.chi.rx.rsp.flitpend
  cdbicn.io.RXRSPFLITV := io.chi.rx.rsp.flitv
  cdbicn.io.RXRSPFLIT := io.chi.rx.rsp.flit
  io.chi.rx.rsp.lcrdv := cdbicn.io.RXRSPLCRDV

  cdbicn.io.RXSNPFLITPEND := io.chi.rx.snp.flitpend
  cdbicn.io.RXSNPFLITV := io.chi.rx.snp.flitv
  cdbicn.io.RXSNPFLIT := io.chi.rx.snp.flit
  io.chi.rx.snp.lcrdv := cdbicn.io.RXSNPLCRDV

  io.chi.tx.dat.flitpend := cdbicn.io.TXDATFLITPEND
  io.chi.tx.dat.flitv := cdbicn.io.TXDATFLITV
  io.chi.tx.dat.flit := cdbicn.io.TXDATFLIT
  cdbicn.io.TXDATLCRDV := io.chi.tx.dat.lcrdv
  cdbicn.io.RXDATFLITPEND := io.chi.rx.dat.flitpend
  cdbicn.io.RXDATFLITV := io.chi.rx.dat.flitv
  cdbicn.io.RXDATFLIT := io.chi.rx.dat.flit
  io.chi.rx.dat.lcrdv := cdbicn.io.RXDATLCRDV

  cdbicn.io.devtoicn_req_wptr_async := io.cdb.devtoicn_req_wptr_async
  cdbicn.io.devtoicn_req_fifo_data_mcp := io.cdb.devtoicn_req_fifo_data_mcp
  io.cdb.icntodev_req_rptr_async := cdbicn.io.icntodev_req_rptr_async
  cdbicn.io.devtoicn_rsp_wptr_async := io.cdb.devtoicn_rsp_wptr_async
  cdbicn.io.devtoicn_rsp_fifo_data_mcp := io.cdb.devtoicn_rsp_fifo_data_mcp
  io.cdb.icntodev_rsp_rptr_async := cdbicn.io.icntodev_rsp_rptr_async
  cdbicn.io.devtoicn_dat_wptr_async := io.cdb.devtoicn_dat_wptr_async
  cdbicn.io.devtoicn_dat_fifo_data_mcp := io.cdb.devtoicn_dat_fifo_data_mcp
  io.cdb.icntodev_dat_rptr_async := cdbicn.io.icntodev_dat_rptr_async
  io.cdb.icntodev_snp_wptr_async := cdbicn.io.icntodev_snp_wptr_async
  io.cdb.icntodev_snp_fifo_data_mcp := cdbicn.io.icntodev_snp_fifo_data_mcp
  cdbicn.io.devtoicn_snp_rptr_async := io.cdb.devtoicn_snp_rptr_async
  io.cdb.icntodev_rsp_wptr_async := cdbicn.io.icntodev_rsp_wptr_async
  io.cdb.icntodev_rsp_fifo_data_mcp := cdbicn.io.icntodev_rsp_fifo_data_mcp
  cdbicn.io.devtoicn_rsp_rptr_async := io.cdb.devtoicn_rsp_rptr_async
  io.cdb.icntodev_dat_wptr_async := cdbicn.io.icntodev_dat_wptr_async
  io.cdb.icntodev_dat_fifo_data_mcp := cdbicn.io.icntodev_dat_fifo_data_mcp
  cdbicn.io.devtoicn_dat_rptr_async := io.cdb.devtoicn_dat_rptr_async
  cdbicn.io.devtoicn_pwr_handshake_async := io.cdb.devtoicn_pwr_handshake_async
  io.cdb.icntodev_SACTIVE_async := cdbicn.io.icntodev_SACTIVE_async
  cdbicn.io.devtoicn_SACTIVE_async := io.cdb.devtoicn_SACTIVE_async
  io.cdb.icntodev_txfifo_qactive_async := cdbicn.io.icntodev_txfifo_qactive_async
  io.cdb.icntodev_rxfifo_qactive_async := cdbicn.io.icntodev_rxfifo_qactive_async
  cdbicn.io.devtoicn_txfifo_qactive_async := io.cdb.devtoicn_txfifo_qactive_async
  cdbicn.io.devtoicn_rxfifo_qactive_async := io.cdb.devtoicn_rxfifo_qactive_async
  cdbicn.io.devtoicn_pwr_qreqn_async := io.cdb.devtoicn_pwr_qreqn_async
  io.cdb.icntodev_pwr_qacceptn_async := cdbicn.io.icntodev_pwr_qacceptn_async
  io.cdb.icntodev_pwr_qdeny_async := cdbicn.io.icntodev_pwr_qdeny_async
  cdbicn.io.devtoicn_ptr_reset_req_async := io.cdb.devtoicn_ptr_reset_req_async
  io.cdb.icntodev_ptr_reset_ack_async := cdbicn.io.icntodev_ptr_reset_ack_async
  cdbicn.io.devtoicn_syscoreq_async := io.cdb.devtoicn_syscoreq_async
  io.cdb.icntodev_syscoack_async := cdbicn.io.icntodev_syscoack_async
}

// 声明 CDB_ICN 的 BlackBox，严格匹配 SV 模块的参数和端口
class cdb_rnf_icn(params: CDBParams) extends BlackBox(Map(
  // 对应 SV 模块的所有参数，默认值与 SV 中一致
  "CDB_REQFIFO_DEPTH_DEV_PARAM    " -> params.CDB_REQFIFO_DEPTH_DEV_PARAM    ,
  "CDB_RSPFIFO_DEPTH_DEV_PARAM    " -> params.CDB_RSPFIFO_DEPTH_DEV_PARAM    ,
  "CDB_DATFIFO_DEPTH_DEV_PARAM    " -> params.CDB_DATFIFO_DEPTH_DEV_PARAM    ,
  "CDB_SNPFIFO_DEPTH_DEV_PARAM    " -> params.CDB_SNPFIFO_DEPTH_DEV_PARAM    ,
  "CDB_REQFIFO_DEPTH_ICN_PARAM    " -> params.CDB_REQFIFO_DEPTH_ICN_PARAM    ,
  "CDB_RSPFIFO_DEPTH_ICN_PARAM    " -> params.CDB_RSPFIFO_DEPTH_ICN_PARAM    ,
  "CDB_DATFIFO_DEPTH_ICN_PARAM    " -> params.CDB_DATFIFO_DEPTH_ICN_PARAM    ,
  "CDB_SNPFIFO_DEPTH_ICN_PARAM    " -> params.CDB_SNPFIFO_DEPTH_ICN_PARAM    ,
  "CDB_REQLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_REQLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_RSPLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_RSPLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_DATLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_DATLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_SNPLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_SNPLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_REQLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_REQLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_RSPLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_RSPLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_DATLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_DATLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_SNPLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_SNPLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_REQFLIT_WIDTH_PARAM        " -> params.CDB_REQFLIT_WIDTH_PARAM        ,
  "CDB_RSPFLIT_WIDTH_PARAM        " -> params.CDB_RSPFLIT_WIDTH_PARAM        ,
  "CDB_DATFLIT_WIDTH_PARAM        " -> params.CDB_DATFLIT_WIDTH_PARAM        ,
  "CDB_SNPFLIT_WIDTH_PARAM        " -> params.CDB_SNPFLIT_WIDTH_PARAM        ,
  "CDB_REQFLIT_OPCODE_LSB_PARAM   " -> params.CDB_REQFLIT_OPCODE_LSB_PARAM   ,
  "CDB_REQFLIT_OPCODE_MSB_PARAM   " -> params.CDB_REQFLIT_OPCODE_MSB_PARAM   ,
  "CDB_RSPFLIT_OPCODE_LSB_PARAM   " -> params.CDB_RSPFLIT_OPCODE_LSB_PARAM   ,
  "CDB_RSPFLIT_OPCODE_MSB_PARAM   " -> params.CDB_RSPFLIT_OPCODE_MSB_PARAM   ,
  "CDB_DATFLIT_OPCODE_LSB_PARAM   " -> params.CDB_DATFLIT_OPCODE_LSB_PARAM   ,
  "CDB_DATFLIT_OPCODE_MSB_PARAM   " -> params.CDB_DATFLIT_OPCODE_MSB_PARAM   ,
  "CDB_SNPFLIT_OPCODE_LSB_PARAM   " -> params.CDB_SNPFLIT_OPCODE_LSB_PARAM   ,
  "CDB_SNPFLIT_OPCODE_MSB_PARAM   " -> params.CDB_SNPFLIT_OPCODE_MSB_PARAM   
)) {
  // 定义端口，严格匹配 SV 模块的输入输出
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val RESETN = Input(Reset())
    val DFTRSTDISABLE = Input(Bool())
    val DFTCGEN = Input(Bool())
    val RXSACTIVE_local = Input(Bool())
    val TXSACTIVE_local = Output(Bool())
    val CLK_QACTIVE = Output(Bool())
    val RXLINKACTIVEREQ = Input(Bool())
    val RXLINKACTIVEACK = Output(Bool())
    val TXLINKACTIVEREQ = Output(Bool())
    val TXLINKACTIVEACK = Input(Bool())
    val SYSCOREQ = Output(Bool())
    val SYSCOACK = Input(Bool())
    val TXREQFLITPEND = Output(Bool())
    val TXREQFLITV = Output(Bool())
    val TXREQFLIT = Output(UInt(params.CDB_REQFLIT_WIDTH.W))
    val TXREQLCRDV = Input(Bool())
    val TXRSPFLITPEND = Output(Bool())
    val TXRSPFLITV = Output(Bool())
    val TXRSPFLIT = Output(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val TXRSPLCRDV = Input(Bool())
    val TXDATFLITPEND = Output(Bool())
    val TXDATFLITV = Output(Bool())
    val TXDATFLIT = Output(UInt(params.CDB_DATFLIT_WIDTH.W))
    val TXDATLCRDV = Input(Bool())
    val RXSNPFLITPEND = Input(Bool())
    val RXSNPFLITV = Input(Bool())
    val RXSNPFLIT = Input(UInt(params.CDB_SNPFLIT_WIDTH.W))
    val RXSNPLCRDV = Output(Bool())
    val RXRSPFLITPEND = Input(Bool())
    val RXRSPFLITV = Input(Bool())
    val RXRSPFLIT = Input(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val RXRSPLCRDV = Output(Bool())
    val RXDATFLITPEND = Input(Bool())
    val RXDATFLITV = Input(Bool())
    val RXDATFLIT = Input(UInt(params.CDB_DATFLIT_WIDTH.W))
    val RXDATLCRDV = Output(Bool())
    val devtoicn_req_wptr_async = Input(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
    val devtoicn_req_fifo_data_mcp = Input(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_DEV).W))
    val icntodev_req_rptr_async = Output(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
    val devtoicn_rsp_wptr_async = Input(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
    val devtoicn_rsp_fifo_data_mcp = Input(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_DEV).W))
    val icntodev_rsp_rptr_async = Output(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
    val devtoicn_dat_wptr_async = Input(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
    val devtoicn_dat_fifo_data_mcp = Input(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_DEV).W))
    val icntodev_dat_rptr_async = Output(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
    val icntodev_snp_wptr_async = Output(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
    val icntodev_snp_fifo_data_mcp = Output(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_ICN).W))
    val devtoicn_snp_rptr_async = Input(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
    val icntodev_rsp_wptr_async = Output(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
    val icntodev_rsp_fifo_data_mcp = Output(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_ICN).W))
    val devtoicn_rsp_rptr_async = Input(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
    val icntodev_dat_wptr_async = Output(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
    val icntodev_dat_fifo_data_mcp = Output(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_ICN).W))
    val devtoicn_dat_rptr_async = Input(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
    val devtoicn_pwr_handshake_async = Input(Bool())
    val icntodev_SACTIVE_async = Output(Bool())
    val devtoicn_SACTIVE_async = Input(Bool())
    val icntodev_txfifo_qactive_async = Output(Bool())
    val icntodev_rxfifo_qactive_async = Output(Bool())
    val devtoicn_txfifo_qactive_async = Input(Bool())
    val devtoicn_rxfifo_qactive_async = Input(Bool())
    val devtoicn_pwr_qreqn_async = Input(Bool())
    val icntodev_pwr_qacceptn_async = Output(Bool())
    val icntodev_pwr_qdeny_async = Output(Bool())
    val devtoicn_ptr_reset_req_async = Input(Bool())
    val icntodev_ptr_reset_ack_async = Output(Bool())
    val devtoicn_syscoreq_async = Input(Bool())
    val icntodev_syscoack_async = Output(Bool())
  })

}

// 声明 CDB_DEV 的 BlackBox，严格匹配 SV 模块的参数和端口
class cdb_rnf_dev(params: CDBParams) extends BlackBox(Map(
  // 对应 SV 模块的所有参数，默认值与 SV 中一致
  "CDB_REQFIFO_DEPTH_DEV_PARAM    " -> params.CDB_REQFIFO_DEPTH_DEV_PARAM    ,
  "CDB_RSPFIFO_DEPTH_DEV_PARAM    " -> params.CDB_RSPFIFO_DEPTH_DEV_PARAM    ,
  "CDB_DATFIFO_DEPTH_DEV_PARAM    " -> params.CDB_DATFIFO_DEPTH_DEV_PARAM    ,
  "CDB_SNPFIFO_DEPTH_DEV_PARAM    " -> params.CDB_SNPFIFO_DEPTH_DEV_PARAM    ,
  "CDB_REQFIFO_DEPTH_ICN_PARAM    " -> params.CDB_REQFIFO_DEPTH_ICN_PARAM    ,
  "CDB_RSPFIFO_DEPTH_ICN_PARAM    " -> params.CDB_RSPFIFO_DEPTH_ICN_PARAM    ,
  "CDB_DATFIFO_DEPTH_ICN_PARAM    " -> params.CDB_DATFIFO_DEPTH_ICN_PARAM    ,
  "CDB_SNPFIFO_DEPTH_ICN_PARAM    " -> params.CDB_SNPFIFO_DEPTH_ICN_PARAM    ,
  "CDB_REQLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_REQLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_RSPLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_RSPLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_DATLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_DATLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_SNPLCRD_MAX_COUNT_DEV_PARAM" -> params.CDB_SNPLCRD_MAX_COUNT_DEV_PARAM,
  "CDB_REQLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_REQLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_RSPLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_RSPLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_DATLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_DATLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_SNPLCRD_MAX_COUNT_ICN_PARAM" -> params.CDB_SNPLCRD_MAX_COUNT_ICN_PARAM,
  "CDB_REQFLIT_WIDTH_PARAM        " -> params.CDB_REQFLIT_WIDTH_PARAM        ,
  "CDB_RSPFLIT_WIDTH_PARAM        " -> params.CDB_RSPFLIT_WIDTH_PARAM        ,
  "CDB_DATFLIT_WIDTH_PARAM        " -> params.CDB_DATFLIT_WIDTH_PARAM        ,
  "CDB_SNPFLIT_WIDTH_PARAM        " -> params.CDB_SNPFLIT_WIDTH_PARAM        ,
  "CDB_REQFLIT_OPCODE_LSB_PARAM   " -> params.CDB_REQFLIT_OPCODE_LSB_PARAM   ,
  "CDB_REQFLIT_OPCODE_MSB_PARAM   " -> params.CDB_REQFLIT_OPCODE_MSB_PARAM   ,
  "CDB_RSPFLIT_OPCODE_LSB_PARAM   " -> params.CDB_RSPFLIT_OPCODE_LSB_PARAM   ,
  "CDB_RSPFLIT_OPCODE_MSB_PARAM   " -> params.CDB_RSPFLIT_OPCODE_MSB_PARAM   ,
  "CDB_DATFLIT_OPCODE_LSB_PARAM   " -> params.CDB_DATFLIT_OPCODE_LSB_PARAM   ,
  "CDB_DATFLIT_OPCODE_MSB_PARAM   " -> params.CDB_DATFLIT_OPCODE_MSB_PARAM   ,
  "CDB_SNPFLIT_OPCODE_LSB_PARAM   " -> params.CDB_SNPFLIT_OPCODE_LSB_PARAM   ,
  "CDB_SNPFLIT_OPCODE_MSB_PARAM   " -> params.CDB_SNPFLIT_OPCODE_MSB_PARAM    


)) {
  // 定义端口，严格匹配 SV 模块的输入输出
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val RESETN = Input(Reset())
    val DFTRSTDISABLE = Input(Bool())
    val DFTCGEN = Input(Bool())
    val PWR_QACTIVE = Output(Bool())
    val PWR_QREQN = Input(Bool())
    val PWR_QACCEPTN = Output(Bool())
    val PWR_QDENY = Output(Bool())
    val RXSACTIVE_local = Input(Bool())
    val TXSACTIVE_local = Output(Bool())
    val CLK_QACTIVE = Output(Bool())
    val RXLINKACTIVEREQ = Input(Bool())
    val RXLINKACTIVEACK = Output(Bool())
    val TXLINKACTIVEREQ = Output(Bool())
    val TXLINKACTIVEACK = Input(Bool())
    val SYSCOREQ = Input(Bool())
    val SYSCOACK = Output(Bool())
    val RXREQFLITPEND = Input(Bool())
    val RXREQFLITV = Input(Bool())
    val RXREQFLIT = Input(UInt(params.CDB_REQFLIT_WIDTH.W))
    val RXREQLCRDV = Output(Bool())
    val RXRSPFLITPEND = Input(Bool())
    val RXRSPFLITV = Input(Bool())
    val RXRSPFLIT = Input(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val RXRSPLCRDV = Output(Bool())
    val RXDATFLITPEND = Input(Bool())
    val RXDATFLITV = Input(Bool())
    val RXDATFLIT = Input(UInt(params.CDB_DATFLIT_WIDTH.W))
    val RXDATLCRDV = Output(Bool())
    val TXSNPFLITPEND = Output(Bool())
    val TXSNPFLITV = Output(Bool())
    val TXSNPFLIT = Output(UInt(params.CDB_SNPFLIT_WIDTH.W))
    val TXSNPLCRDV = Input(Bool())
    val TXRSPFLITPEND = Output(Bool())
    val TXRSPFLITV = Output(Bool())
    val TXRSPFLIT = Output(UInt(params.CDB_RSPFLIT_WIDTH.W))
    val TXRSPLCRDV = Input(Bool())
    val TXDATFLITPEND = Output(Bool())
    val TXDATFLITV = Output(Bool())
    val TXDATFLIT = Output(UInt(params.CDB_DATFLIT_WIDTH.W))
    val TXDATLCRDV = Input(Bool())
    val devtoicn_req_wptr_async = Output(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
    val devtoicn_req_fifo_data_mcp = Output(UInt((params.CDB_REQFLIT_WIDTH * params.CDB_REQFIFO_DEPTH_DEV).W))
    val icntodev_req_rptr_async = Input(UInt(params.CDB_REQFIFO_DEPTH_DEV.W))
    val devtoicn_rsp_wptr_async = Output(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
    val devtoicn_rsp_fifo_data_mcp = Output(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_DEV).W))
    val icntodev_rsp_rptr_async = Input(UInt(params.CDB_RSPFIFO_DEPTH_DEV.W))
    val devtoicn_dat_wptr_async = Output(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
    val devtoicn_dat_fifo_data_mcp = Output(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_DEV).W))
    val icntodev_dat_rptr_async = Input(UInt(params.CDB_DATFIFO_DEPTH_DEV.W))
    val icntodev_snp_wptr_async = Input(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
    val icntodev_snp_fifo_data_mcp = Input(UInt((params.CDB_SNPFLIT_WIDTH * params.CDB_SNPFIFO_DEPTH_ICN).W))
    val devtoicn_snp_rptr_async = Output(UInt(params.CDB_SNPFIFO_DEPTH_ICN.W))
    val icntodev_rsp_wptr_async = Input(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
    val icntodev_rsp_fifo_data_mcp = Input(UInt((params.CDB_RSPFLIT_WIDTH * params.CDB_RSPFIFO_DEPTH_ICN).W))
    val devtoicn_rsp_rptr_async = Output(UInt(params.CDB_RSPFIFO_DEPTH_ICN.W))
    val icntodev_dat_wptr_async = Input(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
    val icntodev_dat_fifo_data_mcp = Input(UInt((params.CDB_DATFLIT_WIDTH * params.CDB_DATFIFO_DEPTH_ICN).W))
    val devtoicn_dat_rptr_async = Output(UInt(params.CDB_DATFIFO_DEPTH_ICN.W))
    val devtoicn_pwr_handshake_async = Output(Bool())
    val icntodev_SACTIVE_async = Input(Bool())
    val devtoicn_SACTIVE_async = Output(Bool())
    val icntodev_txfifo_qactive_async = Input(Bool())
    val icntodev_rxfifo_qactive_async = Input(Bool())
    val devtoicn_txfifo_qactive_async = Output(Bool())
    val devtoicn_rxfifo_qactive_async = Output(Bool())
    val devtoicn_pwr_qreqn_async = Output(Bool())
    val icntodev_pwr_qacceptn_async = Input(Bool())
    val icntodev_pwr_qdeny_async = Input(Bool())
    val devtoicn_ptr_reset_req_async = Output(Bool())
    val icntodev_ptr_reset_ack_async = Input(Bool())
    val devtoicn_syscoreq_async = Output(Bool())
    val icntodev_syscoack_async = Input(Bool())
  })
}

class CHIAsyncDEVDSU(params: CDBParams)(implicit p: Parameters) extends Module {
  // val i = IO(new Bundle {
  //   val dft = new Bundle {
  //   val icg_scan_en = Input(Bool())
  //   val scan_enable = Input(Bool())
  //   }       
  // }) 
  val io = IO(new Bundle {
    val cdb = Flipped(new CHIAsyncIODSU(params))
    val chi = Flipped(new PortIO)
  }
  )
  //---instance cdb bridge ---
  val cdbdev = Module(new cdb_rnf_dev(params))

  cdbdev.io.clk := clock
  cdbdev.io.RESETN := (!reset.asBool).asAsyncReset
  cdbdev.io.DFTRSTDISABLE := false.B //i.dft.scan_enable
  cdbdev.io.DFTCGEN := false.B //i.dft.icg_scan_en
  cdbdev.io.PWR_QREQN := true.B
  //output no connect
  // cdbdev.io.PWR_QACTIVE
  // cdbdev.io.PWR_QACCEPTN
  // cdbdev.io.PWR_QDENY
  cdbdev.io.RXSACTIVE_local := io.chi.txsactive
  io.chi.rxsactive := cdbdev.io.TXSACTIVE_local
  cdbdev.io.RXLINKACTIVEREQ := io.chi.tx.linkactivereq
  io.chi.tx.linkactiveack := cdbdev.io.RXLINKACTIVEACK
  io.chi.rx.linkactivereq := cdbdev.io.TXLINKACTIVEREQ
  cdbdev.io.TXLINKACTIVEACK := io.chi.rx.linkactiveack
  cdbdev.io.SYSCOREQ := io.chi.syscoreq
  io.chi.syscoack := cdbdev.io.SYSCOACK

  cdbdev.io.RXREQFLITPEND := io.chi.tx.req.flitpend
  cdbdev.io.RXREQFLITV := io.chi.tx.req.flitv
  cdbdev.io.RXREQFLIT := io.chi.tx.req.flit
  io.chi.tx.req.lcrdv := cdbdev.io.RXREQLCRDV

  cdbdev.io.RXRSPFLITPEND := io.chi.tx.rsp.flitpend
  cdbdev.io.RXRSPFLITV := io.chi.tx.rsp.flitv
  cdbdev.io.RXRSPFLIT := io.chi.tx.rsp.flit
  io.chi.tx.rsp.lcrdv := cdbdev.io.RXRSPLCRDV
  io.chi.rx.rsp.flitpend := cdbdev.io.TXRSPFLITPEND
  io.chi.rx.rsp.flitv := cdbdev.io.TXRSPFLITV
  io.chi.rx.rsp.flit := cdbdev.io.TXRSPFLIT
  cdbdev.io.TXRSPLCRDV := io.chi.rx.rsp.lcrdv

  io.chi.rx.snp.flitpend := cdbdev.io.TXSNPFLITPEND
  io.chi.rx.snp.flitv := cdbdev.io.TXSNPFLITV
  io.chi.rx.snp.flit := cdbdev.io.TXSNPFLIT
  cdbdev.io.TXSNPLCRDV := io.chi.rx.snp.lcrdv

  cdbdev.io.RXDATFLITPEND := io.chi.tx.dat.flitpend
  cdbdev.io.RXDATFLITV := io.chi.tx.dat.flitv
  cdbdev.io.RXDATFLIT := io.chi.tx.dat.flit
  io.chi.tx.dat.lcrdv := cdbdev.io.RXDATLCRDV
  io.chi.rx.dat.flitpend := cdbdev.io.TXDATFLITPEND
  io.chi.rx.dat.flitv := cdbdev.io.TXDATFLITV
  io.chi.rx.dat.flit := cdbdev.io.TXDATFLIT
  cdbdev.io.TXDATLCRDV := io.chi.rx.dat.lcrdv

  io.cdb.devtoicn_req_wptr_async := cdbdev.io.devtoicn_req_wptr_async
  io.cdb.devtoicn_req_fifo_data_mcp := cdbdev.io.devtoicn_req_fifo_data_mcp
  cdbdev.io.icntodev_req_rptr_async := io.cdb.icntodev_req_rptr_async
  io.cdb.devtoicn_rsp_wptr_async := cdbdev.io.devtoicn_rsp_wptr_async
  io.cdb.devtoicn_rsp_fifo_data_mcp := cdbdev.io.devtoicn_rsp_fifo_data_mcp
  cdbdev.io.icntodev_rsp_rptr_async := io.cdb.icntodev_rsp_rptr_async
  io.cdb.devtoicn_dat_wptr_async := cdbdev.io.devtoicn_dat_wptr_async
  io.cdb.devtoicn_dat_fifo_data_mcp := cdbdev.io.devtoicn_dat_fifo_data_mcp
  cdbdev.io.icntodev_dat_rptr_async := io.cdb.icntodev_dat_rptr_async
  cdbdev.io.icntodev_snp_wptr_async := io.cdb.icntodev_snp_wptr_async
  cdbdev.io.icntodev_snp_fifo_data_mcp := io.cdb.icntodev_snp_fifo_data_mcp
  io.cdb.devtoicn_snp_rptr_async := cdbdev.io.devtoicn_snp_rptr_async
  cdbdev.io.icntodev_rsp_wptr_async := io.cdb.icntodev_rsp_wptr_async
  cdbdev.io.icntodev_rsp_fifo_data_mcp := io.cdb.icntodev_rsp_fifo_data_mcp
  io.cdb.devtoicn_rsp_rptr_async := cdbdev.io.devtoicn_rsp_rptr_async
  cdbdev.io.icntodev_dat_wptr_async := io.cdb.icntodev_dat_wptr_async
  cdbdev.io.icntodev_dat_fifo_data_mcp := io.cdb.icntodev_dat_fifo_data_mcp
  io.cdb.devtoicn_dat_rptr_async := cdbdev.io.devtoicn_dat_rptr_async
  io.cdb.devtoicn_pwr_handshake_async := cdbdev.io.devtoicn_pwr_handshake_async
  cdbdev.io.icntodev_SACTIVE_async := io.cdb.icntodev_SACTIVE_async
  io.cdb.devtoicn_SACTIVE_async := cdbdev.io.devtoicn_SACTIVE_async
  cdbdev.io.icntodev_txfifo_qactive_async := io.cdb.icntodev_txfifo_qactive_async
  cdbdev.io.icntodev_rxfifo_qactive_async := io.cdb.icntodev_rxfifo_qactive_async
  io.cdb.devtoicn_txfifo_qactive_async := cdbdev.io.devtoicn_txfifo_qactive_async
  io.cdb.devtoicn_rxfifo_qactive_async := cdbdev.io.devtoicn_rxfifo_qactive_async
  io.cdb.devtoicn_pwr_qreqn_async := cdbdev.io.devtoicn_pwr_qreqn_async
  cdbdev.io.icntodev_pwr_qacceptn_async := io.cdb.icntodev_pwr_qacceptn_async
  cdbdev.io.icntodev_pwr_qdeny_async := io.cdb.icntodev_pwr_qdeny_async
  io.cdb.devtoicn_ptr_reset_req_async := cdbdev.io.devtoicn_ptr_reset_req_async
  cdbdev.io.icntodev_ptr_reset_ack_async := io.cdb.icntodev_ptr_reset_ack_async
  io.cdb.devtoicn_syscoreq_async := cdbdev.io.devtoicn_syscoreq_async
  cdbdev.io.icntodev_syscoack_async := io.cdb.icntodev_syscoack_async

}
