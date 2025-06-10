package device

import chisel3.{BlackBox, IO, _}
import chisel3.util.{HasBlackBoxResource, _}
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter
import coupledL2.tl2chi

//icn
class CHIAsyncIOCJ extends Bundle {
  val devtoicn_SACTIVE_async = Input(Bool())
  val devtoicn_dat_fifo_data_mcp = Input(UInt(3136.W)) //[3135:0]
  val devtoicn_dat_rptr_async = Input(UInt(8.W)) //[7   :0]
  val devtoicn_dat_wptr_async = Input(UInt(8.W)) //[7   :0]
  val devtoicn_ptr_reset_req_async = Input(Bool())
  val devtoicn_pwr_handshake_async = Input(Bool())
  val devtoicn_pwr_qreqn_async = Input(Bool())
  val devtoicn_req_fifo_data_mcp = Input(UInt(1016.W)) //[1015:0]
  val devtoicn_req_wptr_async = Input(UInt(8.W)) //[7   :0]
  val devtoicn_rsp_fifo_data_mcp = Input(UInt(440.W)) //[439 :0]
  val devtoicn_rsp_rptr_async = Input(UInt(8.W)) //[7   :0]
  val devtoicn_rsp_wptr_async = Input(UInt(8.W)) //[7   :0]
  val devtoicn_rxfifo_qactive_async = Input(Bool()) //
  val devtoicn_snp_rptr_async = Input(UInt(8.W)) //[7   :0]
  val devtoicn_syscoreq_async = Input(Bool())
  val devtoicn_txfifo_qactive_async = Input(Bool())
  val icntodev_SACTIVE_async = Output(Bool())
  val icntodev_dat_fifo_data_mcp = Output(UInt(3136.W)) //[3135:0]
  val icntodev_dat_rptr_async = Output(UInt(8.W)) //[7   :0]
  val icntodev_dat_wptr_async = Output(UInt(8.W)) //[7   :0]
  val icntodev_ptr_reset_ack_async = Output(Bool())
  val icntodev_pwr_qacceptn_async = Output(Bool())
  val icntodev_pwr_qdeny_async = Output(Bool())
  val icntodev_req_rptr_async = Output(UInt(8.W)) //[7   :0]
  val icntodev_rsp_fifo_data_mcp = Output(UInt(440.W)) //[439 :0]
  val icntodev_rsp_rptr_async = Output(UInt(8.W)) //[7   :0]
  val icntodev_rsp_wptr_async = Output(UInt(8.W)) //[7   :0]
  val icntodev_rxfifo_qactive_async = Output(Bool())
  val icntodev_snp_fifo_data_mcp = Output(UInt(704.W)) // [703 :0]
  val icntodev_snp_wptr_async = Output(UInt(8.W)) //[7   :0]
  val icntodev_syscoack_async = Output(Bool())
  val icntodev_txfifo_qactive_async = Output(Bool())
}

class LpIOCJ extends Bundle {
  val PWR_QREQN = Input(Bool())
  val PWR_QACCEPTN = Output(Bool())
  val PWR_QACTIVE = Output(Bool())
  val PWR_QDENY = Output(Bool())
}
class CHIAsyncICNCJ()(implicit p: Parameters) extends Module {
  val i = IO(new Bundle {
    val dft = new Bundle {
      val icg_scan_en = Input(Bool())
      val scan_enable = Input(Bool())
    }
  })
  val io = IO(new Bundle {
    val cdb = new CHIAsyncIOCJ
    val chi = new PortIO
  })
  val cdbicn = Module(new xh_cdb_icn)

  //input
  cdbicn.io.DFTCGEN := i.dft.icg_scan_en
  cdbicn.io.DFTRSTDISABLE := i.dft.scan_enable
  cdbicn.io.RESETN := (!reset.asBool).asAsyncReset
  cdbicn.io.clk := clock
  cdbicn.io.RXDATFLIT := io.chi.rx.dat.flit //i_biu_rxdatflit[391:0]),
  cdbicn.io.RXDATFLITPEND := io.chi.rx.dat.flitpend
  cdbicn.io.RXDATFLITV := io.chi.rx.dat.flitv
  cdbicn.io.RXLINKACTIVEREQ := io.chi.rx.linkactivereq
  cdbicn.io.RXRSPFLIT := io.chi.rx.rsp.flit //[54:0]
  cdbicn.io.RXRSPFLITPEND := io.chi.rx.rsp.flitpend
  cdbicn.io.RXRSPFLITV := io.chi.rx.rsp.flitv
  cdbicn.io.RXSACTIVE_local := io.chi.rxsactive
  cdbicn.io.RXSNPFLIT := io.chi.rx.snp.flit //[87:0]
  cdbicn.io.RXSNPFLITPEND := io.chi.rx.snp.flitpend
  cdbicn.io.RXSNPFLITV := io.chi.rx.snp.flitv
  cdbicn.io.SYSCOACK := io.chi.syscoack
  cdbicn.io.TXDATLCRDV := io.chi.tx.dat.lcrdv
  cdbicn.io.TXLINKACTIVEACK := io.chi.tx.linkactiveack
  cdbicn.io.TXREQLCRDV := io.chi.tx.req.lcrdv
  cdbicn.io.TXRSPLCRDV := io.chi.tx.rsp.lcrdv
  //output
  io.chi.rx.dat.lcrdv := cdbicn.io.RXDATLCRDV
  io.chi.rx.linkactiveack := cdbicn.io.RXLINKACTIVEACK
  io.chi.rx.rsp.lcrdv := cdbicn.io.RXRSPLCRDV
  io.chi.rx.snp.lcrdv := cdbicn.io.RXSNPLCRDV
  io.chi.syscoreq := cdbicn.io.SYSCOREQ
  io.chi.tx.dat.flit := cdbicn.io.TXDATFLIT //[391:0]
  io.chi.tx.dat.flitpend := cdbicn.io.TXDATFLITPEND
  io.chi.tx.dat.flitv := cdbicn.io.TXDATFLITV
  io.chi.tx.linkactivereq := cdbicn.io.TXLINKACTIVEREQ
  io.chi.tx.req.flit := cdbicn.io.TXREQFLIT //[126:0]
  io.chi.tx.req.flitpend := cdbicn.io.TXREQFLITPEND
  io.chi.tx.req.flitv := cdbicn.io.TXREQFLITV
  io.chi.tx.rsp.flit := cdbicn.io.TXRSPFLIT //[54:0]
  io.chi.tx.rsp.flitpend := cdbicn.io.TXRSPFLITPEND
  io.chi.tx.rsp.flitv := cdbicn.io.TXRSPFLITV
  io.chi.txsactive := cdbicn.io.TXSACTIVE_local
  //cdb connect
  cdbicn.io.devtoicn_SACTIVE_async := io.cdb.devtoicn_SACTIVE_async
  cdbicn.io.devtoicn_dat_fifo_data_mcp := io.cdb.devtoicn_dat_fifo_data_mcp
  cdbicn.io.devtoicn_dat_rptr_async := io.cdb.devtoicn_dat_rptr_async
  cdbicn.io.devtoicn_dat_wptr_async := io.cdb.devtoicn_dat_wptr_async
  cdbicn.io.devtoicn_ptr_reset_req_async := io.cdb.devtoicn_ptr_reset_req_async
  cdbicn.io.devtoicn_pwr_handshake_async := io.cdb.devtoicn_pwr_handshake_async
  cdbicn.io.devtoicn_pwr_qreqn_async := io.cdb.devtoicn_pwr_qreqn_async
  cdbicn.io.devtoicn_req_fifo_data_mcp := io.cdb.devtoicn_req_fifo_data_mcp
  cdbicn.io.devtoicn_req_wptr_async := io.cdb.devtoicn_req_wptr_async
  cdbicn.io.devtoicn_rsp_fifo_data_mcp := io.cdb.devtoicn_rsp_fifo_data_mcp
  cdbicn.io.devtoicn_rsp_rptr_async := io.cdb.devtoicn_rsp_rptr_async
  cdbicn.io.devtoicn_rsp_wptr_async := io.cdb.devtoicn_rsp_wptr_async
  cdbicn.io.devtoicn_rxfifo_qactive_async := io.cdb.devtoicn_rxfifo_qactive_async
  cdbicn.io.devtoicn_snp_rptr_async := io.cdb.devtoicn_snp_rptr_async
  cdbicn.io.devtoicn_syscoreq_async := io.cdb.devtoicn_syscoreq_async
  cdbicn.io.devtoicn_txfifo_qactive_async := io.cdb.devtoicn_txfifo_qactive_async

  io.cdb.icntodev_SACTIVE_async := cdbicn.io.icntodev_SACTIVE_async
  io.cdb.icntodev_dat_fifo_data_mcp := cdbicn.io.icntodev_dat_fifo_data_mcp
  io.cdb.icntodev_dat_rptr_async := cdbicn.io.icntodev_dat_rptr_async
  io.cdb.icntodev_dat_wptr_async := cdbicn.io.icntodev_dat_wptr_async
  io.cdb.icntodev_ptr_reset_ack_async := cdbicn.io.icntodev_ptr_reset_ack_async
  io.cdb.icntodev_pwr_qacceptn_async := cdbicn.io.icntodev_pwr_qacceptn_async
  io.cdb.icntodev_pwr_qdeny_async := cdbicn.io.icntodev_pwr_qdeny_async
  io.cdb.icntodev_req_rptr_async := cdbicn.io.icntodev_req_rptr_async
  io.cdb.icntodev_rsp_fifo_data_mcp := cdbicn.io.icntodev_rsp_fifo_data_mcp
  io.cdb.icntodev_rsp_rptr_async := cdbicn.io.icntodev_rsp_rptr_async
  io.cdb.icntodev_rsp_wptr_async := cdbicn.io.icntodev_rsp_wptr_async
  io.cdb.icntodev_rxfifo_qactive_async := cdbicn.io.icntodev_rxfifo_qactive_async
  io.cdb.icntodev_snp_fifo_data_mcp := cdbicn.io.icntodev_snp_fifo_data_mcp
  io.cdb.icntodev_snp_wptr_async := cdbicn.io.icntodev_snp_wptr_async
  io.cdb.icntodev_syscoack_async := cdbicn.io.icntodev_syscoack_async
  io.cdb.icntodev_txfifo_qactive_async := cdbicn.io.icntodev_txfifo_qactive_async
}

class xh_cdb_icn extends BlackBox {
  //chi cdb async interface
  val io = IO(new Bundle {
    val devtoicn_SACTIVE_async = Input(Bool())
    val devtoicn_dat_fifo_data_mcp = Input(UInt(3136.W)) //[3135:0]
    val devtoicn_dat_rptr_async = Input(UInt(8.W)) //[7   :0]
    val devtoicn_dat_wptr_async = Input(UInt(8.W)) //[7   :0]
    val devtoicn_ptr_reset_req_async = Input(Bool())
    val devtoicn_pwr_handshake_async = Input(Bool())
    val devtoicn_pwr_qreqn_async = Input(Bool())
    val devtoicn_req_fifo_data_mcp = Input(UInt(1016.W)) //[1015:0]
    val devtoicn_req_wptr_async = Input(UInt(8.W)) //[7   :0]
    val devtoicn_rsp_fifo_data_mcp = Input(UInt(440.W)) //[439 :0]
    val devtoicn_rsp_rptr_async = Input(UInt(8.W)) //[7   :0]
    val devtoicn_rsp_wptr_async = Input(UInt(8.W)) //[7   :0]
    val devtoicn_rxfifo_qactive_async = Input(Bool()) //
    val devtoicn_snp_rptr_async = Input(UInt(8.W)) //[7   :0]
    val devtoicn_syscoreq_async = Input(Bool())
    val devtoicn_txfifo_qactive_async = Input(Bool())
    val icntodev_SACTIVE_async = Output(Bool())
    val icntodev_dat_fifo_data_mcp = Output(UInt(3136.W)) //[3135:0]
    val icntodev_dat_rptr_async = Output(UInt(8.W)) //[7   :0]
    val icntodev_dat_wptr_async = Output(UInt(8.W)) //[7   :0]
    val icntodev_ptr_reset_ack_async = Output(Bool())
    val icntodev_pwr_qacceptn_async = Output(Bool())
    val icntodev_pwr_qdeny_async = Output(Bool())
    val icntodev_req_rptr_async = Output(UInt(8.W)) //[7   :0]
    val icntodev_rsp_fifo_data_mcp = Output(UInt(440.W)) //[439 :0]
    val icntodev_rsp_rptr_async = Output(UInt(8.W)) //[7   :0]
    val icntodev_rsp_wptr_async = Output(UInt(8.W)) //[7   :0]
    val icntodev_rxfifo_qactive_async = Output(Bool())
    val icntodev_snp_fifo_data_mcp = Output(UInt(704.W)) // [703 :0]
    val icntodev_snp_wptr_async = Output(UInt(8.W)) //[7   :0]
    val icntodev_syscoack_async = Output(Bool())
    val icntodev_txfifo_qactive_async = Output(Bool())
    //clock reset
    val clk = Input(Clock())
    val RESETN = Input(Reset())
    // dft
    val DFTCGEN = Input(Bool()) //i_dft_icg_scan_en
    val DFTRSTDISABLE = Input(Bool()) //i_dft_scan_enable
    //chi interface
    val RXDATFLIT = Input(UInt(392.W)) //    [391 :0] rxdatflit
    val RXDATFLITPEND = Input(Bool()) //rxdatflitpend
    val RXDATFLITV = Input(Bool()) //rxdatflitv
    val RXLINKACTIVEREQ = Input(Bool()) //rxlinkactivereq
    val RXRSPFLIT = Input(UInt(55.W)) //   [54  :0]
    val RXRSPFLITPEND = Input(Bool()) //i_biu_rxrspflitpend
    val RXRSPFLITV = Input(Bool())
    val RXSACTIVE_local = Input(Bool())
    val RXSNPFLIT = Input(UInt(88.W)) //    [87  :0]
    val RXSNPFLITPEND = Input(Bool())
    val RXSNPFLITV = Input(Bool())
    val SYSCOACK = Input(Bool())
    val TXDATLCRDV = Input(Bool())
    val TXLINKACTIVEACK = Input(Bool())
    val TXREQLCRDV = Input(Bool())
    val TXRSPLCRDV = Input(Bool())
    val RXDATLCRDV = Output(Bool()) //rxdatlcrdv
    val RXLINKACTIVEACK = Output(Bool()) //rxlinkactiveack
    val RXRSPLCRDV = Output(Bool())
    val RXSNPLCRDV = Output(Bool())
    val SYSCOREQ = Output(Bool())
    val TXDATFLIT = Output(UInt(392.W)) //  [391 :0]
    val TXDATFLITPEND = Output(Bool())
    val TXDATFLITV = Output(Bool())
    val TXLINKACTIVEREQ = Output(Bool())
    val TXREQFLIT = Output(UInt(127.W)) //  [126 :0]
    val TXREQFLITPEND = Output(Bool())
    val TXREQFLITV = Output(Bool())
    val TXRSPFLIT = Output(UInt(55.W)) //  [54  :0]
    val TXRSPFLITPEND = Output(Bool())
    val TXRSPFLITV = Output(Bool())
    val TXSACTIVE_local = Output(Bool())
    val CLK_QACTIVE = Output(Bool()) //unused
  })
}

class CHIAsyncDEVCJ()(implicit p: Parameters) extends Module {
  val i = IO(new Bundle {
    val dft = new Bundle {
      val icg_scan_en = Input(Bool())
      val scan_enable = Input(Bool())
    }
  })
  val io = IO(new Bundle {
    val cdb = Flipped(new CHIAsyncIOCJ)
    val chi = Flipped(new PortIO)
    val cpulp = new LpIOCJ
  }
  )
  //---instance cdb bridge ---
  val cdbdev = Module(new xh_cdb_dev)
  //===dft connect====
  cdbdev.io.DFTCGEN := i.dft.icg_scan_en
  cdbdev.io.DFTRSTDISABLE := i.dft.scan_enable
  //lp interface with cpu
  cdbdev.io.PWR_QREQN := io.cpulp.PWR_QREQN
  io.cpulp.PWR_QACCEPTN := cdbdev.io.PWR_QACCEPTN
  io.cpulp.PWR_QACTIVE := cdbdev.io.PWR_QACTIVE
  io.cpulp.PWR_QDENY := cdbdev.io.PWR_QDENY
  //===clock reset connect =====
  cdbdev.io.RESETN := (!reset.asBool).asAsyncReset
  cdbdev.io.clk := clock
  //=== chi connect===
  cdbdev.io.RXDATFLIT := io.chi.tx.dat.flit
  cdbdev.io.RXDATFLITPEND := io.chi.tx.dat.flitpend
  cdbdev.io.RXDATFLITV := io.chi.tx.dat.flitv
  cdbdev.io.RXLINKACTIVEREQ := io.chi.tx.linkactivereq
  cdbdev.io.RXREQFLIT := io.chi.tx.req.flit
  cdbdev.io.RXREQFLITPEND := io.chi.tx.req.flitpend
  cdbdev.io.RXREQFLITV := io.chi.tx.req.flitv
  cdbdev.io.RXRSPFLIT := io.chi.tx.rsp.flit
  cdbdev.io.RXRSPFLITPEND := io.chi.tx.rsp.flitpend
  cdbdev.io.RXRSPFLITV := io.chi.tx.rsp.flitv
  cdbdev.io.RXSACTIVE_local := io.chi.txsactive
  cdbdev.io.SYSCOREQ := io.chi.syscoreq
  cdbdev.io.TXDATLCRDV := io.chi.rx.dat.lcrdv
  cdbdev.io.TXLINKACTIVEACK := io.chi.rx.linkactiveack
  cdbdev.io.TXRSPLCRDV := io.chi.rx.rsp.lcrdv
  cdbdev.io.TXSNPLCRDV := io.chi.rx.snp.lcrdv
  //output
  io.chi.tx.dat.lcrdv := cdbdev.io.RXDATLCRDV
  io.chi.tx.linkactiveack := cdbdev.io.RXLINKACTIVEACK
  io.chi.tx.req.lcrdv := cdbdev.io.RXREQLCRDV
  io.chi.tx.rsp.lcrdv := cdbdev.io.RXRSPLCRDV
  io.chi.syscoack := cdbdev.io.SYSCOACK
  io.chi.rx.dat.flit := cdbdev.io.TXDATFLIT
  io.chi.rx.dat.flitpend := cdbdev.io.TXDATFLITPEND
  io.chi.rx.dat.flitv := cdbdev.io.TXDATFLITV
  io.chi.rx.linkactivereq := cdbdev.io.TXLINKACTIVEREQ
  io.chi.rx.rsp.flit := cdbdev.io.TXRSPFLIT
  io.chi.rx.rsp.flitpend := cdbdev.io.TXRSPFLITPEND
  io.chi.rx.rsp.flitv := cdbdev.io.TXRSPFLITV
  io.chi.rxsactive := cdbdev.io.TXSACTIVE_local
  io.chi.rx.snp.flit := cdbdev.io.TXSNPFLIT
  io.chi.rx.snp.flitpend := cdbdev.io.TXSNPFLITPEND
  io.chi.rx.snp.flitv := cdbdev.io.TXSNPFLITV
  //cdb connect
  io.cdb.devtoicn_SACTIVE_async := cdbdev.io.devtoicn_SACTIVE_async
  io.cdb.devtoicn_dat_fifo_data_mcp := cdbdev.io.devtoicn_dat_fifo_data_mcp
  io.cdb.devtoicn_dat_rptr_async := cdbdev.io.devtoicn_dat_rptr_async
  io.cdb.devtoicn_dat_wptr_async := cdbdev.io.devtoicn_dat_wptr_async
  io.cdb.devtoicn_ptr_reset_req_async := cdbdev.io.devtoicn_ptr_reset_req_async
  io.cdb.devtoicn_pwr_handshake_async := cdbdev.io.devtoicn_pwr_handshake_async
  io.cdb.devtoicn_pwr_qreqn_async := cdbdev.io.devtoicn_pwr_qreqn_async
  io.cdb.devtoicn_req_fifo_data_mcp := cdbdev.io.devtoicn_req_fifo_data_mcp
  io.cdb.devtoicn_req_wptr_async := cdbdev.io.devtoicn_req_wptr_async
  io.cdb.devtoicn_rsp_fifo_data_mcp := cdbdev.io.devtoicn_rsp_fifo_data_mcp
  io.cdb.devtoicn_rsp_rptr_async := cdbdev.io.devtoicn_rsp_rptr_async
  io.cdb.devtoicn_rsp_wptr_async := cdbdev.io.devtoicn_rsp_wptr_async
  io.cdb.devtoicn_rxfifo_qactive_async := cdbdev.io.devtoicn_rxfifo_qactive_async
  io.cdb.devtoicn_snp_rptr_async := cdbdev.io.devtoicn_snp_rptr_async
  io.cdb.devtoicn_syscoreq_async := cdbdev.io.devtoicn_syscoreq_async
  io.cdb.devtoicn_txfifo_qactive_async := cdbdev.io.devtoicn_txfifo_qactive_async
    //input
  cdbdev.io.icntodev_SACTIVE_async := io.cdb.icntodev_SACTIVE_async
  cdbdev.io.icntodev_dat_fifo_data_mcp := io.cdb.icntodev_dat_fifo_data_mcp
  cdbdev.io.icntodev_dat_rptr_async := io.cdb.icntodev_dat_rptr_async
  cdbdev.io.icntodev_dat_wptr_async := io.cdb.icntodev_dat_wptr_async
  cdbdev.io.icntodev_ptr_reset_ack_async := io.cdb.icntodev_ptr_reset_ack_async
  cdbdev.io.icntodev_pwr_qacceptn_async := io.cdb.icntodev_pwr_qacceptn_async
  cdbdev.io.icntodev_pwr_qdeny_async := io.cdb.icntodev_pwr_qdeny_async
  cdbdev.io.icntodev_req_rptr_async := io.cdb.icntodev_req_rptr_async
  cdbdev.io.icntodev_rsp_fifo_data_mcp := io.cdb.icntodev_rsp_fifo_data_mcp
  cdbdev.io.icntodev_rsp_rptr_async := io.cdb.icntodev_rsp_rptr_async
  cdbdev.io.icntodev_rsp_wptr_async := io.cdb.icntodev_rsp_wptr_async
  cdbdev.io.icntodev_rxfifo_qactive_async := io.cdb.icntodev_rxfifo_qactive_async
  cdbdev.io.icntodev_snp_fifo_data_mcp := io.cdb.icntodev_snp_fifo_data_mcp
  cdbdev.io.icntodev_snp_wptr_async := io.cdb.icntodev_snp_wptr_async
  cdbdev.io.icntodev_syscoack_async := io.cdb.icntodev_syscoack_async
  cdbdev.io.icntodev_txfifo_qactive_async := io.cdb.icntodev_txfifo_qactive_async

}

class xh_cdb_dev extends BlackBox {
  val io = IO(new Bundle {
    val DFTCGEN = Input(Bool())
    val DFTRSTDISABLE = Input(Bool())
    val PWR_QREQN = Input(Bool())
    val RESETN = Input(Reset())
    val RXDATFLIT = Input(UInt(392.W))
    val RXDATFLITPEND = Input(Bool())
    val RXDATFLITV = Input(Bool())
    val RXLINKACTIVEREQ = Input(Bool())
    val RXREQFLIT = Input(UInt(127.W)) //   [126 :0]
    val RXREQFLITPEND = Input(Bool())
    val RXREQFLITV = Input(Bool())
    val RXRSPFLIT = Input(UInt(55.W))
    val RXRSPFLITPEND = Input(Bool())
    val RXRSPFLITV = Input(Bool())
    val RXSACTIVE_local = Input(Bool())
    val SYSCOREQ = Input(Bool())
    val TXDATLCRDV = Input(Bool())
    val TXLINKACTIVEACK = Input(Bool())
    val TXRSPLCRDV = Input(Bool())
    val TXSNPLCRDV = Input(Bool())
    val clk = Input(Clock())
    val icntodev_SACTIVE_async = Input(Bool())
    val icntodev_dat_fifo_data_mcp = Input(UInt(3136.W)) //   [3135:0]
    val icntodev_dat_rptr_async = Input(UInt(8.W)) //   [7   :0]
    val icntodev_dat_wptr_async = Input(UInt(8.W)) //   [7   :0]
    val icntodev_ptr_reset_ack_async = Input(Bool())
    val icntodev_pwr_qacceptn_async = Input(Bool())
    val icntodev_pwr_qdeny_async = Input(Bool())
    val icntodev_req_rptr_async = Input(UInt(8.W)) //   [7   :0]
    val icntodev_rsp_fifo_data_mcp = Input(UInt(440.W)) //   [439 :0]
    val icntodev_rsp_rptr_async = Input(UInt(8.W)) //   [7   :0]
    val icntodev_rsp_wptr_async = Input(UInt(8.W)) //   [7   :0]
    val icntodev_rxfifo_qactive_async = Input(Bool())
    val icntodev_snp_fifo_data_mcp = Input(UInt(704.W)) //   [703 :0]
    val icntodev_snp_wptr_async = Input(UInt(8.W)) //   [7   :0]
    val icntodev_syscoack_async = Input(Bool())
    val icntodev_txfifo_qactive_async = Input(Bool())
    val CLK_QACTIVE = Output(Bool())
    val PWR_QACCEPTN = Output(Bool())
    val PWR_QACTIVE = Output(Bool())
    val PWR_QDENY = Output(Bool())
    val RXDATLCRDV = Output(Bool())
    val RXLINKACTIVEACK = Output(Bool())
    val RXREQLCRDV = Output(Bool())
    val RXRSPLCRDV = Output(Bool())
    val SYSCOACK = Output(Bool())
    val TXDATFLIT = Output(UInt(392.W)) //  [391 :0]
    val TXDATFLITPEND = Output(Bool())
    val TXDATFLITV = Output(Bool())
    val TXLINKACTIVEREQ = Output(Bool())
    val TXRSPFLIT = Output(UInt(55.W)) //  [54  :0]
    val TXRSPFLITPEND = Output(Bool())
    val TXRSPFLITV = Output(Bool())
    val TXSACTIVE_local = Output(Bool())
    val TXSNPFLIT = Output(UInt(88.W)) //  [87  :0]
    val TXSNPFLITPEND = Output(Bool())
    val TXSNPFLITV = Output(Bool())
    val devtoicn_SACTIVE_async = Output(Bool())
    val devtoicn_dat_fifo_data_mcp = Output(UInt(3136.W)) //  [3135:0]
    val devtoicn_dat_rptr_async = Output(UInt(8.W)) //  [7   :0]
    val devtoicn_dat_wptr_async = Output(UInt(8.W)) //  [7   :0]
    val devtoicn_ptr_reset_req_async = Output(Bool())
    val devtoicn_pwr_handshake_async = Output(Bool())
    val devtoicn_pwr_qreqn_async = Output(Bool())
    val devtoicn_req_fifo_data_mcp = Output(UInt(1016.W)) //   [1015:0]
    val devtoicn_req_wptr_async = Output(UInt(8.W)) //   [7   :0]
    val devtoicn_rsp_fifo_data_mcp = Output(UInt(440.W)) //  [439 :0]
    val devtoicn_rsp_rptr_async = Output(UInt(8.W)) //  [7   :0]
    val devtoicn_rsp_wptr_async = Output(UInt(8.W)) // [7   :0]
    val devtoicn_rxfifo_qactive_async = Output(Bool())
    val devtoicn_snp_rptr_async = Output(UInt(8.W)) //  [7   :0]
    val devtoicn_syscoreq_async = Output(Bool())
    val devtoicn_txfifo_qactive_async = Output(Bool())
  })
}
  


