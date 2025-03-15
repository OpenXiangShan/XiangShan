package device

import chisel3.{BlackBox, IO, _}
import chisel3.util.{HasBlackBoxResource, _}
import coupledL2.tl2chi.PortIO
import freechips.rocketchip.diplomacy.LazyModule
import org.chipsalliance.cde.config.Parameters
import system.HasSoCParameter
import coupledL2.tl2chi

//icn
class CHIAsyncIOSPMT extends Bundle {
  val WPECITMSOIA = Input(Bool())
  val WOCIATISPEM = Input(UInt(3136.W)) //[3135:0]
  val WCSIIAPOMTE = Input(UInt(8.W)) //[7   :0]
  val WITMSEPOCAI = Input(UInt(8.W)) //[7   :0]
  val WAETIPSCOMI = Input(Bool())
  val WPIATSOIMEC = Input(Bool())
  val WEISCOPIAMT = Input(Bool())
  val WCOTIAPEMIS = Input(UInt(1016.W)) //[1015:0]
  val WASOPICIEMT = Input(UInt(8.W)) //[7   :0]
  val WMCOIPISEAT = Input(UInt(440.W)) //[439 :0]
  val WOIECASIMPT = Input(UInt(8.W)) //[7   :0]
  val WTMIIOCPEAS = Input(UInt(8.W)) //[7   :0]
  val WTEICPMIASO = Input(Bool()) //
  val WMACIIEPOTS = Input(UInt(8.W)) //[7   :0]
  val WIIPETAOCMS = Input(Bool())
  val WTOMIAPSICE = Input(Bool())
  val WAPIIEMCTSO = Output(Bool())
  val WOPETSIACIM = Output(UInt(3136.W)) //[3135:0]
  val WIMEITCSAOP = Output(UInt(8.W)) //[7   :0]
  val WECIOTIMSAP = Output(UInt(8.W)) //[7   :0]
  val WITAISPMCEO = Output(Bool())
  val WTMSCEOIAIP = Output(Bool())
  val WISPOCATIEM = Output(Bool())
  val WTCESPIOIAM = Output(UInt(8.W)) //[7   :0]
  val WIOSTAIPCME = Output(UInt(440.W)) //[439 :0]
  val WCIMISAEOTP = Output(UInt(8.W)) //[7   :0]
  val WPMISECOAIT = Output(UInt(8.W)) //[7   :0]
  val WASEMCPIOIT = Output(Bool())
  val WECMATPSOII = Output(UInt(704.W)) // [703 :0]
  val WTCAISPOMIE = Output(UInt(8.W)) //[7   :0]
  val WOSMETIAICP = Output(Bool())
  val WCTMIPOEIAS = Output(Bool())
}

class CHIAsyncICNSPMT()(implicit p: Parameters) extends Module {

//  val clock = IO(Input(Clock()))
//  val reset = IO(Input(Reset()))
  val i = IO(new Bundle {
    val dft = new Bundle {
      val icg_scan_en = Input(Bool())
      val scan_enable = Input(Bool())
    }
  })
  val io = IO(new Bundle {
    val cdb = new CHIAsyncIOSPMT
    val chi = new PortIO
  })
  val cdbicn = Module(new xh_cdb_icn)

  //input
  cdbicn.WMOTSECIPIA := i.dft.icg_scan_en
  cdbicn.WOMSCAIEIPT := i.dft.scan_enable
  cdbicn.WTIIAPSOECM := reset
  cdbicn.clk := clock
  cdbicn.WAMIIOETSPC := io.chi.rx.dat.flit //i_biu_rxdatflit[391:0]),
  cdbicn.WEIAIOTMPCS := io.chi.rx.dat.flitpend
  cdbicn.WMPTOSIEIAC := io.chi.rx.dat.flitv
  cdbicn.WICSTAOIEPM := io.chi.rx.linkactivereq
  cdbicn.WPOIETACMIS := io.chi.rx.rsp.flit //[54:0]
  cdbicn.WMOSIPIACET := io.chi.rx.rsp.flitpend
  cdbicn.WTCAOPSMIIE := io.chi.rx.rsp.flitv
  cdbicn.WIOAPMESICT := io.chi.rxsactive
  cdbicn.WEIPSOMCTIA := io.chi.rx.snp.flit //[87:0]
  cdbicn.WPEIOITMSAC := io.chi.rx.snp.flitpend
  cdbicn.WIPCSEAMOIT := io.chi.rx.snp.flitv
  cdbicn.WAEPISIOMCT := io.chi.syscoack
  cdbicn.WCMIEPTAISO := io.chi.tx.dat.lcrdv
  cdbicn.WOAISIMTCEP := io.chi.tx.linkactiveack
  cdbicn.WEMICTPSOAI := io.chi.tx.req.lcrdv
  cdbicn.WEMCSTIPIAO := io.chi.tx.rsp.lcrdv
  //output
  io.chi.rx.dat.lcrdv := cdbicn.WAPMTCISIEO
  io.chi.rx.linkactiveack := cdbicn.WTPIEMAISCO
  io.chi.rx.rsp.lcrdv := cdbicn.WOCIMISETPA
  io.chi.rx.snp.lcrdv := cdbicn.WOIMIATPSCE
  io.chi.syscoreq := cdbicn.WITSIPAEMOC
  io.chi.tx.dat.flit := cdbicn.WTISMPICAEO //[391:0]
  io.chi.tx.dat.flitpend := cdbicn.WSCMIETAIOP
  io.chi.tx.dat.flitv := cdbicn.WAOPIITCMES
  io.chi.tx.linkactivereq := cdbicn.WMIOIACTPES
  io.chi.tx.req.flit := cdbicn.WOMETPIISCA //[126:0]
  io.chi.tx.req.flitpend := cdbicn.WTSCEOAIPIM
  io.chi.tx.req.flitv := cdbicn.WCOTSEPIAIM
  io.chi.tx.rsp.flit := cdbicn.WETSACMIOIP //[54:0]
  io.chi.tx.rsp.flitpend := cdbicn.WIOAICSTEPM
  io.chi.tx.rsp.flitv := cdbicn.WPSCIMTAEOI
  io.chi.txsactive := cdbicn.WTAEIMSICOP
  //cdb connect
  cdbicn.WPECITMSOIA := io.cdb.WPECITMSOIA
  cdbicn.WOCIATISPEM := io.cdb.WOCIATISPEM
  cdbicn.WCSIIAPOMTE := io.cdb.WCSIIAPOMTE
  cdbicn.WITMSEPOCAI := io.cdb.WITMSEPOCAI
  cdbicn.WAETIPSCOMI := io.cdb.WAETIPSCOMI
  cdbicn.WPIATSOIMEC := io.cdb.WPIATSOIMEC
  cdbicn.WEISCOPIAMT := io.cdb.WEISCOPIAMT
  cdbicn.WCOTIAPEMIS := io.cdb.WCOTIAPEMIS
  cdbicn.WASOPICIEMT := io.cdb.WASOPICIEMT
  cdbicn.WMCOIPISEAT := io.cdb.WMCOIPISEAT
  cdbicn.WOIECASIMPT := io.cdb.WOIECASIMPT
  cdbicn.WTMIIOCPEAS := io.cdb.WTMIIOCPEAS
  cdbicn.WTEICPMIASO := io.cdb.WTEICPMIASO
  cdbicn.WMACIIEPOTS := io.cdb.WMACIIEPOTS
  cdbicn.WIIPETAOCMS := io.cdb.WIIPETAOCMS
  cdbicn.WTOMIAPSICE := io.cdb.WTOMIAPSICE

  io.cdb.WAPIIEMCTSO := cdbicn.WAPIIEMCTSO
  io.cdb.WOPETSIACIM := cdbicn.WOPETSIACIM
  io.cdb.WIMEITCSAOP := cdbicn.WIMEITCSAOP
  io.cdb.WECIOTIMSAP := cdbicn.WECIOTIMSAP
  io.cdb.WITAISPMCEO := cdbicn.WITAISPMCEO
  io.cdb.WTMSCEOIAIP := cdbicn.WTMSCEOIAIP
  io.cdb.WISPOCATIEM := cdbicn.WISPOCATIEM
  io.cdb.WTCESPIOIAM := cdbicn.WTCESPIOIAM
  io.cdb.WIOSTAIPCME := cdbicn.WIOSTAIPCME
  io.cdb.WCIMISAEOTP := cdbicn.WCIMISAEOTP
  io.cdb.WPMISECOAIT := cdbicn.WPMISECOAIT
  io.cdb.WASEMCPIOIT := cdbicn.WASEMCPIOIT
  io.cdb.WECMATPSOII := cdbicn.WECMATPSOII
  io.cdb.WTCAISPOMIE := cdbicn.WTCAISPOMIE
  io.cdb.WOSMETIAICP := cdbicn.WOSMETIAICP
  io.cdb.WCTMIPOEIAS := cdbicn.WCTMIPOEIAS
}

class xh_cdb_icn extends BlackBox with HasBlackBoxResource {
  //chi cdb async interface
  val WPECITMSOIA = Input(Bool())
  val WOCIATISPEM = Input(UInt(3136.W)) //[3135:0]
  val WCSIIAPOMTE = Input(UInt(8.W)) //[7   :0]
  val WITMSEPOCAI = Input(UInt(8.W)) //[7   :0]
  val WAETIPSCOMI = Input(Bool())
  val WPIATSOIMEC = Input(Bool())
  val WEISCOPIAMT = Input(Bool())
  val WCOTIAPEMIS = Input(UInt(1016.W)) //[1015:0]
  val WASOPICIEMT = Input(UInt(8.W)) //[7   :0]
  val WMCOIPISEAT = Input(UInt(440.W)) //[439 :0]
  val WOIECASIMPT = Input(UInt(8.W)) //[7   :0]
  val WTMIIOCPEAS = Input(UInt(8.W)) //[7   :0]
  val WTEICPMIASO = Input(Bool()) //
  val WMACIIEPOTS = Input(UInt(8.W)) //[7   :0]
  val WIIPETAOCMS = Input(Bool())
  val WTOMIAPSICE = Input(Bool())
  val WAPIIEMCTSO = Output(Bool())
  val WOPETSIACIM = Output(UInt(3136.W)) //[3135:0]
  val WIMEITCSAOP = Output(UInt(8.W)) //[7   :0]
  val WECIOTIMSAP = Output(UInt(8.W)) //[7   :0]
  val WITAISPMCEO = Output(Bool())
  val WTMSCEOIAIP = Output(Bool())
  val WISPOCATIEM = Output(Bool())
  val WTCESPIOIAM = Output(UInt(8.W)) //[7   :0]
  val WIOSTAIPCME = Output(UInt(440.W)) //[439 :0]
  val WCIMISAEOTP = Output(UInt(8.W)) //[7   :0]
  val WPMISECOAIT = Output(UInt(8.W)) //[7   :0]
  val WASEMCPIOIT = Output(Bool())
  val WECMATPSOII = Output(UInt(704.W)) // [703 :0]
  val WTCAISPOMIE = Output(UInt(8.W)) //[7   :0]
  val WOSMETIAICP = Output(Bool())
  val WCTMIPOEIAS = Output(Bool())
  //clock reset
  val clk = Input(Clock())
  val WTIIAPSOECM = Input(Reset())
  // dft
  val WMOTSECIPIA = Input(Bool()) //i_dft_icg_scan_en
  val WOMSCAIEIPT = Input(Bool()) //i_dft_scan_enable
  //chi interface
  val WAMIIOETSPC = Input(UInt(392.W)) //    [391 :0] rxdatflit
  val WEIAIOTMPCS = Input(Bool()) //rxdatflitpend
  val WMPTOSIEIAC = Input(Bool()) //rxdatflitv
  val WAPMTCISIEO = Output(Bool()) //rxdatlcrdv
  val WTPIEMAISCO = Output(Bool()) //rxlinkactiveack
  val WICSTAOIEPM = Input(Bool()) //rxlinkactivereq
  val WPOIETACMIS = Input(UInt(55.W)) //   [54  :0]
  val WMOSIPIACET = Input(Bool()) //i_biu_rxrspflitpend
  val WTCAOPSMIIE = Input(Bool())
  val WIOAPMESICT = Input(Bool())
  val WEIPSOMCTIA = Input(UInt(88.W)) //    [87  :0]
  val WPEIOITMSAC = Input(Bool())
  val WIPCSEAMOIT = Input(Bool())
  val WAEPISIOMCT = Input(Bool())
  val WCMIEPTAISO = Input(Bool())
  val WOAISIMTCEP = Input(Bool())
  val WEMICTPSOAI = Input(Bool())
  val WEMCSTIPIAO = Input(Bool())
  val WPCASIOIMTE = Output(Bool()) //unused
  val WOCIMISETPA = Output(Bool())
  val WOIMIATPSCE = Output(Bool())
  val WITSIPAEMOC = Output(Bool())
  val WTISMPICAEO = Output(UInt(392.W)) //  [391 :0]
  val WSCMIETAIOP = Output(Bool())
  val WAOPIITCMES = Output(Bool())
  val WMIOIACTPES = Output(Bool())
  val WOMETPIISCA = Output(UInt(127.W)) //  [126 :0]
  val WTSCEOAIPIM = Output(Bool())
  val WCOTSEPIAIM = Output(Bool())
  val WETSACMIOIP = Output(UInt(55.W)) //  [54  :0]
  val WIOAICSTEPM = Output(Bool())
  val WPSCIMTAEOI = Output(Bool())
  val WTAEIMSICOP = Output(Bool())
  addResource("/aia/src/rtl/imsic/xh_cdb_icn.v")
}

class CHIAsyncDEVSPMT()(implicit p: Parameters) extends Module {
  val i = IO(new Bundle {
    val dft = new Bundle {
      val icg_scan_en = Input(Bool())
      val scan_enable = Input(Bool())
    }
  })
  val io = IO(new Bundle {
    val cdb = Flipped(new CHIAsyncIOSPMT)
    val chi = Flipped(new PortIO)
  })
  //---instance cdb bridge ---
  val cdbdev = Module(new xh_cdb_dev)
  //===dft connect====
  cdbdev.WMOTSECIPIA := i.dft.icg_scan_en
  cdbdev.WOMSCAIEIPT := i.dft.scan_enable
  cdbdev.WSMOACPETII := true.B
  //===clock reset connect =====
  cdbdev.WTIIAPSOECM := reset
  cdbdev.clk := clock
  //=== chi connect===
  cdbdev.WAMIIOETSPC := io.chi.tx.dat.flit
  cdbdev.WEIAIOTMPCS := io.chi.tx.dat.flitpend
  cdbdev.WMPTOSIEIAC := io.chi.tx.dat.flitv
  cdbdev.WICSTAOIEPM := io.chi.tx.linkactivereq
  cdbdev.WMIOPATEISC := io.chi.tx.req.flit
  cdbdev.WCPOASITMEI := io.chi.tx.req.flitpend
  cdbdev.WEIACTIOSMP := io.chi.tx.req.flitv
  cdbdev.WPOIETACMIS := io.chi.tx.rsp.flit
  cdbdev.WMOSIPIACET := io.chi.tx.rsp.flitpend
  cdbdev.WTCAOPSMIIE := io.chi.tx.rsp.flitv
  cdbdev.WIOAPMESICT := io.chi.txsactive
  cdbdev.WITSIPAEMOC := io.chi.syscoreq
  cdbdev.WCMIEPTAISO := io.chi.rx.dat.lcrdv
  cdbdev.WOAISIMTCEP := io.chi.rx.linkactiveack
  cdbdev.WEMCSTIPIAO := io.chi.rx.rsp.lcrdv
  cdbdev.WCTPOSEAMII := io.chi.rx.snp.lcrdv
  //output
  io.chi.tx.dat.lcrdv := cdbdev.WAPMTCISIEO
  io.chi.tx.linkactiveack := cdbdev.WTPIEMAISCO
  io.chi.tx.req.lcrdv := cdbdev.WPMICATIOSE
  io.chi.tx.rsp.lcrdv := cdbdev.WOCIMISETPA
  io.chi.syscoack := cdbdev.WAEPISIOMCT
  io.chi.rx.dat.flit := cdbdev.WTISMPICAEO
  io.chi.rx.dat.flitpend := cdbdev.WSCMIETAIOP
  io.chi.rx.dat.flitv := cdbdev.WAOPIITCMES
  io.chi.rx.linkactivereq := cdbdev.WMIOIACTPES
  io.chi.rx.rsp.flit := cdbdev.WETSACMIOIP
  io.chi.rx.rsp.flitpend := cdbdev.WIOAICSTEPM
  io.chi.rx.rsp.flitv := cdbdev.WPSCIMTAEOI
  io.chi.rxsactive := cdbdev.WTAEIMSICOP
  io.chi.rx.snp.flit := cdbdev.WIACPSIEMOT
  io.chi.rx.snp.flitpend := cdbdev.WIIAETPMSCO
  io.chi.rx.snp.flitv := cdbdev.WMPIAETOCSI
  //cdb connect
  io.cdb.WPECITMSOIA := cdbdev.WPECITMSOIA
  io.cdb.WOCIATISPEM := cdbdev.WOCIATISPEM
  io.cdb.WCSIIAPOMTE := cdbdev.WCSIIAPOMTE
  io.cdb.WITMSEPOCAI := cdbdev.WITMSEPOCAI
  io.cdb.WAETIPSCOMI := cdbdev.WAETIPSCOMI
  io.cdb.WPIATSOIMEC := cdbdev.WPIATSOIMEC
  io.cdb.WEISCOPIAMT := cdbdev.WEISCOPIAMT
  io.cdb.WCOTIAPEMIS := cdbdev.WCOTIAPEMIS
  io.cdb.WASOPICIEMT := cdbdev.WASOPICIEMT
  io.cdb.WMCOIPISEAT := cdbdev.WMCOIPISEAT
  io.cdb.WOIECASIMPT := cdbdev.WOIECASIMPT
  io.cdb.WTMIIOCPEAS := cdbdev.WTMIIOCPEAS
  io.cdb.WTEICPMIASO := cdbdev.WTEICPMIASO
  io.cdb.WMACIIEPOTS := cdbdev.WMACIIEPOTS
  io.cdb.WIIPETAOCMS := cdbdev.WIIPETAOCMS
  io.cdb.WTOMIAPSICE := cdbdev.WTOMIAPSICE
  //input
  cdbdev.WAPIIEMCTSO := io.cdb.WAPIIEMCTSO
  cdbdev.WOPETSIACIM := io.cdb.WOPETSIACIM
  cdbdev.WIMEITCSAOP := io.cdb.WIMEITCSAOP
  cdbdev.WECIOTIMSAP := io.cdb.WECIOTIMSAP
  cdbdev.WITAISPMCEO := io.cdb.WITAISPMCEO
  cdbdev.WTMSCEOIAIP := io.cdb.WTMSCEOIAIP
  cdbdev.WISPOCATIEM := io.cdb.WISPOCATIEM
  cdbdev.WTCESPIOIAM := io.cdb.WTCESPIOIAM
  cdbdev.WIOSTAIPCME := io.cdb.WIOSTAIPCME
  cdbdev.WCIMISAEOTP := io.cdb.WCIMISAEOTP
  cdbdev.WPMISECOAIT := io.cdb.WPMISECOAIT
  cdbdev.WASEMCPIOIT := io.cdb.WASEMCPIOIT
  cdbdev.WECMATPSOII := io.cdb.WECMATPSOII
  cdbdev.WTCAISPOMIE := io.cdb.WTCAISPOMIE
  cdbdev.WOSMETIAICP := io.cdb.WOSMETIAICP
  cdbdev.WCTMIPOEIAS := io.cdb.WCTMIPOEIAS
}

class xh_cdb_dev extends BlackBox with HasBlackBoxResource {
  val WMOTSECIPIA = IO(Input(Bool()))
  val WOMSCAIEIPT = IO(Input(Bool()))
  val WSMOACPETII = IO(Input(Bool()))
  val WTIIAPSOECM = IO(Input(Reset()))
  val WAMIIOETSPC = IO(Input(UInt(392.W)))
  val WEIAIOTMPCS = IO(Input(Bool()))
  val WMPTOSIEIAC = IO(Input(Bool()))
  val WICSTAOIEPM = IO(Input(Bool()))
  val WMIOPATEISC = IO(Input(UInt(127.W))) //   [126 :0]
  val WCPOASITMEI = IO(Input(Bool()))
  val WEIACTIOSMP = IO(Input(Bool()))
  val WPOIETACMIS = IO(Input(UInt(55.W)))
  val WMOSIPIACET = IO(Input(Bool()))
  val WTCAOPSMIIE = IO(Input(Bool()))
  val WIOAPMESICT = IO(Input(Bool()))
  val WITSIPAEMOC = IO(Input(Bool()))
  val WCMIEPTAISO = IO(Input(Bool()))
  val WOAISIMTCEP = IO(Input(Bool()))
  val WEMCSTIPIAO = IO(Input(Bool()))
  val WCTPOSEAMII = IO(Input(Bool()))
  val clk = IO(Input(Clock()))
  val WAPIIEMCTSO = IO(Input(Bool()))
  val WOPETSIACIM = IO(Input(UInt(3136.W))) //   [3135:0]
  val WIMEITCSAOP = IO(Input(UInt(8.W))) //   [7   :0]
  val WECIOTIMSAP = IO(Input(UInt(8.W))) //   [7   :0]
  val WITAISPMCEO = IO(Input(Bool()))
  val WTMSCEOIAIP = IO(Input(Bool()))
  val WISPOCATIEM = IO(Input(Bool()))
  val WTCESPIOIAM = IO(Input(UInt(8.W))) //   [7   :0]
  val WIOSTAIPCME = IO(Input(UInt(440.W))) //   [439 :0]
  val WCIMISAEOTP = IO(Input(UInt(8.W))) //   [7   :0]
  val WPMISECOAIT = IO(Input(UInt(8.W))) //   [7   :0]
  val WASEMCPIOIT = IO(Input(Bool()))
  val WECMATPSOII = IO(Input(UInt(704.W))) //   [703 :0]
  val WTCAISPOMIE = IO(Input(UInt(8.W))) //   [7   :0]
  val WOSMETIAICP = IO(Input(Bool()))
  val WCTMIPOEIAS = IO(Input(Bool()))
  val WPCASIOIMTE = IO(Output(Bool()))
  val WIMACTSPIOE = IO(Output(Bool()))
  val WSIIPMOTCAE = IO(Output(Bool()))
  val WMTIICSPAOE = IO(Output(Bool()))
  val WAPMTCISIEO = IO(Output(Bool()))
  val WTPIEMAISCO = IO(Output(Bool()))
  val WPMICATIOSE = IO(Output(Bool()))
  val WOCIMISETPA = IO(Output(Bool()))
  val WAEPISIOMCT = IO(Output(Bool()))
  val WTISMPICAEO = IO(Output(UInt(392.W))) //  [391 :0]
  val WSCMIETAIOP = IO(Output(Bool()))
  val WAOPIITCMES = IO(Output(Bool()))
  val WMIOIACTPES = IO(Output(Bool()))
  val WETSACMIOIP = IO(Output(UInt(55.W))) //  [54  :0]
  val WIOAICSTEPM = IO(Output(Bool()))
  val WPSCIMTAEOI = IO(Output(Bool()))
  val WTAEIMSICOP = IO(Output(Bool()))
  val WIACPSIEMOT = IO(Output(UInt(88.W))) //  [87  :0]
  val WIIAETPMSCO = IO(Output(Bool()))
  val WMPIAETOCSI = IO(Output(Bool()))
  val WPECITMSOIA = IO(Output(Bool()))
  val WOCIATISPEM = IO(Output(UInt(3136.W))) //  [3135:0]
  val WCSIIAPOMTE = IO(Output(UInt(8.W))) //  [7   :0]
  val WITMSEPOCAI = IO(Output(UInt(8.W))) //  [7   :0]
  val WAETIPSCOMI = IO(Output(Bool()))
  val WPIATSOIMEC = IO(Output(Bool()))
  val WEISCOPIAMT = IO(Output(Bool()))
  val WCOTIAPEMIS = IO(Output(UInt(1016.W))) //   [1015:0]
  val WASOPICIEMT = IO(Output(UInt(8.W))) //   [7   :0]
  val WMCOIPISEAT = IO(Output(UInt(440.W))) //  [439 :0]
  val WOIECASIMPT = IO(Output(UInt(8.W))) //  [7   :0]
  val WTMIIOCPEAS = IO(Output(UInt(8.W))) // [7   :0]
  val WTEICPMIASO = IO(Output(Bool()))
  val WMACIIEPOTS = IO(Output(UInt(8.W))) //  [7   :0]
  val WIIPETAOCMS = IO(Output(Bool()))
  val WTOMIAPSICE = IO(Output(Bool()))
  addResource("/aia/src/rtl/imsic/xh_cdb_dev.v")
}


