 
`define WIMITCPSAOE
`define WPATOMICESI 9
 
 
`define WSAICMIPEOT
 
`define XH_PA_WIDTH_44
`define WICMIESPTAO   44

`define WTSIAMIOCEP

`ifdef WTSIAMIOCEP
`define WTIEMPCASIO     256
`define WPIOAICEMTS  32
`define WMTOPISEIAC 4
`else
`define WTIEMPCASIO     128
`define WPIOAICEMTS  16
`define WMTOPISEIAC 2
`endif

`define WOPIEISATMC 8

`ifdef WIMITCPSAOE
  `define WITAOPICMSE
  `define WAOCEISPIMT

  `ifdef WSAICMIPEOT
    `define WSMTOPCEAII
  `endif

  `define WAPMOTCSEII
  `define WTMIOIASCPE 3
  `define WOETCSIIMAP 4
`endif  

`ifdef WIMITCPSAOE
  `define WEOTIMASIPC
`endif


 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
`define  WTEISOPAMIC          44
 
 
 

 
`define WPSEOTCIIAM            3
`define WMPOCETSIAI          `WPSEOTCIIAM+`WPATOMICESI
`define WMPEASOICIT          `WMPOCETSIAI+`WPATOMICESI
`define WPOIEACITMS          `WMPEASOICIT+`WOPIEISATMC
`define WIIECSOAPMT         `WPOIEACITMS+4
`define WASIMITOECP        `WIIECSOAPMT+2
`define WIPMEITASCO           `WASIMITOECP+3
`define WSTOAIMPIEC       `WIPMEITASCO+3
`ifdef WITAOPICMSE
`define WOTIPCMASEI           `WSTOAIMPIEC+`WOPIEISATMC
`else
`define WTSPIMCIAOE          `WSTOAIMPIEC+3
`define WOTIPCMASEI           `WTSPIMCIAOE+`WOPIEISATMC
`endif
`define WMOTPCISIAE       `WOTIPCMASEI+4
`define WASOCTIMPIE       `WMOTPCISIAE+1
`define WETPSCIIOMA          `WASOCTIMPIE+1

 
`define WISTIAOCEPM            3
`define WCSAMIEPOTI          `WISTIAOCEPM+`WPATOMICESI
`define WOTICIPMEAS          `WCSAMIEPOTI+`WPATOMICESI
`define WTPIMSCAEIO          `WOTICIPMEAS+`WOPIEISATMC
`define WOAETSMIPCI        `WTPIMSCAEIO+`WPATOMICESI
`define WMIOCATEPSI         `WOAETSMIPCI+4
`define WITIOCSPMAE        `WMIOCATEPSI+2
`define WATEIOIMCPS           `WITIOCSPMAE+3
`define WISAPTMEOIC        `WATEIOIMCPS+`WTMIOIASCPE
`ifdef WITAOPICMSE
`define WIMISCOAETP           `WISAPTMEOIC+`WOPIEISATMC
`else
`define WCMTSAPEOII          `WISAPTMEOIC+3
`define WIMISCOAETP           `WCMTSAPEOII+`WOPIEISATMC
`endif
`define WSIAPMICOTE           `WIMISCOAETP+2
`define WIIOSETPAMC         `WSIAPMICOTE+2
`define WOCAPIIMETS       `WIIOSETPAMC+1
`define WEOCTIAIMSP          `WOCAPIIMETS+`WOETCSIIMAP
`define WPIOTISAMEC             `WEOCTIAIMSP+`WPIOAICEMTS
`define WPSIECMAITO           `WPIOTISAMEC+`WTIEMPCASIO
`ifdef WAPMOTCSEII
`define WIAIPTCMOSE      `WPSIECMAITO+`WPIOAICEMTS
`ifdef WSMTOPCEAII
`define WOTCPISEAMI         `WIAIPTCMOSE+`WMTOPISEIAC
`define WMACIPTSEOI          `WOTCPISEAMI+1
`else  
`define WMACIPTSEOI          `WIAIPTCMOSE+1
`endif  
`else  
`ifdef WSMTOPCEAII
`define WOTCPISEAMI         `WPSIECMAITO+`WMTOPISEIAC
`define WMACIPTSEOI          `WOTCPISEAMI+1
`else  
`define WMACIPTSEOI          `WPSIECMAITO+1
`endif  
`endif  

`ifdef WAPMOTCSEII
`define WETICPOIMAS          `WPIOAICEMTS-1
`ifdef WSMTOPCEAII
`define WSCOTEPIIAM             `WETICPOIMAS+`WMTOPISEIAC
`define WAEOTIPISCM               `WSCOTEPIIAM+`WTIEMPCASIO
`else  
`define WAEOTIPISCM               `WETICPOIMAS+`WTIEMPCASIO
`endif  
`else  
`ifdef WSMTOPCEAII
`define WSCOTEPIIAM             `WMTOPISEIAC-1
`define WAEOTIPISCM               `WSCOTEPIIAM+`WTIEMPCASIO
`else  
`define WAEOTIPISCM               `WTIEMPCASIO-1
`endif  
`endif  
`define WISAMPEIOTC             `WAEOTIPISCM+2
`define WCPSEMITOAI               `WISAMPEIOTC+2
`define WIMETCIASOP               `WCPSEMITOAI+`WOPIEISATMC
`ifdef WITAOPICMSE
`define WSIATICPOEM            `WIMETCIASOP+`WTMIOIASCPE
`else
`define WIOMEITACSP              `WIMETCIASOP+3
`define WSIATICPOEM            `WIOMEITACSP+`WTMIOIASCPE
`endif
`define WSCMOPTAEII               `WSIATICPOEM+3
`define WMAISOITCPE            `WSCMOPTAEII+2
`define WTSIAIOMCPE             `WMAISOITCPE+4
`define WITEISCPAOM            `WTSIAIOMCPE+`WPATOMICESI
`define WACMSIIPTEO              `WITEISCPAOM+`WOPIEISATMC
`define WIEOATCIPMS             `WACMSIIPTEO+`XH_SNB_DEPTH
`define WCIPAOTEMSI           `WIEOATCIPMS+5
`define WIECSOAIPMT          `WCIPAOTEMSI+1

`define WICAIMSTOPE            3
`define WSIECOPIATM          `WICAIMSTOPE+`WPATOMICESI
`define WASOMCPIETI          `WSIECOPIATM+`WOPIEISATMC
`define WACESMTPIOI         `WASOMCPIETI+`WPATOMICESI
`define WCEAISIOMTP       `WACESMTPIOI+`WOPIEISATMC
`define WMCAPITISEO         `WCEAISIOMTP+5
`define WMSEOIPCITA           `WMCAPITISEO+41
`define WTMEISPICAO             `WMSEOIPCITA+1
`define WCSOPEMIIAT         `WTMEISPICAO+1
`define WIPISTEAOMC            `WCSOPEMIIAT+1
`define WETOACSPIMI       `WIPISTEAOMC+1
`define WATECPISIMO          `WETOACSPIMI+1

`define WTEPIAIMSCO            3
`define WMASTCPIIOE               `WTEPIAIMSCO+`WPATOMICESI
`define WIOTAPIMECS               `WMASTCPIIOE+`WPATOMICESI
`define WCTESAPIMOI               `WIOTAPIMECS+`WOPIEISATMC
`define WITPAMECOSI            `WCTESAPIMOI+`WPATOMICESI
`define WCTIEOSMPAI       `WITPAMECOSI+1
`define WSPIOMTCAIE            `WCTIEOSMPAI+`WOPIEISATMC
`define WIPMSECIOAT              `WSPIOMTCAIE+6
`define WPTIACOSEMI                `WIPMSECIOAT+3
`define WATPICOEMSI                `WPTIACOSEMI+`WTEISOPAMIC
`define WMEICAIPSOT                  `WATPICOEMSI+1
`define WPEOMICTIAS          `WMEICAIPSOT+1
`define WCIOAIMPEST          `WPEOMICTIAS+1
`define WOTAICESPIM               `WCIOAIMPEST+2
`define WCATOPIMESI            `WOTAICESPIM+4
`define WSPECTIOAMI             `WCATOPIMESI+4
`define WIIPOCAMETS             `WSPECTIOAMI+1
`define WPTIASCOEMI                `WIIPOCAMETS+5
`define WOCIAISMTEP                `WPTIASCOEMI+1
`define WCEPISTAOIM          `WOCIAISMTEP+1
`define WCOEITPMASI            `WCEPISTAOIM+1
`define WEIMCSATPOI               `WCOEITPMASI+4
`define WTSAIMIEPCO          `WEIMCSATPOI+1


 
 
 
 
 
 
 
 
 
 
 
 
 

`define WEOPTSAMICI        8
`define WITMSICAOEP        8
`define WITIESOAPCM        8
`define WSMAIOTCPEI        8
`define WEIOPCTSIAM        8
`define WEIICMSOPTA        8
`define WISMIECPTAO        8
`define WTCEMISIPAO        8

`define WTPIMSOICEA      4
`define WIPCSIMOTAE      4
`define WSTPAIMIECO      4
`define WIOCAETSIMP      4
`define WOSEIPITACM      4
`define WASMPOETCII      4
`define WMTSIOEIACP      4
`define WPIATESIOCM      4

`define WSOEAIMCPIT            `WTSAIMIEPCO
`define WEIPMTIASCO            `WETPSCIIOMA
`define WIPCAISETOM            `WMACIPTSEOI
`define WATISOICPME            `WATECPISIMO

`define WAEICPISOTM       `WSPIOMTCAIE+1
`define WIIEOCPATMS       `WIPMSECIOAT
`define WCTISOIAEPM       `WPOIEACITMS+1
`define WPICIOMAEST       `WIIECSOAPMT
`define WIETMSICAPO       `WOAETSMIPCI+1
`define WAOPEISICMT       `WMIOCATEPSI
`define WISOCIPMATE       `WCEAISIOMTP+1
`define WSMOCIAETIP       `WMCAPITISEO

  
 `define WTISOMICEAP            3'b000
 `define WSOIAPEITMC          3'b001
 `define WTPEIIOMCAS             3'b010
 `define WSIPEAMCOIT    3'b011
 `define WSTMCIAOIEP           3'b100
 `define WPCTAMEIISO       3'b101
 `define WSOMPAICEIT   3'b110
 `define WATEMSCIIOP 3'b111

 `define WIAOSIECTMP     3
 `define WMIIPCOATSE       0
 `define WPETIMSAOCI       2
 `define WISEOMICATP     2:0

  
 `define WOCTPIIEMSA          3'b000
 `define WOATISCEIMP           3'b001
 `define WTEISICMPAO   3'b010
 `define WSEACPIMTOI  3'b011
 `define WPIMEOCSTIA   3'b100
 `define WIPMSIAEOCT         3'b101

 `define WMAEIOSPITC     3
 `define WOICAIMTESP       0
 `define WSPIAOETCMI       2
 `define WCESIOAIPTM     2:0

 `define WEIOTPMAISC        2'b01


`define WTPOMEICISA
`define WCMPEIAITSO

 
 `define WAPMSTEICIO #(\
     parameter WCSIPTIMAEO = 1,   \
     parameter WEMCIPTSAOI      = 4,   \
     parameter WSOIAMEIPCT      = 4   \
                             )

 `define WMCISIPAOTE #(\
     parameter WCSIPTIMAEO = 1,   \
     parameter WCSOIEATMIP = 0,   \
     parameter WSPOMAETIIC = 6,   \
     parameter WCIOTPEISAM = 6,   \
     parameter WMPTICEISOA      = 100, \
     parameter WPESOIICMAT = 1,   \
     parameter WEMPIOITSCA = 2    \
                             )

 `define WCASPIIMETO                      0

 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
module xh_cdb_dev(
  WPCASIOIMTE,
  WMOTSECIPIA,
  WOMSCAIEIPT,
  WIMACTSPIOE,
  WSIIPMOTCAE,
  WMTIICSPAOE,
  WSMOACPETII,
  WTIIAPSOECM,
  WAMIIOETSPC,
  WEIAIOTMPCS,
  WMPTOSIEIAC,
  WAPMTCISIEO,
  WTPIEMAISCO,
  WICSTAOIEPM,
  WMIOPATEISC,
  WCPOASITMEI,
  WEIACTIOSMP,
  WPMICATIOSE,
  WPOIETACMIS,
  WMOSIPIACET,
  WTCAOPSMIIE,
  WOCIMISETPA,
  WIOAPMESICT,
  WAEPISIOMCT,
  WITSIPAEMOC,
  WTISMPICAEO,
  WSCMIETAIOP,
  WAOPIITCMES,
  WCMIEPTAISO,
  WOAISIMTCEP,
  WMIOIACTPES,
  WETSACMIOIP,
  WIOAICSTEPM,
  WPSCIMTAEOI,
  WEMCSTIPIAO,
  WTAEIMSICOP,
  WIACPSIEMOT,
  WIIAETPMSCO,
  WMPIAETOCSI,
  WCTPOSEAMII,
  clk,
  WPECITMSOIA,
  WOCIATISPEM,
  WCSIIAPOMTE,
  WITMSEPOCAI,
  WAETIPSCOMI,
  WPIATSOIMEC,
  WEISCOPIAMT,
  WCOTIAPEMIS,
  WASOPICIEMT,
  WMCOIPISEAT,
  WOIECASIMPT,
  WTMIIOCPEAS,
  WTEICPMIASO,
  WMACIIEPOTS,
  WIIPETAOCMS,
  WTOMIAPSICE,
  WAPIIEMCTSO,
  WOPETSIACIM,
  WIMEITCSAOP,
  WECIOTIMSAP,
  WITAISPMCEO,
  WTMSCEOIAIP,
  WISPOCATIEM,
  WTCESPIOIAM,
  WIOSTAIPCME,
  WCIMISAEOTP,
  WPMISECOAIT,
  WASEMCPIOIT,
  WECMATPSOII,
  WTCAISPOMIE,
  WOSMETIAICP,
  WCTMIPOEIAS
);


 
input             WMOTSECIPIA;                      
input             WOMSCAIEIPT;                
input             WSMOACPETII;                    
input             WTIIAPSOECM;                       
input   [391 :0]  WAMIIOETSPC;                    
input             WEIAIOTMPCS;                
input             WMPTOSIEIAC;                   
input             WICSTAOIEPM;              
input   [126 :0]  WMIOPATEISC;                    
input             WCPOASITMEI;                
input             WEIACTIOSMP;                   
input   [54  :0]  WPOIETACMIS;                    
input             WMOSIPIACET;                
input             WTCAOPSMIIE;                   
input             WIOAPMESICT;              
input             WITSIPAEMOC;                     
input             WCMIEPTAISO;                   
input             WOAISIMTCEP;              
input             WEMCSTIPIAO;                   
input             WCTPOSEAMII;                   
input             clk;                          
input             WAPIIEMCTSO;       
input   [3135:0]  WOPETSIACIM;   
input   [7   :0]  WIMEITCSAOP;      
input   [7   :0]  WECIOTIMSAP;      
input             WITAISPMCEO; 
input             WTMSCEOIAIP;  
input             WISPOCATIEM;     
input   [7   :0]  WTCESPIOIAM;      
input   [439 :0]  WIOSTAIPCME;   
input   [7   :0]  WCIMISAEOTP;      
input   [7   :0]  WPMISECOAIT;      
input             WASEMCPIOIT; 
input   [703 :0]  WECMATPSOII;   
input   [7   :0]  WTCAISPOMIE;      
input             WOSMETIAICP;      
input             WCTMIPOEIAS; 
output            WPCASIOIMTE;                  
output            WIMACTSPIOE;                 
output            WSIIPMOTCAE;                  
output            WMTIICSPAOE;                    
output            WAPMTCISIEO;                   
output            WTPIEMAISCO;              
output            WPMICATIOSE;                   
output            WOCIMISETPA;                   
output            WAEPISIOMCT;                     
output  [391 :0]  WTISMPICAEO;                    
output            WSCMIETAIOP;                
output            WAOPIITCMES;                   
output            WMIOIACTPES;              
output  [54  :0]  WETSACMIOIP;                    
output            WIOAICSTEPM;                
output            WPSCIMTAEOI;                   
output            WTAEIMSICOP;              
output  [87  :0]  WIACPSIEMOT;                    
output            WIIAETPMSCO;                
output            WMPIAETOCSI;                   
output            WPECITMSOIA;       
output  [3135:0]  WOCIATISPEM;   
output  [7   :0]  WCSIIAPOMTE;      
output  [7   :0]  WITMSEPOCAI;      
output            WAETIPSCOMI; 
output            WPIATSOIMEC; 
output            WEISCOPIAMT;     
output  [1015:0]  WCOTIAPEMIS;   
output  [7   :0]  WASOPICIEMT;      
output  [439 :0]  WMCOIPISEAT;   
output  [7   :0]  WOIECASIMPT;      
output  [7   :0]  WTMIIOCPEAS;      
output            WTEICPMIASO; 
output  [7   :0]  WMACIIEPOTS;      
output            WIIPETAOCMS;      
output            WTOMIAPSICE; 

 

 
wire              WPCASIOIMTE;                  
wire              WMOTSECIPIA;                      
wire              WOMSCAIEIPT;                
wire              WIMACTSPIOE;                 
wire              WSIIPMOTCAE;                  
wire              WMTIICSPAOE;                    
wire              WSMOACPETII;                    
wire              WTIIAPSOECM;                       
wire    [391 :0]  WAMIIOETSPC;                    
wire              WEIAIOTMPCS;                
wire              WMPTOSIEIAC;                   
wire              WAPMTCISIEO;                   
wire              WTPIEMAISCO;              
wire              WICSTAOIEPM;              
wire    [126 :0]  WMIOPATEISC;                    
wire              WCPOASITMEI;                
wire              WEIACTIOSMP;                   
wire              WPMICATIOSE;                   
wire    [54  :0]  WPOIETACMIS;                    
wire              WMOSIPIACET;                
wire              WTCAOPSMIIE;                   
wire              WOCIMISETPA;                   
wire              WIOAPMESICT;              
wire              WAEPISIOMCT;                     
wire              WITSIPAEMOC;                     
wire    [391 :0]  WTISMPICAEO;                    
wire              WSCMIETAIOP;                
wire              WAOPIITCMES;                   
wire              WCMIEPTAISO;                   
wire              WOAISIMTCEP;              
wire              WMIOIACTPES;              
wire    [54  :0]  WETSACMIOIP;                    
wire              WIOAICSTEPM;                
wire              WPSCIMTAEOI;                   
wire              WEMCSTIPIAO;                   
wire              WTAEIMSICOP;              
wire    [87  :0]  WIACPSIEMOT;                    
wire              WIIAETPMSCO;                
wire              WMPIAETOCSI;                   
wire              WCTPOSEAMII;                   
wire              clk;                          
wire              WPECITMSOIA;       
wire    [3135:0]  WOCIATISPEM;   
wire    [7   :0]  WCSIIAPOMTE;      
wire    [7   :0]  WITMSEPOCAI;      
wire              WAETIPSCOMI; 
wire              WPIATSOIMEC; 
wire              WEISCOPIAMT;     
wire    [1015:0]  WCOTIAPEMIS;   
wire    [7   :0]  WASOPICIEMT;      
wire    [439 :0]  WMCOIPISEAT;   
wire    [7   :0]  WOIECASIMPT;      
wire    [7   :0]  WTMIIOCPEAS;      
wire              WTEICPMIASO; 
wire    [7   :0]  WMACIIEPOTS;      
wire              WIIPETAOCMS;      
wire              WTOMIAPSICE; 
wire              WAPIIEMCTSO;       
wire    [3135:0]  WOPETSIACIM;   
wire    [7   :0]  WIMEITCSAOP;      
wire    [7   :0]  WECIOTIMSAP;      
wire              WITAISPMCEO; 
wire              WTMSCEOIAIP;  
wire              WISPOCATIEM;     
wire    [7   :0]  WTCESPIOIAM;      
wire    [439 :0]  WIOSTAIPCME;   
wire    [7   :0]  WCIMISAEOTP;      
wire    [7   :0]  WPMISECOAIT;      
wire              WASEMCPIOIT; 
wire    [703 :0]  WECMATPSOII;   
wire    [7   :0]  WTCAISPOMIE;      
wire              WOSMETIAICP;      
wire              WCTMIPOEIAS; 



 
 
 
 
 
 
 
 
 
 
 
 
 

 
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 
module xh_cdb_icn(
  WPCASIOIMTE,
  WMOTSECIPIA,
  WOMSCAIEIPT,
  WTIIAPSOECM,
  WAMIIOETSPC,
  WEIAIOTMPCS,
  WMPTOSIEIAC,
  WAPMTCISIEO,
  WTPIEMAISCO,
  WICSTAOIEPM,
  WPOIETACMIS,
  WMOSIPIACET,
  WTCAOPSMIIE,
  WOCIMISETPA,
  WIOAPMESICT,
  WEIPSOMCTIA,
  WPEIOITMSAC,
  WIPCSEAMOIT,
  WOIMIATPSCE,
  WAEPISIOMCT,
  WITSIPAEMOC,
  WTISMPICAEO,
  WSCMIETAIOP,
  WAOPIITCMES,
  WCMIEPTAISO,
  WOAISIMTCEP,
  WMIOIACTPES,
  WOMETPIISCA,
  WTSCEOAIPIM,
  WCOTSEPIAIM,
  WEMICTPSOAI,
  WETSACMIOIP,
  WIOAICSTEPM,
  WPSCIMTAEOI,
  WEMCSTIPIAO,
  WTAEIMSICOP,
  clk,
  WPECITMSOIA,
  WOCIATISPEM,
  WCSIIAPOMTE,
  WITMSEPOCAI,
  WAETIPSCOMI,
  WPIATSOIMEC,
  WEISCOPIAMT,
  WCOTIAPEMIS,
  WASOPICIEMT,
  WMCOIPISEAT,
  WOIECASIMPT,
  WTMIIOCPEAS,
  WTEICPMIASO,
  WMACIIEPOTS,
  WIIPETAOCMS,
  WTOMIAPSICE,
  WAPIIEMCTSO,
  WOPETSIACIM,
  WIMEITCSAOP,
  WECIOTIMSAP,
  WITAISPMCEO,
  WTMSCEOIAIP,
  WISPOCATIEM,
  WTCESPIOIAM,
  WIOSTAIPCME,
  WCIMISAEOTP,
  WPMISECOAIT,
  WASEMCPIOIT,
  WECMATPSOII,
  WTCAISPOMIE,
  WOSMETIAICP,
  WCTMIPOEIAS
);


 
input             WMOTSECIPIA;                      
input             WOMSCAIEIPT;                
input             WTIIAPSOECM;                       
input   [391 :0]  WAMIIOETSPC;                    
input             WEIAIOTMPCS;                
input             WMPTOSIEIAC;                   
input             WICSTAOIEPM;              
input   [54  :0]  WPOIETACMIS;                    
input             WMOSIPIACET;                
input             WTCAOPSMIIE;                   
input             WIOAPMESICT;              
input   [87  :0]  WEIPSOMCTIA;                    
input             WPEIOITMSAC;                
input             WIPCSEAMOIT;                   
input             WAEPISIOMCT;                     
input             WCMIEPTAISO;                   
input             WOAISIMTCEP;              
input             WEMICTPSOAI;                   
input             WEMCSTIPIAO;                   
input             clk;                          
input             WPECITMSOIA;       
input   [3135:0]  WOCIATISPEM;   
input   [7   :0]  WCSIIAPOMTE;      
input   [7   :0]  WITMSEPOCAI;      
input             WAETIPSCOMI; 
input             WPIATSOIMEC; 
input             WEISCOPIAMT;     
input   [1015:0]  WCOTIAPEMIS;   
input   [7   :0]  WASOPICIEMT;      
input   [439 :0]  WMCOIPISEAT;   
input   [7   :0]  WOIECASIMPT;      
input   [7   :0]  WTMIIOCPEAS;      
input             WTEICPMIASO; 
input   [7   :0]  WMACIIEPOTS;      
input             WIIPETAOCMS;      
input             WTOMIAPSICE; 
output            WPCASIOIMTE;                  
output            WAPMTCISIEO;                   
output            WTPIEMAISCO;              
output            WOCIMISETPA;                   
output            WOIMIATPSCE;                   
output            WITSIPAEMOC;                     
output  [391 :0]  WTISMPICAEO;                    
output            WSCMIETAIOP;                
output            WAOPIITCMES;                   
output            WMIOIACTPES;              
output  [126 :0]  WOMETPIISCA;                    
output            WTSCEOAIPIM;                
output            WCOTSEPIAIM;                   
output  [54  :0]  WETSACMIOIP;                    
output            WIOAICSTEPM;                
output            WPSCIMTAEOI;                   
output            WTAEIMSICOP;              
output            WAPIIEMCTSO;       
output  [3135:0]  WOPETSIACIM;   
output  [7   :0]  WIMEITCSAOP;      
output  [7   :0]  WECIOTIMSAP;      
output            WITAISPMCEO; 
output            WTMSCEOIAIP;  
output            WISPOCATIEM;     
output  [7   :0]  WTCESPIOIAM;      
output  [439 :0]  WIOSTAIPCME;   
output  [7   :0]  WCIMISAEOTP;      
output  [7   :0]  WPMISECOAIT;      
output            WASEMCPIOIT; 
output  [703 :0]  WECMATPSOII;   
output  [7   :0]  WTCAISPOMIE;      
output            WOSMETIAICP;      
output            WCTMIPOEIAS; 

 

 
wire              WPCASIOIMTE;                  
wire              WMOTSECIPIA;                      
wire              WOMSCAIEIPT;                
wire              WTIIAPSOECM;                       
wire    [391 :0]  WAMIIOETSPC;                    
wire              WEIAIOTMPCS;                
wire              WMPTOSIEIAC;                   
wire              WAPMTCISIEO;                   
wire              WTPIEMAISCO;              
wire              WICSTAOIEPM;              
wire    [54  :0]  WPOIETACMIS;                    
wire              WMOSIPIACET;                
wire              WTCAOPSMIIE;                   
wire              WOCIMISETPA;                   
wire              WIOAPMESICT;              
wire    [87  :0]  WEIPSOMCTIA;                    
wire              WPEIOITMSAC;                
wire              WIPCSEAMOIT;                   
wire              WOIMIATPSCE;                   
wire              WAEPISIOMCT;                     
wire              WITSIPAEMOC;                     
wire    [391 :0]  WTISMPICAEO;                    
wire              WSCMIETAIOP;                
wire              WAOPIITCMES;                   
wire              WCMIEPTAISO;                   
wire              WOAISIMTCEP;              
wire              WMIOIACTPES;              
wire    [126 :0]  WOMETPIISCA;                    
wire              WTSCEOAIPIM;                
wire              WCOTSEPIAIM;                   
wire              WEMICTPSOAI;                   
wire    [54  :0]  WETSACMIOIP;                    
wire              WIOAICSTEPM;                
wire              WPSCIMTAEOI;                   
wire              WEMCSTIPIAO;                   
wire              WTAEIMSICOP;              
wire              clk;                          
wire              WPECITMSOIA;       
wire    [3135:0]  WOCIATISPEM;   
wire    [7   :0]  WCSIIAPOMTE;      
wire    [7   :0]  WITMSEPOCAI;      
wire              WAETIPSCOMI; 
wire              WPIATSOIMEC; 
wire              WEISCOPIAMT;     
wire    [1015:0]  WCOTIAPEMIS;   
wire    [7   :0]  WASOPICIEMT;      
wire    [439 :0]  WMCOIPISEAT;   
wire    [7   :0]  WOIECASIMPT;      
wire    [7   :0]  WTMIIOCPEAS;      
wire              WTEICPMIASO; 
wire    [7   :0]  WMACIIEPOTS;      
wire              WIIPETAOCMS;      
wire              WTOMIAPSICE; 
wire              WAPIIEMCTSO;       
wire    [3135:0]  WOPETSIACIM;   
wire    [7   :0]  WIMEITCSAOP;      
wire    [7   :0]  WECIOTIMSAP;      
wire              WITAISPMCEO; 
wire              WTMSCEOIAIP;  
wire              WISPOCATIEM;     
wire    [7   :0]  WTCESPIOIAM;      
wire    [439 :0]  WIOSTAIPCME;   
wire    [7   :0]  WCIMISAEOTP;      
wire    [7   :0]  WPMISECOAIT;      
wire              WASEMCPIOIT; 
wire    [703 :0]  WECMATPSOII;   
wire    [7   :0]  WTCAISPOMIE;      
wire              WOSMETIAICP;      
wire              WCTMIPOEIAS; 
