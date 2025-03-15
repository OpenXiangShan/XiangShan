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
