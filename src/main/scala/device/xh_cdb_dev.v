 
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
wire              WTIIAPSOECM;                       wire    [391 :0]  WAMIIOETSPC;                    
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

endmodule
