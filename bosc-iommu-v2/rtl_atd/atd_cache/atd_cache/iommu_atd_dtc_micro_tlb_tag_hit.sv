module iommu_atd_dtc_micro_tlb_tag_hit #(
    parameter  CACHE_TYPE  = 0, // 0:DDTC, 1:PDTC
    parameter  SPARE_PARAM = 1'b0
)(
    input  logic [23:0]                 device_id_i,
    input  logic [19:0]                 process_id_i,
    input  logic                        valid_i,
    input  logic [23:0]                 hit_tag_device_id,
    input  logic [19:0]                 hit_tag_process_id,
    input  logic [1:0]                  content_lvl,
    output logic                        tag_hit
);
    logic device_id_hit, process_id_hit;
    logic [5:0]   did0_i;
    logic [14:6]  did1_i;
    logic [23:15] did2_i;
    logic did0_hit, did1_hit, did2_hit;
    logic [7:0]   pid0_i;
    logic [16:8]  pid1_i;
    logic [19:17] pid2_i;
    logic pid0_hit, pid1_hit, pid2_hit;
    
    assign did0_i = device_id_i[5:0];
    assign did1_i = device_id_i[14:6];
    assign did2_i = device_id_i[23:15];

    assign did0_hit = (did0_i == hit_tag_device_id[5:0])   |                        // PDTC or DDTC_LVL0, do did0 matching
                      ((CACHE_TYPE=='d0) & (content_lvl=='d2 | content_lvl=='d1));  // DDTC, content_lvl may mask lower did matching
    assign did1_hit = (did1_i == hit_tag_device_id[14:6])  |                        // PDTC or DDTC_LVL0/1, do did1 matching
                      ((CACHE_TYPE=='d0) & (content_lvl=='d2));                      // DDTC, content_lvl may mask lower did matching
    assign did2_hit = (did2_i == hit_tag_device_id[23:15]);                         // always do did2 matching

    assign pid0_i = process_id_i[7:0];
    assign pid1_i = process_id_i[16:8];
    assign pid2_i = process_id_i[19:17];

    assign pid0_hit = (CACHE_TYPE=='d0)  |                                                          // DDTC no pid0_hit need
                      ((pid0_i == hit_tag_process_id[7:0] ) | (content_lvl=='d2 | content_lvl=='d1));// PDTC, content_lvl may mask lower pid matching
    assign pid1_hit = (CACHE_TYPE=='d0)  |                                                          // DDTC no pid1_hit need
                      ((pid1_i == hit_tag_process_id[16:8]) | (content_lvl=='d2));                  // PDTC, content_lvl may mask lower pid matching
    assign pid2_hit = (CACHE_TYPE=='d0)  |                                                          // DDTC no pid2_hit need
                      ((pid2_i == hit_tag_process_id[19:17]));                                      // PDTC, always do pid2 matching


    assign device_id_hit    = ~valid_i ? 1'b0 : (did0_hit & did1_hit & did2_hit);//(device_id_i == hit_tag_device_id);
    assign process_id_hit   = ~valid_i ? 1'b0 : (pid0_hit & pid1_hit & pid2_hit);//(process_id_i== hit_tag_process_id);
    assign tag_hit          = device_id_hit & process_id_hit;

endmodule
