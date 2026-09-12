#! /usr/bin/csh
#set files = (`ls bus_handler_*.sv`)
#echo $files
#
#foreach f ($files)
#    set fname = "iommu_${f}"
#    echo "${fname}.sv"
#    mv $f "${fname}.sv"
#end
#
#
#set bfiles = (`ls *.sv.sv`)
#
#foreach fn ($bfiles)
#    set ff1 = $fn:r
#    echo $ff1
#    set ff = ${ff1}.sv
#    echo $ff
#    #mv $fn ${ff}
#    mv $fn $ff1
#end


set dofiles = (`ls iommu_*.sv`)

foreach fl ($dofiles)
    #sed -i 's/BUS_INFLY/BUS_ONFLY/g' $fl
    #sed -i 's/iommu_main_tlb_dtc/iommu_acd_main_tlb_dtc/g' $fl
    #sed -i 's/iommu_main_tlb_dtc_top/iommu_acd_main_tlb_dtc_top/g' $fl
    #sed -i 's/iommu_main_tlb_ptc/iommu_acd_main_tlb_ptc/g' $fl
    #sed -i 's/iommu_main_tlb_ptc_top/iommu_acd_main_tlb_ptc_top/g' $fl
    #sed -i 's/iommu_main_tlb/iommu_acd_main_tlb/g' $fl
    #sed -i 's/iommu_micro_tlb/iommu_acd_micro_tlb/g' $fl
    #sed -i 's/iommu_tlb_queue/iommu_acd_tlb_queue/g' $fl
    #sed -i 's/iommu_tlb_ram_model/iommu_acd_tlb_ram_model/g' $fl
    #sed -i 's/iommu_tlb_wrap/iommu_acd_tlb_wrap/g' $fl
    #sed -i 's/iommu_translate_unit/iommu_acd_translate_unit/g' $fl


    sed -i 's/iommu_tlb_ram_wrap/iommu_acd_tlb_ram_wrap/g' $fl

    #sed -i 's/iommu_bus_handler_4k_boundary_check/iommu_acd_bus_handler_4k_boundary_check/g' $fl
    #sed -i 's/iommu_bus_handler_b_buf/iommu_acd_bus_handler_b_buf/g' $fl
    #sed -i 's/iommu_bus_handler_idx_fifo/iommu_acd_bus_handler_idx_fifo/g' $fl
    #sed -i 's/iommu_bus_handler_mst_intf/iommu_acd_bus_handler_mst_intf/g' $fl
    #sed -i 's/iommu_bus_handler_package/iommu_acd_bus_handler_package/g' $fl
    #sed -i 's/iommu_bus_handler_r_buf/iommu_acd_bus_handler_r_buf/g' $fl
    #sed -i 's/iommu_bus_handler_regslice/iommu_acd_bus_handler_regslice/g' $fl
    #sed -i 's/iommu_bus_handler_slv_intf/iommu_acd_bus_handler_slv_intf/g' $fl
    #sed -i 's/iommu_bus_handler_top/iommu_acd_bus_handler_top/g' $fl
    #sed -i 's/iommu_bus_handler_trans_arb/iommu_acd_bus_handler_trans_arb/g' $fl
    #sed -i 's/iommu_bus_handler_trans_queue/iommu_acd_bus_handler_trans_queue/g' $fl
    #sed -i 's/iommu_bus_handler_w_buf/iommu_acd_bus_handler_w_buf/g' $fl
    #
    #sed -i 's/iommu_bus_handler_sync_fifo/iommu_acd_bus_handler_sync_fifo/g' $fl
    #sed -i 's/iommu_bus_handler_id_queue/iommu_acd_bus_handler_id_queue/g' $fl
    #sed -i 's/iommu_bus_handler_slv_err/iommu_acd_bus_handler_slv_err/g' $fl
    #sed -i 's/iommu_bus_handler_queue_entry_update/iommu_acd_bus_handler_queue_entry_update/g' $fl
    #sed -i 's/iommu_bus_handler_idx_queue/iommu_acd_bus_handler_idx_queue/g' $fl
    #sed -i 's/iommu_bus_hander_idx_fifo/iommu_acd_bus_hander_idx_fifo/g' $fl


    #set newname = `echo $fl | sed 's/^iommu_/iommu_acd_/'`
    #mv $fl $newname
end
