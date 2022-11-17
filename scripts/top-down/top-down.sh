#!/bin/bash

filename=$1
# debug=1

tmp=$(grep "core_with_l2.core.ctrlBlock.rob: clock_cycle," $filename)
total_cycles=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.decode: fetch_bubbles," $filename)
fetch_bubbles=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.decode: decode_bubbles," $filename)
decode_bubbles=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.decode: slots_issued," $filename)
slots_issued=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.rename: recovery_bubbles," $filename)
recovery_bubbles=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.rob: commitUop," $filename)
slots_retired=${tmp##* }
tmp=$(grep "core_with_l2.core.frontend.ftq: mispredictRedirect," $filename)
br_mispred_retired=${tmp##* }
tmp=$(grep "core_with_l2.core.frontend.icache.mainPipe: icache_bubble_s2_miss," $filename)
icache_miss_cycles=${tmp##* }
tmp=$(grep "core_with_l2.core.frontend.icache.mainPipe: icache_bubble_s0_tlb_miss," $filename)
itlb_miss_cycles=${tmp##* }
tmp=$(grep "core_with_l2.core.frontend.bpu: s2_redirect," $filename)
s2_redirect_cycles=${tmp##* }
tmp=$(grep "core_with_l2.core.frontend.bpu: s3_redirect," $filename)
s3_redirect_cycles=${tmp##* }
tmp=$(grep "stall_stores_bound," $filename)
store_bound_cycles=${tmp##* }
tmp=$(grep "stall_loads_bound," $filename)
load_bound_cycles=${tmp##* }
tmp=$(grep "stall_ls_bandwidth_bound," $filename)
ls_dq_bound_cycles=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.dispatch: stall_cycle_rob," $filename)
stall_cycle_rob=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.dispatch: stall_cycle_int_dq," $filename)
stall_cycle_int_dq=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.dispatch: stall_cycle_fp_dq," $filename)
stall_cycle_fp_dq=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.dispatch: stall_cycle_ls_dq," $filename)
stall_cycle_ls_dq=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.rename: stall_cycle_fp," $filename)
stall_cycle_fp=${tmp##* }
tmp=$(grep "core_with_l2.core.ctrlBlock.rename: stall_cycle_int," $filename)
stall_cycle_int=${tmp##* }
tmp=$(grep "core_with_l2.core.memBlock.lsq.loadQueue: l1d_loads_bound," $filename)
l1d_loads_bound_cycles=${tmp##* }
tmp=$(grep ": l1d_loads_mshr_bound," $filename)
l1d_loads_mshr_bound=${tmp##* }
tmp=$(grep ": l1d_loads_tlb_bound," $filename)
l1d_loads_tlb_bound=${tmp##* }
tmp=$(grep ": l1d_loads_store_data_bound," $filename)
l1d_loads_store_data_bound=${tmp##* }
tmp=$(grep ": l1d_loads_bank_conflict_bound," $filename)
l1d_loads_bank_conflict_bound=${tmp##* }
tmp=$(grep ": l1d_loads_vio_check_redo_bound," $filename)
l1d_loads_vio_check_redo_bound=${tmp##* }
tmp=$(grep "core_with_l2.l2cache: l2_loads_bound," $filename)
l2_loads_bound_cycles=${tmp##* }
tmp=$(grep "l3cacheOpt: l3_loads_bound," $filename)
l3_loads_bound_cycles=${tmp##* }
tmp=$(grep "l3cacheOpt: ddr_loads_bound," $filename)
ddr_loads_bound_cycles=${tmp##* }

tmp=$(grep "ctrlBlock: stage2_redirect_cycles," $filename)
stage2_redirect_cycles=${tmp##* }
tmp=$(grep "ctrlBlock: branch_resteers_cycles," $filename)
branch_resteers_cycles=${tmp##* }
tmp=$(grep "ctrlBlock: robFlush_bubble_cycles," $filename)
robFlush_bubble_cycles=${tmp##* }
tmp=$(grep "ctrlBlock: ldReplay_bubble_cycles," $filename)
ldReplay_bubble_cycles=${tmp##* }

tmp=$(grep "core_with_l2.core.ctrlBlock.decode: ifu2id_allNO_cycle," $filename)
ifu2id_allNO_cycle=${tmp##* }

echo "total_cycles,                    $total_cycles"                     >$filename.csv
echo "fetch_bubbles,                   $fetch_bubbles"                   >>$filename.csv
echo "decode_bubbles,                  $decode_bubbles"                  >>$filename.csv
echo "slots_issued,                    $slots_issued"                    >>$filename.csv
echo "recovery_bubbles,                $recovery_bubbles"                >>$filename.csv
echo "slots_retired,                   $slots_retired"                   >>$filename.csv
echo "br_mispred_retired,              $br_mispred_retired"              >>$filename.csv
echo "icache_miss_cycles,              $icache_miss_cycles"              >>$filename.csv
echo "itlb_miss_cycles,                $itlb_miss_cycles"                >>$filename.csv
echo "s2_redirect_cycles,              $s2_redirect_cycles"              >>$filename.csv
echo "s3_redirect_cycles,              $s3_redirect_cycles"              >>$filename.csv
echo "store_bound_cycles,              $store_bound_cycles"              >>$filename.csv
echo "load_bound_cycles,               $load_bound_cycles"               >>$filename.csv
echo "ls_dq_bound_cycles,              $ls_dq_bound_cycles"              >>$filename.csv
echo "stall_cycle_fp,                  $stall_cycle_fp"                  >>$filename.csv
echo "stall_cycle_int,                 $stall_cycle_int"                 >>$filename.csv
echo "stall_cycle_rob,                 $stall_cycle_rob"                 >>$filename.csv
echo "stall_cycle_int_dq,              $stall_cycle_int_dq"              >>$filename.csv
echo "stall_cycle_fp_dq,               $stall_cycle_fp_dq"               >>$filename.csv
echo "stall_cycle_ls_dq,               $stall_cycle_ls_dq"               >>$filename.csv
echo "l1d_loads_bound_cycles,          $l1d_loads_bound_cycles"          >>$filename.csv
echo "l1d_loads_mshr_bound,            $l1d_loads_mshr_bound"            >>$filename.csv
echo "l1d_loads_tlb_bound,             $l1d_loads_tlb_bound"             >>$filename.csv
echo "l1d_loads_store_data_bound,      $l1d_loads_store_data_bound"      >>$filename.csv
echo "l1d_loads_bank_conflict_bound,   $l1d_loads_bank_conflict_bound"   >>$filename.csv
echo "l1d_loads_vio_check_redo_bound,  $l1d_loads_vio_check_redo_bound"  >>$filename.csv
echo "l2_loads_bound_cycles,           $l2_loads_bound_cycles"           >>$filename.csv
echo "l3_loads_bound_cycles,           $l3_loads_bound_cycles"           >>$filename.csv
echo "ddr_loads_bound_cycles,          $ddr_loads_bound_cycles"          >>$filename.csv
echo "stage2_redirect_cycles,          $stage2_redirect_cycles"          >>$filename.csv
echo "branch_resteers_cycles,          $branch_resteers_cycles"          >>$filename.csv
echo "robFlush_bubble_cycles,          $robFlush_bubble_cycles"          >>$filename.csv
echo "ldReplay_bubble_cycles,          $ldReplay_bubble_cycles"          >>$filename.csv
echo "ifu2id_allNO_cycle,              $ifu2id_allNO_cycle"              >>$filename.csv

[ -z "$debug" ] || cat $filename.csv
