import sys
import re
import subprocess

def extract_bits(num, x, y):
    mask = (1 << (x - y + 1)) - 1
    return (num >> y) & mask

def parse_log_file(filepath, verbose=False):
    cycles = []
    pc_strs = []
    insn_raw = []
    fuinfos = []
    fetch_cache_vals = []
    cache_comp_vals = []
    fetch_vals = []
    decode_vals = []
    rename_vals = []
    block_rob = []
    block_dpq = []
    block_serial = []
    block_rf = []
    block_lq = []
    block_sq = []
    eliminate_move = []
    dispatch_vals = []
    enq_rs_vals = []
    rs_ready_vals = []
    select_vals = []
    issue_vals = []
    complete_vals = []
    commit_vals = []
    rob_vals = []
    lq_vals = []
    sq_vals = []
    iq_vals = []
    fu_vals = []
    src_vec = []
    dst_vals = []
    srcvalid_vec = []
    insns = []

    with open(filepath, 'r', encoding='utf-8') as f:
        simulated = False
        for line in f:
            line = line.strip()
            if not line or line.startswith('[PERF'):
                continue
            parts = line.split(':')

            # Skip the first few instructions
            if len(parts) <= 3:
                print("Skipping line:", line)
            if parts[3].strip() == "0x0080000000":
                simulated = True
            if not simulated:
                continue

            cycles.append(int(parts[0]))
            pc_strs.append(parts[3].strip())
            insn_raw.append("DASM(" + parts[4].strip() + ")")
            fuinfos.append([int(x) for x in re.findall(r'\d+', parts[5])])

            parse_int = lambda x: int(x.split('=')[1])
            parse_multi_int = lambda x: [int(y) for y in x.split('=')[1].split(',')]

            fetch_cache_vals.append(parse_int(parts[ 7]))
            cache_comp_vals.append (parse_int(parts[ 8]))
            fetch_vals.append      (parse_int(parts[ 9]))
            decode_vals.append     (parse_int(parts[10]))
            rename_vals.append     (parse_int(parts[11]))
            block_rob.append       (parse_int(parts[12]))
            block_dpq.append       (parse_int(parts[13]))
            block_serial.append    (parse_int(parts[14]))
            block_rf.append        (parse_int(parts[15]))
            block_lq.append        (parse_int(parts[16]))
            block_sq.append        (parse_int(parts[17]))
            eliminate_move.append  (parse_int(parts[18]))
            dispatch_vals.append   (parse_int(parts[19]))
            enq_rs_vals.append     (parse_int(parts[20]))
            rs_ready_vals.append   (parse_int(parts[21]))
            select_vals.append     (parse_int(parts[22]))
            issue_vals.append      (parse_int(parts[23]))
            complete_vals.append   (parse_int(parts[24]))
            commit_vals.append     (parse_int(parts[25]))
            rob_vals.append        (parse_int(parts[26]))
            lq_vals.append         (parse_int(parts[27]))
            sq_vals.append         (parse_int(parts[28]))
            iq_vals.append         (parse_int(parts[29]))
            fu_vals.append         (parse_int(parts[30]))
            src_vec.append         (parse_multi_int(parts[31]))
            dst_vals.append        (parse_int(parts[32]))
            srcvalid_vec.append    (parse_multi_int(parts[33]))
            if verbose:
                print("Parsed numbers:", fuinfos[-1])

    combined_insn_raw = '\n'.join(insn_raw)
    with open('combined_insns.txt', 'w', encoding='utf-8') as f:
        f.write(combined_insn_raw)

    process = subprocess.Popen(
        'cat combined_insns.txt | spike-dasm && rm combined_insns.txt',
        shell=True,
        stdout=subprocess.PIPE
    )
    output = process.communicate()[0]
    insns_strs = output.decode('utf-8').strip().split('\n')
    with open('insns.trace', 'w', encoding='utf-8') as f:
        f.write('\n'.join(insns_strs))

    deg_trace = []
    
    with open('deg.trace', 'w', encoding='utf-8') as f:
        # simulated = False
        # start_cycle = 0
        trace_len = 0
        for i in range(len(cycles)):
            # if pc_strs[i] == "0x0080000000":
            #     simulated = True
            #     start_cycle = cycles[i]
            # if not simulated:
            #     continue
            if trace_len == 100:
                break
            trace_len += 1
            # Parse the type of instruction
            fu_type = fuinfos[i][0]
            fu_op_type = fuinfos[i][1]
            fpu_type = fuinfos[i][2]
            type_str = "unknown"

            if fu_type < 8:  # isIntExu
                if fu_op_type == 4:
                    type_str = "IntMult"
                elif fu_op_type == 5:
                    type_str = "IntDiv"
                else:
                    type_str = "IntAlu"
            elif fu_type < 12:  # isFpExu
                type_str = "Fp"
            else: # isMemExu
                if fu_type == 12 or fu_type == 15:
                    type_str = "MemRead"
                elif fu_type == 13:
                    type_str = "MemWrite"
                else:
                    assert False, f"Unknown MemExu type {fu_type}"

            # Parse FU
            fu = -1
            # Parse src and dst
            dst = ""
            src = []
            for j in range(len(src_vec[i])):
                if srcvalid_vec[i][j] != 0:
                    src.append(str(src_vec[i][j]))
            if dst_vals[i] != 0:
                dst = str(dst_vals[i])

            # Elaborate trace
            trace = []
            trace.append(f"{cycles[i] * 1000}")
            trace.append(f"system.cpu")
            trace.append(f"T0")
            trace.append(f"{pc_strs[i]}")
            trace.append(f"{insns_strs[i].ljust(max(map(len, insns_strs)), ' ')}")
            trace.append(f"{type_str.ljust(10, ' ')}")
            trace.append(f"_")
            trace.append(f"FetchCacheLine={fetch_cache_vals[i]*1000}")
            trace.append(f"ProcessCacheCompletion={cache_comp_vals[i]*1000}")
            trace.append(f"Fetch={fetch_vals[i]*1000}")
            trace.append(f"DecodeSortInsts={decode_vals[i]*1000}")
            trace.append(f"Decode={decode_vals[i]*1000}")
            trace.append(f"RenameSortInsts={rename_vals[i]*1000}")
            trace.append(f"BlockFromROB={block_rob[i]}")
            trace.append(f"BlockFromRF={block_rf[i]}")
            trace.append(f"BlockFromIQ={block_dpq[i] | block_serial[i]}")
            trace.append(f"BlockFromLQ={block_lq[i]}")
            trace.append(f"BlockFromSQ={block_sq[i]}")
            trace.append(f"Rename={rename_vals[i]*1000}")
            trace.append(f"Dispatch={dispatch_vals[i]*1000}")
            trace.append(f"InsertReadyList={rs_ready_vals[i]*1000}")
            trace.append(f"Issue={issue_vals[i]*1000}")
            trace.append(f"Memory={(issue_vals[i]+1)*1000 if issue_vals[i] != 0 else 0}")
            trace.append(f"Complete={complete_vals[i]*1000}")
            trace.append(f"CompleteMemory={complete_vals[i]*1000}")
            trace.append(f"CommitHead={(commit_vals[i]-1)*1000}")
            trace.append(f"Commit={commit_vals[i]*1000}")
            trace.append(f"ROB={rob_vals[i]}")
            trace.append(f"LQ={lq_vals[i]}")
            trace.append(f"SQ={sq_vals[i]}")
            trace.append(f"IQ={iq_vals[i]}")
            trace.append(f"FU={fu_vals[i]}")
            trace.append(f"SRC={','.join(src)}")
            trace.append(f"DST={dst}")

            trace_str = " : ".join(trace) + "\n"
            f.write(trace_str)

if __name__ == "__main__":
    log_path = "../log"
    if len(sys.argv) > 1:
        log_path = sys.argv[1]
    parse_log_file(log_path)