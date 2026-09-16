#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIM_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
STATE_DIR="$SCRIPT_DIR/.eda_remote"
MODE="sfence_stress_20260915"
TC="basicTest"
TS="memblock_dispatch_real_smoke_vseq"
MEMORY_MODELS="../../../../../build/rtl/array*_ext.v"
REMOTE_HOST="${REMOTE_HOST:-172.28.10.101}"
WAVE="${WAVE:-on}"
VERDI_PLI="/nfs/tools/synopsys/verdi/R-2020.12-SP1/share/PLI/VCS/LINUX64"
REMOTE_BOOTSTRAP="${REMOTE_BOOTSTRAP:-source /usr/share/Modules/init/bash >/dev/null 2>&1 || true; module load synopsys/vcs/Q-2020.03-SP2 license; module load synopsys/verdi/R-2020.12-SP1 license; export VERDI_HOME=/nfs/tools/synopsys/verdi/R-2020.12-SP1; export NOVAS_HOME=\$VERDI_HOME; export LD_LIBRARY_PATH=\$VERDI_HOME/share/PLI/VCS/LINUX64:\$LD_LIBRARY_PATH}"

run_make() {
    make -C "$SIM_DIR" "$@" \
        REMOTE_HOST="$REMOTE_HOST" \
        REMOTE_BOOTSTRAP="$REMOTE_BOOTSTRAP" \
        REMOTE_STATE_DIR="$STATE_DIR" \
        tc="$TC" ts="$TS" mode="$MODE" wave="$WAVE" \
        plus_file=../seq/plus_cfg partcmp_op=on </dev/null
}

run_batch_file() {
    local cfg="$1" seed_file="$2"
    while read -r seed; do
        [[ -z "$seed" || "$seed" == \#* ]] && continue
        echo "[SFENCE] cfg=$cfg seed=$seed"
        run_make eda_batch_run cfg="$cfg" seed="$seed" \
            note="sfence_${cfg}_${seed}"
    done < "$SCRIPT_DIR/$seed_file"
}

mkdir -p "$STATE_DIR"
echo "[SFENCE] compile shared VCS image"
run_make eda_compile cfg=tc_sfence_stress_1k seed=1001 \
    note=sfence_compile_20260915 \
    udf="-P $VERDI_PLI/novas.tab $VERDI_PLI/pli.a $MEMORY_MODELS"
run_batch_file tc_sfence_stress_1k seeds_1k.txt
run_batch_file tc_sfence_stress_10k seeds_10k.txt
echo "[SFENCE] batch completed; inspect $SIM_DIR/$MODE/log"
