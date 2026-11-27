#!/bin/sh
# gpl_driven.sh PART LOG OUT: whether a part's global placement (LOG, its
# 3_3_place_gp.log) ran with both its drivers; the counts go to OUT, and a
# part without either fails.
#
# Neither pass says so in the exit status when it does not run. The
# timing-driven pass prints GPL-0100 per iteration. The routability-driven
# pass prints GPL-0040 per inflation round only, and none when congestion
# never asks for inflation (the single top's parent: 62 routability-mode
# iterations, no inflation); GPL-1017 counts its iterations either way.
# The pin pull's GPL-0198 is counted, not required.
set -eu
part=$1
log=$2
out=$3
if [ ! -r "$log" ]; then
  echo "$part: no global placement log $log" >&2
  exit 1
fi
timing=$(grep -c 'GPL-0100' "$log" || true)
routability=$(grep -o 'GPL-1017\] Routability mode iteration count: *[0-9]*' "$log" | grep -o '[0-9][0-9]*$' | tail -n 1)
routability=${routability:-0}
pull=$(grep -c 'GPL-0198' "$log" || true)
echo "$part: $timing timing-driven iterations, $routability routability-mode iterations, $pull pin pulls" | tee "$out"
if [ "$timing" -gt 0 ] && [ "$routability" -gt 0 ]; then
  exit 0
fi
echo "$part: global placement ran without one of its drivers" >&2
exit 1
