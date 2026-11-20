#!/bin/sh
# gpl_driven_test.sh GATE PARENT NO_ROUTABILITY NO_TIMING: the
# placement-driver gate on three logs cut from global placement's: a parent
# like run 31's, both drivers on and the routability pass never inflating
# (62 routability-mode iterations, no GPL-0040), passes; a log without
# GPL-1017 (routability off) and one without GPL-0100 (timing off) fail.
set -u
gate=$1
out=${TEST_TMPDIR:-.}/gpl_driven.txt
status=0

check() {
  log=$1
  want=$2
  line=$3
  if "$gate" part "$log" "$out" >/dev/null 2>&1; then got=pass; else got=fail; fi
  if [ "$got" != "$want" ]; then
    echo "FAIL: $log should $want, it did $got"
    status=1
  elif [ "$(cat "$out")" != "$line" ]; then
    echo "FAIL: $log: '$(cat "$out")', expected '$line'"
    status=1
  else
    echo "ok: $log $got"
  fi
}

check "$2" pass "part: 2 timing-driven iterations, 62 routability-mode iterations, 0 pin pulls"
check "$3" fail "part: 2 timing-driven iterations, 0 routability-mode iterations, 0 pin pulls"
check "$4" fail "part: 0 timing-driven iterations, 62 routability-mode iterations, 0 pin pulls"
exit $status
